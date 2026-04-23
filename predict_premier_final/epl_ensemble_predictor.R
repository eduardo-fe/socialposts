# =============================================================================
# EPL 2025-26  |  Ensemble Score Predictor
# =============================================================================
#
# WHAT THIS SCRIPT DOES
# ----------------------
# Predicts the SCORELINE (goals for, goals against) of each remaining fixture
# for Arsenal, Man City, Chelsea, Aston Villa, and Manchester United.
#
# The prediction chain is:
#   predict scoreline --> W/D/L --> points per game --> season points --> table
#
# MODEL ARCHITECTURE
# -------------------
#   Stage 1a : Random Forest  (bagging,  500 trees)
#   Stage 1b : GBM            (boosting, 300 trees)
#   Stage 2  : Neural Network (unstacked, sigmoid, trained on original features)
#
# FEATURES  (11 total)
# ----------------------
#   tv_s           — team squad value, scaled [0,1]
#   ov_s           — opponent squad value, scaled [0,1]
#   home           — 1 = home, 0 = away
#   tf5            — team rolling 5-match form (pts won / max possible)
#   tf5_lag1       — team form one match ago  [lagged form]
#   form_trend     — tf5 - tf5_lag1  (positive = improving)
#   of5            — opponent rolling 5-match form
#   opp_tf5_lag1   — opponent form one match ago
#   opp_form_trend — opponent form trend
#   team_xgf       — team season-average xG scored per game
#   opp_xgf        — opponent season-average xG scored per game
#
# NOTE ON xG
# -----------
# Match-level xG is behind paywalls for this season (Understat blocks
# server-side scraping). We use season-average xG/game as a proxy for
# attacking quality. This is a static team attribute, not match-level signal.
# Once match-level xG is available, replace team_xgf/opp_xgf with per-match
# values for a materially better feature.
#
# NOTE ON STACKING
# -----------------
# Earlier versions used a stacked neural network (RF+GBM predictions as
# extra inputs). With only 202 training rows this produced flat Garson
# importance scores — the network could not discriminate between 15 inputs.
# The current version trains the neural network directly on the 11 base
# features, which produces sharper importance scores and is more interpretable.
#
# DATA SOURCES
# ------------
#   Match results : SportRadar API, GWs 22-33, 2025-26 season
#   Squad values  : Transfermarkt / Squawka, September 2025
#   xG per game   : StatMuse / FootyStats, season averages as of GW33
#
# REQUIREMENTS
# ------------
#   install.packages(c("randomForest", "gbm", "neuralnet", "NeuralNetTools"))
# =============================================================================


# =============================================================================
# SECTION 1 — PACKAGES
# =============================================================================
library(randomForest)
library(gbm)
library(neuralnet)
library(NeuralNetTools)

set.seed(2026)


# =============================================================================
# SECTION 2 — SQUAD VALUES AND xG SEASON AVERAGES
# =============================================================================
squad_values <- c(
  ARS=1330, MCI=1290, LFC=1100, CFC=1080, TOT=891,
  MUN=748,  NEW=650,  NFO=589,  AVL=547,  BRI=462,
  CRY=432,  BRE=392,  BOU=389,  WHU=372,  EVE=320,
  FUL=292,  LEE=296,  SUN=292,  WOL=287,  BUR=230
)

# Season-average xG per game (StatMuse, as of GW33)
# xgf = expected goals scored per game
# xga = expected goals conceded per game
season_xg <- data.frame(stringsAsFactors=FALSE,
  team = c("ARS","MCI","LFC","CFC","TOT","MUN","NEW","NFO","AVL","BRI",
           "CRY","BRE","BOU","WHU","EVE","FUL","LEE","SUN","WOL","BUR"),
  xgf  = c(1.79,1.84,1.57,1.92,1.11,1.75,1.58,1.10,1.26,1.50,
            1.56,1.59,1.63,1.12,1.21,1.24,1.42,0.88,0.92,0.88),
  xga  = c(0.71,1.15,1.14,1.41,1.38,1.26,1.20,1.46,1.46,1.46,
            1.41,1.40,1.44,1.68,1.51,1.46,1.50,1.40,1.47,1.96)
)


# =============================================================================
# SECTION 3 — LOAD AND ENRICH TRAINING DATA
# =============================================================================
# Read the base CSV (match results, squad values, rolling form).
# We add four new lagged-form features here so the model can detect
# whether a team's form is improving or deteriorating, not just its level.

training_raw <- read.csv("epl_training_data.csv", stringsAsFactors=FALSE)
training_raw$date <- as.Date(training_raw$date)

# Sort by team then date — mandatory for lag to be meaningful
training_raw <- training_raw[order(training_raw$team, training_raw$date), ]

# ---- Team lagged form ----
# tf5_lag1   : form value from the previous match (shift by 1 within team)
# form_trend : change in form = tf5 - tf5_lag1
#              positive = improving, negative = declining, zero = stable
training_raw$tf5_lag1 <- ave(
  training_raw$team_form5,
  training_raw$team,
  FUN = function(x) c(NA, head(x, -1))
)
training_raw$form_trend <- training_raw$team_form5 - training_raw$tf5_lag1

# First match per team has no prior — fill with current form (trend = 0)
na_idx <- is.na(training_raw$tf5_lag1)
training_raw$tf5_lag1[na_idx]   <- training_raw$team_form5[na_idx]
training_raw$form_trend[na_idx] <- 0

# ---- Opponent lagged form ----
# Build a lookup table of lag values by team + date, then join as opponent
lag_lookup <- training_raw[, c("team","date","tf5_lag1","form_trend")]

training_raw <- merge(
  training_raw,
  setNames(lag_lookup, c("opponent","date","opp_tf5_lag1","opp_form_trend")),
  by  = c("opponent","date"),
  all.x = TRUE
)

# Fill missing opponent lags (first match of season for that opponent)
training_raw$opp_tf5_lag1[is.na(training_raw$opp_tf5_lag1)] <-
  training_raw$opp_form5[is.na(training_raw$opp_tf5_lag1)]
training_raw$opp_form_trend[is.na(training_raw$opp_form_trend)] <- 0

# ---- Attach xG season averages ----
# Drop any stale xG columns from previous runs to avoid .x/.y suffix collisions
xg_cols <- c("xgf","xga","team_xgf","team_xga","opp_xgf","opp_xga",
             "opp_xgf.x","opp_xga.x","opp_xgf.y","opp_xga.y")
training_raw <- training_raw[, !colnames(training_raw) %in% xg_cols]

team_xg        <- season_xg
names(team_xg) <- c("team","team_xgf","team_xga")

opp_xg         <- season_xg
names(opp_xg)  <- c("opponent","opp_xgf","opp_xga")

training_raw <- merge(training_raw, team_xg, by="team",     all.x=TRUE)
training_raw <- merge(training_raw, opp_xg,  by="opponent", all.x=TRUE)

training_raw <- training_raw[order(training_raw$team, training_raw$date), ]

cat("=== Training data ===\n")
cat(sprintf("  Rows  : %d\n", nrow(training_raw)))
cat(sprintf("  Teams : %d\n", length(unique(training_raw$team))))
cat(sprintf("  Dates : %s  to  %s\n",
            min(training_raw$date), max(training_raw$date)))


# =============================================================================
# SECTION 4 — TEST DATA  (all 20 teams, all remaining fixtures)
# =============================================================================
# epl_test_all_teams.csv contains 100 rows (50 matches x 2 perspectives).
# Built from live SportRadar fixtures; already contains all required features:
# form, lag, xG, squad values — ready to use directly.

test <- read.csv("epl_test_all_teams.csv", stringsAsFactors=FALSE)
test$date <- as.Date(test$date)
test <- test[order(test$date, test$team), ]

cat(sprintf("  Test  : %d rows | Clubs: %s\n\n",
            nrow(test), paste(sort(unique(test$team)), collapse=", ")))


# =============================================================================
# SECTION 5 — FEATURE MATRIX
# =============================================================================
# 11 features: context (3) + team form with lag (3) +
#              opponent form with lag (3) + xG (2)

tv_min <- 230; tv_max <- 1330
scl    <- function(x) (x - tv_min) / (tv_max - tv_min)

make_X <- function(df) {
  data.frame(
    tv_s           = scl(df$team_value),
    ov_s           = scl(df$opp_value),
    home           = df$home,
    tf5            = df$team_form5,
    tf5_lag1       = df$tf5_lag1,
    form_trend     = df$form_trend,
    of5            = df$opp_form5,
    opp_tf5_lag1   = df$opp_tf5_lag1,
    opp_form_trend = df$opp_form_trend,
    team_xgf       = df$team_xgf,
    opp_xgf        = df$opp_xgf
  )
}

X_train      <- make_X(training_raw)
X_test       <- make_X(test)
y_gf         <- training_raw$gf
y_ga         <- training_raw$ga
feature_cols <- colnames(X_train)

cat("Features (", length(feature_cols), "):\n")
cat(" ", paste(feature_cols, collapse=", "), "\n\n")


# =============================================================================
# SECTION 6 — RANDOM FOREST
# =============================================================================
# ntree=500, mtry=3 (approx sqrt(11)), nodesize=5, importance=TRUE

cat("=== Training Random Forest ===\n")
rf_gf <- randomForest(x=X_train, y=y_gf,
                      ntree=500, mtry=3, nodesize=5, importance=TRUE)
rf_ga <- randomForest(x=X_train, y=y_ga,
                      ntree=500, mtry=3, nodesize=5, importance=TRUE)
cat("  Done.\n")


# =============================================================================
# SECTION 7 — GBM
# =============================================================================
cat("=== Training GBM ===\n")
df_all_gf <- cbind(X_train, y=y_gf)
df_all_ga <- cbind(X_train, y=y_ga)

gbm_gf <- gbm(y ~ ., data=df_all_gf, distribution="gaussian",
              n.trees=300, interaction.depth=3, shrinkage=0.05,
              n.minobsinnode=5, verbose=FALSE)
gbm_ga <- gbm(y ~ ., data=df_all_ga, distribution="gaussian",
              n.trees=300, interaction.depth=3, shrinkage=0.05,
              n.minobsinnode=5, verbose=FALSE)
cat("  Done.\n\n")


# =============================================================================
# SECTION 8 — NEURAL NETWORK  (sigmoid, unstacked)
# =============================================================================
# The network is trained directly on the 11 base features — no stacking.
# Reason: with 202 rows, the stacked architecture spread weight too diffusely
# across 15 inputs; the unstacked network produces sharper importance scores.
#
# Activation: logistic sigmoid (neuralnet default) — f(x) = 1 / (1 + exp(-x))
# ReLU was attempted but failed to converge on this small dataset (202 rows).
# Sigmoid is smoother and more stable for shallow networks with few training
# rows. It also supports Garson's importance algorithm, which requires
# differentiable activations.
#
# hidden = 8 nodes, linear.output = TRUE (regression, not classification)

train_df_gf <- as.data.frame(X_train); train_df_gf$y <- y_gf
train_df_ga <- as.data.frame(X_train); train_df_ga$y <- y_ga
formula_str <- paste("y ~", paste(feature_cols, collapse=" + "))

cat("=== Training Neural Network (sigmoid, 8 hidden nodes) ===\n")

set.seed(2026)
nn_gf <- neuralnet(
  formula       = as.formula(formula_str),
  data          = train_df_gf,
  hidden        = 8,
  linear.output = TRUE,   # sigmoid hidden, linear output (regression)
  lifesign      = "minimal",
  stepmax       = 1e6
)

set.seed(2026)
nn_ga <- neuralnet(
  formula       = as.formula(formula_str),
  data          = train_df_ga,
  hidden        = 8,
  linear.output = TRUE,
  lifesign      = "minimal",
  stepmax       = 1e6
)
cat("  Done.\n\n")


# =============================================================================
# SECTION 9 — RMSE COMPARISON
# =============================================================================
rmse <- function(y, yh) sqrt(mean((y - pmax(yh, 0))^2))

pred_nn_gf_tr <- pmax(
  neuralnet::compute(nn_gf, as.data.frame(X_train)[, feature_cols])$net.result, 0)
pred_nn_ga_tr <- pmax(
  neuralnet::compute(nn_ga, as.data.frame(X_train)[, feature_cols])$net.result, 0)

stage_rmse <- data.frame(
  Model   = c("Random Forest","GBM","Neural Network"),
  RMSE_GF = round(c(
    rmse(y_gf, pmax(predict(rf_gf, X_train), 0)),
    rmse(y_gf, pmax(predict(gbm_gf, X_train, n.trees=300), 0)),
    rmse(y_gf, pred_nn_gf_tr)), 3),
  RMSE_GA = round(c(
    rmse(y_ga, pmax(predict(rf_ga, X_train), 0)),
    rmse(y_ga, pmax(predict(gbm_ga, X_train, n.trees=300), 0)),
    rmse(y_ga, pred_nn_ga_tr)), 3)
)
cat("=== Training RMSE (lower = better fit; flatters tree models) ===\n")
print(stage_rmse, row.names=FALSE)
cat("\n")


# =============================================================================
# SECTION 10 — VARIABLE IMPORTANCE
# =============================================================================
feature_labels <- c(
  tv_s           = "Team squad value",
  ov_s           = "Opponent squad value",
  home           = "Home advantage",
  tf5            = "Team form (last 5)",
  tf5_lag1       = "Team form lag-1",
  form_trend     = "Team form trend",
  of5            = "Opponent form (last 5)",
  opp_tf5_lag1   = "Opponent form lag-1",
  opp_form_trend = "Opponent form trend",
  team_xgf       = "Team xG/game",
  opp_xgf        = "Opponent xG/game"
)

# --- Random Forest %IncMSE ---
imp_rf_gf <- importance(rf_gf, type=1)
imp_rf_ga <- importance(rf_ga, type=1)

rf_imp <- data.frame(
  Feature = feature_labels[rownames(imp_rf_gf)],
  RF_GF   = round(imp_rf_gf[,1], 2),
  RF_GA   = round(imp_rf_ga[rownames(imp_rf_gf), 1], 2),
  row.names = NULL
)

cat("=================================================================\n")
cat("  VARIABLE IMPORTANCE — Random Forest (%IncMSE)\n")
cat("=================================================================\n")
print(rf_imp[order(-rf_imp$RF_GF), ], row.names=FALSE)

# --- GBM relative influence ---
gbm_imp_gf <- summary(gbm_gf, n.trees=300, plotit=FALSE)
gbm_imp_ga <- summary(gbm_ga, n.trees=300, plotit=FALSE)

gbm_imp <- data.frame(
  Feature = feature_labels[gbm_imp_gf$var],
  GBM_GF  = round(gbm_imp_gf$rel.inf, 2),
  GBM_GA  = round(gbm_imp_ga$rel.inf[match(gbm_imp_gf$var, gbm_imp_ga$var)], 2),
  row.names = NULL
)

cat("\n=================================================================\n")
cat("  VARIABLE IMPORTANCE — GBM (relative influence, sums to 100)\n")
cat("=================================================================\n")
print(gbm_imp, row.names=FALSE)

# --- Garson's algorithm (Neural Network) ---
# garson() returns a data frame with rel_imp as the only column;
# feature names are stored as row names, not a column — convert before merging
imp_nn_gf <- garson(nn_gf, bar_plot=FALSE)
imp_nn_ga <- garson(nn_ga, bar_plot=FALSE)

imp_nn_gf$Feature <- rownames(imp_nn_gf)
imp_nn_ga$Feature <- rownames(imp_nn_ga)

imp_nn <- merge(imp_nn_gf, imp_nn_ga, by="Feature", suffixes=c("_gf","_ga"))
names(imp_nn)[names(imp_nn)=="rel_imp_gf"] <- "NN_GF"
names(imp_nn)[names(imp_nn)=="rel_imp_ga"] <- "NN_GA"
imp_nn$NN_GF <- round(imp_nn$NN_GF, 4)
imp_nn$NN_GA <- round(imp_nn$NN_GA, 4)

cat("\n=================================================================\n")
cat("  VARIABLE IMPORTANCE — Neural Network (Garson's algorithm)\n")
cat("  Sums to 1. Wider spread = network learned clear signal.\n")
cat("=================================================================\n")
cat("\n  Goals scored:\n")
print(imp_nn[order(-imp_nn$NN_GF), c("Feature","NN_GF")], row.names=FALSE)
cat("\n  Goals conceded:\n")
print(imp_nn[order(-imp_nn$NN_GA), c("Feature","NN_GA")], row.names=FALSE)

# --- Combined table: all three models ---
norm01 <- function(x) {
  r <- range(x, na.rm=TRUE)
  if (diff(r)==0) rep(0.5, length(x)) else (x - r[1]) / diff(r)
}

feats <- rownames(imp_rf_gf)
imp_combined <- data.frame(
  Feature  = feature_labels[feats],
  RF_GF    = norm01(imp_rf_gf[feats,1]),
  RF_GA    = norm01(imp_rf_ga[feats,1]),
  GBM_GF   = norm01(gbm_imp_gf$rel.inf[match(feats, gbm_imp_gf$var)]),
  GBM_GA   = norm01(gbm_imp_ga$rel.inf[match(feats, gbm_imp_ga$var)])
)
imp_combined$Mean <- round(rowMeans(imp_combined[,2:5], na.rm=TRUE), 3)
imp_combined <- imp_combined[order(-imp_combined$Mean),
                             c("Feature","RF_GF","RF_GA","GBM_GF","GBM_GA","Mean")]

cat("\n--- Combined normalised importance (RF + GBM, 0=least, 1=most) ---\n")
print(imp_combined, row.names=FALSE)

# Full export table includes NN
imp_full <- Reduce(function(a,b) merge(a,b,by="Feature",all=TRUE), list(
  rf_imp[, c("Feature","RF_GF","RF_GA")],
  gbm_imp[, c("Feature","GBM_GF","GBM_GA")],
  imp_nn[, c("Feature","NN_GF","NN_GA")]
))
write.csv(imp_full[order(-imp_full$RF_GF), ],
          "epl_feature_importance.csv", row.names=FALSE)


# =============================================================================
# SECTION 11 — NETWORK PLOTS
# =============================================================================
# Blue = positive weights, red = negative, line thickness = magnitude.

plotnet(nn_gf,
        x_names    = feature_cols,
        y_names    = "goals_for",
        circle_cex = 3,
        cex_val    = 0.55,
        bord_col   = "steelblue",
        pos_col    = "steelblue",
        neg_col    = "tomato")
title("Neural network — goals scored", line=2.5)

plotnet(nn_ga,
        x_names    = feature_cols,
        y_names    = "goals_against",
        circle_cex = 3,
        cex_val    = 0.55,
        bord_col   = "steelblue",
        pos_col    = "steelblue",
        neg_col    = "tomato")
title("Neural network — goals conceded", line=2.5)


# =============================================================================
# SECTION 12 — PREDICT REMAINING FIXTURES
# =============================================================================
# PRIMARY MODEL: Random Forest
# The neural network is retained for variable importance (Garson) only.
# Its test predictions are erratic on this small dataset — a known limitation
# of sigmoid networks with 202 training rows and 11 features. RF produces
# the lowest OOF RMSE and the most coherent scorelines.
#
# pred_gf / pred_ga   : RF continuous predictions
# pred_gf_r / pred_ga_r : rounded to nearest integer for the scoreline
# pred_result         : W / D / L derived from RF scoreline
# pred_pts            : 3 / 1 / 0

test_df <- as.data.frame(X_test)

# RF predictions (primary)
rf_gf_test <- pmax(predict(rf_gf, X_test), 0)
rf_ga_test <- pmax(predict(rf_ga, X_test), 0)

test$pred_gf   <- round(rf_gf_test, 1)
test$pred_ga   <- round(rf_ga_test, 1)
test$pred_gf_r <- as.integer(round(test$pred_gf))
test$pred_ga_r <- as.integer(round(test$pred_ga))

test$pred_result <- ifelse(test$pred_gf_r > test$pred_ga_r, "W",
                    ifelse(test$pred_gf_r == test$pred_ga_r, "D", "L"))
test$pred_pts    <- ifelse(test$pred_result=="W", 3L,
                    ifelse(test$pred_result=="D", 1L, 0L))
test$rf_score    <- paste0(test$pred_gf_r, "-", test$pred_ga_r)
test$venue       <- ifelse(test$home==1L, "Home", "Away")

# GBM predictions (comparison)
test$gbm_score <- paste0(
  as.integer(round(pmax(predict(gbm_gf, X_test, n.trees=300), 0))), "-",
  as.integer(round(pmax(predict(gbm_ga, X_test, n.trees=300), 0))))

# NN predictions (shown for reference only — do not use for table projection)
nn_gf_test <- pmax(neuralnet::compute(nn_gf, test_df[, feature_cols])$net.result, 0)
nn_ga_test <- pmax(neuralnet::compute(nn_ga, test_df[, feature_cols])$net.result, 0)
test$nn_score <- paste0(
  as.integer(round(nn_gf_test)), "-",
  as.integer(round(nn_ga_test)))


# =============================================================================
# SECTION 13 — PRINT PREDICTIONS
# =============================================================================
cat("\n")
cat("=================================================================\n")
cat("  PREDICTED SCORELINES — remaining fixtures\n")
cat("  PRIMARY: RF  |  Cross-check: GBM  |  Reference: NN (not used)\n")
cat("  trend = team form direction (positive = improving)\n")
cat("=================================================================\n")

for (club in c("ARS","MCI","MUN","AVL","LFC","CFC","BRE","BOU",
               "BRI","EVE","SUN","FUL","CRY","NEW","LEE","NFO",
               "WHU","TOT","BUR","WOL")) {
  d   <- test[test$team == club, ]
  d   <- d[order(d$date), ]
  out <- d[, c("date","opponent","venue","form_trend",
               "rf_score","gbm_score","nn_score",
               "pred_result","pred_pts")]
  names(out)[4] <- "trend"
  cat(sprintf("\n--- %s (%d fixtures) ---\n", club, nrow(d)))
  print(out, row.names=FALSE)
}


# =============================================================================
# SECTION 14 — PROJECTED FINAL TABLE
# =============================================================================
current_pts <- c(
  MCI=70L, ARS=70L, MUN=58L, AVL=58L, LFC=55L, BRI=50L, BOU=49L,
  CFC=48L, BRE=48L, EVE=47L, SUN=46L, FUL=45L, CRY=43L, NEW=42L,
  LEE=40L, NFO=36L, WHU=33L, TOT=31L, BUR=20L, WOL=17L
)
extra_pts   <- tapply(test$pred_pts, test$team, sum)

projected <- data.frame(
  Team            = names(current_pts),
  Current_pts     = as.integer(current_pts),
  Games_remaining = as.integer(table(test$team)[names(current_pts)]),
  Predicted_add   = as.integer(extra_pts[names(current_pts)]),
  stringsAsFactors = FALSE
)
projected$Final_pts    <- projected$Current_pts + projected$Predicted_add
projected$Pts_per_game <- round(projected$Predicted_add /
                                projected$Games_remaining, 2)
projected <- projected[order(-projected$Final_pts), ]
rownames(projected) <- NULL

cat("\n")
cat("=================================================================\n")
cat("  PROJECTED FINAL STANDINGS — top 5\n")
cat("  Table is derived from scoreline predictions, not modelled directly\n")
cat("=================================================================\n")
print(projected)


# =============================================================================
# SECTION 15 — EXPORT
# =============================================================================
write.csv(
  test[order(test$team, test$date),
       c("date","team","opponent","venue","form_trend",
         "rf_score","gbm_score","nn_score",
         "pred_gf","pred_ga","pred_result","pred_pts")],
  "epl_ensemble_predictions.csv",
  row.names=FALSE
)

cat("\nFiles written:\n")
cat("  epl_ensemble_predictions.csv\n")
cat("  epl_feature_importance.csv\n")
cat("\n=== Done ===\n")
