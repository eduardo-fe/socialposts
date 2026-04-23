# =============================================================================
# EPL 2025-26  |  Neural Network Goal & Result Predictor
# =============================================================================
#
# OVERVIEW
# --------
# This script builds a neural-network model to predict match scores in the
# 2025-26 Premier League season.  Two separate networks are trained:
#   - nn_gf : predicts goals scored  by the focal team in a match
#   - nn_ga : predicts goals conceded by the focal team in a match
#
# From these two predictions we derive a scoreline, result (W/D/L) and points,
# then sum across each club's remaining fixtures to project final standings.
#
# DATA SOURCES
# ------------
# - Match results  : SportRadar API (via live sports feed), GW22-33, 2025-26
# - Squad values   : Transfermarkt / Squawka, figures as of September 2025
#
# INPUTS  (training data)
# -----------------------
# Each row = one team's perspective in one completed match.
# Features used by the model:
#   team_value  : market value of focal team's squad (€m)
#   opp_value   : market value of opponent's squad   (€m)
#   home        : 1 = focal team played at home, 0 = away
#   team_form5  : focal team's points per match over last 5 games (0–1 scale)
#   opp_form5   : opponent's points per match over last 5 games   (0–1 scale)
#
# OUTPUTS (test data, remaining fixtures for ARS / MCI / CFC / AVL / MUN)
# -------
#   pred_gf     : predicted goals scored   (continuous, 1 d.p.)
#   pred_ga     : predicted goals conceded (continuous, 1 d.p.)
#   pred_gf_r   : rounded scoreline — goals for
#   pred_ga_r   : rounded scoreline — goals against
#   pred_result : W / D / L
#   pred_pts    : 3 / 1 / 0
#
# REQUIREMENTS
# ------------
# Only base R + the 'nnet' package (ships with every R installation).
# No external packages needed.
# =============================================================================


# =============================================================================
# SECTION 1 — SQUAD VALUES
# =============================================================================
# Squad values are fixed at the start of the season (Sep 2025) and serve as
# a proxy for overall team quality.  They are expressed in €millions and sourced
# from Transfermarkt/Squawka.  These are deliberately static: using transfer
# deadlines rather than week-by-week updates keeps the model clean.

squad_values <- c(
  ARS = 1330,  # Arsenal
  MCI = 1290,  # Manchester City
  LFC = 1100,  # Liverpool
  CFC = 1080,  # Chelsea
  TOT =  891,  # Tottenham
  MUN =  748,  # Manchester United
  NEW =  650,  # Newcastle United
  NFO =  589,  # Nottingham Forest
  AVL =  547,  # Aston Villa
  BRI =  462,  # Brighton
  CRY =  432,  # Crystal Palace
  BRE =  392,  # Brentford
  BOU =  389,  # Bournemouth
  WHU =  372,  # West Ham
  EVE =  320,  # Everton
  FUL =  292,  # Fulham
  LEE =  296,  # Leeds United
  SUN =  292,  # Sunderland
  WOL =  287,  # Wolves
  BUR =  230   # Burnley
)


# =============================================================================
# SECTION 2 — LOAD TRAINING DATA
# =============================================================================
# Read the pre-built CSV that contains every completed match from GW22-33.
# Each match appears TWICE in the dataset: once from each team's perspective.
# This doubles the usable rows and ensures the model learns symmetrically.
#
# The CSV was generated from live API data and contains:
#   match_id, date, team, opponent, home, gf, ga, result, points,
#   team_value, opp_value, team_form5, opp_form5
#
# If you want to regenerate it from scratch, run build_data.R first.

training <- read.csv("epl_training_data.csv", stringsAsFactors = FALSE)

cat("=== Training data loaded ===\n")
cat(sprintf("  Rows   : %d\n", nrow(training)))
cat(sprintf("  Teams  : %d\n", length(unique(training$team))))
cat(sprintf("  Dates  : %s  to  %s\n",
            min(training$date), max(training$date)))
cat("\nFirst 6 rows:\n")
print(head(training[, c("date","team","opponent","home","gf","ga",
                        "team_value","opp_value","team_form5","opp_form5")]))


# =============================================================================
# SECTION 3 — FEATURE SCALING
# =============================================================================
# Neural networks are sensitive to the scale of inputs.  If one feature ranges
# from 0 to 1300 and another from 0 to 1, the large-valued feature will
# dominate the gradient updates during training, potentially preventing the
# network from learning useful patterns from smaller-ranged features.
#
# Solution: min-max normalisation — rescale every input to the [0, 1] range.
#   scaled_x = (x - x_min) / (x_max - x_min)
#
# We compute the min/max from the TRAINING data and reuse the SAME values
# when scoring the test data.  This is important: if you re-scale test data
# using its own range, the transformations won't match and predictions will
# be on a different scale.

tv_min <- min(squad_values)   # 230  (Burnley)
tv_max <- max(squad_values)   # 1330 (Arsenal)

scale_value <- function(x) (x - tv_min) / (tv_max - tv_min)

# form5 is already in [0, 1] so no scaling needed for those columns.
# home is binary (0/1) — also no scaling required.

# Build the feature matrix X for training
X_train <- matrix(
  c(
    scale_value(training$team_value),  # column 1: scaled squad value of focal team
    scale_value(training$opp_value),   # column 2: scaled squad value of opponent
    training$home,                     # column 3: home advantage flag
    training$team_form5,               # column 4: focal team's recent form (0-1)
    training$opp_form5                 # column 5: opponent's recent form    (0-1)
  ),
  ncol = 5,
  dimnames = list(NULL, c("tv_s", "ov_s", "home", "tf5", "of5"))
)

# Targets: what we want the model to predict
y_gf <- training$gf   # goals scored
y_ga <- training$ga   # goals conceded


# =============================================================================
# SECTION 4 — NEURAL NETWORK ARCHITECTURE
# =============================================================================
# We use the nnet() function from the 'nnet' package (base R).
#
# Architecture: a single-hidden-layer feed-forward network (also called
# a multilayer perceptron, or MLP).
#
#   Input layer  : 5 nodes (one per feature)
#   Hidden layer : 12 nodes with sigmoid activation (non-linear transformation)
#   Output layer : 1 node with linear activation (regression, not classification)
#
# Key parameters:
#   size   = 12    — number of hidden neurons.  More neurons = more capacity
#                    to fit complex patterns, but also more risk of overfitting
#                    on small datasets.  12 is a reasonable middle ground here.
#   linout = TRUE  — linear output.  We're predicting a continuous number
#                    (goals), not a class probability, so we need a linear
#                    activation at the output.
#   decay  = 0.01  — L2 weight regularisation (weight decay).  Penalises
#                    large weights and acts as a brake against overfitting.
#                    Higher decay = smoother model but potentially underfits.
#   maxit  = 2000  — maximum training iterations (gradient descent steps).
#                    Training stops early if the loss converges before this.
#   set.seed()     — fixes the random weight initialisation so results are
#                    reproducible across runs.

library(nnet)

set.seed(2026)
nn_gf <- nnet(
  x      = X_train,
  y      = y_gf,
  size   = 12,
  linout = TRUE,
  decay  = 0.01,
  maxit  = 2000,
  trace  = FALSE   # suppress iteration-by-iteration output
)

set.seed(2026)
nn_ga <- nnet(
  x      = X_train,
  y      = y_ga,
  size   = 12,
  linout = TRUE,
  decay  = 0.01,
  maxit  = 2000,
  trace  = FALSE
)

# --- Training diagnostics ---
# RMSE (Root Mean Squared Error): average prediction error in goals.
# An RMSE of ~0.8 means predictions are typically within ~1 goal of the
# actual result on the training set.
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

pred_gf_train <- pmax(predict(nn_gf, X_train), 0)
pred_ga_train <- pmax(predict(nn_ga, X_train), 0)

cat("\n=== Model training diagnostics ===\n")
cat(sprintf("  GF model  RMSE on training set: %.3f goals\n",
            rmse(y_gf, pred_gf_train)))
cat(sprintf("  GA model  RMSE on training set: %.3f goals\n",
            rmse(y_ga, pred_ga_train)))
cat("  (Lower is better; expect ~0.6-1.0 for football data)\n")


# =============================================================================
# SECTION 5 — LOAD TEST DATA & SCORE
# =============================================================================
# The test data contains the remaining fixtures for our five clubs of interest:
# Arsenal, Man City, Chelsea, Aston Villa, Manchester United.
#
# The test CSV contains:
#   date, team, opponent, home, team_value, opp_value, team_form5, opp_form5
#
# team_form5 and opp_form5 here reflect each team's form going into these
# matches — computed from the last 5 completed games in the training window.

test <- read.csv("epl_test_data.csv", stringsAsFactors = FALSE)

cat("\n=== Test data loaded ===\n")
cat(sprintf("  Fixtures : %d\n", nrow(test)))
cat(sprintf("  Clubs    : %s\n", paste(unique(test$team), collapse=", ")))
cat("\nTest fixtures:\n")
print(test[, c("date","team","opponent","home","team_value","opp_value",
               "team_form5","opp_form5")])

# Build the test feature matrix using the SAME scaling as training
X_test <- matrix(
  c(
    scale_value(test$team_value),
    scale_value(test$opp_value),
    test$home,
    test$team_form5,
    test$opp_form5
  ),
  ncol = 5,
  dimnames = list(NULL, c("tv_s", "ov_s", "home", "tf5", "of5"))
)

# Generate predictions.  pmax(..., 0) clips any negative predictions to zero
# because you can't score negative goals.
test$pred_gf   <- round(pmax(predict(nn_gf, X_test), 0), 1)
test$pred_ga   <- round(pmax(predict(nn_ga, X_test), 0), 1)

# Round to nearest integer for a discrete scoreline
test$pred_gf_r <- as.integer(round(test$pred_gf))
test$pred_ga_r <- as.integer(round(test$pred_ga))

# Derive result and points from the rounded scoreline
test$pred_result <- ifelse(test$pred_gf_r > test$pred_ga_r, "W",
                    ifelse(test$pred_gf_r == test$pred_ga_r, "D", "L"))

test$pred_pts <- ifelse(test$pred_result == "W", 3L,
                 ifelse(test$pred_result == "D", 1L, 0L))

test$scoreline <- paste0(test$pred_gf_r, "-", test$pred_ga_r)
test$venue     <- ifelse(test$home == 1L, "Home", "Away")


# =============================================================================
# SECTION 6 — PRINT PREDICTIONS BY CLUB
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  PREDICTED MATCH RESULTS — remaining fixtures\n")
cat("  (continuous scores show model output before rounding)\n")
cat("=================================================================\n")

for (club in c("ARS", "MCI", "CFC", "AVL", "MUN")) {
  d <- test[test$team == club, ]
  cat(sprintf("\n--- %s  (%d fixtures) ---\n", club, nrow(d)))
  out <- d[, c("date", "opponent", "venue",
               "pred_gf", "pred_ga",    # continuous model output
               "scoreline",             # rounded
               "pred_result", "pred_pts")]
  names(out) <- c("Date", "Opponent", "Venue",
                  "xGF", "xGA", "Score", "Result", "Pts")
  print(out, row.names = FALSE)
}


# =============================================================================
# SECTION 7 — PROJECTED FINAL STANDINGS
# =============================================================================
# Start from each club's current points total (as of GW33, Apr 20 2026),
# add the predicted points from remaining fixtures, and rank the five clubs.

current_pts <- c(ARS = 70L, MCI = 67L, MUN = 58L, AVL = 58L, CFC = 48L)

# Sum predicted points per club across all their remaining test fixtures
extra_pts <- tapply(test$pred_pts, test$team, sum)

projected <- data.frame(
  Team          = names(current_pts),
  Current_pts   = as.integer(current_pts),
  Predicted_add = as.integer(extra_pts[names(current_pts)]),
  stringsAsFactors = FALSE
)
projected$Final_pts <- projected$Current_pts + projected$Predicted_add
projected <- projected[order(-projected$Final_pts), ]
rownames(projected) <- NULL

cat("\n")
cat("=================================================================\n")
cat("  PROJECTED FINAL STANDINGS — top 5\n")
cat("  (current points + predicted points from remaining games)\n")
cat("=================================================================\n")
print(projected)


# =============================================================================
# SECTION 8 — EXPORT RESULTS
# =============================================================================
# Save the full predictions table so you can inspect, plot, or share it.

write.csv(test, "epl_predictions.csv", row.names = FALSE)
cat("\nFull predictions written to epl_predictions.csv\n")
cat("=== Script complete ===\n")
