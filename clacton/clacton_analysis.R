## ─── Clacton Elections Analysis, 2005–2026 ────────────────────────────────────
## Base R only (no external packages)

# ── Election-level data ───────────────────────────────────────────────────────

el <- data.frame(
  year        = c(2005, 2010, 2014, 2015, 2017, 2019, 2024, 2026),
  type        = c("Notional","GE","By-el.","GE","GE","GE","GE","By-el."),
  electorate  = c(68363, 67194, 69118, 68936, 68566, 70930, NA, NA),
  turnout_n   = c(42968, 43123, 35338, 44207, 44145, 43506, 45958, 35111),
  turnout_pct = c(62.9, 64.2, 51.2, 64.1, 63.7, 61.3, 58.0, 44.4)
)

# ── Party-level results ──────────────────────────────────────────────────────

con_share <- c(46.6, 53.0, 24.6, 36.7, 61.2, 72.3, 27.9,  0.0)
lab_share <- c(33.1, 25.0, 11.2, 14.4, 25.4, 15.5, 16.2,  0.0)
ld_share  <- c(14.1, 12.9,  1.3,  1.8,  2.0,  5.8,  4.4,  0.0)
ref_share <- c( 0.0,  0.0, 59.7, 44.4,  7.6,  0.0, 46.2, 63.3)
grn_share <- c( 0.0,  1.2,  1.9,  2.7,  1.6,  2.8,  4.2,  0.0)

ref_votes <- c(0, 0, 21113, 19642, 3357, 0, 21225, 22239)
con_votes <- c(20035, 22867, 8709, 16205, 27031, 31438, 12820, 0)
lab_votes <- c(14219, 10799, 3957, 6364, 11203, 6736, 7448, 0)

col_con <- "#0087DC"
col_lab <- "#DC241f"
col_ld  <- "#FDBB30"
col_ref <- "#12B6CF"
col_grn <- "#6AB023"
col_bin <- "#8B008B"

years <- el$year
x     <- seq_along(years)

# ── PLOT 1: Vote share evolution ─────────────────────────────────────────────

png("plot1_vote_share.png", width = 1400, height = 700, res = 150)
par(mar = c(4, 4, 3, 1))

plot(NULL, xlim = c(1, 8), ylim = c(0, 80),
     xaxt = "n", xlab = "", ylab = "Vote share (%)",
     main = "Clacton: Vote Share by Party, 2005\u20132026")
axis(1, at = x, labels = paste0(years, ifelse(el$type == "By-el.", "*", "")))
abline(v = which(el$type == "By-el."), col = "grey80", lty = 3)

draw_party <- function(shares, col) {
  ok <- shares > 0
  lines(x[ok], shares[ok], col = col, lwd = 2.5)
  points(x[ok], shares[ok], col = col, pch = 16, cex = 1.3)
}

draw_party(con_share, col_con)
draw_party(lab_share, col_lab)
draw_party(ld_share,  col_ld)
draw_party(ref_share, col_ref)
draw_party(grn_share, col_grn)

legend("topleft",
       legend = c("Conservative", "Labour", "Lib Dem", "UKIP/Reform", "Green"),
       col = c(col_con, col_lab, col_ld, col_ref, col_grn),
       lwd = 2.5, pch = 16, cex = 0.85, bg = "white")
mtext("* = by-election. 2026: no major-party candidates except Reform.",
      side = 1, line = 2.8, cex = 0.8, adj = 1, col = "grey40")
dev.off()

# ── PLOT 2: UKIP/Reform absolute votes ───────────────────────────────────────

png("plot2_reform_votes.png", width = 1200, height = 600, res = 150)
par(mar = c(4, 5, 3, 1))

bp <- barplot(ref_votes, names.arg = paste0(years, ifelse(el$type == "By-el.", "*", "")),
              col = col_ref, border = "white", ylim = c(0, 26000),
              main = "Clacton: UKIP/Reform Absolute Votes",
              ylab = "Votes", las = 1)
for (i in seq_along(ref_votes)) {
  if (ref_votes[i] > 0)
    text(bp[i], ref_votes[i] + 500, format(ref_votes[i], big.mark = ","), cex = 0.8)
}
text(bp[3], 23500, "Carswell", cex = 0.85, font = 3, col = "grey30")
text(bp[7], 23500, "Farage", cex = 0.85, font = 3, col = "grey30")
dev.off()

# ── PLOT 3: Turnout ──────────────────────────────────────────────────────────

png("plot3_turnout.png", width = 1200, height = 600, res = 150)
par(mar = c(4, 4, 3, 1))

bar_cols <- ifelse(el$type == "GE", "grey40",
            ifelse(el$type == "By-el.", "grey70", "grey85"))

bp <- barplot(el$turnout_pct, names.arg = paste0(years, ifelse(el$type == "By-el.", "*", "")),
              col = bar_cols, border = "white", ylim = c(0, 75),
              main = "Clacton: Turnout, 2005\u20132026",
              ylab = "Turnout (%)", las = 1)
for (i in seq_along(years)) {
  text(bp[i], el$turnout_pct[i] + 1.5, paste0(el$turnout_pct[i], "%"), cex = 0.8)
  text(bp[i], 3, format(el$turnout_n[i], big.mark = ","), cex = 0.7, col = "white", font = 2)
}
legend("topright", legend = c("General election", "By-election / notional"),
       fill = c("grey40", "grey70"), border = "white", cex = 0.85, bg = "white")
dev.off()

# ── PLOT 4: 2024 vs 2026 decomposition ──────────────────────────────────────

png("plot4_decomposition.png", width = 1000, height = 600, res = 150)
par(mar = c(4, 5, 4, 1))

reform <- c(21225, 22239)
other  <- c(45958 - 21225, 35111 - 22239)

bp <- barplot(rbind(reform, other), beside = FALSE,
              names.arg = c("2024 GE", "2026 By-el."),
              col = c(col_ref, "grey55"), border = "white",
              ylim = c(0, 52000), las = 1,
              main = "Farage's vote held; the opposition vanished",
              ylab = "Votes")
text(bp, reform / 2, format(reform, big.mark = ","), col = "white", font = 2, cex = 1.1)
text(bp, reform + other / 2, format(other, big.mark = ","), col = "white", font = 2, cex = 1.1)
text(bp, reform + other + 1000, paste0("Total: ", format(reform + other, big.mark = ",")), cex = 0.85)
legend("topright", legend = c("Reform", "Non-Reform"),
       fill = c(col_ref, "grey55"), border = "white", bg = "white")
dev.off()

# ── PLOT 5: Counterfactual ───────────────────────────────────────────────────

png("plot5_counterfactual.png", width = 1200, height = 600, res = 150)
par(mar = c(4.5, 4.5, 3, 1))

turnout_range <- seq(30000, 50000, by = 500)
farage_cf     <- 22239 / turnout_range * 100

plot(turnout_range, farage_cf, type = "l", col = col_ref, lwd = 2.5,
     xlab = "Total turnout (voters)", ylab = "Farage vote share (%)",
     main = "Counterfactual: Farage's 22,239 votes under varying turnout",
     xaxt = "n", ylim = c(40, 75), las = 1)
axis(1, at = seq(30000, 50000, by = 5000), labels = paste0(seq(30, 50, by = 5), "k"))

abline(h = 50, col = "red", lty = 2, lwd = 1.5)
text(31000, 51.5, "50% threshold", col = "red", cex = 0.8, adj = 0)

points(35111, 63.3, pch = 18, col = col_ref, cex = 2)
text(36800, 65.5, "2026 actual\n(44.4% turnout)", cex = 0.8)

cf_2024 <- 22239 / 45958 * 100
points(45958, cf_2024, pch = 15, col = col_con, cex = 1.5)
text(43500, cf_2024 + 2.5, sprintf("If 2024 turnout\n(%.1f%%)", cf_2024), cex = 0.8)
dev.off()

# ── PLOT 6: Binface — non-Reform composition 2024 vs 2026 ───────────────────

png("plot6_binface.png", width = 1400, height = 600, res = 150)
layout(matrix(c(1, 2), nrow = 1), widths = c(1.2, 1))

# Left: 2024 non-Reform
par(mar = c(5, 6, 4, 1))
nr_2024     <- c(12820, 7448, 2016, 1935, 465)
nr_2024_lab <- c("Con", "Lab", "LD", "Green", "Other")
nr_2024_col <- c(col_con, col_lab, col_ld, col_grn, "grey60")

bp1 <- barplot(rev(nr_2024), horiz = TRUE, col = rev(nr_2024_col), border = "white",
               main = "2024 GE: Non-Reform vote\n(24,733 total)",
               xlab = "Votes", las = 1, names.arg = rev(nr_2024_lab), xlim = c(0, 15000))
for (i in seq_along(nr_2024))
  text(rev(nr_2024)[i] / 2, bp1[i], format(rev(nr_2024)[i], big.mark = ","),
       col = "white", font = 2, cex = 0.9)

# Right: 2026 non-Reform
par(mar = c(5, 4, 4, 2))
nr_2026     <- c(9455, 3417)
nr_2026_lab <- c("Binface", "31 others")
nr_2026_col <- c(col_bin, "grey70")

bp2 <- barplot(rev(nr_2026), horiz = TRUE, col = rev(nr_2026_col), border = "white",
               main = "2026 By-el.: Non-Reform vote\n(12,872 total)",
               xlab = "Votes", las = 1, names.arg = rev(nr_2026_lab), xlim = c(0, 15000))
for (i in seq_along(nr_2026))
  text(rev(nr_2026)[i] / 2, bp2[i], format(rev(nr_2026)[i], big.mark = ","),
       col = "white", font = 2, cex = 0.9)

dev.off()

# ── PLOT 7: Second-place finisher across elections ───────────────────────────

png("plot7_second_place.png", width = 1200, height = 600, res = 150)
par(mar = c(5, 5, 4, 1))

second_votes <- c(14219, 10799, 8709, 16205, 11203, 6736, 12820, 9455)
second_party <- c("Lab", "Lab", "Con", "Con", "Lab", "Lab", "Con", "Binface")
second_cols  <- c(col_lab, col_lab, col_con, col_con, col_lab, col_lab, col_con, col_bin)

bp <- barplot(second_votes,
              names.arg = paste0(years, ifelse(el$type == "By-el.", "*", "")),
              col = second_cols, border = "white",
              main = "Clacton: Second-Place Vote, 2005\u20132026",
              ylab = "Votes", las = 1, ylim = c(0, 20000))
for (i in seq_along(second_votes))
  text(bp[i], second_votes[i] + 500,
       paste0(second_party[i], "\n", format(second_votes[i], big.mark = ",")),
       cex = 0.7)

mtext("Binface's 9,455 sits within the normal range of second-place finishes by actual parties.",
      side = 1, line = 3.5, cex = 0.8, col = "grey30")
dev.off()

# ── Console summary ──────────────────────────────────────────────────────────

cat("\n", strrep("\u2550", 60), "\n")
cat("  CLACTON: TURNOUT, FARAGE, AND COUNT BINFACE\n")
cat(strrep("\u2550", 60), "\n\n")

cat("1. TURNOUT DECOMPOSITION (2024 GE \u2192 2026 By-el.)\n\n")
cat(sprintf("   Farage 2024: %s votes (46.2%%) from %s turnout\n",
            format(21225, big.mark = ","), format(45958, big.mark = ",")))
cat(sprintf("   Farage 2026: %s votes (63.3%%) from %s turnout\n",
            format(22239, big.mark = ","), format(35111, big.mark = ",")))
cat(sprintf("   Turnout drop: %s voters (\u221213.6 pp)\n",
            format(45958 - 35111, big.mark = ",")))
cat(sprintf("   Farage absolute change: +%s votes\n", format(22239 - 21225, big.mark = ",")))
cat(sprintf("   At 2024 turnout his share would be: %.1f%%\n", 22239/45958*100))
cat("   \u2192 ~15pp of his 63.3% is mechanical from lower turnout\n\n")

cat("2. BY-ELECTION COMPARISON (2014 Carswell vs 2026 Farage)\n\n")
cat(sprintf("   2014: %s votes (59.7%%) from %s turnout\n",
            format(21113, big.mark = ","), format(35338, big.mark = ",")))
cat(sprintf("   2026: %s votes (63.3%%) from %s turnout\n",
            format(22239, big.mark = ","), format(35111, big.mark = ",")))
cat("   \u2192 Near-identical turnout, near-identical result\n")
cat("   \u2192 Right-populist ceiling \u2248 21\u201322k votes in Clacton\n\n")

cat("3. COUNT BINFACE AS DE FACTO OPPOSITION\n\n")
cat(sprintf("   Non-Reform 2024: %s (Con %s, Lab %s, LD %s, Grn %s)\n",
            format(24733, big.mark = ","), format(12820, big.mark = ","),
            format(7448, big.mark = ","), format(2016, big.mark = ","),
            format(1935, big.mark = ",")))
cat(sprintf("   Non-Reform 2026: %s (Binface %s, 31 others %s)\n",
            format(12872, big.mark = ","), format(9455, big.mark = ","),
            format(3417, big.mark = ",")))
cat(sprintf("   Stayed home: ~%s non-Reform voters from 2024\n",
            format(24733 - 12872, big.mark = ",")))
cat(sprintf("   Binface %% of non-Reform 2026: %.1f%%\n", 9455/12872*100))
cat("   \u2192 Binface was the Schelling-point opposition vote\n")
cat("   \u2192 His 9,455 exceeds Lab's 2019 Clacton result (6,736)\n")
cat("   \u2192 3 in 4 non-Reform voters who turned out chose Binface\n\n")

cat("4. BOTTOM LINE\n\n")
cat("   Farage's absolute vote is stable at ~21\u201322k (same as Carswell).\n")
cat("   The 63% share is an artefact of by-election dynamics: no mainstream\n")
cat("   opposition stood, turnout cratered, and the protest vote coalesced\n")
cat("   around a satirical candidate. In a contested GE (2024) he gets 46%.\n")
cat("   The seat is safe for Reform; the margin is not.\n\n")

cat("7 plots saved.\n")
