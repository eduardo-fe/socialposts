

########################################################################
# Cycling infrastructure vs modal share — Simpson's Paradox
# Four cycling-culture groups:
#   1. Nordic-Dutch  — century-long continuous tradition (saturated plateau)
#   2. German-Central — rebuilt 1970s-80s, policy + culture
#   3. Policy revival — deliberate investment from near-zero base post-2000
#   4. Low cycling    — nascent, still building
#
# Aggregate slope (0.75) > all within-group slopes.
# Requires: ggplot2, dplyr, ggrepel, patchwork


########################################################################
# ── Cycling culture groups ────────────────────────────────────────────────────
# Group          Slope   R²    Story
# Nordic-Dutch   0.01   0.00   Saturated plateau — culture so strong extra
#                               infrastructure barely moves modal share
# German-Central 0.46   0.60   Rebuilt 1970s-80s; infrastructure still pays off
# Policy revival 0.18   0.51   Culture bottleneck — investment without culture
#                               yields modest gains
# Low cycling    0.55   0.76   Early stage — every km matters at low base
# Aggregate      0.75   0.78   Steeper than all within-group slopes: Simpson's paradox
#
# Nordic-Dutch   : Utrecht, Amsterdam, Copenhagen, Ghent, Antwerp          (n=5)
# German-Central : Munich, Hamburg, Berlin, Cologne, Brussels, Oslo, Wroclaw (n=7)
# Policy revival : Paris, Seville, Barcelona, Vienna, Warsaw, Lyon,
#                  Bologna, Krakow, Bratislava                              (n=9)
# Low cycling    : London, Madrid, Lisbon, Dublin, Rome                    (n=5)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)

# ── Data ─────────────────────────────────────────────────────────────────────
cities <- data.frame(
  city      = c("Utrecht","Copenhagen","Ghent","Amsterdam","Antwerp",
                "Munich","Hamburg","Cologne","Brussels","Berlin","Oslo","Wroclaw",
                "Paris","Barcelona","Vienna","Warsaw","Lyon",
                "Bologna","Krakow","Bratislava","Seville",
                "London","Madrid","Lisbon","Dublin","Rome"),
  infra_pct = c(52, 43, 37, 34, 33,
                37, 33, 24, 19, 17, 17, 20,
                33, 19, 22, 15, 14, 15, 11, 12, 13,
                8,  5,  7,  9,  4),
  modal_pct = c(33, 29, 38, 35, 27,
                21, 22, 15, 13, 18,  8, 15,
                11,  9,  8,  8,  6,  6,  9,  7,  6,
                3,  2,  4,  5,  2),
  culture   = c(
    rep("Nordic-Dutch",    5),   # Utrecht, Copenhagen, Ghent, Amsterdam, Antwerp
    rep("German-Central",  7),   # Munich … Wroclaw
    rep("Policy revival",  9),   # Paris … Seville
    rep("Low cycling",     5)    # London … Rome
  ),
  stringsAsFactors = FALSE
) |>
  mutate(culture = factor(culture,
                          levels = c("Nordic-Dutch", "German-Central",
                                     "Policy revival", "Low cycling")))

# ── Colours ───────────────────────────────────────────────────────────────────
col_nd  <- "#1A80B4"   # blue       – Nordic-Dutch
col_gc  <- "#2A9D60"   # green      – German-Central
col_pr  <- "#E87722"   # orange     – Policy revival
col_lc  <- "#C60B1E"   # red        – Low cycling

cult_cols <- c(
  "Nordic-Dutch"   = col_nd,
  "German-Central" = col_gc,
  "Policy revival" = col_pr,
  "Low cycling"    = col_lc
)

# ── Regressions ───────────────────────────────────────────────────────────────
fit_all <- lm(modal_pct ~ infra_pct, data = cities)

fits <- lapply(levels(cities$culture), function(g) {
  lm(modal_pct ~ infra_pct, data = filter(cities, culture == g))
})
names(fits) <- levels(cities$culture)

# Print slopes
cat("Slopes:\n")
cat(sprintf("  Aggregate      : %.3f  R2=%.2f\n",
            coef(fit_all)[2], summary(fit_all)$r.squared))
for (g in levels(cities$culture)) {
  m <- fits[[g]]
  cat(sprintf("  %-18s: %.3f  R2=%.2f  (n=%d)\n",
              g, coef(m)[2], summary(m)$r.squared,
              nrow(filter(cities, culture == g))))
}

# ── Confidence-interval ribbons + fit lines ───────────────────────────────────
make_ci <- function(fit, x_range) {
  nd <- data.frame(infra_pct = seq(x_range[1], x_range[2], length.out = 80))
  ci <- as.data.frame(predict(fit, nd, interval = "confidence"))
  cbind(nd, ci)
}

x_ranges <- list(
  "Nordic-Dutch"   = c(30, 55),
  "German-Central" = c(15, 40),
  "Policy revival" = c(10, 35),
  "Low cycling"    = c( 3, 11)
)

ci_list <- lapply(levels(cities$culture), function(g) {
  d <- make_ci(fits[[g]], x_ranges[[g]])
  d$culture <- g
  d
})
ci_df <- do.call(rbind, ci_list) |>
  mutate(culture = factor(culture, levels = levels(cities$culture)))

# Overall aggregate line
agg_range <- data.frame(infra_pct = seq(3, 55, 1))
agg_line  <- cbind(agg_range,
                   as.data.frame(predict(fit_all, agg_range,
                                         interval = "confidence")))

# ── Shared scales ─────────────────────────────────────────────────────────────
x_scale <- scale_x_continuous(
  name   = "Protected cycling infrastructure (% of road network)",
  breaks = seq(0, 55, 10), limits = c(0, 58)
)
y_scale <- scale_y_continuous(
  name   = "Cycling modal share (% of all daily trips)",
  breaks = seq(0, 40, 5), limits = c(0, 42)
)

base_theme <- theme_minimal(base_size = 10.5) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(colour = "grey93", linewidth = 0.4),
    plot.title         = element_text(face = "bold", size = 11.5,
                                      margin = margin(b = 3)),
    plot.subtitle      = element_text(size = 8.5, colour = "grey35",
                                      lineheight = 1.4, margin = margin(b = 8)),
    axis.title         = element_text(size = 8.5),
    legend.position    = "top",
    legend.text        = element_text(size = 8.5),
    legend.key.size    = unit(0.4, "cm"),
    plot.margin        = margin(8, 12, 8, 8)
  )

# ── Plot 1: Aggregate (the misleading picture) ─────────────────────────────
p1 <- ggplot(cities, aes(x = infra_pct, y = modal_pct)) +
  geom_ribbon(data = agg_line,
              aes(x = infra_pct, ymin = lwr, ymax = upr),
              fill = "grey60", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = agg_line,
            aes(x = infra_pct, y = fit),
            colour = "#333", linewidth = 0.9, inherit.aes = FALSE) +
  geom_point(colour = "grey40", size = 2.8, alpha = 0.85) +
  geom_text_repel(aes(label = city),
                  size = 2.5, colour = "grey30",
                  segment.size = 0.25, segment.color = "grey70",
                  min.segment.length = 0.3, box.padding = 0.35,
                  max.overlaps = 20) +
  annotate("text", x = 1, y = 39, hjust = 0, size = 2.9, colour = "#333",
           label = sprintf("Aggregate slope: %.2f\nR2 = %.2f",
                           coef(fit_all)[2], summary(fit_all)$r.squared),
           lineheight = 1.3) +
  x_scale + y_scale +
  labs(
    title    = "1. The aggregate picture",
    subtitle = "All cities pooled. Slope = 0.75: each extra 1pp infrastructure -> +0.75pp modal share."
  ) +
  base_theme

# ── Plot 2: Four groups — reveals the paradox ─────────────────────────────────
# Build slope annotation data
slope_labels <- data.frame(
  culture   = levels(cities$culture),
  label_x   = c(50, 20, 35,  5),
  label_y   = c(12, 30,  3,  25),
  hjust_val = c( 0,  0,  0,  0)
) |>
  rowwise() |>
  mutate(
    slope = coef(fits[[culture]])[2],
    r2    = summary(fits[[culture]])$r.squared,
    n     = nrow(filter(cities, culture == .data$culture)),
    label = sprintf("%s\nslope: %.2f  R2=%.2f  n=%d",
                    culture, slope, r2, n)
  ) |>
  ungroup() |>
  mutate(culture = factor(culture, levels = levels(cities$culture)))

p2 <- ggplot(cities, aes(x = infra_pct, y = modal_pct)) +
  # aggregate ghost line
  geom_line(data = agg_line,
            aes(x = infra_pct, y = fit),
            colour = "grey65", linewidth = 0.6,
            linetype = "dashed", inherit.aes = FALSE) +
  # within-group ribbons
  geom_ribbon(data = ci_df,
              aes(x = infra_pct, ymin = lwr, ymax = upr, fill = culture),
              alpha = 0.12, inherit.aes = FALSE) +
  # within-group fit lines
  geom_line(data = ci_df,
            aes(x = infra_pct, y = fit, colour = culture),
            linewidth = 1.0, inherit.aes = FALSE) +
  # points
  geom_point(aes(colour = culture), size = 2.8, alpha = 0.88) +
  geom_text_repel(aes(label = city, colour = culture),
                  size               = 2.5,
                  segment.size       = 0.25,
                  segment.color      = "grey70",
                  min.segment.length = 0.3,
                  box.padding        = 0.35,
                  max.overlaps       = 20,
                  show.legend        = FALSE) +
  # slope annotations
  geom_label(data = slope_labels,
             aes(x = label_x, y = label_y, label = label,
                 colour = culture),
             size = 2.4, fill = "white", label.size = 0.2,
             fontface = "plain", lineheight = 1.3,
             inherit.aes = FALSE, show.legend = FALSE) +
  # aggregate annotation
  annotate("text", x = 1, y = 39, hjust = 0, size = 2.6, colour = "grey50",
           label = "Dashed: aggregate\n(pooled) trend", lineheight = 1.3) +
  scale_colour_manual(values = cult_cols, name = NULL) +
  scale_fill_manual(values   = cult_cols, guide = "none") +
  x_scale + y_scale +
  labs(
    title    = "2. Separated by cycling culture — Simpson's paradox",
    subtitle = paste0(
      "Aggregate slope (0.75, dashed) is steeper than every within-group slope.\n",
      "Nordic-Dutch cities have saturated: culture is the ceiling, not infrastructure."
    )
  ) +
  base_theme +
  guides(colour = guide_legend(override.aes = list(size = 3),
                               nrow = 2))

# ── Combine ───────────────────────────────────────────────────────────────────
combined <- (p1 | p2) +
  plot_annotation(
    title    = "Cycling infrastructure and modal share: Simpson's paradox",
    subtitle = paste0(
      "The aggregate trend (left) overstates infrastructure's causal effect by pooling four very different cycling contexts.\n",
      "Once separated by culture (right), every within-group slope is flatter — and the Nordic-Dutch group is nearly flat:\n",
      "those cities have so much culture that extra infrastructure adds little modal share at the margin.\n",
      "Policy-revival cities (Paris, Seville, Vienna...) show the culture bottleneck: investment without culture yields modest gains."
    ),
    caption  = paste0(
      "Sources: Clean Cities Campaign 2025 (infrastructure); European Cyclists' Federation /\n",
      "Copenhagenize Index 2025 / city transport surveys (modal share). Methodologies vary across cities."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 8.5, colour = "grey30",
                                   lineheight = 1.5, margin = margin(b = 4)),
      plot.caption  = element_text(size = 7.5, colour = "grey50",
                                   lineheight = 1.4, hjust = 0,
                                   margin = margin(t = 8))
    )
  )

ggsave("cycling_simpsons_paradox.pdf", combined, width = 16, height = 8)
ggsave("cycling_simpsons_paradox.png", combined, width = 16, height = 8, dpi = 300)
message("Saved: cycling_simpsons_paradox.pdf / .png")
