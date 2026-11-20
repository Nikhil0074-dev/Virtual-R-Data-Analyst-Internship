# =============================================================================
# Week 2: Data Visualization and Insight Communication using R
# Dataset : Gapminder (life expectancy, GDP per capita, population; 1952-2007)
# Author  : <your name>
# Run     : Rscript scripts/01_visualizations.R   (from the project root)
# Output  : PNG figures in figures/, summary numbers in figures/summary_stats.txt
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)    # main plotting system
  library(dplyr)      # data wrangling
  library(tidyr)      # reshaping
  library(scales)     # nice axis labels
  library(ggrepel)    # non-overlapping text labels
  library(patchwork)  # combine plots
  library(lattice)    # second plotting system (for comparison)
  library(readr)
})

dir.create("figures", showWarnings = FALSE)

# Colour-blind friendly palette (Okabe-Ito) used for continents in every chart
continent_cols <- c(
  "Africa"   = "#D55E00",
  "Americas" = "#009E73",
  "Asia"     = "#E69F00",
  "Europe"   = "#0072B2",
  "Oceania"  = "#CC79A7"
)

# One consistent theme for the whole report
theme_report <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size + 3),
      plot.subtitle   = element_text(colour = "grey30", margin = margin(b = 8)),
      plot.caption    = element_text(colour = "grey45", size = base_size - 3),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text      = element_text(face = "bold")
    )
}
theme_set(theme_report())

save_plot <- function(plot, name, w = 9, h = 5.5) {
  ggsave(file.path("figures", name), plot, width = w, height = h,
         dpi = 200, device = ragg::agg_png, bg = "white")
}

src <- "Source: Gapminder (public dataset)"

# ---- 1. Load and inspect ----------------------------------------------------
gap <- read_csv("data/gapminder.csv", show_col_types = FALSE) |>
  mutate(continent = factor(continent))

glimpse(gap)
cat("\nRows:", nrow(gap), " Countries:", n_distinct(gap$country),
    " Years:", paste(range(gap$year), collapse = "-"), "\n")
cat("Missing values per column:\n"); print(colSums(is.na(gap)))
print(summary(gap |> select(lifeExp, pop, gdpPercap)))

gap07 <- filter(gap, year == 2007)
gap52 <- filter(gap, year == 1952)

# ---- 2. Figure 1 : Histogram, life expectancy 1952 vs 2007 ------------------
p1 <- gap |>
  filter(year %in% c(1952, 2007)) |>
  ggplot(aes(lifeExp, fill = factor(year))) +
  geom_histogram(binwidth = 2.5, colour = "white", alpha = 0.85,
                 position = "identity") +
  scale_fill_manual(values = c("1952" = "#999999", "2007" = "#0072B2"),
                    name = "Year") +
  labs(title = "The whole world shifted toward longer lives between 1952 and 2007",
       subtitle = "Number of countries by life expectancy at birth",
       x = "Life expectancy (years)", y = "Number of countries",
       caption = src)
save_plot(p1, "fig01_histogram_lifeexp.png")

# ---- 3. Figure 2 : Bar chart, average life expectancy by continent, 2007 ----
bar_data <- gap07 |>
  group_by(continent) |>
  summarise(avg_life = mean(lifeExp), .groups = "drop")

p2 <- ggplot(bar_data, aes(reorder(continent, avg_life), avg_life, fill = continent)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", avg_life)), hjust = -0.2, fontface = "bold") +
  scale_fill_manual(values = continent_cols) +
  scale_y_continuous(limits = c(0, 90), expand = c(0, 0)) +
  coord_flip() +
  labs(title = "Africa still trails every other continent in 2007",
       subtitle = "Average life expectancy of countries, by continent",
       x = NULL, y = "Life expectancy (years)", caption = src)
save_plot(p2, "fig02_bar_continent.png", h = 4.5)

# ---- 4. Figure 3 : Scatter, wealth vs health, 2007 --------------------------
label_countries <- c("United States", "China", "India", "Japan", "Nigeria",
                     "South Africa", "Norway", "Sierra Leone", "Swaziland",
                     "Afghanistan", "Brazil", "Vietnam")
p3 <- ggplot(gap07, aes(gdpPercap, lifeExp)) +
  geom_point(aes(size = pop, colour = continent), alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, colour = "grey25",
              linewidth = 0.8, linetype = "dashed") +
  geom_text_repel(data = filter(gap07, country %in% label_countries),
                  aes(label = country), size = 3.4, max.overlaps = Inf,
                  box.padding = 0.5) +
  scale_x_log10(labels = label_dollar(accuracy = 1)) +
  scale_size_area(max_size = 14, breaks = c(2e8, 6e8, 1e9), labels = label_number(scale = 1e-6, suffix = "M"),
                  name = "Population") +
  scale_colour_manual(values = continent_cols, name = "Continent") +
  guides(colour = guide_legend(override.aes = list(size = 4), order = 1),
         size = guide_legend(order = 2)) +
  labs(title = "Richer countries live longer, but the gains flatten out",
       subtitle = "Life expectancy vs GDP per capita, 2007 (log scale on x-axis)",
       x = "GDP per capita (US$, log scale)", y = "Life expectancy (years)",
       caption = src) +
  theme(legend.position = "right")
save_plot(p3, "fig03_scatter_gdp_life_2007.png", w = 11, h = 6.5)

# ---- 5. Figure 4 : Line chart, population-weighted trend by continent -------
trend <- gap |>
  group_by(continent, year) |>
  summarise(life = weighted.mean(lifeExp, pop), .groups = "drop")

p4 <- ggplot(trend, aes(year, life, colour = continent)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_colour_manual(values = continent_cols, name = NULL) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(title = "Every continent improved, but Africa's gains slowed after the 1980s",
       subtitle = "Life expectancy, weighted by population",
       x = NULL, y = "Life expectancy (years)", caption = src) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p4, "fig04_line_trend_continent.png")

# ---- 6. Figure 5 : Boxplot + points, spread within continents, 2007 ---------
p5 <- ggplot(gap07, aes(reorder(continent, lifeExp, median), lifeExp,
                        fill = continent)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1.6, show.legend = FALSE,
              aes(colour = continent)) +
  scale_fill_manual(values = continent_cols) +
  scale_colour_manual(values = continent_cols) +
  labs(title = "Inside Africa and Asia, the gap between countries is wide",
       subtitle = "Life expectancy of each country in 2007 (one dot = one country)",
       x = NULL, y = "Life expectancy (years)", caption = src)
save_plot(p5, "fig05_boxplot_continent.png")

# ---- 7. Figure 6 : Anomalies, biggest drops in life expectancy --------------
drops <- gap |>
  arrange(country, year) |>
  group_by(country) |>
  mutate(change = lifeExp - lag(lifeExp)) |>
  ungroup()

worst <- drops |>
  filter(!is.na(change)) |>
  slice_min(change, n = 8) |>
  select(country, continent, year, change, lifeExp)
print(worst)

highlight <- c("Rwanda", "Zimbabwe", "Cambodia", "China")
p6 <- ggplot() +
  geom_line(data = gap, aes(year, lifeExp, group = country),
            colour = "grey85", linewidth = 0.4) +
  geom_line(data = filter(gap, country %in% highlight),
            aes(year, lifeExp, colour = country), linewidth = 1.4) +
  geom_text_repel(data = filter(gap, country %in% highlight, year == 2007),
                  aes(year, lifeExp, label = country, colour = country),
                  nudge_x = 3, direction = "y", hjust = 0, show.legend = FALSE,
                  fontface = "bold", segment.colour = NA) +
  scale_colour_manual(values = c(Rwanda = "#D55E00", Zimbabwe = "#CC79A7",
                                 Cambodia = "#E69F00", China = "#0072B2")) +
  scale_x_continuous(limits = c(1952, 2018), breaks = seq(1952, 2007, 5)) +
  labs(title = "Life expectancy can fall sharply: four stories hidden in the data",
       subtitle = "Grey lines = all 142 countries; coloured lines = unusual drops",
       x = NULL, y = "Life expectancy (years)", caption = src) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p6, "fig06_anomalies_drops.png", h = 6)

# ---- 8. Figure 7 : Lollipop, biggest gains 1952-2007 ------------------------
gain <- gap |>
  filter(year %in% c(1952, 2007)) |>
  select(country, continent, year, lifeExp) |>
  pivot_wider(names_from = year, values_from = lifeExp,
              names_prefix = "y") |>
  mutate(gain = y2007 - y1952) |>
  slice_max(gain, n = 10)

p7 <- ggplot(gain, aes(y = reorder(country, gain))) +
  geom_segment(aes(x = y1952, xend = y2007, yend = reorder(country, gain)),
               colour = "grey60", linewidth = 1) +
  geom_point(aes(x = y1952), colour = "grey40", size = 3) +
  geom_point(aes(x = y2007, colour = continent), size = 4) +
  geom_text(aes(x = y2007, label = paste0("+", round(gain, 1), " yrs")),
            hjust = -0.25, size = 3.4) +
  scale_colour_manual(values = continent_cols, name = "Continent (2007 dot)") +
  scale_x_continuous(limits = c(25, 90)) +
  labs(title = "Ten countries that added the most years of life",
       subtitle = "Grey dot = 1952, coloured dot = 2007",
       x = "Life expectancy (years)", y = NULL, caption = src)
save_plot(p7, "fig07_lollipop_gain.png", h = 5.5)

# ---- 9. Figure 8 : Small multiples, scatter across four years ---------------
p8 <- gap |>
  filter(year %in% c(1952, 1972, 1992, 2007)) |>
  ggplot(aes(gdpPercap, lifeExp, colour = continent)) +
  geom_point(aes(size = pop), alpha = 0.7, show.legend = c(size = FALSE)) +
  scale_x_log10(labels = label_dollar(accuracy = 1)) +
  scale_size_area(max_size = 9) +
  scale_colour_manual(values = continent_cols, name = NULL) +
  facet_wrap(~ year, nrow = 2) +
  labs(title = "The whole world moved up and to the right",
       subtitle = "Life expectancy vs GDP per capita in four snapshots",
       x = "GDP per capita (US$, log scale)", y = "Life expectancy (years)",
       caption = src)
save_plot(p8, "fig08_small_multiples.png", w = 10, h = 7)

# ---- 10. Figure 9 : Heatmap, 15 most populous countries ---------------------
top15 <- gap07 |> slice_max(pop, n = 15) |> pull(country)
p9 <- gap |>
  filter(country %in% top15) |>
  mutate(country = factor(country, levels = rev(
    gap07 |> filter(country %in% top15) |> arrange(desc(pop)) |> pull(country)))) |>
  ggplot(aes(factor(year), country, fill = lifeExp)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(lifeExp)), size = 2.9, colour = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Years",
                       end = 0.9) +
  labs(title = "Fifteen biggest countries: who caught up, and when",
       subtitle = "Life expectancy over time (countries ordered by 2007 population)",
       x = NULL, y = NULL, caption = src) +
  theme(panel.grid = element_blank(), legend.position = "right")
save_plot(p9, "fig09_heatmap_top15.png", w = 10, h = 6.5)

# ---- 11. Figure 10 : Line chart, income growth by continent -----------------
income <- gap |>
  group_by(continent, year) |>
  summarise(med_gdp = median(gdpPercap), .groups = "drop")

p10 <- ggplot(income, aes(year, med_gdp, colour = continent)) +
  geom_line(linewidth = 1.3) +
  scale_y_log10(labels = label_dollar(accuracy = 1)) +
  scale_colour_manual(values = continent_cols, name = NULL) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(title = "Income grew everywhere, but unevenly",
       subtitle = "Median GDP per capita by continent (log scale)",
       x = NULL, y = "Median GDP per capita (US$, log scale)", caption = src) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p10, "fig10_line_income.png")

# ---- 12. Figure 11 : Base R scatter with regression line --------------------
png("figures/fig11_base_r_scatter.png", width = 1800, height = 1100, res = 200)
par(mar = c(5, 5, 4, 2))
plot(log10(gap07$gdpPercap), gap07$lifeExp,
     pch = 19, col = adjustcolor(continent_cols[as.character(gap07$continent)], 0.7),
     cex = 1.4, xlab = "log10 of GDP per capita (US$)",
     ylab = "Life expectancy (years)",
     main = "Base R: log income explains most of life expectancy (2007)",
     las = 1, bty = "l")
fit <- lm(lifeExp ~ log10(gdpPercap), data = gap07)
abline(fit, lwd = 2.5, lty = 2)
legend("bottomright", legend = names(continent_cols), pch = 19,
       col = continent_cols, bty = "n", cex = 0.9)
mtext(sprintf("Fitted line: R-squared = %.2f", summary(fit)$r.squared),
      side = 3, line = 0.2, cex = 0.8)
dev.off()

# ---- 13. Figure 12 : lattice box plots over time ----------------------------
trellis.device(ragg::agg_png, file = "figures/fig12_lattice_boxplots.png",
               width = 10, height = 6, units = "in", res = 200)
print(bwplot(lifeExp ~ factor(year) | continent, data = gap,
             layout = c(3, 2), as.table = TRUE,
             xlab = "Year", ylab = "Life expectancy (years)",
             main = "Lattice: life expectancy distribution over time",
             scales = list(x = list(rot = 90, cex = 0.7)),
             par.settings = list(box.rectangle = list(col = "#0072B2"),
                                 box.umbrella  = list(col = "#0072B2"),
                                 plot.symbol   = list(col = "#D55E00", pch = 19, cex = 0.5))))
dev.off()

# ---- 14. Numbers used in the written report ---------------------------------
sink("figures/summary_stats.txt")
cat("=== SUMMARY NUMBERS FOR THE REPORT ===\n\n")
cat("Rows:", nrow(gap), "| Countries:", n_distinct(gap$country), "\n\n")

cat("Mean life expectancy 1952:", round(mean(gap52$lifeExp), 1),
    " 2007:", round(mean(gap07$lifeExp), 1), "\n")
cat("Median life expectancy 1952:", round(median(gap52$lifeExp), 1),
    " 2007:", round(median(gap07$lifeExp), 1), "\n")
cat("Countries below 50 yrs in 1952:", sum(gap52$lifeExp < 50),
    " in 2007:", sum(gap07$lifeExp < 50), "\n")
cat("Countries 70+ yrs in 1952:", sum(gap52$lifeExp >= 70),
    " in 2007:", sum(gap07$lifeExp >= 70), "\n\n")

cat("--- Continent averages 2007 ---\n"); print(as.data.frame(bar_data))
cat("\n--- Population-weighted trend (1952, 2007) ---\n")
print(as.data.frame(filter(trend, year %in% c(1952, 2007))))

cat("\n--- Correlations 2007 ---\n")
cat("Pearson r lifeExp vs gdpPercap:", round(cor(gap07$lifeExp, gap07$gdpPercap), 2), "\n")
cat("Pearson r lifeExp vs log10(gdpPercap):",
    round(cor(gap07$lifeExp, log10(gap07$gdpPercap)), 2), "\n")
cat("R-squared of linear fit on log10 income:", round(summary(fit)$r.squared, 2), "\n")
cat("Slope (years per 10x income):", round(coef(fit)[2], 1), "\n\n")

cat("--- Spread within continents 2007 (min, max, range) ---\n")
print(as.data.frame(gap07 |> group_by(continent) |>
        summarise(min = min(lifeExp), max = max(lifeExp), range = max - min,
                  n = n(), .groups = "drop")))

cat("\n--- 8 biggest 5-year drops ---\n"); print(as.data.frame(worst))
cat("\n--- 10 biggest gains 1952-2007 ---\n"); print(as.data.frame(gain))

cat("\n--- Median GDP per capita by continent (1952, 2007) ---\n")
print(as.data.frame(filter(income, year %in% c(1952, 2007))))

cat("\n--- Lowest and highest life expectancy 2007 ---\n")
print(as.data.frame(gap07 |> slice_min(lifeExp, n = 5) |> select(country, continent, lifeExp, gdpPercap)))
print(as.data.frame(gap07 |> slice_max(lifeExp, n = 5) |> select(country, continent, lifeExp, gdpPercap)))

cat("\n--- Countries with life expectancy lower in 2007 than 1992 ---\n")
print(as.data.frame(
  gap |> filter(year %in% c(1992, 2007)) |>
    select(country, continent, year, lifeExp) |>
    pivot_wider(names_from = year, values_from = lifeExp, names_prefix = "y") |>
    filter(y2007 < y1992) |> mutate(change = y2007 - y1992) |> arrange(change)))

cat("\n--- Countries with 2007 life expectancy above 75 but GDP below $10,000 ---\n")
print(as.data.frame(gap07 |> filter(lifeExp > 75, gdpPercap < 10000) |>
                      select(country, continent, lifeExp, gdpPercap)))
sink()

cat("\nDone. Figures written to figures/\n")
