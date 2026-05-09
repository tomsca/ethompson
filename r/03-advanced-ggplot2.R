# =============================================================================
# Module 3: Advanced ggplot2
# Advanced dplyr & ggplot2 Curriculum
# Emmanuel Thompson | Southeast Missouri State University
#
# Topics covered:
#   1. Layered grammar review: geoms, stats, scales, coords, facets, themes
#   2. Annotation strategy & multi-layer storytelling
#   3. Publication-quality theming
#   4. Extension packages: patchwork, ggrepel, gghighlight, ggdist
#   5. Custom stat/geom overview
# =============================================================================

library(tidyverse)
library(lubridate)
library(patchwork)   # combine plots
library(ggrepel)     # non-overlapping labels
library(gghighlight) # highlight subsets
library(ggdist)      # distribution geoms (stat_halfeye, stat_dots, etc.)
library(scales)      # label helpers (comma, percent, dollar, etc.)


# =============================================================================
# 1. LAYERED GRAMMAR DEEP-DIVE
# =============================================================================

# Dataset: gapminder-style economics simulation
set.seed(7)
n_countries <- 40
econ_data <- tibble(
  country   = paste("Country", LETTERS[1:n_countries]),
  region    = rep(c("Africa", "Americas", "Asia", "Europe"), each = 10),
  gdp_pc    = round(exp(rnorm(n_countries, log(15000), 0.9))),
  life_exp  = round(50 + 30 * plogis(log(gdp_pc / 5000) + rnorm(n_countries, 0, 0.3)), 1),
  pop       = round(runif(n_countries, 1e6, 1.5e9))
)

# --- 1a. Scales: x, y, colour, size, alpha ----------------------------------
p_base <- ggplot(econ_data,
                 aes(x = gdp_pc, y = life_exp,
                     colour = region, size = pop, label = country)) +
  geom_point(alpha = 0.75)

p_base +
  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_y_continuous(limits = c(40, 90), breaks = seq(40, 90, 10)) +
  scale_colour_brewer(palette = "Set2") +
  scale_size_continuous(range = c(2, 14), labels = comma_format(scale = 1e-6, suffix = "M")) +
  labs(
    title   = "GDP per capita vs. Life Expectancy",
    x       = "GDP per Capita (log scale, USD)",
    y       = "Life Expectancy (years)",
    colour  = "Region",
    size    = "Population"
  ) +
  theme_minimal(base_size = 13)

# --- 1b. Coordinate systems -------------------------------------------------
# coord_flip: horizontal bars
econ_data |>
  slice_max(gdp_pc, n = 10) |>
  ggplot(aes(x = reorder(country, gdp_pc), y = gdp_pc, fill = region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Top 10 Countries by GDP per Capita",
       x = NULL, y = "GDP per Capita (USD)") +
  theme_minimal(base_size = 12)

# coord_polar: polar/radial chart (e.g., rose chart)
econ_data |>
  count(region) |>
  ggplot(aes(x = region, y = n, fill = region)) +
  geom_col(width = 1) +
  coord_polar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Countries per Region") +
  theme_void()

# --- 1c. Facets: facet_wrap & facet_grid ------------------------------------
# Build a time-series panel per region using stock data
set.seed(42)
stocks_ts <- tibble(
  date    = rep(seq(ymd("2022-01-01"), by = "month", length.out = 24), 4),
  region  = rep(c("Africa", "Americas", "Asia", "Europe"), each = 24),
  index   = c(
    cumprod(1 + rnorm(24, 0.003, 0.04)) * 100,
    cumprod(1 + rnorm(24, 0.005, 0.03)) * 100,
    cumprod(1 + rnorm(24, 0.004, 0.05)) * 100,
    cumprod(1 + rnorm(24, 0.002, 0.03)) * 100
  )
)

ggplot(stocks_ts, aes(x = date, y = index, colour = region)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ region, nrow = 2, scales = "free_y") +
  scale_colour_brewer(palette = "Set2", guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
  labs(title = "Regional Market Indices (2022–2023)",
       x = NULL, y = "Index (base = 100)") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

# facet_grid: cross two variables
econ_data |>
  mutate(
    gdp_group  = cut(gdp_pc, breaks = quantile(gdp_pc, c(0, 0.5, 1)),
                     labels = c("Lower Half", "Upper Half"), include.lowest = TRUE),
    pop_group  = cut(pop, breaks = quantile(pop, c(0, 0.5, 1)),
                     labels = c("Small Pop.", "Large Pop."), include.lowest = TRUE)
  ) |>
  ggplot(aes(x = life_exp, fill = region)) +
  geom_histogram(bins = 8, colour = "white") +
  facet_grid(gdp_group ~ pop_group) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Life Expectancy Distribution by GDP & Population Size",
       x = "Life Expectancy", y = "Count", fill = "Region") +
  theme_minimal(base_size = 12)


# =============================================================================
# 2. ANNOTATION STRATEGY & MULTI-LAYER STORYTELLING
# =============================================================================

# --- 2a. annotate() for custom text, rectangles, arrows --------------------
highlight_country <- econ_data |> slice_max(life_exp, n = 1)

ggplot(econ_data, aes(x = gdp_pc, y = life_exp, colour = region)) +
  geom_point(size = 3, alpha = 0.7) +
  # shade a region of interest
  annotate("rect",
           xmin = 30000, xmax = Inf,
           ymin = 75,    ymax = Inf,
           fill = "#ffd700", alpha = 0.15) +
  annotate("text",
           x = 32000, y = 88,
           label = "High income &\nlong life",
           hjust = 0, size = 3.5, colour = "grey30") +
  # arrow pointing to top country
  annotate("segment",
           x = highlight_country$gdp_pc + 5000,
           xend = highlight_country$gdp_pc + 500,
           y = highlight_country$life_exp - 3,
           yend = highlight_country$life_exp - 0.5,
           arrow = arrow(length = unit(0.2, "cm")),
           colour = "#d95a11") +
  annotate("label",
           x = highlight_country$gdp_pc + 7000,
           y = highlight_country$life_exp - 4,
           label = paste0("Highest LE:\n", highlight_country$country),
           size = 3, fill = "#fff3e0", colour = "#d95a11") +
  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "Highlighting the Healthiest, Wealthiest Nations",
       x = "GDP per Capita (log, USD)", y = "Life Expectancy (years)",
       colour = "Region") +
  theme_minimal(base_size = 13)

# --- 2b. ggrepel: non-overlapping labels ------------------------------------
econ_data |>
  slice_max(gdp_pc, n = 12) |>
  ggplot(aes(x = gdp_pc, y = life_exp, colour = region, label = country)) +
  geom_point(size = 3) +
  geom_label_repel(
    size         = 3,
    max.overlaps = 15,
    box.padding  = 0.4,
    segment.colour = "grey60"
  ) +
  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "Top 12 Countries: Labelled Without Overlap",
       x = "GDP per Capita", y = "Life Expectancy") +
  theme_minimal(base_size = 12)

# --- 2c. gghighlight: dim non-focal series ----------------------------------
ggplot(stocks_ts, aes(x = date, y = index, colour = region)) +
  geom_line(linewidth = 1) +
  gghighlight(region %in% c("Asia", "Africa"),
              label_key = region,
              unhighlighted_params = list(colour = "grey85", linewidth = 0.4)) +
  scale_colour_manual(values = c("Asia" = "#2567b3", "Africa" = "#d95a11")) +
  scale_x_date(date_labels = "%b '%y") +
  labs(title = "Spotlight: Asia & Africa Market Indices",
       x = NULL, y = "Index") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


# =============================================================================
# 3. PUBLICATION-QUALITY THEMING
# =============================================================================

# --- 3a. Building a custom theme --------------------------------------------
theme_publication <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Axes
      axis.title      = element_text(size = rel(0.9), colour = "#333333"),
      axis.text       = element_text(size = rel(0.85), colour = "#555555"),
      axis.line       = element_line(colour = "#cccccc"),
      axis.ticks      = element_line(colour = "#cccccc"),
      # Grid
      panel.grid.major = element_line(colour = "#eeeeee"),
      panel.grid.minor = element_blank(),
      # Title / subtitle / caption
      plot.title       = element_text(size = rel(1.2), face = "bold",
                                      colour = "#174ea6", margin = margin(b = 6)),
      plot.subtitle    = element_text(size = rel(0.95), colour = "#555555",
                                      margin = margin(b = 10)),
      plot.caption     = element_text(size = rel(0.75), colour = "#888888",
                                      hjust = 1, margin = margin(t = 6)),
      # Legend
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold", size = rel(0.85)),
      legend.text       = element_text(size = rel(0.85)),
      legend.key.size   = unit(0.5, "lines"),
      # Facet strip
      strip.background  = element_rect(fill = "#e3ecf7", colour = NA),
      strip.text        = element_text(face = "bold", colour = "#174ea6"),
      # Plot background
      plot.background   = element_rect(fill = "white", colour = NA),
      plot.margin       = margin(12, 12, 8, 12)
    )
}

# Apply the custom theme globally for all subsequent plots in a session
theme_set(theme_publication())

p_custom <- ggplot(econ_data,
                   aes(x = gdp_pc, y = life_exp, colour = region, size = pop)) +
  geom_point(alpha = 0.78) +
  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_size_continuous(range = c(2, 12), guide = "none") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    title    = "GDP per Capita vs. Life Expectancy",
    subtitle = "Simulated cross-sectional data by region",
    caption  = "Note: Bubble size proportional to population.",
    x        = "GDP per Capita (log scale, USD)",
    y        = "Life Expectancy (years)",
    colour   = "Region"
  )

p_custom


# =============================================================================
# 4. EXTENSION PACKAGES
# =============================================================================

# --- 4a. patchwork: composing multiple plots --------------------------------
p1 <- ggplot(econ_data, aes(x = gdp_pc, fill = region)) +
  geom_histogram(bins = 15, colour = "white", show.legend = FALSE) +
  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution of GDP per Capita", x = "GDP pc", y = "Count")

p2 <- ggplot(econ_data, aes(x = life_exp, fill = region)) +
  geom_histogram(bins = 15, colour = "white") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution of Life Expectancy", x = "Life Expectancy", y = NULL)

p3 <- ggplot(econ_data, aes(x = region, y = life_exp, fill = region)) +
  geom_boxplot(show.legend = FALSE, width = 0.5) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Life Expectancy by Region", x = NULL, y = "Years")

# Compose: top row has two plots, bottom row spans full width
(p1 + p2) / p3 +
  plot_annotation(
    title   = "Economic & Health Profile Dashboard",
    caption = "Simulated data",
    theme   = theme(plot.title = element_text(size = 16, face = "bold",
                                              colour = "#174ea6"))
  )

# --- 4b. ggdist: rich distribution visualizations ---------------------------
# stat_halfeye: half violin + dot plot + interval
ggplot(econ_data, aes(x = life_exp, y = region, fill = region)) +
  stat_halfeye(
    adjust    = 0.8,
    width     = 0.6,
    .width    = c(0.5, 0.95),     # 50% and 95% intervals
    justification = -0.25,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.5
  ) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title = "Life Expectancy Distribution by Region",
    subtitle = "Half-eye + boxplot with 50% and 95% credible intervals",
    x = "Life Expectancy (years)", y = NULL
  )

# stat_dots: donut/beeswarm of dots
ggplot(econ_data, aes(x = gdp_pc, y = region, fill = region)) +
  stat_dots(
    side         = "both",
    quantiles    = 100,
    dotsize      = 0.9,
    show.legend  = FALSE
  ) +
  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "K")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "GDP per Capita Dot Distribution by Region",
       x = "GDP per Capita (log, USD)", y = NULL)


# =============================================================================
# 5. CUSTOM STAT OVERVIEW
# =============================================================================
# A custom Stat transforms data before rendering.
# Here we create StatMidpoint that computes the midpoint of y for each x group.

StatMidpoint <- ggproto(
  "StatMidpoint", Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales) {
    data |>
      group_by(x) |>
      summarise(y = (max(y) + min(y)) / 2, .groups = "drop")
  }
)

stat_midpoint <- function(mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat        = StatMidpoint,
    data        = data,
    mapping     = mapping,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(na.rm = na.rm, ...)
  )
}

# Demo: overlay midpoints on a scatter
ggplot(econ_data, aes(x = factor(region), y = life_exp)) +
  geom_jitter(width = 0.2, alpha = 0.4, colour = "#2567b3") +
  stat_midpoint(
    aes(x = factor(region), y = life_exp),
    size   = 5,
    colour = "#d95a11",
    shape  = 18
  ) +
  labs(title = "Life Expectancy with Custom Midpoint Stat",
       subtitle = "Orange diamond = midpoint of min & max per region",
       x = "Region", y = "Life Expectancy (years)")
