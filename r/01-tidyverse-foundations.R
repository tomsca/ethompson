# =============================================================================
# Module 1: Tidyverse Foundations
# Advanced dplyr & ggplot2 Curriculum
# Emmanuel Thompson | Southeast Missouri State University
#
# Topics covered:
#   1. The pipe operator (|> and %>%)
#   2. Tidy data principles (tidyr: pivot_longer, pivot_wider, separate, unite)
#   3. Factor handling (forcats)
#   4. Date/time workflows (lubridate)
# =============================================================================

library(tidyverse)   # loads dplyr, ggplot2, tidyr, readr, purrr, tibble, stringr, forcats
library(lubridate)


# -----------------------------------------------------------------------------
# 1. THE PIPE OPERATOR
# -----------------------------------------------------------------------------
# The native pipe |> (R >= 4.1) and magrittr's %>% both pass the left-hand
# result as the first argument of the right-hand function.

# Base R – nested calls (hard to read)
round(sqrt(abs(-16)), digits = 2)

# With the pipe – left-to-right reading
-16 |> abs() |> sqrt() |> round(digits = 2)

# dplyr pipeline: filter → select → mutate → summarise
mtcars |>
  as_tibble(rownames = "car") |>
  filter(cyl >= 6) |>
  select(car, cyl, mpg, hp) |>
  mutate(hp_per_cyl = hp / cyl) |>
  summarise(
    n          = n(),
    mean_mpg   = mean(mpg),
    mean_hp_pc = mean(hp_per_cyl)
  )


# -----------------------------------------------------------------------------
# 2. TIDY DATA PRINCIPLES
# -----------------------------------------------------------------------------
# Tidy data: one observation per row, one variable per column, one value per cell.

# --- 2a. pivot_longer: wide → long ----------------------------------------
# Each column named "Q1"–"Q4" represents one quarter; that is not tidy.
sales_wide <- tibble(
  region = c("North", "South", "East", "West"),
  Q1     = c(120, 200, 150, 180),
  Q2     = c(135, 210, 160, 195),
  Q3     = c(128, 190, 175, 170),
  Q4     = c(145, 220, 185, 205)
)

sales_long <- sales_wide |>
  pivot_longer(
    cols      = starts_with("Q"),
    names_to  = "quarter",
    values_to = "sales"
  )
sales_long

# --- 2b. pivot_wider: long → wide ------------------------------------------
sales_long |>
  pivot_wider(names_from = quarter, values_from = sales)

# --- 2c. separate & unite ---------------------------------------------------
patients <- tibble(
  id       = 1:4,
  dob      = c("1985-04-12", "1990-07-23", "1978-11-05", "2001-02-18"),
  bp       = c("120/80", "130/85", "118/75", "125/82")
)

patients_tidy <- patients |>
  separate(dob, into = c("year", "month", "day"), sep = "-", convert = TRUE) |>
  separate(bp,  into = c("systolic", "diastolic"), sep = "/", convert = TRUE)

patients_tidy

# Reunite columns
patients_tidy |>
  unite("dob", year, month, day, sep = "-") |>
  unite("bp",  systolic, diastolic, sep = "/")


# -----------------------------------------------------------------------------
# 3. FACTOR HANDLING WITH FORCATS
# -----------------------------------------------------------------------------
# Factors encode categorical variables with a fixed set of levels.

# --- 3a. Relevel by frequency -----------------------------------------------
survey <- tibble(
  satisfaction = c("Good", "Poor", "Excellent", "Good", "Fair",
                   "Excellent", "Good", "Poor", "Excellent", "Good")
)

survey <- survey |>
  mutate(satisfaction = fct_infreq(satisfaction))   # order by count (descending)

levels(survey$satisfaction)

# --- 3b. Relevel manually ---------------------------------------------------
survey <- survey |>
  mutate(
    satisfaction = fct_relevel(satisfaction, "Poor", "Fair", "Good", "Excellent")
  )
levels(survey$satisfaction)

# --- 3c. Lump rare levels into "Other" --------------------------------------
product_data <- tibble(
  brand = c("Apple", "Samsung", "Apple", "Google", "LG",
            "Motorola", "Nokia", "Apple", "Samsung", "OnePlus",
            "Apple", "Samsung", "Xiaomi", "Apple", "LG")
)

product_data |>
  mutate(brand_lumped = fct_lump_n(brand, n = 3)) |>
  count(brand_lumped, sort = TRUE)

# --- 3d. Recode levels ------------------------------------------------------
survey |>
  mutate(
    satisfaction = fct_recode(
      satisfaction,
      "Very Good" = "Excellent",
      "OK"        = "Good"
    )
  ) |>
  count(satisfaction)


# -----------------------------------------------------------------------------
# 4. DATE / TIME WORKFLOWS WITH LUBRIDATE
# -----------------------------------------------------------------------------

# --- 4a. Parsing dates ------------------------------------------------------
dates_raw <- c("2024-01-15", "15/03/2024", "March 22, 2024", "20240630")

ymd("2024-01-15")
dmy("15/03/2024")
mdy("March 22, 2024")
ymd("20240630")

# --- 4b. Extracting components ----------------------------------------------
today_dt <- ymd("2024-06-15")
year(today_dt)
month(today_dt, label = TRUE)
day(today_dt)
wday(today_dt, label = TRUE, abbr = FALSE)
quarter(today_dt)

# --- 4c. Arithmetic with durations and periods ------------------------------
start <- ymd_hms("2024-01-01 08:00:00")
end   <- ymd_hms("2024-03-15 17:30:00")

# Duration (exact seconds)
as.duration(end - start)

# Period (human-friendly)
as.period(end - start)

# Add one month (period) vs. exactly 30 days (duration)
start + months(1)
start + days(30)

# --- 4d. Time-series pipeline -----------------------------------------------
transactions <- tibble(
  date   = seq(ymd("2023-01-01"), ymd("2023-12-31"), by = "week"),
  amount = round(rnorm(53, mean = 5000, sd = 800), 2)
)

monthly_summary <- transactions |>
  mutate(
    month      = floor_date(date, "month"),
    quarter    = paste0("Q", quarter(date)),
    is_weekend = wday(date) %in% c(1, 7)
  ) |>
  group_by(month) |>
  summarise(
    total_amount = sum(amount),
    avg_amount   = mean(amount),
    n_weeks      = n(),
    .groups = "drop"
  )

monthly_summary

# Quick visualisation: monthly totals
ggplot(monthly_summary, aes(x = month, y = total_amount)) +
  geom_col(fill = "#2567b3", alpha = 0.85) +
  geom_line(colour = "#d95a11", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Monthly Transaction Totals – 2023",
    subtitle = "Weekly transactions aggregated to month",
    x        = NULL,
    y        = "Total Amount (USD)"
  ) +
  theme_minimal(base_size = 13)
