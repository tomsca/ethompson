# =============================================================================
# Module 2: Advanced dplyr
# Advanced dplyr & ggplot2 Curriculum
# Emmanuel Thompson | Southeast Missouri State University
#
# Topics covered:
#   1. Grouped operations & window functions (lag/lead, cumulative, ranking)
#   2. Joins (inner, left, right, full, anti, semi) including non-equi joins
#   3. Rowwise workflows & list-columns
#   4. Tidy evaluation ({{ }}, across(), .data, pick())
#   5. Performance-aware pipelines (dtplyr, collapse overview)
# =============================================================================

library(tidyverse)
library(lubridate)


# =============================================================================
# 1. GROUPED OPERATIONS & WINDOW FUNCTIONS
# =============================================================================

# Sample dataset: daily stock returns for three tickers
set.seed(42)
stocks <- tibble(
  date   = rep(seq(ymd("2023-01-02"), by = "day", length.out = 252), 3),
  ticker = rep(c("AAPL", "GOOG", "MSFT"), each = 252),
  price  = c(
    cumprod(1 + rnorm(252, 0.0004, 0.012)) * 150,
    cumprod(1 + rnorm(252, 0.0003, 0.010)) * 2900,
    cumprod(1 + rnorm(252, 0.0005, 0.011)) * 240
  )
)

# --- 1a. Standard grouped summary -------------------------------------------
stocks |>
  group_by(ticker) |>
  summarise(
    start_price = first(price),
    end_price   = last(price),
    total_return = (last(price) - first(price)) / first(price),
    volatility   = sd(price / lag(price) - 1, na.rm = TRUE),
    .groups = "drop"
  )

# --- 1b. Window functions: lag / lead ---------------------------------------
stocks_returns <- stocks |>
  arrange(ticker, date) |>
  group_by(ticker) |>
  mutate(
    daily_return   = (price - lag(price)) / lag(price),
    return_5d_ago  = lag(daily_return, n = 5),
    return_fwd_1d  = lead(daily_return, n = 1)
  ) |>
  ungroup()

# --- 1c. Cumulative window functions ----------------------------------------
stocks_returns <- stocks_returns |>
  group_by(ticker) |>
  mutate(
    cumulative_return = cumprod(1 + replace_na(daily_return, 0)) - 1,
    rolling_mean_5d   = slider::slide_dbl(daily_return, mean, .before = 4,
                                          na.rm = TRUE, .complete = TRUE)
    # Note: slider package provides rolling/sliding window helpers
  ) |>
  ungroup()

# --- 1d. Ranking functions --------------------------------------------------
stocks_returns |>
  filter(!is.na(daily_return)) |>
  group_by(ticker) |>
  mutate(
    rank_asc    = rank(daily_return),                    # ties averaged
    rank_min    = rank(daily_return, ties.method = "min"),
    pct_rank    = percent_rank(daily_return),            # 0–1 percentile
    ntile_10    = ntile(daily_return, 10)                # decile
  ) |>
  slice_max(daily_return, n = 3) |>
  select(ticker, date, daily_return, rank_asc, pct_rank, ntile_10) |>
  ungroup()

# --- 1e. within-group running totals ----------------------------------------
stocks_returns |>
  filter(ticker == "AAPL", !is.na(daily_return)) |>
  mutate(
    pos_return  = if_else(daily_return > 0, daily_return, 0),
    cum_pos_ret = cumsum(pos_return)
  ) |>
  select(date, price, daily_return, cum_pos_ret) |>
  head(10)


# =============================================================================
# 2. JOINS
# =============================================================================

# --- Sample tables ----------------------------------------------------------
employees <- tibble(
  emp_id     = 1:6,
  name       = c("Alice", "Bob", "Carol", "Dave", "Eve", "Frank"),
  dept_id    = c(10, 20, 10, 30, 20, NA),
  salary     = c(75000, 82000, 91000, 67000, 78000, 55000),
  hire_date  = ymd(c("2018-03-01","2019-06-15","2017-11-20",
                     "2021-02-28","2020-07-05","2022-09-10"))
)

departments <- tibble(
  dept_id   = c(10, 20, 30, 40),
  dept_name = c("Finance", "Engineering", "Marketing", "HR"),
  budget    = c(500000, 800000, 300000, 250000)
)

performance <- tibble(
  emp_id = c(1, 2, 3, 4, 5),
  score  = c(88, 92, 76, 85, 90),
  rating = c("Good", "Excellent", "Satisfactory", "Good", "Excellent")
)

# --- 2a. Core joins ---------------------------------------------------------
# inner_join: keep only matching rows in both tables
inner_join(employees, departments, by = "dept_id")

# left_join: keep all employees, attach dept info where available
left_join(employees, departments, by = "dept_id")

# anti_join: employees NOT in any department (missing dept_id match)
anti_join(employees, departments, by = "dept_id")

# semi_join: employees that HAVE a matching department (no extra columns)
semi_join(employees, departments, by = "dept_id")

# --- 2b. Multi-table join chain ---------------------------------------------
employees |>
  left_join(departments,  by = "dept_id") |>
  left_join(performance,  by = "emp_id") |>
  select(name, dept_name, salary, score, rating)

# --- 2c. Join with renamed / differently named keys -------------------------
dept_alt <- departments |> rename(department_id = dept_id)

left_join(employees, dept_alt, by = c("dept_id" = "department_id"))

# --- 2d. Non-equi joins with dplyr::join_by() (dplyr >= 1.1.0) -------------
# Example: assign each employee to a "pay band" based on salary range
pay_bands <- tibble(
  band       = c("Band 1", "Band 2", "Band 3", "Band 4"),
  salary_min = c(0,      60000, 75000, 90000),
  salary_max = c(59999,  74999, 89999, Inf)
)

# join_by() supports inequality conditions
employees |>
  left_join(
    pay_bands,
    join_by(salary >= salary_min, salary <= salary_max)
  ) |>
  select(name, salary, band)

# --- 2e. Rolling / nearest join ---------------------------------------------
# Match each employee's hire date to the nearest fiscal year start
fiscal_years <- tibble(
  fy_start = ymd(c("2016-10-01","2017-10-01","2018-10-01",
                   "2019-10-01","2020-10-01","2021-10-01","2022-10-01")),
  fy_label = paste0("FY", 2017:2023)
)

employees |>
  left_join(
    fiscal_years,
    join_by(closest(hire_date >= fy_start))
  ) |>
  select(name, hire_date, fy_label)


# =============================================================================
# 3. ROWWISE WORKFLOWS & LIST-COLUMNS
# =============================================================================

# --- 3a. rowwise() for row-level operations ----------------------------------
scores <- tibble(
  student = c("Alice", "Bob", "Carol"),
  exam1   = c(88, 76, 92),
  exam2   = c(91, 84, 88),
  exam3   = c(79, 90, 95)
)

# rowwise() + c_across(): compute row-level statistics safely
scores |>
  rowwise() |>
  mutate(
    avg_score = mean(c_across(starts_with("exam"))),
    max_score = max(c_across(starts_with("exam"))),
    min_score = min(c_across(starts_with("exam")))
  ) |>
  ungroup()

# --- 3b. List-columns with nest() / unnest() --------------------------------
# Each ticker gets its own nested data frame → fit a model per ticker
models_df <- stocks_returns |>
  filter(!is.na(daily_return), !is.na(return_5d_ago)) |>
  nest(data = -ticker) |>
  mutate(
    model = map(data, \(d) lm(daily_return ~ return_5d_ago, data = d)),
    glance = map(model, broom::glance),
    tidy   = map(model, broom::tidy)
  )

# Inspect model summaries
models_df |>
  select(ticker, glance) |>
  unnest(glance) |>
  select(ticker, r.squared, adj.r.squared, p.value, AIC)

# Inspect coefficients
models_df |>
  select(ticker, tidy) |>
  unnest(tidy) |>
  filter(term != "(Intercept)")

# --- 3c. List-columns for arbitrary objects (e.g. ggplot objects) -----------
plots_df <- stocks_returns |>
  filter(!is.na(daily_return)) |>
  nest(data = -ticker) |>
  mutate(
    plot = map2(data, ticker, \(d, t)
      ggplot(d, aes(x = date, y = daily_return)) +
        geom_line(colour = "#2567b3", alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        labs(title = paste(t, "– Daily Returns"), x = NULL, y = "Return") +
        theme_minimal(base_size = 12)
    )
  )

# Save all plots at once with purrr::walk2
# walk2(plots_df$ticker, plots_df$plot, \(ticker, p)
#   ggsave(paste0("r/plots/", ticker, "_returns.png"), p, width = 8, height = 4)
# )


# =============================================================================
# 4. TIDY EVALUATION
# =============================================================================
# Tidy eval lets you write functions that take column names as arguments,
# enabling reusable, DRY dplyr workflows.

# --- 4a. Embracing {{ }} (injection) ----------------------------------------
group_summary <- function(df, group_var, value_var) {
  df |>
    group_by({{ group_var }}) |>
    summarise(
      n       = n(),
      mean    = mean({{ value_var }}, na.rm = TRUE),
      sd      = sd({{ value_var }},   na.rm = TRUE),
      .groups = "drop"
    )
}

group_summary(employees, dept_id, salary)
group_summary(stocks,    ticker,  price)

# --- 4b. across() for column-wise operations --------------------------------
# Apply the same function(s) to multiple columns at once
employees |>
  group_by(dept_id) |>
  summarise(
    across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE),
    .groups = "drop"
  )

# Named transformations with glue-style naming
mtcars |>
  as_tibble() |>
  summarise(
    across(
      c(mpg, hp, wt),
      list(min = min, max = max, mean = mean),
      .names = "{.col}_{.fn}"
    )
  )

# --- 4c. .data pronoun for string-based column access -----------------------
select_and_filter <- function(df, col_name, threshold) {
  df |>
    filter(.data[[col_name]] > threshold) |>
    select(all_of(col_name))
}

select_and_filter(mtcars, "mpg", 25)

# --- 4d. pick() for predicate-based column selection inside summarise --------
mtcars |>
  as_tibble(rownames = "car") |>
  group_by(cyl) |>
  summarise(
    across(pick(where(is.numeric), -cyl), mean)
  )

# --- 4e. Passing multiple columns with ... -----------------------------------
compute_stats <- function(df, ...) {
  cols <- enquos(...)
  df |>
    summarise(across(c(!!!cols), list(mean = mean, sd = sd), na.rm = TRUE))
}

compute_stats(mtcars, mpg, hp, wt)


# =============================================================================
# 5. PERFORMANCE-AWARE PIPELINES
# =============================================================================

# --- 5a. dtplyr: lazy evaluation on data.table back end ---------------------
# install.packages("dtplyr")
library(dtplyr)
library(data.table)

# Create a larger dataset to show performance benefit
large_stocks <- bind_rows(
  replicate(10, stocks_returns, simplify = FALSE)
)  # ~7,500 rows for illustration (scale to millions in practice)

# dtplyr: wrap with lazy_dt(), write dplyr verbs, collect at the end
dt_result <- large_stocks |>
  lazy_dt() |>
  filter(!is.na(daily_return)) |>
  group_by(ticker) |>
  summarise(
    mean_return = mean(daily_return),
    vol         = sd(daily_return),
    .groups = "drop"
  ) |>
  collect()

dt_result

# --- 5b. Tips for large-data dplyr ------------------------------------------
# 1. Use filter() and select() early to reduce data volume.
# 2. Avoid rowwise() on large data; prefer vectorised operations or across().
# 3. Use compute() in dbplyr to push computation to the database.
# 4. collapse::fgroup_by() / fsummarise() for in-memory C-speed grouping.
# 5. Profile with system.time() or bench::mark() before optimising.
