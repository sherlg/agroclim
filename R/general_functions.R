library(dplyr)

#' Build season mapping for a daily-regular series
#'
#' @description
#' Given the number of daily observations and a start date, builds:
#' (i) a table of non-overlapping seasons defined by `season_start` and `season_end`
#' (which may cross calendar years), and (ii) a vector `season_id` mapping each
#' day index (1..n_days) to its season (NA if outside all seasons).
#'
#' @param n_days Integer, length of the daily series.
#' @param start_date Date, date of the first observation.
#' @param season_start Character "mm-dd", season start (default "07-01").
#' @param season_end   Character "mm-dd", season end   (default "06-30").
#'
#' @return A list with:
#' * `seasons`: tibble with columns `start`, `end`, `season_id`, `season_label`.
#' * `season_id`: integer vector length `n_days`, mapping day index -> season id (or NA).
#' * `dates`: Date vector length `n_days`.
#'
#' @examples
#' SM <- build_seasons_map(730, as.Date("2019-01-01"))
#'
build_seasons_map <- function(n_days, start_date,
                              season_start = "07-01", season_end = "06-30") {
  if (!inherits(start_date, "Date")) start_date <- as.Date(start_date)
  dates <- seq.Date(start_date, by = "day", length.out = n_days)

  # Does the season cross calendar year? (e.g., Jul->Jun)
  ms <- as.integer(substr(season_start, 1, 2))
  ds <- as.integer(substr(season_start, 4, 5))
  me <- as.integer(substr(season_end, 1, 2))
  de <- as.integer(substr(season_end, 4, 5))
  crosses <- (me < ms) || (me == ms && de < ds)

  # Build many candidate seasons around the data range
  y_min <- as.integer(format(min(dates), "%Y")) - 1L
  y_max <- as.integer(format(max(dates), "%Y")) + 1L

  S <- tibble::tibble(
    start = as.Date(paste0(seq.int(y_min, y_max), "-", season_start)),
    end   = as.Date(paste0(seq.int(y_min, y_max) + crosses, "-", season_end))
  ) |>
    dplyr::filter(start <= end) |>
    dplyr::mutate(
      season_id    = dplyr::row_number(),
      season_label = paste0(format(start, "%Y"), "-", format(end, "%Y"))
    )

  # Map each date to its season (seasons are ordered and non-overlapping)
  idx <- findInterval(dates, S$start)
  season_id <- ifelse(idx > 0 & dates <= S$end[pmax(idx, 1)], idx, NA_integer_)

  list(seasons = S, season_id = season_id, dates = dates)
}


#' Condition Duration Index
#'
#' @description
#' Daily accumulator for multi-variable day counts.
#' For each `id` and season, compute a *daily* cumulative counter `cum_days`
#' of days that satisfy a combined condition across multiple variables, with
#' a minimum-run constraint `min_duration`. The function supports flexible
#' logic to combine conditions across variables and to control how missing
#' values are treated. It uses an **online** algorithm with **per-run bias
#' correction**, ensuring that cumulative values match retrospective counts
#' for completed runs, while remaining conservative for ongoing (incomplete)
#' runs.
#'
#' @param df Tibble/data.frame with one row per day and per `id`. Must have
#'   the same number of rows for each `id`. Variables referenced in `bounds`
#'   must be numeric columns in `df`.
#' @param id Character, name of the id column (default `"id"`).
#' @param start_date Date of the first row for each `id` (daily-regular series).
#' @param season_start,season_end Character `"mm-dd"`, defining the start and
#'   end of the season window. The window may cross calendar years (e.g.
#'   `"07-01"` to `"06-30"`).
#'
#' @param bounds Tibble or data frame defining **per-variable thresholds**.
#'   It must contain one row per variable with at least the column:
#'   \describe{
#'     \item{`var`}{Character: name of the variable in `df` to evaluate.}
#'   }
#'   Optional columns allow specifying the interval and inclusivity criteria
#'   for each variable:
#'   \describe{
#'     \item{`lower`}{Numeric lower bound (default `-Inf`).}
#'     \item{`upper`}{Numeric upper bound (default `Inf`).}
#'     \item{`inc_lower`}{Logical; whether the lower bound is inclusive (default `TRUE`).}
#'     \item{`inc_upper`}{Logical; whether the upper bound is inclusive (default `TRUE`).}
#'   }
#'   For example:
#'   ```
#'   bounds <- tibble::tibble(
#'     var = c("temp","humidity"),
#'     lower = c(10, 40),
#'     upper = c(30, 80),
#'     inc_lower = TRUE,
#'     inc_upper = TRUE
#'   )
#'   ```
#'   A day is considered valid for a given variable if its value lies within
#'   the specified range (taking inclusivity into account).
#'
#' @param combiner Character specifying how to **combine** per-variable conditions
#'   into a single daily condition (`cond`):
#'   \describe{
#'     \item{`"all"`}{All variables must meet their condition for the day to be
#'       considered valid. Equivalent to a logical AND.}
#'     \item{`"any"`}{At least one variable meets its condition. Equivalent to
#'       a logical OR.}
#'     \item{`"k_of_n"`}{At least `k` of the `n` variables meet their condition.
#'       Useful for indices that require simultaneous satisfaction of several,
#'       but not necessarily all, criteria (e.g., "at least 2 of 3 stressors
#'       below threshold"). Requires argument `k`.}
#'   }
#'
#' @param k Integer ≥ 1, required only when `combiner = "k_of_n"`. It defines
#'   the minimum number of variables that must pass their individual conditions
#'   for a day to be marked as valid (`cond = TRUE`). For example, with
#'   three variables and `k = 2`, a day is valid if at least two of them are
#'   within their specified ranges.
#'
#' @param min_duration Integer ≥ 1. Minimum number of **consecutive days**
#'   that must satisfy the combined daily condition (`cond = TRUE`) before
#'   the run starts contributing to the cumulative count.
#'   - If `min_duration = 1`, every valid day contributes immediately.
#'   - If `min_duration = 5`, only when a run of 5 consecutive valid days is
#'     reached do those days begin to count, and the function automatically
#'     adds the previous 4 days at that point (bias correction).
#'
#' @param na_action Character, controlling how **missing values** (`NA`) are
#'   handled. Options:
#'   \describe{
#'     \item{`"false"`}{Treats `NA` as *not satisfying* the condition.
#'       Missing values break runs and are equivalent to days that fail the
#'       thresholds.}
#'     \item{`"skip_vars"`}{When combining across variables, ignore `NA`
#'       for that variable only. For `"all"` or `"k_of_n"`, this means a day
#'       can still be valid if enough other variables are available and meet
#'       their conditions.}
#'     \item{`"skip_days"`}{Drops entire days that contain at least one `NA`
#'       among the variables referenced in `bounds`, before evaluating
#'       conditions. Consecutivity is preserved only across remaining (non-NA)
#'       days.}
#'   }
#'
#' @return Tibble with daily rows and columns:
#'   \itemize{
#'     \item `id` — station or spatial cell identifier.
#'     \item `day_idx` — daily index within the full time series.
#'     \item `season_id`, `season_label` — season grouping identifiers.
#'     \item `cond` — logical vector of days meeting the combined condition.
#'     \item `valid` — logical vector indicating if the day is part of a run
#'           that reached the required `min_duration`.
#'     \item `cum_days` — cumulative number of valid days since season start,
#'           corrected for incomplete runs.
#'   }
#'
#' @details
#' The algorithm proceeds day by day (“online”), tracking consecutive valid
#' runs and applying a **per-run correction** once a run first reaches
#' `min_duration`. This ensures that the cumulative counts per season match
#' what would be obtained with a complete retrospective analysis, while
#' remaining unbiased for incomplete ongoing runs (e.g., when the current
#' season is still in progress).
#'
#' @examples
#' bounds <- tibble::tibble(
#'   var = c("temp", "humidity"),
#'   lower = c(10, 40),
#'   upper = c(30, 80)
#' )
#'
#' # Count days where both variables meet their conditions
#' CDI(df,
#'   id = "id", start_date = min(df$date),
#'   bounds = bounds, combiner = "all", min_duration = 3
#' )
#'
#' # Require at least 2 of 3 variables to meet their conditions
#' CDI(df,
#'   id = "id", start_date = min(df$date),
#'   bounds = bounds, combiner = "k_of_n", k = 2
#' )
#'
CDI <- function(
    df,
    id = "id",
    start_date,
    season_start = "07-01",
    season_end = "06-30",
    bounds,
    combiner = c("all", "any", "k_of_n")[1],
    k = NULL,
    min_duration = 1,
    na_action = c("false", "skip_vars", "skip_days")[1]) {
  
  # ---- 0) Validation & setup
  
  stopifnot(min_duration >= 1L)
  combiner <- match.arg(combiner)
  na_action <- match.arg(na_action)

  df <- tibble::as_tibble(df) |>
    dplyr::group_by(.data[[id]]) |>
    dplyr::mutate(.n = dplyr::n()) |>
    dplyr::ungroup()
  nd <- df$.n[1]
  if (any(df$.n != nd)) stop("All groups (id) must share the same number of rows.")
  df <- dplyr::select(df, -".n")

  # Seasons (built once using the common length)
  SM <- build_seasons_map(nd, start_date, season_start, season_end)
  S <- SM$seasons
  sid <- SM$season_id
  if (all(is.na(sid))) stop("No days fall within any season window.")

  # Index days per id and attach season_id
  df <- df |>
    dplyr::group_by(.data[[id]]) |>
    dplyr::mutate(
      day_idx   = dplyr::row_number(),
      season_id = sid[day_idx]
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(season_id))

  
  # ---- 1) Per-variable conditions
  
  B <- tibble::as_tibble(bounds)
  if (!"lower" %in% names(B)) B <- dplyr::mutate(B, lower = -Inf)
  if (!"upper" %in% names(B)) B <- dplyr::mutate(B, upper = Inf)
  if (!"inc_lower" %in% names(B)) B <- dplyr::mutate(B, inc_lower = TRUE)
  if (!"inc_upper" %in% names(B)) B <- dplyr::mutate(B, inc_upper = TRUE)
  if (!all(B$var %in% names(df))) stop("Some bounds$var not found in df.")

  in_interval <- function(v, lo, hi, il, iu) {
    lo_ok <- if (il) v >= lo else v > lo
    hi_ok <- if (iu) v <= hi else v < hi
    lo_ok & hi_ok
  }

  # Optionally drop days with any NA on referenced variables
  if (na_action == "skip_days") {
    df <- df |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(B$var), ~ !is.na(.x)))
  }

  # Build per-variable logical columns cond__var
  for (i in seq_len(nrow(B))) {
    v <- B$var[i]
    df <- df |>
      dplyr::mutate(
        !!paste0("cond__", v) :=
          in_interval(
            .data[[v]], B$lower[i], B$upper[i],
            isTRUE(B$inc_lower[i]), isTRUE(B$inc_upper[i])
          )
      )
  }
  cond_cols <- paste0("cond__", B$var)

  # NA policy per-variable
  if (na_action == "false") {
    df <- df |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(cond_cols),
        ~ dplyr::if_else(is.na(.x), FALSE, .x)
      ))
  }

  # ---- 2) Combine variables into a single daily condition

  # Count TRUEs (optionally ignoring NA with na.rm)
  df <- df |>
    dplyr::mutate(
      n_true = rowSums(
        dplyr::across(dplyr::all_of(cond_cols),
          ~ as.integer(.x %in% TRUE),
          .names = NULL
        ),
        na.rm = (na_action == "skip_vars")
      ),
      n_vars = if (na_action == "skip_vars") {
        rowSums(dplyr::across(dplyr::all_of(cond_cols), ~ !is.na(.x), .names = NULL))
      } else {
        length(cond_cols)
      }
    )

  # Build 'cond' by chosen combiner
  if (combiner == "all") {
    df <- df |> dplyr::mutate(cond = n_true == n_vars)
  } else if (combiner == "any") {
    df <- df |> dplyr::mutate(cond = n_true >= 1L)
  } else {
    if (is.null(k)) stop("Provide k for combiner='k_of_n'")
    kk <- as.integer(k)
    df <- df |> dplyr::mutate(cond = n_true >= !!kk)
  }

  # ---- 3) Run-length logic (online with per-run correction)
  
  # We avoid self-reference by computing a TRUE-run counter via grouping trick.
  df <- df |>
    dplyr::arrange(.data[[id]], season_id, day_idx) |>
    dplyr::group_by(.data[[id]], season_id) |>
    dplyr::mutate(
      # Group index that increases on FALSE -> gives 1,2,3,... within TRUE runs
      grp = cumsum(!cond),
      run_len = dplyr::if_else(cond,
        as.integer(ave(seq_along(cond), grp, FUN = seq_along)),
        0L
      ),
      valid = cond & (run_len >= min_duration),
      # Bonus: add min_duration-1 days exactly when a run first hits the threshold
      bonus = as.integer(run_len == min_duration) * pmax(min_duration - 1L, 0L),
      cum_days = cumsum(as.integer(valid) + bonus)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(S |> dplyr::select(season_id, season_label), by = "season_id") |>
    dplyr::arrange(.data[[id]], season_id, day_idx) |>
    dplyr::select(!!id, day_idx, season_id, season_label, cond, valid, cum_days)

  return(df)
}



#' Condition Excess Index
#'
#' @description
#' Daily accumulator of *excess over lower* for a single variable.
#' For each `id` and season, computes a *daily* cumulative series `cum_excess`
#' from a **single climatic variable** `x`. A day contributes the value
#' `excess = (x - lower)` **only if** `x` lies within the interval
#' `[lower, upper]` (with configurable inclusivity). The contribution is further
#' gated by a **minimum run-length** requirement `min_duration`: a day only
#' counts if it belongs to a TRUE-run whose length has reached at least
#' `min_duration`.
#'
#' @param df Tibble/data.frame with one row per day and per `id`. All `id`s must
#'   have the same number of rows (daily-regular series).
#' @param id Character, name of the `id` column (default `"id"`).
#' @param x_col Character, name of the **single** numeric variable to analyse.
#' @param start_date Date of the first row for each `id` (used to build the
#'   calendar and season mapping).
#' @param season_start,season_end Character `"mm-dd"` giving the season window.
#'   The window may **cross years** (e.g., `"07-01"` → `"06-30"`).
#'
#' @param lower,upper Numeric bounds defining the valid interval for `x`.
#'   \describe{
#'     \item{`lower`}{**Required** and must be finite. The excess is defined as
#'       `x - lower` (capped at 0 below `lower`).}
#'     \item{`upper`}{Optional (default `Inf`). Use to restrict the valid range
#'       above; values beyond `upper` contribute 0 and break runs depending on
#'       `na_action`.}
#'   }
#' @param inc_lower,inc_upper Logical flags controlling interval inclusivity
#'   (both default `TRUE`):
#'   \describe{
#'     \item{`inc_lower = TRUE`}{`x >= lower` qualifies; `FALSE` means
#'     `x > lower`.}
#'     \item{`inc_upper = TRUE`}{`x <= upper` qualifies; `FALSE` means
#'     `x < upper`.}
#'   }
#'
#' @param min_duration Integer ≥ 1. Minimum number of **consecutive qualifying
#'   days** required before the run contributes to the cumulative:
#'   \itemize{
#'     \item If `min_duration = 1`, every qualifying day contributes its excess
#'           immediately.
#'     \item If `min_duration = m > 1`, the first day that brings the run length
#'           to `m` contributes its own excess **plus** a one-off **bonus** equal
#'           to the sum of the previous `m - 1` days’ excesses in that run (bias
#'           correction). Subsequent days in the run contribute their own excess
#'           normally.
#'   }
#'
#' @param na_action Character controlling how missing values in `x` are handled:
#'   \describe{
#'     \item{`"false"`}{Treat `NA` as out-of-range (i.e., `cond = FALSE`). This
#'       **breaks runs** and contributes 0 excess on those days.}
#'     \item{`"skip_days"`}{Drop days with `NA` in `x` **before** evaluating
#'       conditions. Consecutivity is then defined over the remaining observed
#'       days.}
#'   }
#'
#' @return A tibble with daily rows and columns:
#'   \itemize{
#'     \item `id` — identifier (station/cell).
#'     \item `day_idx` — 1..n index within the full time series per `id`.
#'     \item `season_id`, `season_label` — season identifiers.
#'     \item `cond` — logical; `TRUE` when `x` lies within the specified
#'            interval.
#'     \item `valid` — logical; `TRUE` once the current TRUE-run reaches
#'            `min_duration`.
#'     \item `excess` — numeric; `pmax(x - lower, 0)` on qualifying days,
#'            else 0.
#'     \item `cum_excess` — season-wise cumulative of excess with per-run
#'            correction.
#'   }
#'
#' @details
#' **Units.** `excess` and `cum_excess` are in the same units as `x`.
#' **Capping.** By default, `excess` is not capped at `(upper - lower)`; if you
#' want to cap it, post-process with `excess <- pmin(excess, upper - lower)`
#' before accumulation.
#' **Semantics.** The online bias-corrected cumulative equals the retrospective
#' (full-season) total for runs that have already completed, while remaining
#' conservative for runs that are still in progress at the season boundary.
#'
#' @examples
#' # Suppose df has columns: id, date, temp
#' # Accumulate excess temperature above 25°C whenever 25 ≤ temp (no upper cap),
#' # with runs of at least 3 consecutive qualifying days.
#' CEI(
#'   df,
#'   id = "id", x_col = "temp",
#'   start_date = min(df$date),
#'   lower = 25, upper = Inf,
#'   min_duration = 3
#' )
#'
#' # Same but only count when 25 ≤ temp ≤ 35, exclusive upper bound
#' CEI(
#'   df,
#'   id = "id", x_col = "temp",
#'   start_date = min(df$date),
#'   lower = 25, upper = 35,
#'   inc_upper = FALSE,
#'   min_duration = 2
#' )
#'
CEI <- function(
    df,
    id = "id",
    x_col,
    start_date,
    season_start = "07-01",
    season_end = "06-30",
    lower,
    upper = Inf,
    inc_lower = TRUE,
    inc_upper = TRUE,
    min_duration = 1L,
    na_action = c("false", "skip_days")[1]) {
  
  # ---- 0) Validation & setup
  
  stopifnot(is.finite(lower), min_duration >= 1L)
  na_action <- match.arg(na_action)
  stopifnot(id %in% names(df), x_col %in% names(df))
  
  df <- tibble::as_tibble(df) |>
    dplyr::group_by(.data[[id]]) |>
    dplyr::mutate(.n = dplyr::n()) |>
    dplyr::ungroup()
  nd <- df$.n[1]
  if (any(df$.n != nd)) stop("All groups (id) must share the same number of rows.")
  df <- dplyr::select(df, -".n")
  
  SM <- build_seasons_map(nd, start_date, season_start, season_end)
  S <- SM$seasons
  sid <- SM$season_id
  if (all(is.na(sid))) stop("No days fall within any season window.")
  
  df <- df |>
    dplyr::group_by(.data[[id]]) |>
    dplyr::mutate(
      day_idx   = dplyr::row_number(),
      season_id = sid[day_idx]
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(season_id))
  
  
  # ---- 1) In-range condition and per-day excess
  
  if (na_action == "skip_days") df <- dplyr::filter(df, !is.na(.data[[x_col]]))
  
  x <- df[[x_col]]
  lo_ok <- if (inc_lower) x >= lower else x > lower
  hi_ok <- if (inc_upper) x <= upper else x < upper
  cond <- lo_ok & hi_ok
  if (na_action == "false") cond[is.na(cond)] <- FALSE
  
  df <- df |>
    dplyr::mutate(
      cond   = cond,
      excess = dplyr::if_else(cond, pmax(.data[[x_col]] - lower, 0), 0)
    )
  
  # ---- 2) Run-length logic (online with per-run correction)
  
  k <- max(min_duration - 1L, 0L)
  
  df <- df |>
    dplyr::arrange(.data[[id]], season_id, day_idx) |>
    dplyr::group_by(.data[[id]], season_id) |>
    dplyr::mutate(
      # TRUE-run counter via grouping trick (no self-reference)
      grp = cumsum(!cond),
      run_len = dplyr::if_else(cond,
                               as.integer(ave(seq_along(cond), grp, FUN = seq_along)),
                               0L
      ),
      valid = cond & (run_len >= min_duration),
      # cumulative excess within group
      cum_exc = cumsum(excess),
      prev_cum = dplyr::lag(cum_exc, 1, default = 0),
      prev_k = if (k > 0L) dplyr::lag(prev_cum, k, default = 0) else 0,
      # bonus = sum of previous (min_duration - 1) excesses when run hits threshold
      bonus = dplyr::if_else(run_len == min_duration, prev_cum - prev_k, 0),
      cum_excess = cumsum((as.numeric(valid) * excess) + bonus)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(S |> dplyr::select(season_id, season_label), by = "season_id") |>
    dplyr::arrange(.data[[id]], season_id, day_idx) |>
    dplyr::select(!!id, day_idx, season_id, season_label, cond, valid, excess, cum_excess)
  
  return(df)
}