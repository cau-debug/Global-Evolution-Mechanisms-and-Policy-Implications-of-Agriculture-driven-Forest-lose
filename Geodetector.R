# ============================================================
# GeoDetector with country-specific discretization
#
# This script computes:
# 1) Country-level single-factor q values (factor detector)
# 2) Country-level pairwise interaction q values (interaction detector)
#    and interaction type classification
# 3) An Excel output with three sheets:
#    - factor_q
#    - interaction_q
#    - discretization_log
#
# Assumption:
# All rows with missing values have already been removed
# (for the response variable and all explanatory variables).
# ============================================================

# install.packages(c("readxl", "dplyr", "stringr", "purrr", "tidyr", "openxlsx", "geodetector"))

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(openxlsx)
library(geodetector)

# ============================================================
# 1) Parameters
# ============================================================

# Use a relative path so the script can be shared publicly
infile <- "data/input/geodetector_input.xlsx"

id_country <- "country"
y_var <- "T_00_10"
x_vars <- c("dem", "slope", "gdp10", "pop10", "pre10", "tem10", "urban10", "npp10")

# Discretization settings
min_n_country <- 30      # minimum number of observations per country
min_bin_n     <- 10      # minimum number of observations per bin
k_min <- 3
k_max <- 7

# Variables to transform with log1p before discretization
log_vars <- c("gdp10", "pop10")

# Zero-inflated variables: 0 as a separate class, positive values discretized separately
zero_inflated_vars <- c( "urban10")

# Minimum sample size required for interaction detection
min_n_interaction <- 50

# Output file
outfile <- "outputs/geodetector_country_q_and_interaction.xlsx"

# ============================================================
# 2) Helper functions
# ============================================================

choose_k <- function(n, k_min = 3, k_max = 7) {
  if (n < 80)   return(max(k_min, 3))
  if (n < 200)  return(min(k_max, 4))
  if (n < 500)  return(min(k_max, 5))
  if (n < 2000) return(min(k_max, 6))
  return(k_max)
}

log1p_shift <- function(x) {
  ok <- !is.na(x)
  if (!any(ok)) return(x)

  mn <- min(x[ok], na.rm = TRUE)
  if (mn < 0) x[ok] <- x[ok] - mn

  x[ok] <- log1p(x[ok])
  x
}

disc_quantile <- function(x, k, min_bin_n = 10) {
  ok <- !is.na(x)
  x0 <- x[ok]

  if (length(unique(x0)) < 2) {
    return(rep(NA_integer_, length(x)))
  }

  probs <- seq(0, 1, length.out = k + 1)
  brks <- suppressWarnings(quantile(x0, probs = probs, na.rm = TRUE, type = 7))
  brks <- unique(as.numeric(brks))

  while (length(brks) - 1 < 2 && k > 2) {
    k <- k - 1
    probs <- seq(0, 1, length.out = k + 1)
    brks <- unique(as.numeric(quantile(x0, probs = probs, na.rm = TRUE, type = 7)))
  }

  if (length(brks) - 1 < 2) {
    return(rep(NA_integer_, length(x)))
  }

  g <- rep(NA_integer_, length(x))
  g[ok] <- as.integer(cut(
    x0,
    breaks = brks,
    include.lowest = TRUE,
    right = TRUE,
    labels = FALSE
  ))

  tab <- table(g, useNA = "no")

  while (length(tab) > 1 && min(tab) < min_bin_n && k > 2) {
    k <- k - 1
    probs <- seq(0, 1, length.out = k + 1)
    brks <- unique(as.numeric(quantile(x0, probs = probs, na.rm = TRUE, type = 7)))
    if (length(brks) - 1 < 2) break

    g[ok] <- as.integer(cut(
      x0,
      breaks = brks,
      include.lowest = TRUE,
      right = TRUE,
      labels = FALSE
    ))
    tab <- table(g, useNA = "no")
  }

  g
}

disc_zero_inflated <- function(x, k, min_bin_n = 10) {
  ok <- !is.na(x)
  x0 <- x[ok]

  if (length(unique(x0)) < 2) {
    return(rep(NA_integer_, length(x)))
  }

  is0  <- ok & x == 0
  isn0 <- ok & x > 0

  # If there are too few positive values, use a simple binary split: 0 vs >0
  if (sum(isn0) < max(2 * min_bin_n, 20)) {
    g <- rep(NA_integer_, length(x))
    g[is0]  <- 1L
    g[isn0] <- 2L
    return(g)
  }

  # Reserve one class for zeros, use the remaining classes for positive values
  k2 <- max(2, k - 1)
  g_non0 <- disc_quantile(x[isn0], k = k2, min_bin_n = min_bin_n)

  g <- rep(NA_integer_, length(x))
  g[is0]  <- 1L
  g[isn0] <- 1L + g_non0
  g
}

classify_interaction <- function(q1, q2, q12, eps = 1e-12) {
  if (is.na(q1) || is.na(q2) || is.na(q12)) return(NA_character_)

  s <- q1 + q2
  m <- max(q1, q2)

  if (q12 < m - eps) return("Nonlinear weaken")
  if (abs(q12 - m) <= eps) return("Single-factor dominate")
  if (q12 > m + eps && q12 < s - eps) return("Bi-factor enhance")
  if (abs(q12 - s) <= eps) return("Independent")
  if (q12 > s + eps) return("Nonlinear enhance")

  "Unclassified"
}

# ============================================================
# 3) Read data and perform consistency checks
# ============================================================

raw <- read_excel(infile)

dat <- raw %>%
  mutate(
    !!id_country := as.character(.data[[id_country]]) %>% str_trim(),
    across(all_of(c(y_var, x_vars)), ~ suppressWarnings(as.numeric(.x)))
  )

# Strict check: all required columns must be complete
cols_check <- c(id_country, y_var, x_vars)
na_cnt <- sapply(dat[, cols_check], function(v) sum(is.na(v)))

if (any(na_cnt > 0)) {
  print(na_cnt[na_cnt > 0])
  stop("Missing values detected. Please remove all rows with NA values in the country ID, response variable, and all explanatory variables before running this script.")
}

# Filter countries with insufficient sample size
dat <- dat %>%
  group_by(.data[[id_country]]) %>%
  filter(n() >= min_n_country) %>%
  ungroup()

# ============================================================
# 4) Country-level analysis: factor q + interaction q
# ============================================================

dat_list <- split(dat, dat[[id_country]])

run_one_country <- function(df_c, country_name) {
  n0 <- nrow(df_c)
  if (n0 < min_n_country) return(NULL)

  # ---- 4.1 Country-specific discretization
  k <- choose_k(n0, k_min = k_min, k_max = k_max)
  df_disc <- df_c

  binning_log <- vector("list", length(x_vars))
  names(binning_log) <- x_vars

  for (x in x_vars) {
    xnum <- df_disc[[x]]

    if (x %in% log_vars) {
      xnum <- log1p_shift(xnum)
    }

    if (x %in% zero_inflated_vars) {
      g <- disc_zero_inflated(xnum, k = k, min_bin_n = min_bin_n)
      note <- "0 as separate stratum; positive values discretized by within-country quantiles"
    } else {
      g <- disc_quantile(xnum, k = k, min_bin_n = min_bin_n)
      note <- if (x %in% log_vars) {
        "log1p (with shift if needed), then within-country quantiles"
      } else {
        "within-country quantiles"
      }
    }

    df_disc[[x]] <- g
    k_used <- if (all(is.na(g))) NA_integer_ else length(unique(g[!is.na(g)]))

    binning_log[[x]] <- tibble(
      country = country_name,
      var = x,
      n = n0,
      k_target = k,
      k_used = k_used,
      note = note
    )
  }

  binning_df <- bind_rows(binning_log)

  # ---- 4.2 Factor detector: q for each variable
  factor_df <- map_dfr(x_vars, function(x) {
    gx <- df_disc[[x]]

    if (all(is.na(gx)) || length(unique(gx)) < 2) {
      return(tibble(
        country = country_name,
        var = x,
        n_used = n0,
        q = NA_real_,
        p_value = NA_real_
      ))
    }

    tmp_df <- data.frame(
      response = df_disc[[y_var]],
      X = df_disc[[x]]
    )

    names(tmp_df) <- c(y_var, x)

    # The geodetector package is sensitive to variable type;
    # convert the stratified variable to character
    tmp_df[[x]] <- as.character(tmp_df[[x]])

    out_list <- factor_detector(y_var, x, tmp_df)
    out_df <- out_list[[1]]

    tibble(
      country = country_name,
      var = x,
      n_used = nrow(tmp_df),
      q = as.numeric(out_df[1, "q-statistic"]),
      p_value = as.numeric(out_df[1, "p-value"])
    )
  })

  # ---- 4.3 Interaction detector: q12 for each variable pair
  valid_x <- factor_df %>%
    filter(!is.na(q)) %>%
    pull(var)

  if (length(valid_x) < 2 || n0 < min_n_interaction) {
    inter_df <- tibble(
      country = character(),
      var1 = character(),
      var2 = character(),
      q12 = numeric(),
      q1 = numeric(),
      q2 = numeric(),
      interaction_type = character(),
      n_used = integer()
    )
  } else {
    tmp2_df <- df_disc[, c(y_var, valid_x), drop = FALSE]
    tmp2_df <- as.data.frame(tmp2_df)

    # Remove variables if discretization failed and produced NA
    for (v in valid_x) {
      if (any(is.na(tmp2_df[[v]]))) {
        valid_x <- setdiff(valid_x, v)
      }
    }

    if (length(valid_x) < 2) {
      inter_df <- tibble(
        country = character(),
        var1 = character(),
        var2 = character(),
        q12 = numeric(),
        q1 = numeric(),
        q2 = numeric(),
        interaction_type = character(),
        n_used = integer()
      )
    } else {
      tmp2_df <- df_disc[, c(y_var, valid_x), drop = FALSE]
      tmp2_df <- as.data.frame(tmp2_df)

      for (v in valid_x) {
        tmp2_df[[v]] <- as.factor(tmp2_df[[v]])
      }

      inter_raw <- interaction_detector(y_var, valid_x, tmp2_df)
      inter0 <- as.data.frame(inter_raw, stringsAsFactors = FALSE)

      if (ncol(inter0) >= 3) {
        inter0 <- inter0[, 1:3]
      }

      colnames(inter0) <- c("var1", "var2", "q12")
      inter0$q12 <- as.numeric(inter0$q12)

      # Remove diagonal pairs and duplicated unordered pairs
      inter_pairs <- inter0 %>%
        filter(var1 != var2) %>%
        mutate(vmin = pmin(var1, var2), vmax = pmax(var1, var2)) %>%
        distinct(vmin, vmax, .keep_all = TRUE) %>%
        transmute(var1 = vmin, var2 = vmax, q12 = q12)

      q_lookup <- factor_df %>%
        select(var, q)

      inter_df <- inter_pairs %>%
        left_join(q_lookup, by = c("var1" = "var")) %>%
        rename(q1 = q) %>%
        left_join(q_lookup, by = c("var2" = "var")) %>%
        rename(q2 = q) %>%
        mutate(
          interaction_type = mapply(classify_interaction, q1, q2, q12),
          country = country_name,
          n_used = nrow(tmp2_df),
          .before = 1
        )
    }
  }

  list(
    factor = factor_df,
    interaction = inter_df,
    binning = binning_df
  )
}

# Run analysis for all countries
res_list <- imap(dat_list, ~ run_one_country(.x, .y))
res_list <- compact(res_list)

factor_all <- bind_rows(map(res_list, "factor"))
interaction_all <- bind_rows(map(res_list, "interaction"))
binning_all <- bind_rows(map(res_list, "binning"))

# ============================================================
# 5) Export results
# ============================================================

write.xlsx(
  x = list(
    factor_q = factor_all,
    interaction_q = interaction_all,
    discretization_log = binning_all
  ),
  file = outfile,
  overwrite = TRUE
)

message("Done. Output saved to: ", normalizePath(outfile))

