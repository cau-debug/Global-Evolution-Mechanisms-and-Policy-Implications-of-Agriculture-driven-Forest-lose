# ============================================================
# Grid-level bivariate Moran's I
#
# Goal:
# For each country and each explanatory variable x, compute the
# bivariate Moran's I statistic between x and y (spatially lagged
# association), together with a permutation-based p-value.
#
# Output:
# An Excel file with two sheets:
# - bivariate_moran
# - weights_log
#
# Assumptions:
# 1) The input table must contain the information needed to build a
#    spatial weights matrix W (at minimum, grid-center coordinates
#    such as x/y or lon/lat, or alternatively row/column indices).
# 2) All rows with missing values have already been removed
#    (for the response variable and all explanatory variables).
# ============================================================

# install.packages(c("spdep", "sf", "openxlsx", "readxl", "dplyr", "stringr", "purrr", "tidyr"))

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(openxlsx)
library(spdep)
library(sf)

# ----------------------------
# 1) Parameters
# ----------------------------

# Use a relative path so the script can be shared publicly
infile <- "data/input/geodetector_input.xlsx"

id_country <- "country"
y_var <- "T_10_20"
x_vars <- c("dem", "slope", "gdp10", "pop10", "pre10", "tem10", "urban10", "npp10")

min_n_country <- 30
nsim <- 999        # number of permutations
k_nn <- 8          # number of nearest neighbors
styleW <- "W"      # row-standardized spatial weights

# Coordinate columns (longitude/latitude)
coord_lon <- "longitude"
coord_lat <- "latitude"

# Output file
outfile <- "outputs/bivariate_moranI_country.xlsx"

# ----------------------------
# 2) Read data and basic checks
# ----------------------------

raw <- read_excel(infile)

dat <- raw %>%
  mutate(
    !!id_country := as.character(.data[[id_country]]) %>% str_trim(),
    across(all_of(c(y_var, x_vars)), ~ suppressWarnings(as.numeric(.x)))
  )

cols_check <- c(id_country, y_var, x_vars)
na_cnt <- sapply(dat[, cols_check], function(v) sum(is.na(v)))

if (any(na_cnt > 0)) {
  stop("Missing values detected. Please remove all rows with NA values in the country ID, response variable, and all explanatory variables before running this script.")
}

dat <- dat %>%
  group_by(.data[[id_country]]) %>%
  filter(n() >= min_n_country) %>%
  ungroup()

dat_list <- split(dat, dat[[id_country]])

# ----------------------------
# 3) Build spatial weights
# Priority: lon/lat coordinates
# ----------------------------

has_cols <- function(df, cols) {
  all(cols %in% names(df))
}

build_listw_country <- function(df_c) {
  n <- nrow(df_c)

  if (n < 2) {
    stop("Not enough observations to build spatial weights.")
  }

  # Build KNN weights from longitude/latitude
  if (has_cols(df_c, c(coord_lon, coord_lat))) {
    coords <- as.matrix(df_c[, c(coord_lon, coord_lat)])

    knei <- knearneigh(coords, k = min(k_nn, n - 1))
    nb <- knn2nb(knei)
    lw <- nb2listw(nb, style = styleW, zero.policy = TRUE)

    return(list(
      lw = lw,
      note = paste0(
        "KNN weights using ",
        coord_lon, "/", coord_lat,
        ", k = ", min(k_nn, n - 1),
        " (longitude/latitude)"
      )
    ))
  }

  stop("Unable to construct spatial weights. Please provide coordinate columns (e.g., x/y or lon/lat) or grid row/column indices in the input table.")
}

# ----------------------------
# 4) Bivariate Moran's I
#
# I_xy = (n / S0) * (z_x' W z_y) / (z_x' z_x)
#
# Permutation test:
# Randomly permute y to obtain a two-sided p-value
# ----------------------------

biv_moran_I <- function(x, y, lw, nsim = 999) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]

  # This script assumes there are no missing values in the input.
  # If subsetting occurs here in future use cases, the spatial weights
  # object should be rebuilt accordingly.

  z_x <- as.numeric(scale(x))
  z_y <- as.numeric(scale(y))

  Wy <- lag.listw(lw, z_y, zero.policy = TRUE)
  n <- length(z_x)
  S0 <- spdep::Szero(lw)

  obs <- (n / S0) * as.numeric(t(z_x) %*% Wy) / as.numeric(t(z_x) %*% z_x)

  sim <- replicate(nsim, {
    z_yp <- sample(z_y)
    Wy_p <- lag.listw(lw, z_yp, zero.policy = TRUE)
    (n / S0) * as.numeric(t(z_x) %*% Wy_p) / as.numeric(t(z_x) %*% z_x)
  })

  p_two <- (sum(abs(sim) >= abs(obs)) + 1) / (nsim + 1)

  list(I = obs, p_value = p_two)
}

run_one_country_biv <- function(df_c, country_name) {
  n0 <- nrow(df_c)
  if (n0 < min_n_country) return(NULL)

  w <- build_listw_country(df_c)
  lw <- w$lw

  out <- map_dfr(x_vars, function(xn) {
    res <- biv_moran_I(df_c[[xn]], df_c[[y_var]], lw = lw, nsim = nsim)

    tibble(
      country = country_name,
      x_var = xn,
      y_var = y_var,
      n = n0,
      I_bivariate = res$I,
      p_value = res$p_value
    )
  })

  wlog <- tibble(
    country = country_name,
    n = n0,
    weights_note = w$note
  )

  list(
    biv = out,
    wlog = wlog
  )
}

# Run for all countries
res_list <- imap(dat_list, ~ run_one_country_biv(.x, .y)) %>%
  compact()

biv_all <- bind_rows(map(res_list, "biv"))
wlog_all <- bind_rows(map(res_list, "wlog"))

# ----------------------------
# 5) Export results
# ----------------------------

write.xlsx(
  x = list(
    bivariate_moran = biv_all,
    weights_log = wlog_all
  ),
  file = outfile,
  overwrite = TRUE
)

message("Done. Output saved to: ", normalizePath(outfile))

