# ============================================================
# Two-period q-value clustering with K constrained to 4-6
#
# Metrics used for model selection:
# - Silhouette coefficient (higher is better)
# - Davies-Bouldin index (lower is better)
# - Calinski-Harabasz index (higher is better)
#
# Final K is selected based on:
# - metric ranking
# - interpretability
# - clustering stability (ARI)
#
# Output files:
# - country_clusters_km.csv
# - country_cluster_list_km.csv
# - cluster_centers_km.csv
# - cluster_profile_km.csv
# - cluster_top1_share_km.csv
# - k_selection_metrics.csv
# - stability_ari_summary.csv
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(cluster)
library(mclust)

# -----------------------------
# 0) File path and basic settings
# -----------------------------
infile <- "data/input/q_value_clustering_input.xlsx"
set.seed(42)

# -----------------------------
# 1) Read long-format table and rename columns
# -----------------------------
df <- read_excel(infile) %>%
  rename(
    q00_10 = `q_2000-2010`,
    q10_20 = `q_2010-2020`
  ) %>%
  mutate(
    country = as.character(country),
    var = as.character(var),
    q00_10 = as.numeric(q00_10),
    q10_20 = as.numeric(q10_20)
  )

# -----------------------------
# 2) Standardize variable names
# -----------------------------
df <- df %>%
  mutate(
    var = str_replace_all(var, "\\s+", " "),
    var = case_when(
      var %in% c("Urban area", "Urban Area", "Urban") ~ "Urban_area",
      var %in% c("NPP", "NPPy") ~ "NPP",
      var %in% c("Elevation", "Slope", "GDP", "Population",
                 "Precipitation", "Temperature") ~ var,
      TRUE ~ var
    )
  )

vars8 <- c(
  "Elevation", "Slope", "GDP", "Population",
  "Precipitation", "Temperature", "Urban_area", "NPP"
)

# -----------------------------
# 3) Convert to wide format:
#    one 16-dimensional vector per country
# -----------------------------
wide00 <- df %>%
  select(country, var, q00_10) %>%
  pivot_wider(names_from = var, values_from = q00_10, names_prefix = "q00_10_")

wide10 <- df %>%
  select(country, var, q10_20) %>%
  pivot_wider(names_from = var, values_from = q10_20, names_prefix = "q10_20_")

X <- wide00 %>%
  inner_join(wide10, by = "country")

# Ensure all required columns exist and follow a fixed order
need00 <- paste0("q00_10_", vars8)
need10 <- paste0("q10_20_", vars8)

for (cc in c(need00, need10)) {
  if (!cc %in% names(X)) {
    X[[cc]] <- NA_real_
  }
}

X <- X %>%
  select(country, all_of(need00), all_of(need10))

# -----------------------------
# 4) Missing values:
#    median imputation
# -----------------------------
num_cols <- setdiff(names(X), "country")
X_imp <- X

for (cc in num_cols) {
  med <- median(X_imp[[cc]], na.rm = TRUE)
  if (!is.finite(med)) med <- 0
  X_imp[[cc]][is.na(X_imp[[cc]])] <- med
}

# -----------------------------
# 5) Standardize variables
# -----------------------------
X_mat <- scale(as.matrix(X_imp[, num_cols]))
stopifnot(!any(is.na(X_mat)))
stopifnot(all(is.finite(X_mat)))

# ============================================================
# 6) Metric functions
# ============================================================

# Davies-Bouldin index (lower is better)
dbi_manual <- function(X, cl) {
  X <- as.matrix(X)
  cl <- as.integer(cl)
  labs <- sort(unique(cl))
  
  centers <- t(sapply(labs, function(g) {
    colMeans(X[cl == g, , drop = FALSE])
  }))
  
  S <- sapply(labs, function(g) {
    Xg <- X[cl == g, , drop = FALSE]
    cg <- as.numeric(centers[which(labs == g), ])
    d <- sqrt(rowSums(sweep(Xg, 2, cg, FUN = "-")^2))
    mean(d)
  })
  
  M <- as.matrix(dist(centers))
  diag(M) <- Inf
  
  R <- outer(S, S, "+") / M
  mean(apply(R, 1, max, na.rm = TRUE), na.rm = TRUE)
}

# Calinski-Harabasz index (higher is better)
ch_manual <- function(X, cl) {
  X <- as.matrix(X)
  cl <- as.integer(cl)
  n <- nrow(X)
  labs <- sort(unique(cl))
  k <- length(labs)
  
  if (k <= 1 || k >= n) return(NA_real_)
  
  overall_center <- colMeans(X)
  
  centers <- t(sapply(labs, function(g) {
    colMeans(X[cl == g, , drop = FALSE])
  }))
  
  nk <- sapply(labs, function(g) sum(cl == g))
  
  ssb <- sum(sapply(seq_along(labs), function(i) {
    nk[i] * sum((centers[i, ] - overall_center)^2)
  }))
  
  ssw <- sum(sapply(seq_along(labs), function(i) {
    Xg <- X[cl == labs[i], , drop = FALSE]
    cg <- centers[i, ]
    sum(rowSums(sweep(Xg, 2, cg, FUN = "-")^2))
  }))
  
  if (ssw <= 0) return(NA_real_)
  
  (ssb / (k - 1)) / (ssw / (n - k))
}

# Evaluate clustering quality for a given K
eval_k <- function(K, X_mat, nstart = 300) {
  km <- kmeans(X_mat, centers = K, nstart = nstart)
  cl <- km$cluster
  
  sil <- cluster::silhouette(cl, dist(X_mat))
  sil_avg <- mean(sil[, "sil_width"])
  
  dbi <- dbi_manual(X_mat, cl)
  ch <- ch_manual(X_mat, cl)
  
  tibble(
    K = K,
    silhouette = sil_avg,
    DBI = dbi,
    CH = ch
  )
}

# -----------------------------
# 7) Compute metrics for K = 4:6
# -----------------------------
k_grid <- 4:6

metrics <- do.call(rbind, lapply(k_grid, eval_k, X_mat = X_mat, nstart = 300)) %>%
  mutate(
    rank_sil = rank(-silhouette, ties.method = "min"),
    rank_dbi = rank(DBI, ties.method = "min"),
    rank_ch  = rank(-CH, ties.method = "min"),
    rank_sum = rank_sil + rank_dbi + rank_ch
  ) %>%
  arrange(rank_sum, rank_sil, rank_dbi, rank_ch)

print(metrics)

# Candidate K:
# keep values close to the best silhouette, then use rank sum
best_sil <- max(metrics$silhouette)
cand <- metrics %>%
  filter(silhouette >= 0.95 * best_sil) %>%
  arrange(rank_sum)

if (nrow(cand) == 0) {
  cand <- metrics %>% slice(1)
}

K_final <- cand$K[1]
cat("Selected K_final =", K_final, "\n")

# -----------------------------
# 8) Fit final K-means model
# -----------------------------
km_final <- kmeans(X_mat, centers = K_final, nstart = 500)
X_out <- X_imp %>%
  mutate(cluster_km = km_final$cluster)

# -----------------------------
# 9) Cluster centers
#    mean values on the original scale
# -----------------------------
cluster_centers <- X_out %>%
  group_by(cluster_km) %>%
  summarise(across(all_of(num_cols), mean), .groups = "drop") %>%
  arrange(cluster_km)

# -----------------------------
# 10) Top-1 factor shares
#     calculated separately for each period
# -----------------------------
cols00 <- paste0("q00_10_", vars8)
cols10 <- paste0("q10_20_", vars8)

top1_fun <- function(row, cols) {
  cols[which.max(as.numeric(row[cols]))]
}

top1_share <- X_out %>%
  rowwise() %>%
  mutate(
    top1_00_10 = top1_fun(cur_data(), cols00),
    top1_10_20 = top1_fun(cur_data(), cols10)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(top1_00_10, top1_10_20),
    names_to = "period",
    values_to = "top1"
  ) %>%
  group_by(cluster_km, period, top1) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster_km, period) %>%
  mutate(share = n / sum(n)) %>%
  arrange(cluster_km, period, desc(share))

# -----------------------------
# 11) Composite dimensions
#     and temporal change
# -----------------------------
cluster_profile <- X_out %>%
  mutate(
    topo00  = q00_10_Elevation + q00_10_Slope,
    clim00  = q00_10_Precipitation + q00_10_Temperature,
    socio00 = q00_10_GDP + q00_10_Population + q00_10_Urban_area,
    prod00  = q00_10_NPP,
    
    topo10  = q10_20_Elevation + q10_20_Slope,
    clim10  = q10_20_Precipitation + q10_20_Temperature,
    socio10 = q10_20_GDP + q10_20_Population + q10_20_Urban_area,
    prod10  = q10_20_NPP,
    
    d_topo  = topo10 - topo00,
    d_clim  = clim10 - clim00,
    d_socio = socio10 - socio00,
    d_prod  = prod10 - prod00
  ) %>%
  group_by(cluster_km) %>%
  summarise(
    n = n(),
    topo00 = mean(topo00),
    clim00 = mean(clim00),
    socio00 = mean(socio00),
    prod00 = mean(prod00),
    topo10 = mean(topo10),
    clim10 = mean(clim10),
    socio10 = mean(socio10),
    prod10 = mean(prod10),
    d_topo = mean(d_topo),
    d_clim = mean(d_clim),
    d_socio = mean(d_socio),
    d_prod = mean(d_prod),
    .groups = "drop"
  ) %>%
  arrange(cluster_km)

# -----------------------------
# 12) Stability analysis:
#     repeat clustering across seeds
#     and compute pairwise ARI
# -----------------------------
seeds <- c(1, 7, 42, 123, 2024, 999)

labs_list <- lapply(seeds, function(s) {
  set.seed(s)
  kmeans(X_mat, centers = K_final, nstart = 500)$cluster
})

ari <- expand.grid(i = seq_along(seeds), j = seq_along(seeds)) %>%
  filter(i < j) %>%
  rowwise() %>%
  mutate(
    seed_i = seeds[i],
    seed_j = seeds[j],
    ARI = mclust::adjustedRandIndex(labs_list[[i]], labs_list[[j]])
  ) %>%
  ungroup() %>%
  select(seed_i, seed_j, ARI)

stability_summary <- ari %>%
  summarise(
    K_final = K_final,
    ARI_mean = mean(ARI),
    ARI_min = min(ARI),
    ARI_p25 = quantile(ARI, 0.25),
    ARI_median = median(ARI),
    ARI_p75 = quantile(ARI, 0.75),
    ARI_max = max(ARI)
  )

print(ari)
print(stability_summary)
print(table(km_final$cluster))

# -----------------------------
# 13) Save outputs
# -----------------------------
write_csv(X_out, "country_clusters_km.csv")
write_csv(cluster_centers, "cluster_centers_km.csv")
write_csv(cluster_profile, "cluster_profile_km.csv")
write_csv(top1_share, "cluster_top1_share_km.csv")
write_csv(metrics, "k_selection_metrics.csv")
write_csv(stability_summary, "stability_ari_summary.csv")
write_csv(X_out %>% select(country, cluster_km), "country_cluster_list_km.csv")

cat("Done. Files saved to the working directory.\n")