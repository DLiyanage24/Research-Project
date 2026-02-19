# ============================================================
# Generate 24 lineups using EXACT Table 1 parameters only
# (6 curvature pairs × 2 replicates × 2 scales)
# -> outputs 24 CSV files under ./lineup_csv/
# ============================================================

# Packages

library(dplyr)
library(tibble)
library(glue)
library(nullabor)  # for lineup(null=..., true=...)


# ---- Fixed constraints (paper) ----------------------------------------------
X_DOMAIN <- c(0, 20)
Y_RANGE  <- c(10, 100)
N_POINTS <- 50

# ---- Table 1 parameters (EXACT) ---------------------------------------------
# Curvature levels and their (alpha, beta, theta, sigma)
params_tbl1 <- tribble(
  ~curvature, ~alpha, ~beta, ~theta,  ~sigma,
  "high",       0.91,  0.23,   9.10,   0.25,
  "medium",     6.86,  0.13,   3.14,   0.12,
  "low",       37.26,  0.06, -27.26,   0.05
)

param_of <- function(level, name) {
  params_tbl1 %>% filter(curvature == level) %>% pull({{name}})
}

# ---- Panel simulator (Algorithm 2 with alpha-tilde correction) ---------------
simulate_panel <- function(alpha, beta, theta, sigma, N = N_POINTS,
                           domain = X_DOMAIN) {
  # 1) evenly spaced grid across full domain
  x_grid <- seq(domain[1], domain[2], length.out = 34)
  # 2) sample N x-values (with replacement)
  x <- sample(x_grid, N, replace = TRUE)
  # 3) jitter to avoid vertical stacks
  x <- jitter(x, amount = 0.2)
  # 4) adjust alpha for multiplicative noise (E[exp(e)] = exp(sigma^2/2))
  alpha_tilde <- alpha * exp(sigma^2 / 2)
  # 5) simulate y
  y <- alpha_tilde * exp(beta * x + rnorm(N, 0, sigma)) + theta
  tibble(x = x, y = y)
}

# ---- Six target–null curvature combinations ---------------------------------
combos <- tribble(
  ~target,  ~null,
  "high",   "low",
  "low",    "high",
  "medium", "low",
  "low",    "medium",
  "high",   "medium",
  "medium", "high"
)

# ---- Build one lineup (returns a data frame for saving) ----------------------
simulate_lineup_df <- function(target_level, null_level, seed = 1, scale_type = "linear") {
  set.seed(seed)
  
  tgt <- simulate_panel(
    alpha = param_of(target_level, alpha),
    beta  = param_of(target_level, beta),
    theta = param_of(target_level, theta),
    sigma = param_of(target_level, sigma)
  )
  
  null_fun <- function(i) simulate_panel(
    alpha = param_of(null_level, alpha),
    beta  = param_of(null_level, beta),
    theta = param_of(null_level, theta),
    sigma = param_of(null_level, sigma)
  )
  
  # ✅ new version–aware lineup generation
  if ("lineup_generate" %in% ls("package:nullabor")) {
    d <- lineup_generate(null_fun, true = tgt, n = 20)
  } else {
    d <- lineup(true = tgt, null = null_fun, nsim = 19)
  }
  
  pos <- attr(d, "pos")
  
  d %>%
    mutate(
      .type       = ifelse(.sample == pos, "target", "null"),
      target_pos  = pos,
      target_curv = target_level,
      null_curv   = null_level,
      scale_type  = scale_type
    )
}


# ---- Output directory --------------------------------------------------------
OUT_DIR <- "Lineups/lineup_csv"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ---- Generate 24 CSVs: 6 combos × 2 reps × 2 scales -------------------------
R_REPS <- 2
replicate_seeds <- 100 + seq_len(R_REPS)  # simple distinct seeds

for (i in seq_len(nrow(combos))) {
  tgt <- combos$target[i]; nul <- combos$null[i]
  
  for (r in seq_along(replicate_seeds)) {
    s <- replicate_seeds[r]
    
    # linear-scale lineup
    df_lin <- simulate_lineup_df(tgt, nul, seed = s, scale_type = "linear")
    cat(glue("Target position (linear) for {tgt} vs {nul}, rep {r}: {unique(df_lin$target_pos)}\n"))
    write.csv(df_lin,
              file.path(OUT_DIR, glue("lineup_{tgt}_vs_{nul}_rep{r}_linear.csv")),
              row.names = FALSE)
    
    # log-scale lineup (data identical; plotted later with log scale)
    df_log <- simulate_lineup_df(tgt, nul, seed = s, scale_type = "log")
    cat(glue("Target position (log)    for {tgt} vs {nul}, rep {r}: {unique(df_log$target_pos)}\n"))
    write.csv(df_log,
              file.path(OUT_DIR, glue("lineup_{tgt}_vs_{nul}_rep{r}_log.csv")),
              row.names = FALSE)
  }
}

cat(glue("\nDone. Wrote 24 files to {normalizePath(OUT_DIR)}\n"))
cat("Each CSV has: x, y, .sample (1..20), .type (target/null), target_pos, target_curv, null_curv, scale_type.\n")

