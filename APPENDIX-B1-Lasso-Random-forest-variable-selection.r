### Set up 

install.packages(c("dplyr", "glmnet", "ranger", "broom", "purrr"))
library(dplyr)
library(glmnet)
library(ranger)
library(broom)
library(purrr)


### Data prep 
kpi_merged <- readRDS("/Users/maxglanville/Desktop/MPAID/api-209/final-exercise/data/kpi_merged.rds")
# Outcome and predictors
kpi_var <- "ratioCombined"
X_vars  <- names(kpi_merged)[65:ncol(kpi_merged)]

# Base df: keep outcome + predictors, drop rows with missing Y
df_raw <- kpi_merged %>%
  select(all_of(kpi_var), all_of(X_vars)) %>%
  filter(!is.na(.data[[kpi_var]]))

# --- 1A. Drop variables with too much missingness (e.g. > 40%) ---
miss_col <- colMeans(is.na(df_raw[X_vars]))
keep_vars <- names(miss_col[miss_col <= 0.40])

length(keep_vars)  # how many predictors survive

df1 <- df_raw %>%
  select(all_of(kpi_var), all_of(keep_vars))

# --- 1B. Drop municipalities with extreme missingness across remaining vars ---
miss_row <- rowMeans(is.na(df1[keep_vars]))
summary(miss_row)  # check distribution (you sent this screenshot already)

# Keep rows with <= 70% missing among the remaining vars
df2 <- df1[miss_row <= 0.70, ]

# --- 1C. Mean-impute remaining gaps (now they’re relatively small) ---

# Set up X matrix and y vector
y <- df2[[kpi_var]]
X <- as.matrix(df2[keep_vars])

# Column-wise mean imputation for X (no imputation on y)
for (j in seq_len(ncol(X))) {
  col_j <- X[, j]
  m_j   <- mean(col_j, na.rm = TRUE)
  col_j[is.na(col_j)] <- m_j
  X[, j] <- col_j
}

# Final modelling data frame (imputed predictors)
df_model <- data.frame(
  ratioCombined = y,
  X,
  check.names = FALSE
)


### Build helper function 
fit_post_ols <- function(vars, method_name, df = df_model, y_name = kpi_var) {
  vars <- unique(vars)
  vars <- vars[vars != "(Intercept)"]
  if (length(vars) == 0) return(NULL)
  
  fmla <- as.formula(
    paste(y_name, "~", paste(vars, collapse = " + "))
  )
  
  mod <- lm(fmla, data = df)
  
  broom::tidy(mod) %>%
    filter(term != "(Intercept)") %>%
    mutate(method = method_name, .before = 1)
}


### LASSO
set.seed(123)

lasso_cv <- cv.glmnet(
  x = as.matrix(df_model[keep_vars]),
  y = df_model[[kpi_var]],
  alpha = 1,         # LASSO
  nfolds = 10,
  standardize = TRUE
)

coef_lasso <- coef(lasso_cv, s = "lambda.1se")   # more parsimonious
selected_lasso <- rownames(coef_lasso)[coef_lasso[, 1] != 0]

post_lasso <- fit_post_ols(selected_lasso, "LASSO")


### Elastic net
set.seed(123)

enet_cv <- cv.glmnet(
  x = as.matrix(df_model[keep_vars]),
  y = df_model[[kpi_var]],
  alpha = 0.5,       # Elastic Net
  nfolds = 10,
  standardize = TRUE
)

coef_enet <- coef(enet_cv, s = "lambda.1se")
selected_enet <- rownames(coef_enet)[coef_enet[, 1] != 0]

post_enet <- fit_post_ols(selected_enet, "Elastic Net")


### Random forest 
top_k <- 20   # number of predictors you want from RF & correlations

set.seed(123)
rf_fit <- ranger(
  formula = as.formula(paste(kpi_var, "~ .")),
  data    = df_model,
  num.trees = 500,
  importance = "impurity"
)

rf_imp <- sort(rf_fit$variable.importance, decreasing = TRUE)
selected_rf <- names(rf_imp)[1:min(top_k, length(rf_imp))]

post_rf <- fit_post_ols(selected_rf, "Random Forest")


### Correlations
# Pearson correlations between each predictor and Y
cors <- sapply(df_model[keep_vars], function(z) cor(z, df_model[[kpi_var]], use = "complete.obs"))

cors_abs <- sort(abs(cors), decreasing = TRUE)
selected_corr <- names(cors_abs)[1:min(top_k, length(cors_abs))]

post_corr <- fit_post_ols(selected_corr, "Correlation")

### Results 
results_all <- bind_rows(
  post_lasso,
  post_enet,
  post_rf,
  post_corr
)

# Optional: join on code descriptions
# assuming `unique_codes` has columns: code, name_en

results_labeled <- results_all %>%
  rename(code = term) %>%
  left_join(unique_codes, by = "code") %>%
  select(method, code, name_en, estimate, std.error, statistic, p.value)

results_labeled


comparison_wide <- results_labeled %>%
  select(method, code, name_en, estimate) %>%     # <-- keep label column
  tidyr::pivot_wider(
    names_from = method,
    values_from = estimate
  )


comparison_wide




