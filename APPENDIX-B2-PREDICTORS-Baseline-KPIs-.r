library(modelsummary)
library(sandwich)
library(lmtest)

# List of all models
models <- list(
  "Combined KPI score" = model,
  "Vaccination"        = model2,
  "Diabetes"           = model3,
  "Hypertension"       = model4,
  "Dental"             = model5,
  "STD"                = model6,
  "Papsmear"           = model7,
  "Prenatal visits"    = model8
)

# Robust variance-covariance matrices for all models
vcov_list <- lapply(models, function(m) vcovHC(m, type = "HC1"))

# Clean coefficient names for table
coef_labels <- c(
  "(Intercept)"      = "Constant",
  "ln_DFSAUSM_pc"    = "Log health spending per capita",
  "ADH_IDHM_E_scaled"       = "Education index (IDHM-E)",
  "black_pct"        = "Black population share (%)",
  "ADH_PPOB"         = "Poverty rate (%)",
  "ln_pop"           = "Log population",
  "ln_gdp_pc"           = "Log GDP per capita",
  "rural_pct"        = "Rural population share (%)"
  # region FE will appear automatically unless omitted
)



library(tibble)

modelsummary(
  models,
  vcov = vcov_list,
  statistic = NULL,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = coef_labels,
  gof_omit = "IC|Adj|Log|F$",
  add_rows = tibble::tibble(
    term               = "Region fixed effects",
    `Combined KPI score` = "Yes",
    Vaccination        = "Yes",
    Diabetes           = "Yes",
    Hypertension       = "Yes",
    Dental             = "Yes",
    STD                = "Yes",
    Papsmear           = "Yes",
    `Prenatal visits`  = "Yes"
  ),
  output = "/Users/maxglanville/Desktop/MPAID/api-209/final-exercise/all_kpi_models.docx"    # or "all_kpi_models.docx" / "all_kpi_models.tex"
)


