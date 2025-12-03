kpi_merged <- kpi_merged %>%
  mutate(
    ratio_vaccine      = ratio_vaccine * 100,
    ratio_diabetes     = ratio_diabetes * 100,
    ratio_hypertension = ratio_hypertension * 100,
    ratio_dental       = ratio_dental * 100,
    ratio_stdTest      = ratio_stdTest * 100,
    ratio_papsmear     = ratio_papsmear * 100,
    ratio_prenatalVisit = ratio_prenatalVisit * 100
  )


model2 <- lm(
  ratio_vaccine ~ 
    ln_DFSAUSM_pc + 
    ADH_IDHM_E_scaled +
    black_pct + 
    ADH_PPOB +
    ln_pop + 
    ln_gdp_pc +
    rural_pct +
    factor(region),
  data = kpi_merged
)

summary(model2)

model3 <- lm(
  ratio_diabetes ~ 
    ln_DFSAUSM_pc + 
    ADH_IDHM_E_scaled +
    black_pct + 
    ADH_PPOB +
    ln_pop + 
    ln_gdp_pc +
    rural_pct +
    factor(region),
  data = kpi_merged
)

summary(model3)

model4 <- lm(
  ratio_hypertension ~ 
    ln_DFSAUSM_pc + 
    ADH_IDHM_E_scaled +
    black_pct + 
    ADH_PPOB +
    ln_pop + 
    ln_gdp_pc +
    rural_pct +
    factor(region),
  data = kpi_merged
)

summary(model4)

model5 <- lm(
  ratio_dental ~ 
    ln_DFSAUSM_pc + 
    ADH_IDHM_E_scaled +
    black_pct + 
    ADH_PPOB +
    ln_pop + 
    ln_gdp_pc +
    rural_pct +
    factor(region),
  data = kpi_merged
)

summary(model5)

model6 <- lm(
  ratio_stdTest ~ 
    ln_DFSAUSM_pc + 
    ADH_IDHM_E_scaled +
    black_pct + 
    ADH_PPOB +
    ln_pop + 
    ln_gdp_pc +
    rural_pct +
    factor(region),
  data = kpi_merged
)

summary(model6)

model7 <- lm(
  ratio_papsmear ~ 
    ln_DFSAUSM_pc + 
    ADH_IDHM_E_scaled +
    black_pct + 
    ADH_PPOB +
    ln_pop + 
    ln_gdp_pc +
    rural_pct +
    factor(region),
  data = kpi_merged
)

summary(model7)

model8 <- lm(
  ratio_prenatalVisit ~ 
    ln_DFSAUSM_pc + 
    ADH_IDHM_E_scaled +
    black_pct + 
    ADH_PPOB +
    ln_pop + 
    ln_gdp_pc +
    rural_pct +
    factor(region),
  data = kpi_merged
)

summary(model8)
