
### Packages
#install.packages(c("lmtest", "sandwich"))  # if you want robust SEs
library(lmtest)
library(sandwich)
#install.packages("tidyverse")
library(tidyverse)

### Import data 
# Load kpi_merged
#kpi_merged <- readRDS("C:/Users/mag1695/Desktop/kpi_merged.rds")
#kpi_merged221 <- readRDS("C:/Users/mag1695/Desktop/kpi_merged221.rds")
kpi_merged <- readRDS("/Users/maxglanville/Desktop/MPAID/api-209/final-exercise/data/kpi_merged.rds")
#kpi_merged221 <- readRDS("/Users/maxglanville/Desktop/MPAID/api-209/final-exercise/data/kpi_merged221.rds")

# Load unique_codes
#unique_codes <- readRDS("C:/Users/mag1695/Desktop/unique_codes.rds")
unique_codes <- readRDS("/Users/maxglanville/Desktop/MPAID/api-209/final-exercise/data/unique_codes.rds")

# Load all code
#mdata <- readRDS("C:/Users/mag1695/Desktop/series_md.rds")
mdata <- readRDS("/Users/maxglanville/Desktop/MPAID/api-209/final-exercise/data/series_md.rds")

### CLEAN DATA 

# Download area data
# install.packages("ipeadatar")
# library(ipeadatar)
# area <- ipeadata("AREA")
# area_now <- area %>% filter(date >= "2023-01-02")
# area_now_mun <- area_now %>% filter(is.na(uname))
# mun <- area_now_mun %>%
#   mutate(
#     tcode = as.character(tcode),        # ensure string
#     first6 = str_sub(tcode, 1, 6),      # digits 1–6
#     digit7 = str_sub(tcode, 7, 7)       # digit 7
#   )
# mun <- mun %>% mutate(area = value)
# kpi_merged <- kpi_merged %>%
#   left_join(
#     mun %>% select(first6, area),
#     by = c("municipality_id" = "first6")
#   )
# kpi_merged221 <- kpi_merged221 %>%
#   left_join(
#     mun %>% select(first6, area),
#     by = c("municipality_id" = "first6")
#   )


# Log spending per capita 
kpi_merged <- kpi_merged %>%
  mutate(
    # Per-capita municipal health spending
    DFSAUSM_pc    = DFSAUSM / ADH_PESOTOT,
    # Log of per-capita spending (add 1 to avoid log(0))
    ln_DFSAUSM_pc = log(DFSAUSM_pc + 1)
  )

# Share of black and rural 
kpi_merged <- kpi_merged %>%
  mutate(
    black_pct = 100 * ADH_PESOTOT_NEG / ADH_PESOTOT,
    rural_pct = 100 * ADH_PESORUR / ADH_PESOTOT
  )

# Population 
kpi_merged <- kpi_merged %>%
  mutate(
    ln_pop = log(ADH_PESOTOT + 1)
  )

# Remake education score 
kpi_merged <- kpi_merged %>%
  mutate(
    ADH_IDHM_E_scaled = ADH_IDHM_E*100
  )

# # Pop density 
# kpi_merged <- kpi_merged %>%
#   mutate(
#     pop_density = ADH_PESOTOT / area
#   )

# # log pop dneisty 
# kpi_merged <- kpi_merged %>%
#   mutate(
#     ln_pop_density = log(pop_density + 1)
#   )

# log gdp
kpi_merged <- kpi_merged %>%
  mutate(
    ln_gdp = log(PIB_IBGE_5938_37 + 1)
  )

# log gdp_pc
kpi_merged <- kpi_merged %>%
  mutate(
    gdp_pc = PIB_IBGE_5938_37 / ADH_PESOTOT,
    ln_gdp_pc = log(gdp_pc + 1)
  )

# # Pop density Quartiles
# kpi_merged <- kpi_merged %>%
#   mutate(
#     # quartiles of *raw* density
#     dens_q = ntile(pop_density, 4),
#     
#     # make it a labelled factor so R automatically creates dummies
#     dens_q = factor(
#       dens_q,
#       levels = 1:4,
#       labels = c("Q1_lowest", "Q2", "Q3", "Q4_highest")
#     )
#   )

# Regions 
kpi_merged <- kpi_merged %>%
  mutate(
    region = case_when(
      # North
      state_abbr %in% c("AC","AP","AM","PA","RO","RR","TO") ~ "North",
      
      # Northeast
      state_abbr %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Northeast",
      
      # Central-West
      state_abbr %in% c("DF","GO","MT","MS") ~ "Central-West",
      
      # Southeast
      state_abbr %in% c("ES","MG","RJ","SP") ~ "Southeast",
      
      # South
      state_abbr %in% c("PR","RS","SC") ~ "South",
      
      TRUE ~ NA_character_
    ),
    region = factor(region,
                    levels = c("North","Northeast","Central-West","Southeast","South"))
  )

# Baseline model 

model <- lm(
  ratioCombined ~ 
    ln_DFSAUSM_pc + 
    ADH_IDHM_E_scaled +
    black_pct + 
    ADH_PPOB +
    ln_pop + 
    ln_gdp +
    rural_pct +
    #ln_pop_density +
    factor(region),
  data = kpi_merged
)

summary(model)


