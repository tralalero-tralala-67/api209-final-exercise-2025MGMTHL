######################################
### 1. SET UP
######################################

### Load packages
library(haven)
library(tidyverse)

### Import data 
data <- read_dta("/Users/maxglanville/Desktop/MPAID/api-209/ipea_all2.dta")
#data <- readRDS("/Users/maxglanville/Desktop/MPAID/api-209/ipea_all.rds")
mdata <- readRDS("/Users/maxglanville/Desktop/MPAID/api-209/series_md.rds")

######################################
### 2. FILTER COMBNINED IPEA DATA FOR 25_2
######################################


### Filter IPEA data
# 1. Keep rows where uname is NA or == 8
data <- data %>%
  mutate(uname_num = as.numeric(uname)) %>% 
  filter(is.na(uname_num) | uname_num == 8)

# 2. Keep only rows where tcode is 6 or 7 digits long
data <- data %>%
  mutate(
    tcode_chr = as.character(tcode),
    tcode_len = nchar(trimws(tcode_chr))
  ) %>%
  filter(tcode_len %in% c(6, 7))

# 3. Coerce date column 
data <- data %>% mutate(date = as.Date(as.character(date)))
typeof(data$date)

# 3. Filter out future data (2026+)
data <- data %>%
  filter(date < as.Date("2026-01-01"))

# 4. Split muncicipality id
data <- data %>% mutate(
    first6 = if_else(tcode_len == 7, substr(tcode_chr, 1, 6), NA_character_),
    last1  = if_else(tcode_len == 7, substr(tcode_chr, 7, 7), NA_character_)
  )

# 5. Keep relevant columns
data <- data %>% select(code, date, value, first6)

# 6c. Export for Herman
write_dta(
  data,
  "/Users/maxglanville/Desktop/MPAID/api-209/ipea_clean.dta"
)

# 6d. Keep version of original data 
odata <- data

# 6. Keep latest data
data <- data %>%
  group_by(code, first6) %>%
  arrange(desc(date), .by_group = TRUE) %>%  # newest date first in each group
  slice(1) %>%                               # keep the first (latest) row
  ungroup()

# 6b. Arrange data
data <- data %>% arrange(first6, code, desc(date))

# 6d. additional df for inequality comparisons
data_221 <- odata %>%
  filter(date <= as.Date("2022-04-30")) %>%      # drop data after April 2022
  group_by(code, first6) %>%
  arrange(desc(date), .by_group = TRUE) %>%      # newest date first in each group
  slice(1) %>%                                   # keep the latest row
  ungroup() %>%
  arrange(first6, code, desc(date))

# # 6c. Export for Herman
# write_dta(
#   data,
#   "/Users/maxglanville/Desktop/MPAID/api-209/ipea_clean.dta"
# )

# 7a. Drop date 
data <- data %>% select(-date)

# 7. Pivot wider
data_wide <- data %>%
  pivot_wider(
    names_from = code,
    values_from = value
  )


### Final variables included 
unique_codes <- data %>% distinct(code)

# # Merge in name of final codes 
# merged_codes <- unique_codes %>%
#   left_join(
#     mdata %>% select(code, name, name_en, freq, lastupdate),
#     by = "code"
#   )

######################################
### 3. FILTER KPI DATA FOR 25_2
######################################

kpi <- read_dta("/Users/maxglanville/Desktop/Combined.dta")
kpi_251 <- kpi %>% filter(period == "25_1")
kpi_221 <- kpi %>% filter(period == "22_1")

######################################
### 4. MERGE TOGETHER 
######################################

# March types 
kpi_251 <- kpi_251 %>%
  mutate(municipality_id = as.character(municipality_id))

data_wide <- data_wide %>%
  mutate(first6 = as.character(first6))

# Merge
kpi_merged <- kpi_251 %>%
  left_join(data_wide, by = c("municipality_id" = "first6"))

######################################
### 4. MERGE TOGETHER FOR 22_1
######################################

## Finish making 22_1
# 7a. Drop date 
data_221 <- data_221 %>% select(-date)

# 7. Pivot wider
data_wide221 <- data_221 %>%
  pivot_wider(
    names_from = code,
    values_from = value
  )


# March types 
kpi_221 <- kpi_221 %>%
  mutate(municipality_id = as.character(municipality_id))

data_wide221 <- data_wide221 %>%
  mutate(first6 = as.character(first6))

# Merge
kpi_merged221 <- kpi_221 %>%
  left_join(data_wide221, by = c("municipality_id" = "first6"))

######################################
### 5. Code list 
######################################

# Merge in series' names
unique_codes <- unique_codes %>%
  left_join(
    mdata %>% select(code, name_en),
    by = "code"
  )


######################################
### 5. Export 
######################################

# Export kpi_merged
saveRDS(
  kpi_merged,
  file = "/Users/maxglanville/Desktop/MPAID/api-209/kpi_merged.rds"
)

# Export kpi_merged221
saveRDS(
  kpi_merged221,
  file = "/Users/maxglanville/Desktop/MPAID/api-209/kpi_merged221.rds"
)

# Export unique_codes
saveRDS(
  unique_codes,
  file = "/Users/maxglanville/Desktop/MPAID/api-209/unique_codes.rds"
)


# ######################################
# ### 3. FILTER IPEA DATA FOR 25_2
# ######################################
# 
# # Keep recent data
# data7 <- data6 %>%
#   group_by(first6, code) %>%
#   filter(date == max(date)) %>%
#   ungroup()
# 
# # Keep relevant columns
# data8 <- data7 %>% select(code, date, value, first6)
# 
# # Check unique identification 
# dup_check <- data8 %>%
#   count(code, date, first6) %>%
#   filter(n > 1)
# 
# # Merge in series' names
# unique_codes <- unique_codes %>%
#   left_join(
#     mdata %>% select(code, name_en),
#     by = "code"
#   )
# 
# # Check unique identification again 
# dup_check <- data9 %>%
#   count(code, first6) %>%
#   filter(n > 1)
# 
# # Remove old data again --> didn't work before
# data10 <- data9 %>%
#   mutate(date = as.Date(date)) %>%        # only if not already Date
#   group_by(code, first6) %>%
#   arrange(desc(date), .by_group = TRUE) %>%
#   slice(1) %>%                            # keep the first (latest) row in each group
#   ungroup()
# 
# # Keep variable dictionary
# var_dict <- data10 %>%
#   distinct(code, name_en)
# 
# # Free up space before pivot
# rm(list = paste0("data", 0:9))
# 
# # Pivot wider
# data_wide <- data10 %>%
#   select(first6, date, code, value) %>%
#   pivot_wider(
#     names_from = code,
#     values_from = value
#   )
# 
# # Arrange
# data_wide <- data_wide %>% arrange(first6, desc(date))
# 
# # Keep latest (not working pre-pivot)
# data_latest <- data_wide %>%
#   group_by(first6) %>%
#   slice(1) %>%
#   ungroup()
# 
