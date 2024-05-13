library(tidyverse)
library(gtsummary)
library(survey)
library(srvyr)


df <- read_rds("df.rds")



t1 <- df %>% 
  gtsummary::tbl_summary(
    include = ends_with("_group")
  )
t1



NHANES_design <- df %>% 
  drop_na(WT2YR) %>% 
  as_survey_design(
    ids               = SDMVPSU,
    strata            = SDMVSTRA,
    nest              = TRUE,
    weights           = WT2YR,  #<<
    survey.lonely.psu = "adjust"
  )


t2 <- NHANES_design %>%
  tbl_svysummary(
    include = ends_with("_group"),
    statistic = list(all_categorical() ~ "{n} ({p}%)")
  ) %>% 
  add_ci()
t2



list(t2, t1) %>% 
  tbl_merge(
    tab_spanner = c("**权重估算人群数量**", "**非权重估算人群**")
  ) %>% 
  modify_table_body(
    ~ .x %>% dplyr::relocate(stat_0_2, .before = stat_0_1)
  ) 


