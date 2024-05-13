library(tidyverse)
library(gtsummary)
library(survey)
library(srvyr)
library(svyVGAM)
source("./helper/tidy_svyVGAM.R")


df <- read_rds("df.rds")
  


df_design <- df %>% 
  drop_na() %>% 
  as_survey_design(
    ids               = SDMVPSU,
    strata            = SDMVSTRA,
    nest              = TRUE,
    weights           = WT2YR,  
    survey.lonely.psu = "adjust"
  )  
 
 
mod1 <- svyVGAM::svy_vglm(
    formula = pain_group ~ caffeine_group,
    design  = df_design, 
    family  = multinomial(refLevel = "无疼痛或疼痛持续时间<24h")
  ) 



mod2 <- svyVGAM::svy_vglm(
  formula = pain_group ~ caffeine_group + age_group + sex_group + race_group,
  design  = df_design, 
  family  = multinomial(refLevel = "无疼痛或疼痛持续时间<24h")
) 



mod3 <- svyVGAM::svy_vglm(
  formula = pain_group ~ caffeine_group + 
                         age_group + sex_group + race_group +
                         edu_group + marital_group + PIR_group  +  
                         BMI_group + drink_group + smoking_group + 
                         PA_group  + diabetes_group,
  design = df_design, 
  family = multinomial(refLevel = "无疼痛或疼痛持续时间<24h")
) 



lst("模型1" = mod1, "模型2" = mod2, "模型3" = mod3) %>% 
  map(
    ~ .x %>% 
      tidy_svyVGAM(exponentiate = TRUE, conf.int = TRUE) %>% 
      filter(str_detect(term, "^caffeine_group"))
  ) %>% 
  list_rbind(names_to = "model") %>% 
  mutate(across(where(is.numeric), function(x) format(round(x, 3), nsmall = 3)) ) %>% 
  mutate(estimate = str_c(estimate, " (", conf.low, "~", conf.high, ")")) %>% 
  select(y.level, model, term, estimate, p.value) %>% 
  mutate(term = str_remove(term, "caffeine_group")) %>% 
  pivot_wider(
    names_from  = term,
    values_from = c(estimate, p.value),
    names_glue  = "{term}_{.value}",
    names_vary  = "slowest"
  ) %>% 
  arrange(y.level) %>% 
  mutate(y.level = case_match(
    y.level,
    "1"  ~ "急性疼痛",
    "2"  ~ "亚急性疼痛",
    "3"  ~ "慢性疼痛"
  )) %>% 
  flextable::flextable(
    col_keys = c(names(.)[1:2], "blank1", names(.)[3:4], "blank2", names(.)[5:6])
  ) %>%
  flextable::merge_v(j = 1) %>% 
  flextable::valign(j = 1, valign = "top") %>% 
  ftExtra::span_header(sep = "\\_") %>% 
  flextable::align(i = 1, align = 'center', part = "header") %>% 
  flextable::empty_blanks() %>% 
  flextable::autofit(add_w = 0.2)






mod3 %>% 
  tidy_svyVGAM(exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)", y.level == 3)  %>% 
  tidyr::separate_wider_delim(
    term, 
    delim = "_group",
    names = c("group", "index")
  ) %>% 
  select(group, index, estimate, std.error, conf.low, conf.high, p.value) %>% 
  mutate(p.value = prettyunits::pretty_p_value(p.value)) %>% 
  flextable::as_grouped_data(groups = "group") %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::align(j = "p.value", align = "right", part = "all") %>% 
  flextable::autofit()





