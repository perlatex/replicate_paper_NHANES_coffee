library(tidyverse)
library(haven)



# 协变量
#####################################################################
# 人口学特征
demo_a <- read_xpt("F:/NHANES/1999-2000/Demographics/demo.xpt")
demo_b <- read_xpt("F:/NHANES/2001-2002/Demographics/demo_b.xpt")
demo_c <- read_xpt("F:/NHANES/2003-2004/Demographics/demo_c.xpt")




demo_d <-
  list("1999-2000" = demo_a, 
       "2001-2002" = demo_b, 
       "2003-2004" = demo_c) %>%
  map(~ .x %>% select(
    SEQN,                # 编号
    age     = RIDAGEYR,  # 年龄
    sex     = RIAGENDR,  # 性别
    race    = RIDRETH1,  # 种族
    PIR     = INDFMPIR,  # 贫穷比率
    marital = DMDMARTL,  # 婚姻状态
    edu     = DMDEDUC2   # 教育程度
  )) %>%
  list_rbind(names_to = "NHAENS") 



demo_data <- demo_d %>%
  mutate(age_group = factor(
    case_when(
      age >= 20 & age < 45 ~ "20~<45 years",
      age >= 45 & age < 60 ~ "45~<60 years",
      age >= 60            ~ "60+ years",
      .default             = NA 
    ),
    levels = c("20~<45 years", "45~<60 years", "60+ years")
  )) %>%
  
  mutate(sex_group = if_else(sex == 1, "男", "女")) %>% 
  
  mutate(race_group = factor(
    case_match(
      race,
      1         ~ "墨裔美籍人",
      2         ~ "其他西班牙裔",
      3         ~ "非西班牙裔白人",
      4         ~ "非西班牙裔黑人",
      5         ~ "其他",
      .default  =  NA
    ),
    levels = c("墨裔美籍人", "其他西班牙裔", "非西班牙裔白人", "非西班牙裔黑人", "其他")
  )) %>% 
  
  mutate(edu_group = factor(
    case_when(
      edu %in% c(1, 2) ~ "高中以下",
      edu %in% c(3)    ~ "高中",
      edu %in% c(4, 5) ~ "高中以上",
      .default         = NA
    ),
    levels = c("高中以下", "高中", "高中以上")
  )) %>%
  
  mutate(
    marital_group = factor(
      case_match(
        marital,
        1        ~ "已婚",
        2        ~ "丧偶",
        3        ~ "离异",
        4        ~ "分居",
        5        ~ "单身",
        6        ~ "同居",
        .default = NA 
      ),
      levels = c("已婚", "丧偶", "离异", "分居", "单身", "同居")
    )) %>% 

  mutate(PIR_group = factor(
    case_when(
      PIR < 1.3              ~ "<1.3",
      PIR >= 1.3 & PIR < 1.8 ~ "1.3~<1.8",
      PIR >= 1.8             ~ ">=1.8",
      .default               = NA
      ),
    levels = c("<1.3", "1.3~<1.8", ">=1.8")
  )) 

  
#####################################################################







#####################################################################
# body mass index
bmx_a <- read_xpt("F:/NHANES/1999-2000/Examination/bmx.xpt")
bmx_b <- read_xpt("F:/NHANES/2001-2002/Examination/bmx_b.xpt")
bmx_c <- read_xpt("F:/NHANES/2003-2004/Examination/bmx_c.xpt")

bmx_data <- list(bmx_a, bmx_b, bmx_c) %>% 
  map(~ .x %>% select(SEQN, BMXBMI)) %>% 
  list_rbind() %>% 
  mutate(
    BMI_group = factor(
      case_when(
        BMXBMI <  20               ~ "< 20",
        BMXBMI >= 20 & BMXBMI < 25 ~ "20~<25",
        BMXBMI >= 25 & BMXBMI < 30 ~ "25~<30",
        BMXBMI >= 30               ~ ">=30",
        .default                   = NA
      ),
      levels = c("< 20", "20~<25", "25~<30", ">=30")
   )) 


#####################################################################







#####################################################################
# 喝酒
# ALD100 每年至少喝12杯酒-
# ALQ130 alcoholic drinks/day -past 12 mos

alq_a <- read_xpt("F:/NHANES/1999-2000/Questionnaire/alq.xpt")
alq_b <- read_xpt("F:/NHANES/2001-2002/Questionnaire/alq_b.xpt")
alq_c <- read_xpt("F:/NHANES/2003-2004/Questionnaire/alq_c.xpt")



alq_data <- list(alq_a, alq_b, alq_c) %>% 
  map(~ .x %>% select(SEQN, ALD100 = any_of(c("ALQ100", "ALD100", "ALQ101")), ALQ130)) %>% 
  list_rbind() %>% 
  mutate(
    drink_group = factor(
      case_when(
        ALD100 == 2                  ~ "0-不饮酒",
        ALD100 == 1 & ALQ130 <= 2    ~ "1-2杯",
        ALD100 == 1 & ALQ130 > 2     ~ "> 2杯",
        .default                     = NA
      ),
      levels = c("0-不饮酒", "1-2杯", "> 2杯")
    )) 


#####################################################################






#####################################################################
# 抽烟
# 是否吸烟至少100支-SMQ020
smq_a <- read_xpt("F:/NHANES/1999-2000/Questionnaire/smq.xpt")
smq_b <- read_xpt("F:/NHANES/2001-2002/Questionnaire/smq_b.xpt")
smq_c <- read_xpt("F:/NHANES/2003-2004/Questionnaire/smq_c.xpt")

smq_data <- list(smq_a, smq_b, smq_c) %>% 
  map(~ .x %>% select(SEQN, SMQ020)) %>% 
  list_rbind() %>% 
  mutate(
    smoking_group = factor(
      case_when(
        SMQ020 == 1  ~ "Yes",
        SMQ020 == 2  ~ "No",
       .default      = NA
      ),
      levels = c("Yes", "No")
   )) 

#####################################################################







#####################################################################
# 日常体力活动

paq_a <- read_xpt("F:/NHANES/1999-2000/Questionnaire/paq.xpt")
paq_b <- read_xpt("F:/NHANES/2001-2002/Questionnaire/paq_b.xpt")
paq_c <- read_xpt("F:/NHANES/2003-2004/Questionnaire/paq_c.xpt")

paq_data <- list(paq_a, paq_b, paq_c) %>% 
  map(~ .x %>% select(SEQN, PAQ180)) %>% 
  list_rbind() %>% 
  mutate(PA_group = factor(
    case_match(
        PAQ180,
        1         ~ "久坐",
        2         ~ "轻度体力活动",
        3         ~ "中度体力活动",
        4         ~ "重度体力活动",
        .default  = NA
    ),
    levels = c("久坐", "轻度体力活动", "中度体力活动", "重度体力活动")
  )) 

#####################################################################










#####################################################################
# 报告糖尿病

diq_a <- read_xpt("F:/NHANES/1999-2000/Questionnaire/diq.xpt")
diq_b <- read_xpt("F:/NHANES/2001-2002/Questionnaire/diq_b.xpt")
diq_c <- read_xpt("F:/NHANES/2003-2004/Questionnaire/diq_c.xpt")



diq_data <- list(diq_a, diq_b, diq_c) %>% 
  map(~ .x %>% select(SEQN, DIQ010)) %>% 
  list_rbind() %>% 
  mutate(
    diabetes_group = factor(
      case_match(
        DIQ010,
        1         ~ "Yes",
        c(2, 3)   ~ "No",
        .default  = NA
      ),
      levels = c("Yes", "No")
    ))  

#####################################################################











# 结果变量
#####################################################################
# 疼痛相关数据提取
mpq_a <- read_xpt("F:/NHANES/1999-2000/Questionnaire/mpq.xpt")
mpq_b <- read_xpt("F:/NHANES/2001-2002/Questionnaire/mpq_b.xpt")
mpq_c <- read_xpt("F:/NHANES/2003-2004/Questionnaire/mpq_c.xpt")

mpq_data <- list(mpq_a, mpq_b, mpq_c) %>% 
  map(~ .x %>% select(SEQN, MPQ100, MPQ110)) %>% 
  list_rbind() %>% 
  mutate(pain_group = factor(
    case_when(
      MPQ100 == 2                       ~ "无疼痛或疼痛持续时间<24h",
      MPQ100 == 1 & MPQ110 == 1         ~ "急性疼痛",
      MPQ100 == 1 & MPQ110 == 2         ~ "亚急性疼痛",
      MPQ100 == 1 & MPQ110 %in% c(3, 4) ~ "慢性疼痛",
      .default                          = NA
    ),
    levels = c("无疼痛或疼痛持续时间<24h", "急性疼痛", "亚急性疼痛", "慢性疼痛")
  ))  

#####################################################################







# 暴露变量
#####################################################################
# 咖啡摄入数据提取
# 很多食物都含有咖啡因（比如如茶、可乐等），文章只统计食物是咖啡的


# 1. 根据美国农业部食品编码找出咖啡的编码，用于筛查
drxfmt_a <- read_xpt("F:/NHANES/1999-2000/Dietary/drxfmt.xpt")
drxfmt_b <- read_xpt("F:/NHANES/2001-2002/Dietary/drxfmt_b.xpt")
drxfcd_c <- read_xpt("F:/NHANES/2003-2004/Dietary/drxFCD_c.xpt")

drxfmt_a %>% 
 filter(str_detect(LABEL, "^COFFEE,")) %>% 
 flextable::flextable() %>% 
 flextable::autofit()



coffee_code <- list(drxfmt_a, drxfmt_b, drxfcd_c) %>% 
  map(
    ~ .x %>% 
      select(
        START = any_of(c("START", "DRXFDCD")), 
        LABEL = any_of(c("LABEL", "DRXFCSD"))
      ) %>% 
      filter(str_detect(LABEL, "^COFFEE,")) %>% 
      filter(!str_detect(LABEL, "DECAF"))       # 不含咖啡因的咖啡 Decaffeinated
  ) %>% 
  list_rbind() %>% 
  pull(START) %>% 
  unique()





# 2. 
# 只统计食物为咖啡的咖啡因
# 简便起见，2003-2004周期我们只用第1天的数据

dr1iff_a <- read_xpt("F:/NHANES/1999-2000/Dietary/drxiff.xpt")
dr1iff_b <- read_xpt("F:/NHANES/2001-2002/Dietary/drxiff_b.xpt")
dr1iff_c <- read_xpt("F:/NHANES/2003-2004/Dietary/dr1iff_c.xpt")
# dr2iff_c <- read_xpt("F:/NHANES/2003-2004/Dietary/dr2iff_c.xpt") 



dr1iff_data <- list(dr1iff_a, dr1iff_b, dr1iff_c) %>% 
  map(
    ~ .x %>% 
       select(
         SEQN, 
         food_code         = any_of(c("DRDIFDCD", "DR1IFDCD")),
         caffeine_quantity = any_of(c("DRXICAFF", "DR1ICAFF"))
        ) %>% 
       drop_na(caffeine_quantity) %>% 
       summarise(
         caffeine_daily = sum( caffeine_quantity[food_code %in% coffee_code] ),
        .by = SEQN
        ) 
  ) %>% 
  list_rbind() %>% 
  mutate(caffeine_group = factor(
    case_when(
      caffeine_daily == 0                         ~ "0 mg",
      caffeine_daily > 0 & caffeine_daily <= 200  ~ "0~200 mg",
      caffeine_daily > 200                        ~ ">200 mg",
      .default                        = NA
    ),
    levels = c("0 mg", "0~200 mg", ">200 mg")
  ))  

#################################################################################









#####################################################################
# 权重
drxtot_a <- read_xpt("F:/NHANES/1999-2000/Dietary/drxtot.xpt")
drxtot_b <- read_xpt("F:/NHANES/2001-2002/Dietary/drxtot_b.xpt")
dr1tot_c <- read_xpt("F:/NHANES/2003-2004/Dietary/dr1tot_c.xpt")


weight_data <- list("1999-2000" = drxtot_a,  
                    "2001-2002" = drxtot_b, 
                    "2003-2004" = dr1tot_c) %>%
  map(~ .x %>% select(SEQN, WT = any_of(c("WTDR4YR"," WTDRD1")))) %>%
  list_rbind(names_to = "NHAENS") %>%
  mutate( 
    WT2YR = case_when(
      NHAENS == "1999-2000" ~ WT * 2.0 / 3,
      NHAENS == "2001-2002" ~ WT * 2.0 / 3,
      NHAENS == "2003-2004" ~ WT * 1.0 / 3)
  ) %>% 
  select(-NHAENS)



survey_design_data <- list(demo_a, demo_b, demo_c) %>%
  map(~ .x %>% select(SEQN, SDMVPSU, SDMVSTRA)) %>%
  list_rbind()
#####################################################################






#####################################################################
# 合并
d <- list(demo_data, 
          bmx_data,  
          alq_data,
          smq_data,
          paq_data,
          diq_data,
          mpq_data,
          dr1iff_data,
          weight_data,
          survey_design_data
) %>% 
  purrr::reduce(left_join, by = "SEQN") 
#####################################################################





#####################################################################
# 纳排
d %>%
  count(NHAENS)

d %>%
  filter(age >= 20) %>%
  filter(if_all(c(caffeine_group, pain_group), ~!is.na(.x))) %>%
  dim()


df <- d %>%
  filter(age >= 20) %>%
  drop_na(caffeine_group, pain_group) %>%
  select(SEQN, SDMVPSU, SDMVSTRA, WT2YR, ends_with("_group")) 



dim(df) # 13428

sjPlot::view_df(df)
#####################################################################






#####################################################################
# 保存
df %>% write_rds("df.rds")
#####################################################################



