
# Table 1  ----------------------------------------------------------------


rm(list=ls(all=TRUE))
gc()

# ssh node29   
# R




# Data input  --------------------------------------------------------------------

setwd("/public/home/xiaomeng/Rawdata_CKB/DAR-2023-00171-V1_Release18.01")
dbq <- read.csv("data_baseline_questionnaires_addHTN2.csv", header=T)
# Menopausal absence 
dbq$menopause_status[is.na(dbq$menopause_status)]<-9999






# Data cleaning -----------------------------------------------------------

# Excluding individuals with null BMI
dbq1 <- dbq[!is.na(dbq$bmi_calc),] 

# Exclude data that is lost at the end of the baseline 
library(lubridate)
dbq1$censor_year <- year(dbq1$censoring_date)
dbq1$censor_month <- month(dbq1$censoring_date)
dbq2 <- dbq1[-which((dbq1$study_date_year == dbq1$censor_year) & 
                      (dbq1$study_date_month == dbq1$censor_month) &
                      (dbq1$censoring_reason !="Dead                          ")),] 

# 1) Excluding men reporting a history of pregnancy
dbq3 <- dbq2[-which(dbq2$is_female==0 & dbq2$has_reproductive_histories> 0),] 

# 2) live births > pregnancies
dbq4 <- dbq3[which(((dbq3$live_birth_count <= dbq3$preg_count) & dbq3$is_female==1) |
                     dbq3$is_female==0),] 
# dbqt <- subset(dbq4, select = c("live_birth_count", "preg_count"))

# 3) Remove outliers (>= 20)
# Number of biological children for men & biological children, pregnancies, and live births for women
dbq5 <- dbq4[-which((dbq4$children >= 20) |
                      (dbq4$preg_count>=20 & dbq4$is_female==1) |
                      (dbq4$live_birth_count>=20 & dbq4$is_female==1)),] 



# Load data ---------------------------------------------------------------

# save(dbq5, file = "/public/home/xiaomeng/Rawdata_CKB/DAR-2023-00171-V1_Release18.01/dbq5.RData")
load("/public/home/xiaomeng/Rawdata_CKB/DAR-2023-00171-V1_Release18.01/dbq5.RData") 







##### Variable grouping #####

attach(dbq5)
# biological children: 4 groups
dbq5$child_grp[dbq5$children == 0] <- 0 
dbq5$child_grp[dbq5$children == 1 | dbq5$children == 2] <- 1
dbq5$child_grp[dbq5$children == 3] <- 2
dbq5$child_grp[dbq5$children >= 4] <- 3

# biological children: 2 groups
dbq5$parity_grp[dbq5$children == 0] <- 0 
dbq5$parity_grp[dbq5$children >= 1] <- 1

# marriage : 1 → Married, 2 → Widowed+Separated/divorced+Never married)
dbq5$marriage[dbq5$marital_status == 0] <- 1 
dbq5$marriage[dbq5$marital_status == 1 | dbq5$marital_status == 2 |dbq5$marital_status == 3] <- 2

# age
dbq5$age <- dbq5$age_at_study_date_x100/100

# education : 1 → Primary school or below, 2 →  junior or high school，3 → college or above
dbq5$education[highest_education==0 | highest_education==1] <- 1 
dbq5$education[highest_education==2 | highest_education==3] <- 2
dbq5$education[highest_education==4 | highest_education==5] <- 3 

# household_income：1 → "<10000/year", 2 → "10000-19999/year", 3 → ">20000/year"
dbq5$income[household_income == 0 | household_income == 1 | household_income == 2] <- 1
dbq5$income[household_income == 3 ] <- 2
dbq5$income[household_income == 4 | household_income == 5] <- 3

# household_size
dbq5$hhold_size[household_size == 3 | household_size == 4 | household_size == 5 ] <- 1
dbq5$hhold_size[household_size == 1 | household_size == 2 ] <- 2
dbq5$hhold_size[household_size >= 6] <- 3

# healthy diet：1 → "Unhealthy diet"，2 → "Healthy diet:score 4-5"
dbq5$diet_component1 <- ifelse((dbq5$diet_freq_fresh_veg == 0), 1, 0) 
dbq5$diet_component2 <- ifelse((dbq5$diet_freq_fresh_fruit == 0), 1, 0)
dbq5$diet_component3 <- ifelse((dbq5$diet_freq_meat == 1 | dbq5$diet_freq_meat == 2), 1, 0)
dbq5$diet_component4 <- ifelse((dbq5$diet_freq_fish <= 2), 1, 0)
dbq5$diet_component5 <- ifelse((dbq5$diet_freq_soybean <= 1), 1, 0)
dbq5$diet_score <- apply(dbq5[,c("diet_component1","diet_component2","diet_component3",
                                 "diet_component4","diet_component5")], 1, sum, na.rm =T)
dbq5$healthy_diet <- ifelse((dbq5$diet_score == 4 | dbq5$diet_score == 5), 2, 1)

# alcohol =5 groups ## note: ex-drinker → excessive drinker (>=60g/d)
# 1 → "Not daily", 2 → "Daily 1-14g/d", 3 → "Daily 15-29g/d", 4 → "Daily 30-59g/d", 5 → "Ex-drinker or Daily >=60g/d"
dbq5$alchol[(alcohol_category < 6 & alcohol_category != 2 & alcohol_category != 5) | 
              (alcohol_category == 6 & alc_weekly < 2)] <- 1
dbq5$alchol[alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g < 15.00] <- 2
dbq5$alchol[alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 15.00 & total_alc_typ_day_g < 30.00)] <- 3
dbq5$alchol[alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 30.00 & total_alc_typ_day_g < 60.00)] <- 4
dbq5$alchol[alcohol_category == 2 |  alcohol_category == 5 | 
              (alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g >= 60.00)] <- 5

# smoking=5 groups : 1 → "Never/occasional", 2 → "Former", 3 → "1-9 cig/d", 4 → "10-19 cig/d", 5 → ">=20 cig/d"
dbq5$smoke[smoking_category == 1 | smoking_category == 2] <- 1
dbq5$smoke[smoking_category == 3 & (smoking_stopped_reason > 0 & smoking_stopped_reason <= 4)] <- 2
dbq5$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day < 10.00] <- 3
dbq5$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & 
             cig_equiv_day >= 10.00 & cig_equiv_day < 20.00] <- 4
dbq5$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day >= 20.00] <- 5

# BMI
dbq5$BMIgrp[bmi_calc < 18.5] <- 1
dbq5$BMIgrp[bmi_calc >= 18.5 & bmi_calc <24] <- 2
dbq5$BMIgrp[bmi_calc >= 24 & bmi_calc <28] <- 3
dbq5$BMIgrp[bmi_calc >= 28] <- 4

# still_birth, spont_abortion,induced_abortion: (1) 0 & NA; (2) >=1
dbq5$still_birth <-
  ifelse(
    (dbq5$still_birth_count>=1),
    1, 0)

dbq5$spont_abortion <-
  ifelse(
    (dbq5$spont_abortion_count>=1),
    1, 0)

dbq5$induced_abortion <-
  ifelse(
    (dbq5$induced_abortion_count>=1),
    1, 0)


# The age of the last delivery
dbq5$PregMaxAge <- apply (dbq5[
  c("live_birth_age01","live_birth_age02","live_birth_age03",
    "live_birth_age04","live_birth_age05","live_birth_age06",
    "live_birth_age07","live_birth_age08","live_birth_age09",
    "live_birth_age10","live_birth_age11","live_birth_age12",
    "live_birth_age13","live_birth_age14","live_birth_age15")], 1, max, na.rm =T)
dbq5$PregMaxAge[dbq5$PregMaxAge == -Inf] <- 9999 

# Age group for the last delivery
dbq5$PregMaxAge_grp[dbq5$PregMaxAge == 9999] <- 0 
dbq5$PregMaxAge_grp[dbq5$PregMaxAge <= 45] <- 1
dbq5$PregMaxAge_grp[dbq5$PregMaxAge > 45 & dbq5$PregMaxAge < 9999] <- 2


# Passive smoking:Inhaling smoke from other smokers for>=1 day per week and>=5 minutes per session
dbq5$Pass_smok <- 0
dbq5$Pass_smok[(dbq5$smoke_exposure == 2 | dbq5$smoke_exposure ==3 | 
                  dbq5$smoke_exposure ==4) ] <- 1

# menopause_status: 2 = Completely amenorrhea
dbq5$menopause_grp <- ifelse((dbq5$menopause_status==2), 2, 1)
detach(dbq5)






##### Disease at baseline: hypertension, respiratory diseases, CVD, cancer #####
dbq5$has_hypertension <-
  ifelse(
    (dbq5$hypertension_diag==1)|(dbq5$sbp_mean>=140)|
      (dbq5$dbp_mean>=90)|(dbq5$used_blood_pressure_drugs==1),   
    1, 0)

dbq5$has_respiratory_disease <-
  ifelse(
    (dbq5$has_copd==1)|(dbq5$emph_bronc_diag==1)|(dbq5$asthma_diag==1)|(dbq5$tb_diag==1),   
    1, 0)

dbq5$has_CVD <-
  ifelse(
    (dbq5$chd_diag==1)| (dbq5$stroke_or_tia_diag==1),   
    1, 0)

dbq5$has_cancer <-
  ifelse(
    (dbq5$cancer_diag==1) ,   
    1, 0)

dbq5$marriage_factor <- as.factor(dbq5$marriage)
dbq5$age_factor <- as.factor(dbq5$age)
dbq5$gender_factor <- as.factor(dbq5$is_female)
dbq5$region_is_urban_factor <- as.factor(dbq5$region_is_urban)
dbq5$education_factor <- as.factor(dbq5$education)
dbq5$income_factor <- as.factor(dbq5$income)
dbq5$householdsize_factor <- as.factor(dbq5$hhold_size)
dbq5$diet_factor <- as.factor(dbq5$healthy_diet)
dbq5$alc_grp_factor <- as.factor(dbq5$alchol)
dbq5$smoke_factor <- as.factor(dbq5$smoke)
dbq5$BMIgrp_factor <- as.factor(dbq5$BMIgrp)
dbq5$satisfaction_level_factor <- as.factor(dbq5$satisfaction_level)
dbq5$diabetes_factor <- as.factor(dbq5$has_diabetes)
dbq5$hypertension_factor <- as.factor(dbq5$has_hypertension)
dbq5$respiratory_disease_factor <- as.factor(dbq5$has_respiratory_disease)
dbq5$CVD_factor <- as.factor(dbq5$has_CVD)
dbq5$cancer_factor <- as.factor(dbq5$has_cancer)
dbq5$region_code_factor <- as.factor(dbq5$region_code)
dbq5$Meno_factor <- as.factor(dbq5$menopause_grp)
dbq5$child_grp_factor <- as.factor(dbq5$child_grp)
dbq5$parity_grp_factor <- as.factor(dbq5$parity_grp)
dbq5$diet_had_shortage_factor <- as.factor(dbq5$diet_had_shortage)
dbq5$pill_use_factor <- as.factor(dbq5$pill_use)
dbq5$still_birth_factor <- as.factor(dbq5$still_birth)
dbq5$spont_abortion_factor <- as.factor(dbq5$spont_abortion)
dbq5$induced_abortion_factor <- as.factor(dbq5$induced_abortion)
dbq5$PregMaxAge_grp_factor <- as.factor(dbq5$PregMaxAge_grp)
dbq5$Pass_smok_factor <- as.factor(dbq5$Pass_smok)




# Calculate the proportion of children in Table 1-------------------------------------------

dbq_m <- dbq5[which(dbq5$is_female==0),] 
dbq_w <- dbq5[which(dbq5$is_female==1),] 

##### men #####
### age & met
library(gmodels)
aggregate(dbq_m$age, by=list(dbq_m$child_grp_factor), FUN=mean) 
aggregate(dbq_m$age, by=list(dbq_m$child_grp_factor), FUN=sd)




library(tableone)
covar1.1 <-c("region_is_urban_factor","marriage_factor","education_factor",
             "income_factor", "householdsize_factor","alc_grp_factor",
             "smoke_factor","diet_factor", "BMIgrp_factor","satisfaction_level_factor",
             "hypertension_factor", "respiratory_disease_factor","CVD_factor",
             "cancer_factor", "diabetes_factor","region_code_factor", 
             "diet_had_shortage_factor","Pass_smok_factor")
tab_m <- CreateTableOne(vars=covar1.1, strata=c("child_grp_factor"), data=dbq_m,addOverall=TRUE )
tab_m <- print(tab_m)
setwd("/public/home/xiaomeng/Parity/results")
write.table(tab_m,"20231207_Table1_Parity_men.csv",row.names=TRUE,col.names=TRUE,sep=",")




##### women #####

### age & met
library(gmodels)
aggregate(dbq_w$age, by=list(dbq_w$child_grp_factor), FUN=mean) 
aggregate(dbq_w$age, by=list(dbq_w$child_grp_factor), FUN=sd)



library(tableone)
covar1.1 <-c("region_is_urban_factor","marriage_factor","education_factor","income_factor",
             "householdsize_factor","alc_grp_factor","smoke_factor","diet_factor",
             "BMIgrp_factor","satisfaction_level_factor","hypertension_factor",
             "respiratory_disease_factor","CVD_factor","cancer_factor",
             "diabetes_factor","region_code_factor","Meno_factor",
             "diet_had_shortage_factor","Pass_smok_factor","pill_use_factor",
             "still_birth_factor","spont_abortion_factor","induced_abortion_factor",
             "PregMaxAge_grp_factor")
tab_w <- CreateTableOne(vars=covar1.1, strata=c("child_grp_factor"), data=dbq_w,addOverall=TRUE )
tab_w <- print(tab_w)
setwd("/public/home/xiaomeng/Parity/results")
write.table(tab_w,"20231207_Table1_Parity_women.csv",row.names=TRUE,col.names=TRUE,sep=",")




