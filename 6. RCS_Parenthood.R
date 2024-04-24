rm(list=ls(all=TRUE))
gc()








# Load data ---------------------------------------------------------------

load("/public/home/xiaomeng/Rawdata_CKB/DAR-2023-00171-V1_Release18.01/4. Parity&KM_Patients_RData/dat_AnyPatients_KM_Parity.RData") 


# analysis by population  --------------------------------------------------------

# 1.1)  All men 
dat_total <- dbq_Any_dis[!is.na(dbq_Any_dis$children),]
dat_anal <- dat_total[dat_total$is_female==0,] 
# 1.2) All women
dat_total <- dbq_Any_dis[!is.na(dbq_Any_dis$children),]
dat_anal <- dat_total[dat_total$is_female==1,]

# 2.1) Only include men with children
dat_total <- dbq_Any_dist[!is.na(dbq_Any_dist$children),]
dat_anal <- dat_total[dat_total$is_female==0,] 
# 2.2) Only include women with children
dat_total <- dbq_Any_dist[!is.na(dbq_Any_dist$children),]
dat_anal <- dat_total[dat_total$is_female==1,]








# Generate follow-up time-----------------------------------------------------------------

library(lubridate)
# Generate endpoint outcome
dat_anal$death_outcome <- ifelse(dat_anal$censoring_reason == "Dead                          ", 2,1)
table(dat_anal$death_outcome) 

# time_in
dat_anal$age_in <- as.Date(substr(dat_anal$dob_anon,1,10))
dat_anal$time_in <- round(interval(dat_anal$age_in, as.Date(dat_anal$study_date)) / years(1) ,4) 

# endpoint
dat_anal$follow_end <- dat_anal[,c("censoring_date")]
dat_anal$follow_end <- as.Date(substr(dat_anal$follow_end,1,10))

# time_out
dat_anal$time_out <- round(interval(dat_anal$age_in, dat_anal$follow_end) / years(1) ,4) 
dat_anal$death_time <- dat_anal$time_out-dat_anal$time_in










# Disease at baseline: HTN,respiratory_disease,cancer,CVD
dat_anal$has_hypertension <-
  ifelse(
    (dat_anal$hypertension_diag==1)|(dat_anal$sbp_mean>=140)|
      (dat_anal$dbp_mean>=90)|(dat_anal$used_blood_pressure_drugs==1),   
    1, 0)

dat_anal$has_respiratory_disease <-
  ifelse(
    (dat_anal$has_copd==1)|(dat_anal$emph_bronc_diag==1)|(dat_anal$asthma_diag==1)|(dat_anal$tb_diag==1),   
    1, 0)

dat_anal$has_cancer <- 
  ifelse(
    (dat_anal$cancer_diag==1), 
    1, 0)

dat_anal$has_CVD <-
  ifelse(
    (dat_anal$chd_diag==1)|(dat_anal$stroke_or_tia_diag==1),   
    1, 0)




##### Variable grouping #####

# age
dat_anal$age_at_study_date <- dat_anal$age_at_study_date_x100/100

attach(dat_anal)
#  marriage : 1 → Married, 2 → Widowed+Separated/divorced+Never married
dat_anal$marriage[dat_anal$marital_status == 0] <- 1 
dat_anal$marriage[dat_anal$marital_status == 1 | dat_anal$marital_status == 2 | 
                    dat_anal$marital_status == 3] <- 2

# children
dat_anal$child_grp[dat_anal$children == 0] <- 0 
dat_anal$child_grp[dat_anal$children == 1] <- 1
dat_anal$child_grp[dat_anal$children == 2] <- 2
dat_anal$child_grp[dat_anal$children == 3] <- 3
dat_anal$child_grp[dat_anal$children >= 4] <- 4

# age
dat_anal$age[age_at_study_date >= 30 &  age_at_study_date < 35] <- 1
dat_anal$age[age_at_study_date >= 35 &  age_at_study_date < 40] <- 2
dat_anal$age[age_at_study_date >= 40 &  age_at_study_date < 45] <- 3
dat_anal$age[age_at_study_date >= 45 &  age_at_study_date < 50] <- 4
dat_anal$age[age_at_study_date >= 50 &  age_at_study_date < 55] <- 5
dat_anal$age[age_at_study_date >= 55 &  age_at_study_date < 60] <- 6
dat_anal$age[age_at_study_date >= 60 &  age_at_study_date < 65] <- 7
dat_anal$age[age_at_study_date >= 65 &  age_at_study_date < 70] <- 8
dat_anal$age[age_at_study_date >= 70 &  age_at_study_date < 75] <- 9
dat_anal$age[age_at_study_date >= 75 &  age_at_study_date < 80] <- 10

# education : 1 → Primary school or below, 2 →  junior or high school，3 → college or above
dat_anal$education[highest_education==0 | highest_education==1] <- 1 
dat_anal$education[highest_education==2 | highest_education==3] <- 2
dat_anal$education[highest_education==4 | highest_education==5] <- 3 

# household_income：1 → "<10000/year", 2 → "10000-19999/year", 3 → ">20000/year"
dat_anal$income[household_income == 0 | household_income == 1 | household_income == 2] <- 1
dat_anal$income[household_income == 3 ] <- 2
dat_anal$income[household_income == 4 | household_income == 5] <- 3

# household_size
dat_anal$hhold_size[household_size == 3 | household_size == 4 | household_size == 5 ] <- 1
dat_anal$hhold_size[household_size == 1 | household_size == 2 ] <- 2
dat_anal$hhold_size[household_size >= 6] <- 3

# healthy diet：1 → "Unhealthy diet"，2 → "Healthy diet:score 4-5"
dat_anal$diet_component1 <- ifelse((dat_anal$diet_freq_fresh_veg == 0), 1, 0) 
dat_anal$diet_component2 <- ifelse((dat_anal$diet_freq_fresh_fruit == 0), 1, 0)
dat_anal$diet_component3 <- ifelse((dat_anal$diet_freq_meat == 1 | dat_anal$diet_freq_meat == 2), 1, 0)
dat_anal$diet_component4 <- ifelse((dat_anal$diet_freq_fish <= 2), 1, 0)
dat_anal$diet_component5 <- ifelse((dat_anal$diet_freq_soybean <= 1), 1, 0)
dat_anal$diet_score <- apply(dat_anal[,c("diet_component1","diet_component2","diet_component3",
                                         "diet_component4","diet_component5")], 1, sum, na.rm =T)
dat_anal$healthy_diet <- ifelse((dat_anal$diet_score == 4 | dat_anal$diet_score == 5), 2, 1)

# alcohol =5 groups ## note: ex-drinker → excessive drinker (>=60g/d)
# 1 → "Not daily", 2 → "Daily 1-14g/d", 3 → "Daily 15-29g/d", 4 → "Daily 30-59g/d", 5 → "Ex-drinker or Daily >=60g/d"
dat_anal$alchol[(alcohol_category < 6 & alcohol_category != 2 & alcohol_category != 5) | 
                  (alcohol_category == 6 & alc_weekly < 2)] <- 1
dat_anal$alchol[alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g < 15.00] <- 2
dat_anal$alchol[alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 15.00 & total_alc_typ_day_g < 30.00)] <- 3
dat_anal$alchol[alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 30.00 & total_alc_typ_day_g < 60.00)] <- 4
dat_anal$alchol[alcohol_category == 2 |  alcohol_category == 5 | 
                  (alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g >= 60.00)] <- 5

# smoking=5 groups : 1 → "Never/occasional", 2 → "Former", 3 → "1-9 cig/d", 4 → "10-19 cig/d", 5 → ">=20 cig/d"
dat_anal$smoke[smoking_category == 1 | smoking_category == 2] <- 1
dat_anal$smoke[smoking_category == 3 & (smoking_stopped_reason > 0 & smoking_stopped_reason <= 4)] <- 2
dat_anal$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day < 10.00] <- 3
dat_anal$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & 
                 cig_equiv_day >= 10.00 & cig_equiv_day < 20.00] <- 4
dat_anal$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day >= 20.00] <- 5

# BMI
dat_anal$BMIgrp[bmi_calc < 18.5] <- 1
dat_anal$BMIgrp[bmi_calc >= 18.5 & bmi_calc <24] <- 2
dat_anal$BMIgrp[bmi_calc >= 24 & bmi_calc <28] <- 3
dat_anal$BMIgrp[bmi_calc >= 28] <- 4

# family history : 0 → "No", 1 → "Yes", 2 → "Don't know"   
dat_anal$stroke_family <- NA
dat_anal$stroke_family[(mother_stroke == 0 & father_stroke == 0 & siblings_stroke == 0) |
                         (mother_stroke == 0 & father_stroke == 0 & siblings == 0)] <- 0
dat_anal$stroke_family[mother_stroke == 1 | father_stroke == 1 | (siblings_stroke >= 1 & (!is.na(siblings_stroke)))] <- 1
dat_anal$stroke_family <- ifelse(!is.na(dat_anal$stroke_family), dat_anal$stroke_family, 2) 

dat_anal$HA_family <- NA
dat_anal$HA_family[(mother_heart_attack == 0 & father_heart_attack == 0 & siblings_heart_attack == 0) |
                     (mother_heart_attack == 0 & father_heart_attack == 0 & siblings == 0)] <- 0
dat_anal$HA_family[mother_heart_attack == 1 | father_heart_attack == 1 | (siblings_heart_attack >= 1 & (!is.na(siblings_heart_attack)))] <- 1
dat_anal$HA_family <- ifelse(!is.na(dat_anal$HA_family), dat_anal$HA_family, 2) 
table(dat_anal$HA_family)

dat_anal$diabetes_family <- NA
dat_anal$diabetes_family[(mother_diabetes == 0 & father_diabetes == 0 & siblings_diabetes == 0) |
                           (mother_diabetes == 0 & father_diabetes == 0 & siblings == 0)] <- 0
dat_anal$diabetes_family[mother_diabetes == 1 | father_diabetes == 1 | (siblings_diabetes >= 1 & (!is.na(siblings_diabetes)))] <- 1
dat_anal$diabetes_family <- ifelse(!is.na(dat_anal$diabetes_family), dat_anal$diabetes_family, 2) 
table(dat_anal$diabetes_family)

dat_anal$Mental_all_family <- NA
dat_anal$Mental_all_family[(mother_mental_disorder == 0 & father_mental_disorder == 0 & siblings_mental_disorder == 0) |
                             (mother_mental_disorder == 0 & father_mental_disorder == 0 & siblings == 0)] <- 0
dat_anal$Mental_all_family[mother_mental_disorder == 1 | father_mental_disorder == 1 | (siblings_mental_disorder >= 1 & (!is.na(siblings_mental_disorder)))] <- 1
dat_anal$Mental_all_family <- ifelse(!is.na(dat_anal$Mental_all_family), dat_anal$Mental_all_family, 2) 

dat_anal$cancer_family <- NA
dat_anal$cancer_family[(mother_cancer == 0 & father_cancer == 0 & siblings_cancer == 0) |
                         (mother_cancer == 0 & father_cancer == 0 & siblings == 0)] <- 0
dat_anal$cancer_family[mother_cancer == 1 | father_cancer == 1 | (siblings_cancer >= 1 & (!is.na(siblings_cancer)))] <- 1
dat_anal$cancer_family <- ifelse(!is.na(dat_anal$cancer_family), dat_anal$cancer_family, 2) 
table(dat_anal$cancer_family)

### merge: family history
# 1 → "Yes"; 0 → "No"/"Don't know"
dat_anal$has_familyhis <- NA 
dat_anal$has_familyhis <-
  ifelse(
    (dat_anal$stroke_family==1 | dat_anal$HA_family==1 | dat_anal$diabetes_family==1 | 
       dat_anal$Mental_all_family==1 | dat_anal$cancer_family==1),   
    1, 0)

# menopause_status: 2 = Completely amenorrhea
dat_anal$menopause_grp <- ifelse((dat_anal$menopause_status==2), 2, 1)
detach(dat_anal)






dat_anal$marriage_factor <- as.factor(dat_anal$marriage) 
dat_anal$age_factor <- as.factor(dat_anal$age)
dat_anal$gender_factor <- as.factor(dat_anal$is_female)
dat_anal$education_factor <- as.factor(dat_anal$education)
dat_anal$income_factor <- as.factor(dat_anal$income)
dat_anal$householdsize_factor <- as.factor(dat_anal$hhold_size)
dat_anal$diet_factor <- as.factor(dat_anal$healthy_diet)
dat_anal$alc_grp_factor <- as.factor(dat_anal$alchol)
dat_anal$smoke_factor <- as.factor(dat_anal$smoke)
dat_anal$BMIgrp_factor <- as.factor(dat_anal$BMIgrp)
dat_anal$diabetes_factor <- as.factor(dat_anal$has_diabetes)
dat_anal$hypertension_factor <- as.factor(dat_anal$has_hypertension)
dat_anal$respiratory_disease_factor <- as.factor(dat_anal$has_respiratory_disease)
dat_anal$cancer_factor <- as.factor(dat_anal$has_cancer)
dat_anal$CVD_factor <- as.factor(dat_anal$has_CVD)
dat_anal$satisfaction_level_factor <- as.factor(dat_anal$satisfaction_level)
dat_anal$region_code_factor <- as.factor(dat_anal$region_code)
dat_anal$stroke_family_factor <- as.factor(dat_anal$stroke_family)
dat_anal$HA_family_factor <- as.factor(dat_anal$HA_family)
dat_anal$diabetes_family_factor <- as.factor(dat_anal$diabetes_family)
dat_anal$Mental_all_family_factor <- as.factor(dat_anal$Mental_all_family)
dat_anal$has_familyhis_factor <- as.factor(dat_anal$has_familyhis)  
dat_anal$cancer_family_factor <- as.factor(dat_anal$cancer_family)
dat_anal$Meno_factor <- as.factor(dat_anal$menopause_grp)
dat_anal$child_grp_factor <- as.factor(dat_anal$child_grp)









# RCS -----------------------------------------------------------------

library("survival")
library(Hmisc)
library(designGG)
library(plyr)
library(ggplot2)
library(rms)


# Strata in RCS: Men ------------------------------------------------------

dat_m <- subset(dat_anal, 
                select = c("time_in","time_out","death_outcome","child_grp",
                           "marriage_factor", "education_factor", "income_factor",
                           "householdsize_factor","diet_factor","alc_grp_factor",
                           "smoke_factor","BMIgrp","met","satisfaction_level_factor",
                           "diabetes_factor", "hypertension_factor",
                           "respiratory_disease_factor","CVD_factor", "cancer_factor",
                           "has_familyhis_factor", "region_code_factor",
                           "age_factor","gender_factor","children"))
ddm <- datadist(dat_m)
options(datadist='ddm')

fitm_5knots_stra <- cph(Surv(time_in, time_out,death_outcome) ~ rcs(children,5)+ marriage_factor +education_factor + 
                          income_factor + householdsize_factor + diet_factor + alc_grp_factor + smoke_factor + 
                          BMIgrp + met +  satisfaction_level_factor +　diabetes_factor + hypertension_factor+
                          respiratory_disease_factor + cancer_factor + CVD_factor + has_familyhis_factor + 
                          strat(region_code_factor) + strat(age_factor), data=dat_m)
anova(fitm_5knots_stra) # Overall P<.0001; P-Nonlinear<.0001




HRm_5knots_stra <- Predict(fitm_5knots_stra,children,fun=exp,ref.zero = T) 

P1m_5knots_stra <- ggplot(HRm_5knots_stra)
P1m_5knots_stra

P2m_5knots_stra <- ggplot() + geom_line(data=HRm_5knots_stra, aes(children,yhat),linetype="solid",linewidth=1, alpha=0.7,colour="red") + 
  geom_ribbon(data=HRm_5knots_stra, aes(children,ymin=lower,ymax=upper), alpha=0.1,fill="red") 
P2m_5knots_stra

P3m_5knots_stra <- P2m_5knots_stra + theme_classic() + geom_hline(yintercept=1, linetype=2,size=1)+
  labs(title="RCS_Men_5knots", x="Children", y="aHR(95%CI")+ 
  scale_x_continuous(breaks=seq(0, 10, 1)) + 
  theme(axis.text.x = element_text(size = 12)) + 
  theme(axis.text.y = element_text(size = 12)) 
P3m_5knots_stra


P4m_5knots_stra <- P3m_5knots_stra + geom_vline(xintercept = 2, color="blue",linetype=2,size=1)
P4m_5knots_stra


setwd("/public/home/xiaomeng/Parity")
pdf("/public/home/xiaomeng/Parity/RCS_AnyPatients_Ment_5knots.pdf",
    width=7,height=5)

P4m_5knots_stra

dev.off()




# Strata in RCS: Women ------------------------------------------------------
dat_w <- subset(dat_anal, 
                select = c("time_in","time_out","death_outcome","child_grp",
                           "marriage_factor","education_factor", "income_factor",
                           "householdsize_factor","diet_factor","alc_grp_factor",
                           "smoke_factor","BMIgrp","met","satisfaction_level_factor",
                           "diabetes_factor", "hypertension_factor",
                           "respiratory_disease_factor","CVD_factor", "cancer_factor",
                           "has_familyhis_factor", "Meno_factor","region_code_factor",
                           "age_factor", "gender_factor","children"))
ddw <- datadist(dat_w)
options(datadist='ddw')


fitw_5knots_stra <- cph(Surv(time_in, time_out,death_outcome) ~ rcs(children,5)+ marriage_factor +education_factor + 
                          income_factor + householdsize_factor + diet_factor + alc_grp_factor + smoke_factor + 
                          BMIgrp + met +  satisfaction_level_factor +　diabetes_factor + hypertension_factor+
                          respiratory_disease_factor + cancer_factor + CVD_factor + has_familyhis_factor + 
                          Meno_factor + strat(region_code_factor) + strat(age_factor), data=dat_w)
anova(fitw_5knots_stra) # Overall P =0.0014; P-Nonlinear =.0008



HRw_5knots_stra <- Predict(fitw_5knots_stra,children,fun=exp,ref.zero = T) 

P1w_5knots_stra <- ggplot(HRw_5knots_stra)
P1w_5knots_stra

P2w_5knots_stra <- ggplot() + geom_line(data=HRw_5knots_stra, aes(children,yhat),linetype="solid",linewidth=1, alpha=0.7,colour="red")+
  geom_ribbon(data=HRw_5knots_stra,aes(children,ymin=lower,ymax=upper), alpha=0.1,fill="red") 
P2w_5knots_stra

P3w_5knots_stra <- P2w_5knots_stra + theme_classic() + geom_hline(yintercept=1, linetype=2,size=1)+
  labs(title="RCS_Women_5knots", x="Children", y="aHR(95%CI")+ 
  scale_x_continuous(breaks=seq(0, 10, 1)) + 
  theme(axis.text.x = element_text(size = 12)) + 
  theme(axis.text.y = element_text(size = 12))  geom_vline(xintercept = 2, color="blue",linetype=2,size=1)
P4w_5knots_stra


setwd("/public/home/xiaomeng/RCS")
pdf("/public/home/xiaomeng/RCS/public/home/xiaomeng/Parityknots.pd/public/home/xiaomeng/Parityt=5")

P4w_5knots_stra

dev.off()


# End ---------------------------------------------------------------------


