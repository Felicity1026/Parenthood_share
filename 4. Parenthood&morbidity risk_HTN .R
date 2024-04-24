rm(list=ls(all=TRUE))
gc()

# ssh node29   
# R




# Data input  --------------------------------------------------------------------

setwd("/public/home/xiaomeng/Rawdata_CKB/DAR-2023-00171-V1_Release18.01")
dbq <- read.csv("data_baseline_questionnaires_addHTN2.csv", header=T)
dee <- read.csv("event_endpoints.csv", header=T) 
dee$csid <- substr(dee$csid,start=12,stop=17)
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








# Generate follow-up time and outcomes-----------------------------------------------------------------

# Excluding individuals with HTN at baseline 
dbq5$has_hypertension <-
  ifelse(
    (dbq5$hypertension_diag==1)|(dbq5$sbp_mean>=140)|
      (dbq5$dbp_mean>=90)|(dbq5$used_blood_pressure_drugs==1),   
    1, 0)
dbq_HTN <- dbq5[-which(dbq5$has_hypertension==1),]
dim(dbq_HTN)

# Individuals with disease records
dee1 <- dee[dee$csid %in% dbq_HTN$csid,]  
dim(dee1) 
length(unique(dee1$csid)) 

# Merge "csid+onset time+disease diagnosis" and remove duplicate diagnostic records (same day, same diagnosis) 
dee1$datedeveloped_new <- as.Date(substr(dee1$datedeveloped,1,10))
dee1$date_fulldiag <- paste(dee1$csid,dee1$datedeveloped_new,dee1$diagnosis)  
dee2 <- dee1[!duplicated(dee1$date_fulldiag),]  
dim(dee2)

# Remove rows with the same "csid+disease diagnosis", and only keep the earliest record of the onset time for the same person
dee2 <- dee2[order(dee2$csid),] 
dee2 <- dee2[order(dee2$datedeveloped_new),]
dee2$date_fulldiag2 <- paste(dee2$csid,dee2$diagnosis) 
dee_norep1 <- dee2[!duplicated(dee2$date_fulldiag2),]  
dim(dee_norep1) 

# Generate individuals with HTN during follow-up: I10
dee_norep1$disease_diag <- substr(dee_norep1$diagnosis,1,3)
dee_norep1$HTN_diagnosis <- NA
dee_norep1$HTN_diagnosis <-
  ifelse(
    ((dee_norep1$disease_diag=="I10") 
    ), 2, 1)  
table(dee_norep1$HTN_diagnosis)  

dee_norep1$disease_yn <- paste(dee_norep1$csid, dee_norep1$HTN_diagnosis)  # 合并 "csid + 疾病诊断（1或者2）"
dee_norep2 <- dee_norep1[!duplicated(dee_norep1$disease_yn),] 
dim(dee_norep2) 
table(dee_norep2$HTN_diagnosis)  

# Only one record is retained for each object, arranged in descending order.
dee_norep2 <- dee_norep2[order(dee_norep2$csid,dee_norep2$disease_yn,decreasing = TRUE),] 
dee_final <- dee_norep2[!duplicated(dee_norep2$csid),] 
dim(dee_final) 
table(dee_final$HTN_diagnosis)  

# Merge Datasets
dat_HTN <- merge(dbq_HTN, dee_final, by="csid", all = TRUE)
dim(dat_HTN)   
dat_HTN$HTN_diagnosis[is.na(dat_HTN$HTN_diagnosis)] <- 1
table(dat_HTN$HTN_diagnosis) 







# Load data ---------------------------------------------------------------
save(dat_HTN, file = "/public/home/xiaomeng/Rawdata_CKB/DAR-2023-00171-V1_Release18.01/3. Parity&Dieases_General_RData/dat_Parity_HTN.RData")

load("/public/home/xiaomeng/Rawdata_CKB/DAR-2023-00171-V1_Release18.01/3. Parity&Dieases_General_RData/dat_Parity_HTN.RData") 




# Calculate follow-up time and enrollment age-------------------------------------------------------------

# Using age as a scale, enrollment time
dat_HTN$age_in <- as.Date(substr(dat_HTN$dob_anon,1,10))
dat_HTN$time_in <- round(interval(dat_HTN$age_in, as.Date(dat_HTN$study_date)) / years(1) ,4) 

# participants who have an interesting outcome
dat_HTN$follow_end <- NA
dat_HTN[dat_HTN$HTN_diagnosis==2,]$follow_end <- apply(dat_HTN[(dat_HTN$HTN_diagnosis==2),
                                                               c("censoring_date","datedeveloped_new")], 1, min, na.rm =T)

# participants who do not have an interesting outcome:/censor
dat_HTN[dat_HTN$HTN_diagnosis==1,]$follow_end <- dat_HTN[dat_HTN$HTN_diagnosis==1,c("censoring_date")]
dat_HTN$follow_end <- as.Date(substr(dat_HTN$follow_end,1,10))

# time_out: the age at the end of the follow-up
dat_HTN$time_out <- round(interval(dat_HTN$age_in, dat_HTN$follow_end) / years(1) ,4) 
dat_HTN$HTN_time <- dat_HTN$time_out-dat_HTN$time_in
median(dat_HTN$HTN_time) 








# Pre analysis data processing-----------------------------------------------------------------

##### Disease at baseline: respiratory diseases, CVD, cancer #####
dat_HTN$has_respiratory_disease <-
  ifelse(
    (dat_HTN$has_copd==1)|(dat_HTN$emph_bronc_diag==1)|(dat_HTN$asthma_diag==1)|(dat_HTN$tb_diag==1),   
    1, 0)

dat_HTN$has_CVD <-
  ifelse(
    (dat_HTN$chd_diag==1)| (dat_HTN$stroke_or_tia_diag==1),   
    1, 0)

dat_HTN$has_cancer <-
  ifelse(
    (dat_HTN$cancer_diag==1) ,   
    1, 0)




##### Variable grouping #####

# age
dat_HTN$age_at_study_date<-dat_HTN$age_at_study_date_x100/100

attach(dat_HTN)
# biological children: 4 groups
dat_HTN$child_grp[dat_HTN$children == 0] <- 0 
dat_HTN$child_grp[dat_HTN$children == 1 | dat_HTN$children == 2] <- 1
dat_HTN$child_grp[dat_HTN$children == 3] <- 2
dat_HTN$child_grp[dat_HTN$children >= 4] <- 3

#  marriage : 1 → Married, 2 → Widowed+Separated/divorced+Never married
dat_HTN$marriage[dat_HTN$marital_status == 0] <- 1 
dat_HTN$marriage[dat_HTN$marital_status == 1 | dat_HTN$marital_status == 2 | 
                   dat_HTN$marital_status == 3] <- 2

# age
dat_HTN$age[age_at_study_date >= 30 &  age_at_study_date < 35] <- 1
dat_HTN$age[age_at_study_date >= 35 &  age_at_study_date < 40] <- 2
dat_HTN$age[age_at_study_date >= 40 &  age_at_study_date < 45] <- 3
dat_HTN$age[age_at_study_date >= 45 &  age_at_study_date < 50] <- 4
dat_HTN$age[age_at_study_date >= 50 &  age_at_study_date < 55] <- 5
dat_HTN$age[age_at_study_date >= 55 &  age_at_study_date < 60] <- 6
dat_HTN$age[age_at_study_date >= 60 &  age_at_study_date < 65] <- 7
dat_HTN$age[age_at_study_date >= 65 &  age_at_study_date < 70] <- 8
dat_HTN$age[age_at_study_date >= 70 &  age_at_study_date < 75] <- 9
dat_HTN$age[age_at_study_date >= 75 &  age_at_study_date < 80] <- 10

# age group
dat_HTN$agegroup[age_at_study_date  <50] <- 0
dat_HTN$agegroup[age_at_study_date  >= 50] <- 1

# education : 1 → Primary school or below, 2 →  junior or high school，3 → college or above
dat_HTN$education[highest_education==0 | highest_education==1] <- 1 
dat_HTN$education[highest_education==2 | highest_education==3] <- 2
dat_HTN$education[highest_education==4 | highest_education==5] <- 3 

# household_income：1 → "<10000/year", 2 → "10000-19999/year", 3 → ">20000/year"
dat_HTN$income[household_income == 0 | household_income == 1 | household_income == 2] <- 1
dat_HTN$income[household_income == 3 ] <- 2
dat_HTN$income[household_income == 4 | household_income == 5] <- 3

# household_size
dat_HTN$hhold_size[household_size == 3 | household_size == 4 | household_size == 5 ] <- 1
dat_HTN$hhold_size[household_size == 1 | household_size == 2 ] <- 2
dat_HTN$hhold_size[household_size >= 6] <- 3

# healthy diet：1 → "Unhealthy diet"，2 → "Healthy diet:score 4-5"
dat_HTN$diet_component1 <- ifelse((dat_HTN$diet_freq_fresh_veg == 0), 1, 0) 
dat_HTN$diet_component2 <- ifelse((dat_HTN$diet_freq_fresh_fruit == 0), 1, 0)
dat_HTN$diet_component3 <- ifelse((dat_HTN$diet_freq_meat == 1 | dat_HTN$diet_freq_meat == 2), 1, 0)
dat_HTN$diet_component4 <- ifelse((dat_HTN$diet_freq_fish <= 2), 1, 0)
dat_HTN$diet_component5 <- ifelse((dat_HTN$diet_freq_soybean <= 1), 1, 0)
dat_HTN$diet_score <- apply(dat_HTN[,c("diet_component1","diet_component2","diet_component3",
                                       "diet_component4","diet_component5")], 1, sum, na.rm =T)
dat_HTN$healthy_diet <- ifelse((dat_HTN$diet_score == 4 | dat_HTN$diet_score == 5), 2, 1)

# alcohol =5 groups ## note: ex-drinker → excessive drinker (>=60g/d)
# 1 → "Not daily", 2 → "Daily 1-14g/d", 3 → "Daily 15-4g/d", 4 → "Daily 30-59g/d", 5 → "Ex-drinker or Daily >=60g/d"
dat_HTN$alchol[(alcohol_category < 6 & alcohol_category != 2 & alcohol_category != 5) | (alcohol_category == 6 & alc_weekly < 2)] <- 1
dat_HTN$alchol[alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g < 15.00] <- 2
dat_HTN$alchol[alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 15.00 & total_alc_typ_day_g < 30.00)] <- 3
dat_HTN$alchol[alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 30.00 & total_alc_typ_day_g < 60.00)] <- 4
dat_HTN$alchol[alcohol_category == 2 |  alcohol_category == 5 | (alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g >= 60.00)] <- 5

# smoking=5 groups : 1 → "Never/occasional", 2 → "Former", 3 → "1-9 cig/d", 4 → "10-19 cig/d", 5 → ">=20 cig/d"
dat_HTN$smoke[smoking_category == 1 | smoking_category == 2] <- 1
dat_HTN$smoke[smoking_category == 3 & (smoking_stopped_reason > 0 & smoking_stopped_reason <= 4)] <- 2
dat_HTN$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day < 10.00] <- 3
dat_HTN$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & 
                cig_equiv_day >= 10.00 & cig_equiv_day < 20.00] <- 4
dat_HTN$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day >= 20.00] <- 5

# BMI
dat_HTN$BMIgrp[bmi_calc < 18.5] <- 1
dat_HTN$BMIgrp[bmi_calc >= 18.5 & bmi_calc <24] <- 2
dat_HTN$BMIgrp[bmi_calc >= 24 & bmi_calc <28] <- 3
dat_HTN$BMIgrp[bmi_calc >= 28] <- 4

# still_birth, spont_abortion,induced_abortion: (1) 0 & NA; (2) >=1
dat_HTN$still_birth <-
  ifelse(
    (dat_HTN$still_birth_count>=1),
    1, 0)

dat_HTN$spont_abortion <-
  ifelse(
    (dat_HTN$spont_abortion_count>=1),
    1, 0)

dat_HTN$induced_abortion <-
  ifelse(
    (dat_HTN$induced_abortion_count>=1),
    1, 0)


# The age of the last delivery
dat_HTN$PregMaxAge <- apply (dat_HTN[
  c("live_birth_age01","live_birth_age02","live_birth_age03",
    "live_birth_age04","live_birth_age05","live_birth_age06",
    "live_birth_age07","live_birth_age08","live_birth_age09",
    "live_birth_age10","live_birth_age11","live_birth_age12",
    "live_birth_age13","live_birth_age14","live_birth_age15")], 1, max, na.rm =T)
dat_HTN$PregMaxAge[dat_HTN$PregMaxAge == -Inf] <- 9999 

# Age group for the last delivery
dat_HTN$PregMaxAge_grp[dat_HTN$PregMaxAge == 9999] <- 0 
dat_HTN$PregMaxAge_grp[dat_HTN$PregMaxAge <= 45] <- 1
dat_HTN$PregMaxAge_grp[dat_HTN$PregMaxAge > 45 & dat_HTN$PregMaxAge < 9999] <- 2


# Passive smoking:Inhaling smoke from other smokers for>=1 day per week and>=5 minutes per session
dat_HTN$Pass_smok <- 0
dat_HTN$Pass_smok[(dat_HTN$smoke_exposure == 2 | dat_HTN$smoke_exposure ==3 | 
                     dat_HTN$smoke_exposure ==4) ] <- 1

# menopause_status: 2 = Completely amenorrhea
dat_HTN$menopause_grp <- ifelse((dat_HTN$menopause_status==2), 2, 1)
detach(dat_HTN)









dat_HTN$marriage_factor <- as.factor(dat_HTN$marriage)
dat_HTN$age_factor <- as.factor(dat_HTN$age)
dat_HTN$gender_factor <- as.factor(dat_HTN$is_female)
dat_HTN$region_is_urban_factor <- as.factor(dat_HTN$region_is_urban)
dat_HTN$education_factor <- as.factor(dat_HTN$education)
dat_HTN$income_factor <- as.factor(dat_HTN$income)
dat_HTN$householdsize_factor <- as.factor(dat_HTN$hhold_size)
dat_HTN$diet_factor <- as.factor(dat_HTN$healthy_diet)
dat_HTN$alc_grp_factor <- as.factor(dat_HTN$alchol)
dat_HTN$smoke_factor <- as.factor(dat_HTN$smoke)
dat_HTN$BMIgrp_factor <- as.factor(dat_HTN$BMIgrp)
dat_HTN$satisfaction_level_factor <- as.factor(dat_HTN$satisfaction_level)
dat_HTN$respiratory_disease_factor <- as.factor(dat_HTN$has_respiratory_disease)
dat_HTN$CVD_factor <- as.factor(dat_HTN$has_CVD)
dat_HTN$cancer_factor <- as.factor(dat_HTN$has_cancer)
dat_HTN$diabetes_factor <- as.factor(dat_HTN$has_diabetes)
dat_HTN$region_code_factor <- as.factor(dat_HTN$region_code)
dat_HTN$Meno_factor <- as.factor(dat_HTN$menopause_grp)
dat_HTN$child_grp_factor <- factor(dat_HTN$child_grp, levels=c("1","0","2","3")) 
dat_HTN$diet_had_shortage_factor <- as.factor(dat_HTN$diet_had_shortage)
dat_HTN$pill_use_factor <- as.factor(dat_HTN$pill_use)
dat_HTN$still_birth_factor <- as.factor(dat_HTN$still_birth)
dat_HTN$spont_abortion_factor <- as.factor(dat_HTN$spont_abortion)
dat_HTN$induced_abortion_factor <- as.factor(dat_HTN$induced_abortion)
dat_HTN$PregMaxAge_grp_factor <- as.factor(dat_HTN$PregMaxAge_grp)
dat_HTN$Pass_smok_factor <- as.factor(dat_HTN$Pass_smok)





# Cox for men -----------------------------------------------------------------

library("survival")
library(Hmisc)
library(designGG)
library(plyr)

Ynames <- c("HTN_diagnosis")
Xnames <- c("child_grp_factor")
Cnames <- c("marriage_factor + education_factor + income_factor + householdsize_factor",     
            
            "marriage_factor + education_factor + income_factor + householdsize_factor + 
            diet_factor + alc_grp_factor + smoke_factor + BMIgrp + met",   
            
            "marriage_factor + education_factor + income_factor +  householdsize_factor +
            diet_factor + alc_grp_factor + smoke_factor + BMIgrp + met + 
            diabetes_factor + respiratory_disease_factor + CVD_factor + 
            cancer_factor + satisfaction_level_factor",   
            
            "marriage_factor + education_factor + income_factor +  householdsize_factor +
            diet_factor + alc_grp_factor + smoke_factor + BMIgrp + met + 
            diabetes_factor + respiratory_disease_factor + CVD_factor + 
            cancer_factor + satisfaction_level_factor + diet_had_shortage_factor + Pass_smok_factor") 

Ynames_names <- Ynames
Xnames_names <- Xnames
Cnames_names <- Cs("model1","model2","model3","model4")

col_list <- c("beta_m","rHR_m","se_m", "Pr_m", 
              "beta_mless50","rHR_mless50","se_mless50", "Pr_mless50", 
              "beta_mmore50","rHR_mmore50","se_mmore50", "Pr_mmore50",  
              "beta_um","rHR_um","se_um", "Pr_um", 
              "beta_rm","rHR_rm","se_rm", "Pr_rm", 
              
              "HR_m", "LHR_m","HHR_m","HR_CI_m","P_m", 
              "HR_mless50", "LHR_mless50","HHR_mless50","HR_CI_mless50","P_mless50", 
              "HR_mmore50", "LHR_mmore50","HHR_mmore50","HR_CI_mmore50","P_mmore50", 
              "HR_um", "LHR_um","HHR_um","HR_CI_um","P_um",  
              "HR_rm", "LHR_rm","HHR_rm","HR_CI_rm","P_rm")

Table_assoc <- as.data.frame(
  matrix(rep(NA,length(Ynames)*length(Xnames)*length(Cnames)*length(col_list)),
         ncol=length(col_list),
         nrow=length(Ynames)*length(Xnames)*length(Cnames)*3)
)

colnames(Table_assoc) <- col_list
dim(Table_assoc)




# Cox analysis -----------------------------------------------------------------

for(i in c(1:length(Xnames))) {
  for(j in c(1:length(Ynames))) {
    for(k in c(1:length(Cnames))) {
      
      model_m <- coxph(as.formula(paste0("Surv(","time_in, time_out",",", Ynames[j], ")~", Xnames[i],
                                         "+ strata(region_code_factor) + strata(age_factor) + ", Cnames[k])), 
                       data=dat_HTN, subset=is_female==0)
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 1:4] =
        summary(model_m)$coefficients[c(1:3), c(1,2,3,5)]
      
      
      
      model_mless50 <- coxph(as.formula(paste0("Surv(","time_in, time_out",",",Ynames[j], ")~", Xnames[i],
                                               "+ strata(region_code_factor) + strata(age_factor) +", Cnames[k])), 
                             data=dat_HTN, subset=(is_female==0 & agegroup==0))
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*+length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 5:8] =     
        summary(model_mless50)$coefficients[c(1:3), c(1,2,3,5)]
      
      
      
      model_mmore50 <- coxph(as.formula(paste0("Surv(","time_in, time_out",",",Ynames[j], ")~", Xnames[i],
                                               "+ strata(region_code_factor) + strata(age_factor) +", Cnames[k])), 
                             data=dat_HTN,  subset=(is_female==0 & agegroup==1))
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 9:12] =     
        summary(model_mmore50)$coefficients[c(1:3), c(1,2,3,5)] 
      
      
      
      model_um <- coxph(as.formula(paste0("Surv(","time_in, time_out",",",Ynames[j], ")~", Xnames[i],
                                          "+ strata(region_code_factor) + strata(age_factor) +",Cnames[k])), 
                        data=dat_HTN, subset=(is_female==0 & region_is_urban==1))
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 13:16] =     
        summary(model_um)$coefficients[c(1:3), c(1,2,3,5)] 
      
      
      
      model_rm <- coxph(as.formula(paste0("Surv(","time_in, time_out",",",Ynames[j], ")~", Xnames[i],
                                          "+ strata(region_code_factor) + strata(age_factor) +",Cnames[k])), 
                        data=dat_HTN, subset=(is_female==0 & region_is_urban==0))
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 17:20] =     
        summary(model_rm)$coefficients[c(1:3), c(1,2,3,5)] 
      
      
      
      rownames(Table_assoc)[1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)] <-
        paste(Ynames_names[j],Cnames_names[k],sep="_")
    }}}








# 95%CI -----------------------------------------------------------------

Table_assoc$HR_m<-round(Table_assoc$rHR_m,2)
Table_assoc$LHR_m<-round(exp(Table_assoc$beta_m-1.96*Table_assoc$se_m),2)
Table_assoc$HHR_m<-round(exp(Table_assoc$beta_m+1.96*Table_assoc$se_m),2)
Table_assoc$HR_CI_m<-paste0(Table_assoc$HR_m, " (",Table_assoc$LHR_m,"-",Table_assoc$HHR_m,")")
Table_assoc$P_m<-round(Table_assoc$Pr_m,4)

Table_assoc$HR_mless50<-round(Table_assoc$rHR_mless50,2)
Table_assoc$LHR_mless50<-round(exp(Table_assoc$beta_mless50-1.96*Table_assoc$se_mless50),2)
Table_assoc$HHR_mless50<-round(exp(Table_assoc$beta_mless50+1.96*Table_assoc$se_mless50),2)
Table_assoc$HR_CI_mless50<-paste0(Table_assoc$HR_mless50, " (",Table_assoc$LHR_mless50,"-",Table_assoc$HHR_mless50,")")
Table_assoc$P_mless50<-round(Table_assoc$Pr_mless50,4)

Table_assoc$HR_mmore50<-round(Table_assoc$rHR_mmore50,2)
Table_assoc$LHR_mmore50<-round(exp(Table_assoc$beta_mmore50-1.96*Table_assoc$se_mmore50),2)
Table_assoc$HHR_mmore50<-round(exp(Table_assoc$beta_mmore50+1.96*Table_assoc$se_mmore50),2)
Table_assoc$HR_CI_mmore50<-paste0(Table_assoc$HR_mmore50, " (",Table_assoc$LHR_mmore50,"-",Table_assoc$HHR_mmore50,")")
Table_assoc$P_mmore50<-round(Table_assoc$Pr_mmore50,4)

Table_assoc$HR_um<-round(Table_assoc$rHR_um,2)
Table_assoc$LHR_um<-round(exp(Table_assoc$beta_um-1.96*Table_assoc$se_um),2)
Table_assoc$HHR_um<-round(exp(Table_assoc$beta_um+1.96*Table_assoc$se_um),2)
Table_assoc$HR_CI_um<-paste0(Table_assoc$HR_um, " (",Table_assoc$LHR_um,"-",Table_assoc$HHR_um,")")
Table_assoc$P_um<-round(Table_assoc$Pr_um,4)

Table_assoc$HR_rm<-round(Table_assoc$rHR_rm,2)
Table_assoc$LHR_rm<-round(exp(Table_assoc$beta_rm-1.96*Table_assoc$se_rm),2)
Table_assoc$HHR_rm<-round(exp(Table_assoc$beta_rm+1.96*Table_assoc$se_rm),2)
Table_assoc$HR_CI_rm<-paste0(Table_assoc$HR_rm, " (",Table_assoc$LHR_rm,"-",Table_assoc$HHR_rm,")")
Table_assoc$P_rm<-round(Table_assoc$Pr_rm,4)

setwd("/public/home/xiaomeng/Rawdata_CKB/20230921_AgeScale_Parity_Cox_Diseases")
write.csv(Table_assoc, "20230809_MenCox1_VS1-2_Parity&HTN.csv")




# Cox for women -----------------------------------------------------------------

library("survival")
library(Hmisc)
library(designGG)
library(plyr)

Ynames <- c("HTN_diagnosis")
Xnames <- c("child_grp_factor")
Cnames <- c("marriage_factor + education_factor + income_factor + householdsize_factor",     
            
            "marriage_factor + education_factor + income_factor + householdsize_factor + 
            diet_factor + alc_grp_factor + smoke_factor + BMIgrp + met",   
            
            "marriage_factor + education_factor + income_factor +  householdsize_factor +
            diet_factor + alc_grp_factor + smoke_factor + BMIgrp + met + 
            diabetes_factor + respiratory_disease_factor + CVD_factor + 
            cancer_factor + satisfaction_level_factor + Meno_factor",   
            
            "marriage_factor + education_factor + income_factor +  householdsize_factor +
            diet_factor + alc_grp_factor + smoke_factor + BMIgrp + met + 
            diabetes_factor + respiratory_disease_factor + CVD_factor + 
            cancer_factor + satisfaction_level_factor + Meno_factor + diet_had_shortage_factor +
            Pass_smok_factor + pill_use_factor + still_birth_factor+ spont_abortion_factor + 
            induced_abortion_factor + PregMaxAge_grp_factor") 


Ynames_names <- Ynames
Xnames_names <- Xnames
Cnames_names <- Cs("model1","model2","model3","model4")

# 相关参数命名 [前4列是循环提出来的（summary前写的“1:4”），后3列是空着自己算的（1:3和7:9中间隔了3列）]
col_list <- c("beta_f","rHR_f","se_f", "Pr_f", 
              "beta_fMeno1","rHR_fMeno1","se_fMeno1", "Pr_fMeno1", 
              "beta_fMeno2","rHR_fMeno2","se_fMeno2", "Pr_fMeno2",  
              "beta_uf","rHR_uf","se_uf", "Pr_uf", 
              "beta_rf","rHR_rf","se_rf", "Pr_rf",  
              
              "HR_f", "LHR_f","HHR_f","HR_CI_f","P_f", 
              "HR_fMeno1", "LHR_fMeno1","HHR_fMeno1","HR_CI_fMeno1","P_fMeno1", 
              "HR_fMeno2", "LHR_fMeno2","HHR_fMeno2","HR_CI_fMeno2","P_fMeno2", 
              "HR_uf", "LHR_uf","HHR_uf","HR_CI_uf","P_uf",  
              "HR_rf", "LHR_rf","HHR_rf","HR_CI_rf","P_rf")

# 生育是4组，结果表示的是后两组和参照组比的结果，所以就是3
Table_assoc <- as.data.frame(
  matrix(rep(NA,length(Ynames)*length(Xnames)*length(Cnames)*length(col_list)),
         ncol=length(col_list),
         nrow=length(Ynames)*length(Xnames)*length(Cnames)*3)
)

colnames(Table_assoc) <- col_list
dim(Table_assoc)




# Cox analysis -----------------------------------------------------------------

for(i in c(1:length(Xnames))) {
  for(j in c(1:length(Ynames))) {
    for(k in c(1:length(Cnames))) {
      
      model_f <- coxph(as.formula(paste0("Surv(","time_in, time_out",",", Ynames[j], ")~", Xnames[i],
                                         "+ strata(region_code_factor) + strata(age_factor) + ", Cnames[k])), 
                       data=dat_HTN, subset=is_female==1)
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 1:4] =
        summary(model_f)$coefficients[c(1:3), c(1,2,3,5)]
      
      
      
      model_fMeno1 <- coxph(as.formula(paste0("Surv(","time_in, time_out",",",Ynames[j], ")~", Xnames[i],
                                              "+ strata(region_code_factor) + strata(age_factor) +", Cnames[k])), 
                            data=dat_HTN, subset=(is_female==1 & Meno_factor==1))
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*+length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 5:8] =     
        summary(model_fMeno1)$coefficients[c(1:3), c(1,2,3,5)]
      
      
      
      model_fMeno2 <- coxph(as.formula(paste0("Surv(","time_in, time_out",",",Ynames[j], ")~", Xnames[i],
                                              "+ strata(region_code_factor) + strata(age_factor) +", Cnames[k])), 
                            data=dat_HTN, subset=(is_female==1 & Meno_factor==2))
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 9:12] =     
        summary(model_fMeno2)$coefficients[c(1:3), c(1,2,3,5)] 
      
      
      
      model_uf <- coxph(as.formula(paste0("Surv(","time_in, time_out",",",Ynames[j], ")~", Xnames[i],
                                          "+ strata(region_code_factor) + strata(age_factor) +",Cnames[k])), 
                        data=dat_HTN, subset=(is_female==1 & region_is_urban==1))
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 13:16] =     
        summary(model_uf)$coefficients[c(1:3), c(1,2,3,5)] 
      
      
      
      model_rf <- coxph(as.formula(paste0("Surv(","time_in, time_out",",",Ynames[j], ")~", Xnames[i],
                                          "+ strata(region_code_factor) + strata(age_factor) +",Cnames[k])), 
                        data=dat_HTN, subset=(is_female==1 & region_is_urban==0))
      Table_assoc[(1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)):
                    (3+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)), 17:20] =     
        summary(model_rf)$coefficients[c(1:3), c(1,2,3,5)] 
      
      
      
      rownames(Table_assoc)[1+3*length(Xnames)*length(Cnames)*(j-1)+3*length(Xnames)*(k-1)] <-
        paste(Ynames_names[j],Cnames_names[k],sep="_")
    }}}







# -

Table_assoc$HR_f<-round(Table_assoc$rHR_f,2)
Table_assoc$LHR_f<-round(exp(Table_assoc$beta_f-1.96*Table_assoc$se_f),2)
Table_assoc$HHR_f<-round(exp(Table_assoc$beta_f+1.96*Table_assoc$se_f),2)
Table_assoc$HR_CI_f<-paste0(Table_assoc$HR_f, " (",Table_assoc$LHR_f,"-",Table_assoc$HHR_f,")")
Table_assoc$P_f<-round(Table_assoc$Pr_f,4)

Table_assoc$HR_fMeno1<-round(Table_assoc$rHR_fMeno1,2)
Table_assoc$LHR_fMeno1<-round(exp(Table_assoc$beta_fMeno1-1.96*Table_assoc$se_fMeno1),2)
Table_assoc$HHR_fMeno1<-round(exp(Table_assoc$beta_fMeno1+1.96*Table_assoc$se_fMeno1),2)
Table_assoc$HR_CI_fMeno1<-paste0(Table_assoc$HR_fMeno1, " (",Table_assoc$LHR_fMeno1,"-",Table_assoc$HHR_fMeno1,")")
Table_assoc$P_fMeno1<-round(Table_assoc$Pr_fMeno1,4)

Table_assoc$HR_fMeno2<-round(Table_assoc$rHR_fMeno2,2)
Table_assoc$LHR_fMeno2<-round(exp(Table_assoc$beta_fMeno2-1.96*Table_assoc$se_fMeno2),2)
Table_assoc$HHR_fMeno2<-round(exp(Table_assoc$beta_fMeno2+1.96*Table_assoc$se_fMeno2),2)
Table_assoc$HR_CI_fMeno2<-paste0(Table_assoc$HR_fMeno2, " (",Table_assoc$LHR_fMeno2,"-",Table_assoc$HHR_fMeno2,")")
Table_assoc$P_fMeno2<-round(Table_assoc$Pr_fMeno2,4)

Table_assoc$HR_uf<-round(Table_assoc$rHR_uf,2)
Table_assoc$LHR_uf<-round(exp(Table_assoc$beta_uf-1.96*Table_assoc$se_uf),2)
Table_assoc$HHR_uf<-round(exp(Table_assoc$beta_uf+1.96*Table_assoc$se_uf),2)
Table_assoc$HR_CI_uf<-paste0(Table_assoc$HR_uf, " (",Table_assoc$LHR_uf,"-",Table_assoc$HHR_uf,")")
Table_assoc$P_uf<-round(Table_assoc$Pr_uf,4)

Table_assoc$HR_rf<-round(Table_assoc$rHR_rf,2)
Table_assoc$LHR_rf<-round(exp(Table_assoc$beta_rf-1.96*Table_assoc$se_rf),2)
Table_assoc$HHR_rf<-round(exp(Table_assoc$beta_rf+1.96*Table_assoc$se_rf),2)
Table_assoc$HR_CI_rf<-paste0(Table_assoc$HR_rf, " (",Table_assoc$LHR_rf,"-",Table_assoc$HHR_rf,")")
Table_assoc$P_rf<-round(Table_assoc$Pr_rf,4)

setwd("/public/home/xiaomeng/Rawdata_CKB/20230921_AgeScale_Parity_Cox_Diseases")
write.csv(Table_assoc, "20230809_WomenCox1_VS1-2_Parity&HTN.csv")






##### End #####
