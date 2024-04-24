rm(list=ls(all=TRUE))
gc()

# ssh node29 
# R 



# Load data ---------------------------------------------------------------
load("/public/home/xiaomeng/Rawdata_CKB/DAR-2023-00171-V1_Release18.01/dbq5.RData") 










# Perform PheWAS analysis by population  --------------------------------------------------------

# 1.1) All men 
dat_total <- dbq5[!is.na(dbq5$children),]
dat <- dat_total[dat_total$is_female==0,] 
# 1.2) All women
dat_total <- dbq5[!is.na(dbq5$children),]
dat <- dat_total[dat_total$is_female==1,]

# 2.1) Only include men with children
dat_total <- dbq5t[!is.na(dbq5t$children),]
dat <- dat_total[dat_total$is_female==0,] 
# 2.2) Only include women with children
dat_total <- dbq5t[!is.na(dbq5t$children),]
dat <- dat_total[dat_total$is_female==1,]





# Variable grouping -----------------------------------------------------------------

### (1) Among all participants
# Re group the number of biological children: ref=with biological children 
dat$child_grp <-
  ifelse(
    (dat$children==0),
    1, 0)
table(dat$child_grp)



### (2)Among participants with biological children

# >2 biological children vs. 1-2 biological children: ref=1-2 biological children 
dat$child_grp <-
  ifelse(
    (dat$children==1 | dat$children==2),
    0, 1)
table(dat$child_grp)





# Disease at baseline: hypertension, respiratory diseases, CVD, cancer
dat$has_hypertension <-
  ifelse(
    (dat$hypertension_diag==1)|(dat$sbp_mean>=140)|
      (dat$dbp_mean>=90)|(dat$used_blood_pressure_drugs==1),   
    1, 0)

dat$has_respiratory_disease <-
  ifelse(
    (dat$has_copd==1)|(dat$emph_bronc_diag==1)|(dat$asthma_diag==1)|(dat$tb_diag==1),   
    1, 0)

dat$has_CVD <-
  ifelse(
    (dat$chd_diag==1)|(dat$stroke_or_tia_diag==1),   
    1, 0)

dat$has_cancer <- 
  ifelse(
    (dat$cancer_diag==1), 
    1, 0)

# age
dat$age_at_study_date <- dat$age_at_study_date_x100/100

attach(dat)
dat$age[age_at_study_date >= 30 &  age_at_study_date < 35] <- 1
dat$age[age_at_study_date >= 35 &  age_at_study_date < 40] <- 2
dat$age[age_at_study_date >= 40 &  age_at_study_date < 45] <- 3
dat$age[age_at_study_date >= 45 &  age_at_study_date < 50] <- 4
dat$age[age_at_study_date >= 50 &  age_at_study_date < 55] <- 5
dat$age[age_at_study_date >= 55 &  age_at_study_date < 60] <- 6
dat$age[age_at_study_date >= 60 &  age_at_study_date < 65] <- 7
dat$age[age_at_study_date >= 65 &  age_at_study_date < 70] <- 8
dat$age[age_at_study_date >= 70 &  age_at_study_date < 75] <- 9
dat$age[age_at_study_date >= 75 &  age_at_study_date < 80] <- 10

# education : 1 → Primary school or below, 2 →  junior or high school，3 → college or above
dat$education[highest_education==0 | highest_education==1] <- 1 
dat$education[highest_education==2 | highest_education==3] <- 2
dat$education[highest_education==4 | highest_education==5] <- 3 

# household_income：1 → "<10000/year", 2 → "10000-19999/year", 3 → ">=20000/year"
dat$income[household_income == 0 | household_income == 1 | household_income == 2] <- 1
dat$income[household_income == 3 ] <- 2
dat$income[household_income == 4 | household_income == 5] <- 3

# household_size
dat$hhold_size[household_size == 3 | household_size == 4 | household_size == 5 ] <- 1
dat$hhold_size[household_size == 1 | household_size == 2 ] <- 2
dat$hhold_size[household_size >= 6] <- 3

# healthy diet：1 → "Unhealthy diet"，2 → "Healthy diet:score 4-5"
dat$diet_component1 <- ifelse((dat$diet_freq_fresh_veg == 0), 1, 0) 
dat$diet_component2 <- ifelse((dat$diet_freq_fresh_fruit == 0), 1, 0)
dat$diet_component3 <- ifelse((dat$diet_freq_meat == 1 | dat$diet_freq_meat == 2), 1, 0)
dat$diet_component4 <- ifelse((dat$diet_freq_fish <= 2), 1, 0)
dat$diet_component5 <- ifelse((dat$diet_freq_soybean <= 1), 1, 0)
dat$diet_score <- apply(dat[,c("diet_component1","diet_component2","diet_component3",
                               "diet_component4","diet_component5")], 1, sum, na.rm =T)
dat$healthy_diet <- ifelse((dat$diet_score == 4 | dat$diet_score == 5), 2, 1)

# alcohol =5 groups ## note: ex-drinker → excessive drinker (>=60g/d)
# 1 → "Not daily", 2 → "Daily 1-14g/d", 3 → "Daily 15-29g/d", 4 → "Daily 30-59g/d", 5 → "Ex-drinker or Daily >=60g/d"
dat$alchol[(alcohol_category < 6 & alcohol_category != 2 & alcohol_category != 5) | 
             (alcohol_category == 6 & alc_weekly < 2)] <- 1
dat$alchol[alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g < 15.00] <- 2
dat$alchol[alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 15.00 & total_alc_typ_day_g < 30.00)] <- 3
dat$alchol[alcohol_category == 6 & alc_weekly == 2 & (total_alc_typ_day_g >= 30.00 & total_alc_typ_day_g < 60.00)] <- 4
dat$alchol[alcohol_category == 2 |  alcohol_category == 5 | 
             (alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g >= 60.00)] <- 5

# smoking=5 groups: 1 → "Never/occasional", 2 → "Former", 3 → "1-9 cig/d", 4 → "10-19 cig/d", 5 → ">=20 cig/d"
dat$smoke[smoking_category == 1 | smoking_category == 2] <- 1
dat$smoke[smoking_category == 3 & (smoking_stopped_reason > 0 & smoking_stopped_reason <= 4)] <- 2
dat$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day < 10.00] <- 3
dat$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & 
            cig_equiv_day >= 10.00 & cig_equiv_day < 20.00] <- 4
dat$smoke[(smoking_category == 4 | (smoking_category == 3 & smoking_stopped_reason == 0)) & cig_equiv_day >= 20.00] <- 5

# BMI
dat$BMIgrp[bmi_calc < 18.5] <- 1
dat$BMIgrp[bmi_calc >= 18.5 & bmi_calc <24] <- 2
dat$BMIgrp[bmi_calc >= 24 & bmi_calc <28] <- 3
dat$BMIgrp[bmi_calc >= 28] <- 4

# family history : 0 → "No", 1 → "Yes", 2 → "Don't know"   
dat$stroke_family <- NA
dat$stroke_family[(dat$mother_stroke == 0 & dat$father_stroke == 0 & dat$siblings_stroke == 0) | 
                    (dat$mother_stroke == 0 & dat$father_stroke == 0 & dat$siblings == 0)] <- 0
dat$stroke_family[dat$mother_stroke == 1 | dat$father_stroke == 1 | 
                    (dat$siblings_stroke >= 1 & (!is.na(dat$siblings_stroke)))] <- 1
dat$stroke_family <- ifelse(!is.na(dat$stroke_family), dat$stroke_family, 2) 

dat$HA_family <- NA
dat$HA_family[(dat$mother_heart_attack == 0 & dat$father_heart_attack == 0 & dat$siblings_heart_attack == 0) |
                (dat$mother_heart_attack == 0 & dat$father_heart_attack == 0 & dat$siblings == 0)] <- 0
dat$HA_family[dat$mother_heart_attack == 1 | dat$father_heart_attack == 1 | 
                (dat$siblings_heart_attack >= 1 & (!is.na(dat$siblings_heart_attack)))] <- 1
dat$HA_family <- ifelse(!is.na(dat$HA_family), dat$HA_family, 2) 
table(dat$HA_family)

dat$diabetes_family <- NA
dat$diabetes_family[(dat$mother_diabetes == 0 & dat$father_diabetes == 0 & dat$siblings_diabetes == 0) |
                      (dat$mother_diabetes == 0 & dat$father_diabetes == 0 & dat$siblings == 0)] <- 0
dat$diabetes_family[dat$mother_diabetes == 1 | dat$father_diabetes == 1 | 
                      (dat$siblings_diabetes >= 1 & (!is.na(dat$siblings_diabetes)))] <- 1
dat$diabetes_family <- ifelse(!is.na(dat$diabetes_family), dat$diabetes_family, 2) 
table(dat$diabetes_family)

dat$Mental_all_family <- NA
dat$Mental_all_family[(dat$mother_mental_disorder == 0 & dat$father_mental_disorder == 0 & dat$siblings_mental_disorder == 0) |
                        (dat$mother_mental_disorder == 0 & dat$father_mental_disorder == 0 & dat$siblings == 0)] <- 0
dat$Mental_all_family[dat$mother_mental_disorder == 1 | dat$father_mental_disorder == 1 | 
                        (dat$siblings_mental_disorder >= 1 & (!is.na(dat$siblings_mental_disorder)))] <- 1
dat$Mental_all_family <- ifelse(!is.na(dat$Mental_all_family), dat$Mental_all_family, 2) 

dat$cancer_family <- NA
dat$cancer_family[(dat$mother_cancer == 0 & dat$father_cancer == 0 & dat$siblings_cancer == 0) |
                    (dat$mother_cancer == 0 & dat$father_cancer == 0 & dat$siblings == 0)] <- 0
dat$cancer_family[dat$mother_cancer == 1 | dat$father_cancer == 1 | 
                    (dat$siblings_cancer >= 1 & (!is.na(dat$siblings_cancer)))] <- 1
dat$cancer_family <- ifelse(!is.na(dat$cancer_family), dat$cancer_family, 2) 
table(dat$cancer_family)

# id.sex
dat$id_sex[is_female == 0] <- 'M'
dat$id_sex[is_female == 1] <- 'F'

detach(dat)



dat$marriage_factor <- as.factor(dat$marital_status) 
dat$age_factor <- as.factor(dat$age)
dat$gender_factor <- as.factor(dat$is_female)
dat$education_factor <- as.factor(dat$education)
dat$income_factor <- as.factor(dat$income)
dat$householdsize_factor <- as.factor(dat$hhold_size)
dat$diet_factor <- as.factor(dat$healthy_diet)
dat$alc_grp_factor <- as.factor(dat$alchol)
dat$smoke_factor <- as.factor(dat$smoke)
dat$BMIgrp_factor <- as.factor(dat$BMIgrp)
dat$diabetes_factor <- as.factor(dat$has_diabetes)
dat$hypertension_factor <- as.factor(dat$has_hypertension)
dat$respiratory_disease_factor <- as.factor(dat$has_respiratory_disease)
dat$CVD_factor <- as.factor(dat$has_CVD)
dat$cancer_factor <- as.factor(dat$has_cancer)
dat$satisfaction_level_factor <- as.factor(dat$satisfaction_level)
dat$region_code_factor <- as.factor(dat$region_code)
dat$Meno_factor <- as.factor(dat$menopause_status)
dat$stroke_family_factor <- as.factor(dat$stroke_family)
dat$HA_family_factor <- as.factor(dat$HA_family)
dat$diabetes_family_factor <- as.factor(dat$diabetes_family)
dat$Mental_all_family_factor <- as.factor(dat$Mental_all_family)
dat$cancer_family_factor <- as.factor(dat$cancer_family)
dat$child_grp_factor <- as.factor(dat$child_grp)




# Phenotypic data for PheWAS--------------------------------------------------------------

# Individuals with disease records
dee1 <- dee[dee$csid %in% dat$csid,]   
dee1 <- dee1[,c("csid","type","datedeveloped","diagnosis")]
# Merge "csid+onset time+disease diagnosis" and remove duplicate diagnostic records (same day, same diagnosis)
dee1$date_diag <- paste(dee1$csid,dee1$datedeveloped,dee1$diagnosis) # dee1=607289
dee2 <- dee1[!duplicated(dee1$date_diag),] # dee2=554079
dee2 <- dee2[,c("csid","type","diagnosis")]
dee2$type <- "ICD10CM"
dee2$count <- 1
# Remove the ICD encoding ". -" from dee2, otherwise the data cannot be read in
dee2$diagnosis <- gsub(".-","",dee2$diagnosis)



# Individuals who were diagnosed at baseline were included, and the results were saved in dee3
dee4 <- data.frame() 
disease_list <- c("csid","has_diabetes","has_copd","chd_diag","stroke_or_tia_diag",
                  "has_hypertension","rheum_heart_dis_diag","tb_diag","emph_bronc_diag",
                  "asthma_diag","cirrhosis_hep_diag","peptic_ulcer_diag","gall_diag",
                  "kidney_dis_diag","fracture_diag","rheum_arthritis_diag",
                  "psych_disorder_diag","neurasthenia_diag","head_injury_diag",
                  "cancer_diag","cancer_site")
ICD_10_list <- c("E11","J44","I25","I64","I10","I05","A15","J43","J45","K74",
                 "K27","K80","N11","T10","M05","F20","F48","S02","C34","C15",
                 "C16","C22","C18","C50","C61","C53")

dat3 <- dat[,disease_list]
for (i in 1:18){
  dee3 <- data.frame(dat3[dat3[,i+1]==1,"csid"])
  dee3$type <- "ICD10CM"
  dee3$diagnosis <- ICD_10_list[i]
  dee3$count <- 1
  colnames(dee3) <- c("csid","type","diagnosis","count")
  dee4 <- rbind(dee4,dee3)
}
#Check which types of cancer are not present in this group
table(dat3$cancer_site)



#Cancer (there are 8 different types of cancer in the questionnaire: 0-7, 8: others)
# for (i in c(0:6)){   #When dividing by sex, men do not have cancer 7; Women do not have cancer 6
for (i in c(0:5,7)){
  dee3<-data.frame(dat3[dat3[,"cancer_diag"]==1 & dat3[,"cancer_site"]==i,"csid"])
  dee3$type<-"ICD10CM"
  dee3$diagnosis<-ICD_10_list[i+19]
  dee3$count<-1
  colnames(dee3)<-c("csid","type","diagnosis","count")
  dee4<-rbind(dee4,dee3)
}

id.sex<-dat[,c("csid","id_sex")]





# PheWAS analysis ----------------------------------------------------------------

library(PheWAS)
dee4 <- rbind(dee2,dee4)
phenotypes <- createPhenotypes(dee4,min.code.count=1,aggregate.fun=sum,id.sex=id.sex)
dim(phenotypes)

# Delete phenotypes with fewer than 30 cases
phe_30 <- c()
for (i in 2:ncol(phenotypes)){
  if (length(which(phenotypes[,i]=="TRUE"))<30){
    phe_30<-c(phe_30,i)
  }
}
phenotypes1 <- phenotypes[,-phe_30]
dim(phenotypes1)

library(PheWAS)
genotypes <- dat[,c("csid","child_grp")]





# model 1
covariate_list <- c("csid",  "age_factor","education_factor","income_factor",
                    "householdsize_factor","region_code_factor", "marriage_factor") 
covariate <- dat[,covariate_list] 
dat_PheWAS <- merge(merge(phenotypes1,genotypes, by="csid"),covariate,by="csid")


# model 2
covariate_list <- c("csid","age_factor","education_factor", "income_factor",
                    "householdsize_factor", "region_code_factor", "marriage_factor",
                    "diet_factor", "alc_grp_factor", "smoke_factor","BMIgrp_factor","met") 
covariate <- dat[,covariate_list] 
dat_PheWAS <- merge(merge(phenotypes1,genotypes, by="csid"),covariate,by="csid")


# model 3 for men
covariate_list <- c("csid", "age_factor","education_factor","income_factor",
                    "householdsize_factor","region_code_factor", "marriage_factor", 
                    "diet_factor", "alc_grp_factor", "smoke_factor","BMIgrp_factor","met",
                    "diabetes_factor","hypertension_factor","respiratory_disease_factor",
                    "CVD_factor","cancer_factor", "satisfaction_level_factor", 
                    "stroke_family_factor", "diabetes_family_factor", "HA_family_factor", 
                    "Mental_all_family_factor", "cancer_family_factor") 
covariate <- dat[,covariate_list] 
dat_PheWAS <- merge(merge(phenotypes1,genotypes, by="csid"),covariate,by="csid")


# model 3 for women
covariate_list <- c("csid", "age_factor","education_factor","income_factor",
                    "householdsize_factor","region_code_factor","marriage_factor",
                    "diet_factor", "alc_grp_factor", "smoke_factor","BMIgrp_factor","met",
                    "diabetes_factor","hypertension_factor","respiratory_disease_factor",
                    "CVD_factor","cancer_factor", "satisfaction_level_factor","Meno_factor",
                    "stroke_family_factor", "diabetes_family_factor", "HA_family_factor", 
                    "Mental_all_family_factor", "cancer_family_factor") 
covariate <- dat[,covariate_list] 
dat_PheWAS <- merge(merge(phenotypes1,genotypes, by="csid"),covariate,by="csid")






# PheWAS results and plots ----------------------------------------------------------------

setwd("/public/home/xiaomeng/PheWAS_Parity")
library(PheWAS)
results <- data.frame()
plot_list = list() 
for (i in 2:ncol(genotypes)){
  result=phewas_ext(dat_PheWAS,phenotypes=names(phenotypes1)[-1],genotypes=names(genotypes)[i],
                    covariates=covariate_list[-1],cores=4)
  P=phewasManhattan(result,annotate.angle=0,OR.direction=T,title=paste("PheWAS:",names(genotypes)[i]),
                    point.size=3.5,annotate.size=3,annotate.only.largest=T,
                    OR.size = T)
  plot_list[[i]]=P
  results <- rbind(results,result)
}
results_d=addPhecodeInfo(results)





# results output -------------

setwd("/public/home/xiaomeng/5. PheWAS/3. PheWAS_Parity/2. Cutoff_20")
write.table(results_d,"Parity_VS0_Women_d_model3.csv",row.names=FALSE,col.names=TRUE,sep=",") 
write.table(results,"Parity_VS0_Women_model3.csv",row.names=FALSE,col.names=TRUE,sep=",")


setwd("/public/home/xiaomeng/5. PheWAS/3. PheWAS_Parity/2. Cutoff_20")
pdf("/public/home/xiaomeng/5. PheWAS/3. PheWAS_Parity/2. Cutoff_20/Parity_VS0_Women_model3.pdf",
    width=10,height=8)

for (i in 2:ncol(genotypes)){
  print(plot_list[[i]])
}
while (!is.null(dev.list()))
  dev.off()





# END ---------------------------------------------------------------------


