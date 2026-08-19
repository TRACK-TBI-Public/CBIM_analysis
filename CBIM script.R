# LOAD LIBRARIES ----------------------------------------------------------------
library(devtools)
library(devEMF)
library(plyr)
library(lubridate)
library(tableone)
library(dplyr)
library(xlsx)
library(readr)
library(readxl)
library(ggplot2)
library(fmsb)
library(stringr)
library(jsonlite)
library(PerformanceAnalytics) 
library(psych)
library(data.table)
library(finalfit)
library(MASS)
library(car)
library(rsq)
library(haven)
library(lme4)
library(DescTools)
library(ordinal)
library(eulerr)
library(ggpubr)
library(ggalluvial)
library(MuMIn)
library(performance)
library(MatchIt)
library(UpSetR)
library(mice)
library(broom)
library(segmented)
library(naniar)
library(viridis)
library(ComplexUpset)
library(ClustOfVar)
library(Rcpp)
library(pROC)
library(OptimalCutpoints)
library(ggbeeswarm)
library(rstatix)
library(FSA)
library(mltools)
library(Metrics)
library(GGally)
library(FactoMineR)
library(factoextra)
library(scales)
library(CalibrationCurves)
library(rms)
library(Hmisc)
library(jtools)
require(tidyverse)
require(rcompanion)
library(scales)
library(ggh4x)
library(ggpp)
library(ggrepel)

# COLOR SCHEME ------------------------------------------------------------
C_COLOR <- "#223BC9"
B_COLOR <- "#e63b60"
I_COLOR <- "#60b0b0"
M_COLOR <- "#762FD2"
cols <- c("Linear" = B_COLOR, "RCS" = I_COLOR)

# IMPORT, RECODE, CURATE DATA ----------------------------------------------------
### CLINICAL PILLAR ----------------------------------------------------------------
# https://center-tbi.incf.org/_660056faab2e6d9f49883464
C <- read.csv("IMPORT/Clinical.24.03.2024.csv")
str(C)

## RECODE CATEGORICAL VARIABLES
table(C$InjuryHx.PupilsBaselineDerived)
# derived centrally, recommended for baseline risk adjustment: missing values imputed centrally using IMPACT methodology
# 1 = One reacting (other pupil is either unreactive, missing or untestable)
# Untestable pupil ignored (ie, 1 reactive + 1 untestable = 1 reactive) - this assumption applies only to a small proportion of the data
C$InjuryHx.PupilsBaselineDerived <- as.factor(C$InjuryHx.PupilsBaselineDerived)
levels(C$InjuryHx.PupilsBaselineDerived) <- c("Both reacting", "One reacting", "Both unreacting")
table(C$InjuryHx.PupilsBaselineDerived)

table(C$InjuryHx.LOCGCSSumDet)
# used as auxiliary variable in imputation
C$InjuryHx.LOCGCSSumDet <- as.factor(C$InjuryHx.LOCGCSSumDet)
levels(C$InjuryHx.LOCGCSSumDet) <- c("None", "1 point", "2 or more points", NA)
table(C$InjuryHx.LOCGCSSumDet)

## RECODE NUMERICAL VARIABLES, PLOT HISTOGRAMS, TEST NORMAL DISTRIBUTION
for (i in c("InjuryHx.GCSScoreBaselineDerived", "InjuryHx.GCSMotorBaselineDerived")) {
  print(i)
  print(table(C[, i]))
  C[, i] <- as.numeric(C[, i])
  print(table(C[, i]))
  hist(C[, i], main=i)
  print(shapiro.test(C[, i]))
}  

## IMPUTE BASELINE DERIVED GCS EYES, VERBAL COMPONENTS (WERE NOT CENTRALLY IMPUTED IN CENTER-TBI), USING SAME THE SAME METHODOLOGY AS FOR GCS MOTOR BASELINE DERIVED

# Recode "" as missing
C[,c("InjuryHx.GCSPreHospBestEyes", "InjuryHx.GCSPreHospBestMotor", "InjuryHx.GCSPreHospBestVerbal", "InjuryHx.GCSPreHospBestScore",
     "InjuryHx.GCSFirstHospEyes", "InjuryHx.GCSFirstHospMotor", "InjuryHx.GCSFirstHospVerbal", "InjuryHx.GCSFirstHospScore",
     "InjuryHx.GCSEDArrEyes", "InjuryHx.GCSEDArrMotor", "InjuryHx.GCSEDArrVerbal", "InjuryHx.GCSEDArrScore", 
     "InjuryHx.GCSEDDischEyes", "InjuryHx.GCSEDDischMotor",  "InjuryHx.GCSEDDischVerbal", "InjuryHx.GCSEDDischScore",
     "InjuryHx.GCSOtherEyes", "InjuryHx.GCSOtherMotor", "InjuryHx.GCSOtherVerbal", "InjuryHx.GCSOtherScore")][C[,c("InjuryHx.GCSPreHospBestEyes", "InjuryHx.GCSPreHospBestMotor", "InjuryHx.GCSPreHospBestVerbal", "InjuryHx.GCSPreHospBestScore",
                                                                                                                   "InjuryHx.GCSFirstHospEyes", "InjuryHx.GCSFirstHospMotor", "InjuryHx.GCSFirstHospVerbal", "InjuryHx.GCSFirstHospScore",
                                                                                                                   "InjuryHx.GCSEDArrEyes", "InjuryHx.GCSEDArrMotor", "InjuryHx.GCSEDArrVerbal", "InjuryHx.GCSEDArrScore", 
                                                                                                                   "InjuryHx.GCSEDDischEyes", "InjuryHx.GCSEDDischMotor",  "InjuryHx.GCSEDDischVerbal", "InjuryHx.GCSEDDischScore",
                                                                                                                   "InjuryHx.GCSOtherEyes", "InjuryHx.GCSOtherMotor", "InjuryHx.GCSOtherVerbal", "InjuryHx.GCSOtherScore")] ==""] <- NA

# REPRODUCE IMPUTATION METHOD FOR BASELINE DERIVED EYES AND VERBAL 
C$InjuryHx.GCSEyesBaselineDerived <- ifelse(!C$InjuryHx.GCSEDDischEyes %in% c(NA, "UN", "S", "O"), C$InjuryHx.GCSEDDischEyes, 
                                            ifelse(!C$InjuryHx.GCSEDArrEyes %in% c(NA, "UN", "S", "O"), C$InjuryHx.GCSEDArrEyes, 
                                                   ifelse(!C$InjuryHx.GCSFirstHospEyes %in% c(NA, "UN", "S", "O"), C$InjuryHx.GCSFirstHospEyes, 
                                                          ifelse(!C$InjuryHx.GCSPreHospBestEyes %in% c(NA, "UN", "S", "O"), C$InjuryHx.GCSPreHospBestEyes, NA))))
table(C$InjuryHx.GCSEyesBaselineDerived)
table(is.na(C$InjuryHx.GCSEyesBaselineDerived))
C$InjuryHx.GCSEyesBaselineDerived <- as.numeric(C$InjuryHx.GCSEyesBaselineDerived)

C$InjuryHx.GCSVerbalBaselineDerived <- ifelse(!C$InjuryHx.GCSEDDischVerbal %in% c(NA, "UN", "T", "O"), C$InjuryHx.GCSEDDischVerbal, 
                                              ifelse(!C$InjuryHx.GCSEDArrVerbal %in% c(NA, "UN", "T", "O"), C$InjuryHx.GCSEDArrVerbal, 
                                                     ifelse(!C$InjuryHx.GCSFirstHospVerbal %in% c(NA, "UN", "T", "O"), C$InjuryHx.GCSFirstHospVerbal, 
                                                            ifelse(!C$InjuryHx.GCSPreHospBestVerbal %in% c(NA, "UN", "T", "O"), C$InjuryHx.GCSPreHospBestVerbal, NA))))
table(C$InjuryHx.GCSVerbalBaselineDerived)
table(is.na(C$InjuryHx.GCSVerbalBaselineDerived))
C$InjuryHx.GCSVerbalBaselineDerived <- as.numeric(C$InjuryHx.GCSVerbalBaselineDerived)

## ADD TBI SEVERITY VARIABLE
table(C$InjuryHx.GCSScoreBaselineDerived)
C$TBISeverity <- as.factor(ifelse((C$InjuryHx.GCSScoreBaselineDerived >= 13), "Mild (Baseline GCS>=13)", 
                                  ifelse((C$InjuryHx.GCSScoreBaselineDerived <= 8), "Severe (Baseline GCS<=8)", 
                                         ifelse((C$InjuryHx.GCSScoreBaselineDerived < 13 & C$InjuryHx.GCSScoreBaselineDerived > 8), "Moderate (Baseline GCS 9-12)", 
                                                NA))))
table(C$TBISeverity)
table(is.na(C$TBISeverity))

## KEEP RELEVANT VARIABLES FOR IMPUTATION 
C <- C[, c("subjectId", "TBISeverity", "InjuryHx.PupilsBaselineDerived", "InjuryHx.GCSScoreBaselineDerived", 
           "InjuryHx.GCSEyesBaselineDerived", "InjuryHx.GCSMotorBaselineDerived", "InjuryHx.GCSVerbalBaselineDerived",                     
           "InjuryHx.LOCGCSSumDet")]

### BIOMARKERS PILLAR --------------------------------------------------------------
# https://center-tbi.incf.org/_6699106e4f12bd0241e5744e
B <- read.csv("IMPORT/Biomarkers.18.07.2024.csv")
length(unique(B$subjectId)) 

## SELECT PATIENT SUBSET WITH AT LEAST 1/6 BIOMARKERS SAMPLED (GFAP | S100B | UCH-L1 | NFL | NSE | Tau)

# Exclude patients with no samples
table(B$Biomarkers.SampleId != "")
B <- B[B$Biomarkers.SampleId != "",]
length(unique(B$subjectId))   # 706 patients with no samples collected -> excluded

# Exclude patients with samples in which none of the 6 biomarkers were measured
B$number_of_biomearkers_measured <- rowSums(!is.na(B[, c("Biomarkers.GFAP", "Biomarkers.S100B", "Biomarkers.UCH.L1",
                                                         "Biomarkers.NFL", "Biomarkers.NSE", "Biomarkers.Tau")]))
table(B$number_of_biomearkers_measured)   # remove 15 samples with all 6 biomarker values missing
B <- B[B$number_of_biomearkers_measured != 0,]
length(unique(B$subjectId)) # 1 patient excluded

## CALCULATE TIME FROM INJURY TO SAMPLING
B$bbb_collection_dt <- as.POSIXct(paste(B$Biomarkers.CollectionDate, B$Biomarkers.CollectionTime), format="%Y-%m-%d %H:%M:%S")

# Add date and time of injury
DT <- read.csv("IMPORT/InjuryDT.6.05.2024.csv")
DT$injury_dt <- as.POSIXct(paste(DT$Subject.DateInj, DT$Subject.TimeInj), format="%Y-%m-%d %H:%M:%S")

# Impute 4 missing times of injury to earliest possible time
DT[is.na(DT$injury_dt), ]
DT[is.na(DT$injury_dt), ]$injury_dt <- "1970-01-01 00:00:00 CET"

B <- merge(B, DT[, c("subjectId", "injury_dt")], by = "subjectId", all.x = T)
B$bbb_sampling_time <- as.numeric(difftime(B$bbb_collection_dt, B$injury_dt, units = c("hours"))) 
summary(B$bbb_sampling_time) 

## REMOVE SAMPLES WITH UNKNOWN COLLECTION DATE AND TIME
# patients with a single sample with unknown collection date and time are excluded
# in patients with multiple samples, the samples with unknown collection date and time are excluded

B <- B[!is.na(B$bbb_sampling_time),]
length(unique(B$subjectId)) # 55 patients excluded

## SELECT FIRST BIOMARKER SAMPLE PER PATIENT
# the earliest sample with known collection date and time is selected
B <- data.table(B)
B_first <- B[, .SD[which.min(bbb_collection_dt)], by = subjectId]
length(unique(B_first$subjectId)) 

## SELECT ONLY FIRST SAMPLES COLLECTED IN THE FIRST 24H AFTER INJURY
table(B_first$bbb_sampling_time <= 0) # 8 samples have collection date and time BEFORE injury date and time, assume entry error (next day) 
B_first[B_first$bbb_sampling_time <= 0, ]$bbb_sampling_time <- 24 + B_first[B_first$bbb_sampling_time <= 0, ]$bbb_sampling_time

table(B_first$bbb_sampling_time <= 24)
B_first_24 <- B_first[B_first$bbb_sampling_time <= 24, ] # 498 patients excluded
length(unique(B_first_24$subjectId))

## SELECT THE (NEXT) EARLIEST SAMPLE COLLECTED PER PATIENT, TO USE IN IMPUTATION
# in the 498 patients with the first sample collected after 24h, this first sample is selected
# in patients with the first sample collected within 24h, the next sample is selected
B_next <- setdiff(B, B_first_24)
B_next <- B_next[, .SD[which.min(bbb_collection_dt)], by = subjectId]
length(unique(B_next$subjectId)) 

names(B_next) <- paste0("next_earliest_", names(B_next))
B_first_24_next <- merge(B_first_24, B_next, by.x = "subjectId", by.y = "next_earliest_subjectId", all = T)

table(is.na(B_first_24_next$bbb_sampling_time), is.na(B_first_24_next$next_earliest_bbb_sampling_time))

## KEEP RELEVANT VARIABLES FOR IMPUTATION 
B <- B_first_24_next[, c("subjectId", "bbb_sampling_time",   
                         "Biomarkers.GFAP", "Biomarkers.S100B", "Biomarkers.UCH.L1", 
                         "Biomarkers.NFL", "Biomarkers.NSE", "Biomarkers.Tau",
                         "next_earliest_bbb_sampling_time",
                         "next_earliest_Biomarkers.GFAP", "next_earliest_Biomarkers.S100B", "next_earliest_Biomarkers.UCH.L1",
                         "next_earliest_Biomarkers.NFL", "next_earliest_Biomarkers.NSE", "next_earliest_Biomarkers.Tau")]

# LOG TRANSFORM BIOMARKER VALUES (natural log)
B$logGFAP <- log(B$Biomarkers.GFAP)
B$logUCHL1 <- log(B$Biomarkers.UCH.L1)
B$logS100B <- log(B$Biomarkers.S100B)
B$logNFL <- log(B$Biomarkers.NFL)
B$logNSE <- log(B$Biomarkers.NSE)
B$logTau <- log(B$Biomarkers.Tau)

# SCALE LOGs
B$zlogGFAP <- scale(B$logGFAP)
B$zlogUCHL1 <- scale(B$logUCHL1)
B$zlogS100B <- scale(B$logS100B)
B$zlogNFL <- scale(B$logNFL)
B$zlogNSE <- scale(B$logNSE)
B$zlogTau <- scale(B$logTau)

B$lognext_earliest_GFAP <- log(B$next_earliest_Biomarkers.GFAP)
B$lognext_earliest_S100B <- log(B$next_earliest_Biomarkers.S100B)
B$lognext_earliest_UCH.L1 <- log(B$next_earliest_Biomarkers.UCH.L1)
B$lognext_earliest_NFL <- log(B$next_earliest_Biomarkers.NFL)
B$lognext_earliest_NSE <- log(B$next_earliest_Biomarkers.NSE)
B$lognext_earliest_Tau <- log(B$next_earliest_Biomarkers.Tau)

B$next_earliest_Biomarkers.GFAP <- NULL
B$next_earliest_Biomarkers.S100B <- NULL
B$next_earliest_Biomarkers.UCH.L1 <- NULL
B$next_earliest_Biomarkers.NFL <- NULL
B$next_earliest_Biomarkers.NSE <- NULL
B$next_earliest_Biomarkers.Tau <- NULL

B <- data.frame(B)

## HISTOGRAMS, TEST NORMAL DISTRIBUTION 
for (i in c("Biomarkers.GFAP", "logGFAP", "zlogGFAP",
            "Biomarkers.S100B", "logS100B", "zlogS100B", 
            "Biomarkers.UCH.L1", "logUCHL1", "zlogUCHL1")) {
  hist(B[, i], main=i)
  print(shapiro.test(B[, i]))
  print(summary(B[, i]))
}  

rm(B_first, B_first_24, B_first_24_next, B_next)

### IMAGING PILLAR -----------------------------------------------------------------
## IMPORT STRUCTURED REPORTS OF THE FIRST CT SCAN PER PATIENT
I <- read.csv("IMPORT/JSON extraction script/Summary/all_lesions_CT.10.05.2024.csv")
I$X <- NULL
# 422 patients have no first scan report (not uploaded/uninterpretable)

# FIX DATA ERROR (uncovered previously during curation)
I[I$subjectId== "2UBy682",]$ventricular_compression_count_p <- 0

## CALCULATE TIME FROM INJURY TO SCAN
I$scan_dt <- as.POSIXct(paste(I$Imaging.ExperimentDate, I$Imaging.ExperimentTime), format="%Y-%m-%d %H:%M:%S")
I <- merge(I, DT[, c("subjectId", "injury_dt")], by = "subjectId")
I$scan_time <- as.numeric(difftime(I$scan_dt, I$injury_dt, units = c("hours"))) 
summary(I$scan_time) 

table(I$scan_time <= 0) # 28 scans have date and time BEFORE injury date and time, assume within 24h window
# impute to median scan_time (for use in imputation later)
I[I$scan_time <= 0, ]$scan_time <- median(I$scan_time) 

PercTable(I$scan_time <= 24) # 57 scans performed more than 24h after injury

## CREATE NEW COMPOSITE VARIABLES

# Create new variable (predictor) for Any Abnormality, includes: isolated skull fracture; excludes: isolated incidental findings, the mass lesion CDE
I$AnyAbnormality <- apply(I[, c("skull_fracture_count_p",
                                "epidural_hematoma_count_p",                            
                                "intraparenchymal_hemorrhage_count_p",                  
                                "intraventricular_hemorrhage_ivh_count_p",              
                                "subdural_hematoma_acute_count_p",                      
                                "subdural_hematoma_subacute_count_p",                   
                                "subdural_hematoma_mixed_density_count_p",              
                                "extraaxial_hematoma_count_p",                          
                                "tsah_count_p",                                      
                                "cisternal_compression_count_p",                     
                                "mls_count_p",                            
                                "ventricular_compression_count_p" ,                     
                                "cortical_sulcus_effacement_count_p",
                                "brain_herniation_count_p",                        
                                "penetrating_injury_count_p",
                                "cervicomedullary_junction_brainstem_injury_count_p",                     
                                "vascular_dissection_count_p",                       
                                "venous_sinus_injury_count_p",                      
                                "traumatic_aneurysm_count_p",
                                "dai_count_p",                                         
                                "tai_count_p",
                                "edema_hyperemia_ischemia_count")], 1, function(x) length(which(x>=1)))
table(I$AnyAbnormality)
I$AnyAbnormality <- as.factor(ifelse(I$AnyAbnormality==0, 0, 1))
table(I$AnyAbnormality)

I$NoAbnormality <- as.factor(ifelse(I$AnyAbnormality==0, 1, 0))
table(I$NoAbnormality)

# Create new variable (end-point) for Any INTRACRANIAL Abnormality, excludes: isolated skull fracture, isolated incidental findings, the mass lesion CDE
# centrally derived Imaging.AnyIntracranTraumaticAbnormality variables has conflicts for isolated indeterminate (TAI, for some reason vascular dissection) -> present; 
# does not consider all CDEs (isolated vascular dissection)
I$AnyIntracranAbnormality <- apply(I[, c("epidural_hematoma_count_p",                            
                                         "intraparenchymal_hemorrhage_count_p",                  
                                         "intraventricular_hemorrhage_ivh_count_p",              
                                         "subdural_hematoma_acute_count_p",                      
                                         "subdural_hematoma_subacute_count_p",                   
                                         "subdural_hematoma_mixed_density_count_p",              
                                         "extraaxial_hematoma_count_p",                          
                                         "tsah_count_p",                                      
                                         "cisternal_compression_count_p",                     
                                         "mls_count_p",                             
                                         "ventricular_compression_count_p" ,                     
                                         "cortical_sulcus_effacement_count_p",
                                         "brain_herniation_count_p",                        
                                         "penetrating_injury_count_p",
                                         "cervicomedullary_junction_brainstem_injury_count_p",                     
                                         "vascular_dissection_count_p",                       
                                         "venous_sinus_injury_count_p",                      
                                         "traumatic_aneurysm_count_p",
                                         "dai_count_p",                                         
                                         "tai_count_p",
                                         "edema_hyperemia_ischemia_count")], 1, function(x) length(which(x>=1)))
table(I$AnyIntracranAbnormality)
I$AnyIntracranAbnormality <- as.factor(ifelse(I$AnyIntracranAbnormality==0, 0, 1))
table(I$AnyIntracranAbnormality)

table(I$AnyIntracranAbnormality, I$AnyAbnormality)

## CLEAN VARIABLES
# Transform number of lesions variables 0, 1, 2... to binary variables
for (i in c("skull_fracture_count_p", "epidural_hematoma_count_p", "intraparenchymal_hemorrhage_count_p",                  
            "intraventricular_hemorrhage_ivh_count_p", "subdural_hematoma_acute_count_p", "subdural_hematoma_subacute_count_p",                   
            "subdural_hematoma_mixed_density_count_p", "tsah_count_p", "cisternal_compression_count_p",                     
            "mls_count_p", "ventricular_compression_count_p", "brain_herniation_count_p", "dai_count_p", "tai_count_p",
            "extraaxial_hematoma_count_p", "penetrating_injury_count_p", "vascular_dissection_count_p", "edema_hyperemia_ischemia_count")) {
  print(i)
  print(table(I[, i]))
  I[, i] <- as.factor(ifelse(I[, i] == 0, 0, 1))
  print(table(I[, i]))
} 

## COMBINE ALL SDHs (acute/subacute/chronic/mixed-density)
I$subdural_hematoma <- apply(I[, c("subdural_hematoma_acute_count_p",                      
                                   "subdural_hematoma_subacute_count_p",                   
                                   "subdural_hematoma_mixed_density_count_p")], 1, function(x) length(which(x>=1)))
table(I$subdural_hematoma)
I$subdural_hematoma <- as.factor(ifelse(I$subdural_hematoma==0, 0, 1))
table(I$subdural_hematoma)

## CREATE LESION VOLUME VARIABLE FOR SDH
I$subdural_hematoma_volume <- apply(I[, c("subdural_hematoma_acute_total_volume_p", 
                                          "subdural_hematoma_subacute_total_volume_p", 
                                          "subdural_hematoma_mixed_density_total_volume_p")], 1, sum)

## SEPARATE CONTUSION AND ICH
### import supplemental and emerging CDEs
iph <- read.csv("IMPORT/JSON extraction script/Individual/intraparenchymal_hemorrhage.10.05.2024.csv")
iph <- iph[iph$Imaging.Timepoint == "CT Early", ]
iph <- iph[iph$intraparenchymal_hemorrhage_basic_observation %in% ("present"), ]
length(unique(iph$subjectId))

table(iph$intraparenchymal_hemorrhage_ic_hemorraghe, iph$intraparenchymal_hemorrhage_contusion)
iph_contusion <- iph[iph$intraparenchymal_hemorrhage_contusion %in% c(1), ]
length(unique(iph_contusion$subjectId))
iph_ICH <- iph[iph$intraparenchymal_hemorrhage_ic_hemorraghe %in% c(1) & iph$intraparenchymal_hemorrhage_contusion %in% c(0), ]
length(unique(iph_ICH$subjectId))

I$Contusion <- as.factor(ifelse(I$subjectId %in% iph_contusion$subjectId, 1, 0))
table(I$Contusion) 
I$ICH <- as.factor(ifelse(I$subjectId %in% iph_ICH$subjectId, 1, 0))
table(I$ICH) 

contusion_volume <- aggregate(iph_contusion[!is.na(iph_contusion$intraparenchymal_hemorrhage_descriptive_volume),]$intraparenchymal_hemorrhage_descriptive_volume, 
                              by=list(subjectId=iph_contusion[!is.na(iph_contusion$intraparenchymal_hemorrhage_descriptive_volume),]$subjectId), FUN=sum)
names(contusion_volume) <- c("subjectId", "Contusion_volume")
ICH_volume <- aggregate(iph_ICH[!is.na(iph_ICH$intraparenchymal_hemorrhage_descriptive_volume),]$intraparenchymal_hemorrhage_descriptive_volume, 
                        by=list(subjectId=iph_ICH[!is.na(iph_ICH$intraparenchymal_hemorrhage_descriptive_volume),]$subjectId), FUN=sum)
names(ICH_volume) <- c("subjectId", "ICH_volume")

I <- merge(I, contusion_volume, by = "subjectId", all.x = T)
I <- merge(I, ICH_volume, by = "subjectId", all.x = T)

# Copy volume stored in separate column
I[I$Contusion == 1 & is.na(I$Contusion_volume),]
I[I$subjectId == "7afB407",]$Contusion_volume <- iph_contusion[iph_contusion$subjectId == "7afB407",]$intraparenchymal_hemorrhage_contusion_volume
I[I$subjectId == "7cGS275",]$Contusion_volume <- iph_contusion[iph_contusion$subjectId == "7cGS275",]$intraparenchymal_hemorrhage_contusion_volume
I[I$ICH == 1 & is.na(I$ICH_volume),]
I[I$subjectId == "2ood816",]$ICH_volume <- iph_ICH[iph_ICH$subjectId == "2ood816",]$intraparenchymal_hemorrhage_hemorrhage_volume
I[I$subjectId == "9GoK336",]$ICH_volume <- iph_ICH[iph_ICH$subjectId == "9GoK336",]$intraparenchymal_hemorrhage_hemorrhage_volume
I[I$Contusion == 0,]$Contusion_volume <- 0
I[I$ICH == 0,]$ICH_volume <- 0
rm(iph, iph_contusion, iph_ICH)

## COMBINE DAI AND TAI into TAMVI
I$TAMVI <- apply(I[, c("dai_count_p", "tai_count_p")], 1, function(x) length(which(x>=1)))
table(I$TAMVI)
I$TAMVI <- as.factor(ifelse(I$TAMVI==0, 0, 1))
table(I$TAMVI)

## CREATE COMPOSITE MASS EFFECT VARIABLE
I$mass_effect <- apply(I[, c("cisternal_compression_count_p", "ventricular_compression_count_p", "mls_count_p", 
                             "brain_herniation_count_p")], 1, function(x) length(which(x>=1)))
table(I$mass_effect)
I$mass_effect <- as.factor(ifelse(I$mass_effect==0, 0, 1))
table(I$mass_effect)

# CREATE TOTAL LESION VOLUME >= 25 ml VARIABLE
# lesions with volume: 
# "epidural_hematoma_total_volume_p" "intraparenchymal_hemorrhage_total_volume_p"  
# "subdural_hematoma_acute_total_volume_p" "subdural_hematoma_subacute_total_volume_p" 
# "subdural_hematoma_mixed_density_total_volume_p" "extraaxial_hematoma_total_volume_p"          

I$total_lesion_volume <- apply(I[, c("epidural_hematoma_total_volume_p", "intraparenchymal_hemorrhage_total_volume_p",
                                     "subdural_hematoma_acute_total_volume_p", "subdural_hematoma_subacute_total_volume_p", 
                                     "subdural_hematoma_mixed_density_total_volume_p", "extraaxial_hematoma_total_volume_p")], 1, sum)   # includes volume of extra-axial hematoma - co-occurs with other bleeding types
hist(I$total_lesion_volume)
shapiro.test(I$total_lesion_volume)
I$total_lesion_volume_25 <- as.factor(ifelse(I$total_lesion_volume >= 25, 1, 0))

## KEEP RELEVANT VARIABLES FOR IMPUTATION 
I <- I[, c("subjectId", "scan_time", 
           "skull_fracture_count_p", 
           "epidural_hematoma_total_volume_p", "epidural_hematoma_count_p", 
           "subdural_hematoma_acute_total_volume_p", "subdural_hematoma_acute_count_p", 
           "subdural_hematoma_subacute_total_volume_p", "subdural_hematoma_subacute_count_p",            
           "subdural_hematoma_mixed_density_total_volume_p", "subdural_hematoma_mixed_density_count_p", 
           "Contusion_volume", "Contusion", "ICH_volume", "ICH", "intraparenchymal_hemorrhage_count_p",  
           "extraaxial_hematoma_total_volume_p",  "extraaxial_hematoma_count_p",
           "tsah_count_p", "intraventricular_hemorrhage_ivh_count_p",
           "cisternal_compression_count_p", "ventricular_compression_count_p", "mls_count_p", "brain_herniation_count_p",
           "penetrating_injury_count_p", "dai_count_p", "tai_count_p",
           "edema_hyperemia_ischemia_count", 
           "lesions_classification_fisher_classication", "lesions_classification_marshall_ct_classification",    
           "lesions_classification_morris_marshall_classification", 
           "TAMVI", "subdural_hematoma_volume", "subdural_hematoma", "total_lesion_volume", "total_lesion_volume_25", 
           "mass_effect", "AnyAbnormality", "AnyIntracranAbnormality","NoAbnormality")]

for (i in c("NoAbnormality","AnyAbnormality", "AnyIntracranAbnormality",                             
            "skull_fracture_count_p", "epidural_hematoma_count_p",                                 
            "subdural_hematoma", "subdural_hematoma_acute_count_p",                      
            "subdural_hematoma_subacute_count_p",                   
            "subdural_hematoma_mixed_density_count_p", "tsah_count_p",                                               
            "Contusion", "ICH", "intraparenchymal_hemorrhage_count_p", "TAMVI", "intraventricular_hemorrhage_ivh_count_p",                                               
            "mass_effect", "cisternal_compression_count_p",                              
            "ventricular_compression_count_p", "mls_count_p"        ,                                          
            "brain_herniation_count_p", "total_lesion_volume_25", "extraaxial_hematoma_count_p", 
            "penetrating_injury_count_p",  "dai_count_p", "tai_count_p", "edema_hyperemia_ischemia_count")) {
  print(i)
  print(table(I[, i]))
  I[, i] <- as.factor(ifelse(I[, i] == 0, "No", "Yes"))
  print(table(I[, i]))
} 

# Fisher and Morris-Marshall classifications used in imputation (grade TSAH)
for (i in c("lesions_classification_fisher_classication", "lesions_classification_marshall_ct_classification",    
            "lesions_classification_morris_marshall_classification")) {
  print(i)
  print(table(I[, i]))
  I[, i] <- as.factor(I[, i])
  print(table(I[, i]))
} 

# CT Classification	
table(I$lesions_classification_marshall_ct_classification)
# combine 5-6 (only 2 observations with score 5)
levels(I$lesions_classification_marshall_ct_classification) <- c("1", "2", "3", "4", "5/6", "5/6")

### MODIFIER PILLAR ---------------------------------------------------------------
# https://center-tbi.incf.org/_6638dd64afc657023eaf8ddf
M <- read.csv("IMPORT/Modifiers.6.05.2024.csv")
# Add ASAPS score, Education, Employment variables
# https://center-tbi.incf.org/_66455315340a069536410ba6
M_extra <- read.csv("IMPORT/ExtraModifiers.16.05.2024.csv")
M <- merge(M, M_extra, by = "subjectId")
rm(M_extra)
summary(M)

## RECODE CATEGORICAL VARIABLES
M$Subject.Sex <- as.factor(M$Subject.Sex)
levels(M$Subject.Sex) <- c("Female", "Male")

table(M$MedHx.MedHxPsychiatric)
M$MedHx.MedHxPsychiatric <- as.factor(M$MedHx.MedHxPsychiatric)
levels(M$MedHx.MedHxPsychiatric) <- c("absent", "present", NA) 
table(M$MedHx.MedHxPsychiatric)

table(M$MedHx.MedHxDevelopmental)
M$MedHx.MedHxDevelopmental <- as.factor(M$MedHx.MedHxDevelopmental)
levels(M$MedHx.MedHxDevelopmental) <- c("absent", "present", NA) 
table(M$MedHx.MedHxDevelopmental)

table(M$MedHx.MedHxPreTBIConcussions)
M$MedHx.MedHxPreTBIConcussions <- as.factor(M$MedHx.MedHxPreTBIConcussions)
levels(M$MedHx.MedHxPreTBIConcussions) <- c("absent", "present", NA) 
table(M$MedHx.MedHxPreTBIConcussions)

table(M$MedHx.MedHxPreInjASAPSClass)
M$MedHx.MedHxPreInjASAPSClass <- as.factor(M$MedHx.MedHxPreInjASAPSClass)
levels(M$MedHx.MedHxPreInjASAPSClass) <- c("Normal healthy patient", "Mild systemic disease",
                                           "Severe systemic disease", "Severe systemic disease that is a constant threat to life", NA) 
table(M$MedHx.MedHxPreInjASAPSClass)

## CREATE NEW COMPOSITE VARIABLES

# Major extracranial injury 
naniar::gg_miss_var(M[, c("InjuryHx.CervicalSpineAIS", "InjuryHx.FaceAIS", "InjuryHx.ThoraxChestAIS", 
                          "InjuryHx.ThoracicSpineAIS", "InjuryHx.AbdomenPelvicContentsAIS", "InjuryHx.LumbarSpineAIS", 
                          "InjuryHx.UpperExtremitiesAIS", "InjuryHx.LowerExtremitiesAIS", "InjuryHx.PelvicGirdleAIS",
                          "InjuryHx.ExternaAIS")], show_pct = TRUE) 
M$major_extracran_injury <- apply(M[, c("InjuryHx.CervicalSpineAIS", "InjuryHx.FaceAIS", "InjuryHx.ThoraxChestAIS", 
                                        "InjuryHx.ThoracicSpineAIS", "InjuryHx.AbdomenPelvicContentsAIS", "InjuryHx.LumbarSpineAIS", 
                                        "InjuryHx.UpperExtremitiesAIS", "InjuryHx.LowerExtremitiesAIS", "InjuryHx.PelvicGirdleAIS",
                                        "InjuryHx.ExternaAIS")], 1, function(x) length(which(x>=3)))
# InjuryHx.BrainInjuryAIS and InjuryHx.HeadNeckAIS not included in major_extracran_injury (10.1097/TA.0b013e31827d602e)

# if all NA -> set as missing
for (i in 1:length(M$subjectId)) {
  M[i,"allNA"] <- all(is.na(M[i, c("InjuryHx.CervicalSpineAIS", "InjuryHx.FaceAIS", "InjuryHx.ThoraxChestAIS", 
                                   "InjuryHx.ThoracicSpineAIS", "InjuryHx.AbdomenPelvicContentsAIS", "InjuryHx.LumbarSpineAIS", 
                                   "InjuryHx.UpperExtremitiesAIS", "InjuryHx.LowerExtremitiesAIS", "InjuryHx.PelvicGirdleAIS",
                                   "InjuryHx.ExternaAIS")]))
}
table(M$allNA)
M$allNA <- NULL

table(M$major_extracran_injury)
M$major_extracran_injury <- as.factor(ifelse(M$major_extracran_injury %in% c(0), "No", "Yes"))
table(M$major_extracran_injury)

# Non-accidental cause
table(M$InjuryHx.InjCause)
M$InjuryHx.InjCause <- as.factor(M$InjuryHx.InjCause)
levels(M$InjuryHx.InjCause) <- c("Road traffic incident", "Incidental fall", "Other non-intentional injury",
                                 "Violence/assault", "Act of mass violence", "Suicide attempt", NA, "Other") 
table(M$InjuryHx.InjCause)

M$InjuryHx.NonAccInjCause <- as.factor(M$InjuryHx.InjCause)
levels(M$InjuryHx.NonAccInjCause) <- c("Accidental", "Accidental", "Accidental",
                                       "Intentional", "Intentional", "Intentional", "Accidental") 
table(M$InjuryHx.NonAccInjCause) 
table(M$InjuryHx.InjCause, M$InjuryHx.NonAccInjCause) 

# High vs. Low velocity mechanism 
table(M$InjuryHx.InjMech) 
M[M$InjuryHx.InjMech %in% c(""),]$InjuryHx.InjMech <- NA
M$InjuryHx.InjMech <- as.factor(M$InjuryHx.InjMech)
levels(M$InjuryHx.InjMech) 

M$Mechanism <- ifelse(M$InjuryHx.InjMech %in% c(",1", "1"), "1", 
                      ifelse(M$InjuryHx.InjMech %in% c(",2", "2"), "2", 
                             ifelse(M$InjuryHx.InjMech %in% c(",3", "3"), "3",
                                    ifelse(M$InjuryHx.InjMech %in% c(",6", "6"), "6", 
                                           ifelse(M$InjuryHx.InjMech %in% c(",7", "7"), "7", 
                                                  ifelse(is.na(M$InjuryHx.InjMech), NA, 
                                                         ifelse(M$InjuryHx.InjMech %in% c(",99", "99"), "99", 
                                                                "Combinations")))))))
table(M$Mechanism)
table(M$InjuryHx.InjMech, M$Mechanism)
M$Mechanism <- as.factor(M$Mechanism)
levels(M$Mechanism) <-  c("High velocity trauma (acceleration/deceleration)", 
                          "Direct impact: blow to head", 
                          "Direct impact: head against object", 
                          "Ground level fall", 
                          "Fall from height > 1 meter/5 stairs", 
                          "Other closed head injury", 
                          "Combined mechanism of injury")   
table(M$Mechanism)
#1 == High velocity trauma (acceleration/deceleration)
#2 == Direct impact: blow to head
#3 == Direct impact: head against object
#6 == Ground level fall
#7 == Fall from height > 1 meter/5 stairs
#99 == Other closed head injury

# High = "High velocity trauma (acceleration/deceleration)" OR 
# "Fall from height > 1 meter/5 stairs" OR 
# combinations including these 
M$MechanismHighvLowVelocity <- ifelse(str_detect(M$InjuryHx.InjMech, "1") | str_detect(M$InjuryHx.InjMech, "7"), "High", 
                                      ifelse(is.na(M$InjuryHx.InjMech), NA, "Low"))
table(M$MechanismHighvLowVelocity)
M$MechanismHighvLowVelocity <- as.factor(M$MechanismHighvLowVelocity)
table(M$MechanismHighvLowVelocity)

table(M$InjuryHx.InjMech, M$MechanismHighvLowVelocity)
table(M$Mechanism, M$MechanismHighvLowVelocity)

# Medical history
# recode "88" as missing
M[, c("MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT",              
      "MedHx.MedHxGastro", "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", 
      "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro", "MedHx.MedHxNeuroPain", 
      "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal")][M[, c("MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT",              
                                                                                 "MedHx.MedHxGastro", "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", 
                                                                                 "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro", "MedHx.MedHxNeuroPain", 
                                                                                 "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal")] == 88] <- NA
naniar::gg_miss_var(M[, c("MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT",              
                          "MedHx.MedHxGastro", "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", 
                          "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro", "MedHx.MedHxNeuroPain", 
                          "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal")], show_pct = TRUE) 

M$med_hx <- apply(M[, c("MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT",              
                        "MedHx.MedHxGastro", "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", 
                        "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro", "MedHx.MedHxNeuroPain", 
                        "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal")], 1, function(x) length(which(x %in% c(1))))
table(M$med_hx)

# if all 88 or NA -> set as missing
for (i in 1:length(M$subjectId)) {
  M[i,"anyNA"] <- any(is.na(M[i, c("MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT",              
                                   "MedHx.MedHxGastro", "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", 
                                   "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro", "MedHx.MedHxNeuroPain", 
                                   "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal")]))
}
table(M$anyNA)
table(M$anyNA, M$med_hx)
M[M$anyNA %in% c(TRUE) & M$med_hx %in% c(0), ]$med_hx <- NA
M$allNA <- NULL
table(M$med_hx)

M$med_hx <- as.factor(ifelse(M$med_hx %in% c(0), "No", ifelse(is.na(M$med_hx), NA, "Yes")))
table(M$med_hx, M$MedHx.MedHxOther) # 186 patients have some isolated "Other" comorbidity
write.csv(table(M[M$med_hx %in% c("No") & M$MedHx.MedHxOtherTxt != "",]$MedHx.MedHxOtherTxt), "EXPORT/MedicalHxOther.csv")

# some have diabetes recorded in the free text variable, but the Endocrine variable is 0 -> MedHx should be "Yes"
M[str_detect(M$MedHx.MedHxOtherTxt, "diab") | str_detect(M$MedHx.MedHxOtherTxt, "Diab") , ]$MedHx.MedHxOtherTxt
M[str_detect(M$MedHx.MedHxOtherTxt, "diab") | str_detect(M$MedHx.MedHxOtherTxt, "Diab") , ]$med_hx <- "Yes"

table(M$med_hx)
table(M$MedHx.MedHxPreInjASAPSClass, M$med_hx)

# Employment Status
table(M$Subject.EmplmtStatus)
M$Subject.EmplmtStatus <- as.factor(M$Subject.EmplmtStatus)
levels(M$Subject.EmplmtStatus) <- c("Working (35 hours or more per week)",
                                    "Working (20-34 hours per week)",
                                    "Working (less than 20 hours per week)",
                                    "In working force, but currently on sick leave",
                                    "Special employment/sheltered employment",
                                    "Looking for work, unemployed",
                                    "Unable to work",
                                    "Retired",
                                    "Student/schoolgoing",
                                    "Homemaker, keeping house") 
table(M$Subject.EmplmtStatus)

# Recode into 4 categories
M$Subject.EmplmtStatus4cat <- as.factor(M$Subject.EmplmtStatus)
levels(M$Subject.EmplmtStatus4cat) <- c("Working",
                                        "Working",
                                        "Working",
                                        "Working",
                                        "Working",
                                        "Not working",
                                        "Not working",
                                        "Retired",
                                        "Student",
                                        "Not working") 
table(M$Subject.EmplmtStatus4cat)
table(M$Subject.EmplmtStatus, M$Subject.EmplmtStatus4cat)

table(is.na(M$Subject.EmplmtStatus))

# Job category (only if working)
table(M$Subject.JobclassCat)
M$Subject.JobclassCat <- as.factor(M$Subject.JobclassCat)
levels(M$Subject.JobclassCat) <- c("None",
                                   "Manager/Professional",
                                   "Technician/Supervisor/Associate Professional",
                                   "Clerk/Sales",
                                   "Skilled manual worker",
                                   "Manual worker",
                                   "Other") 

# Combined employments status/job category variables
table(M$Subject.EmplmtStatus4cat)
table(M$Subject.JobclassCat)
table(is.na(M$Subject.JobclassCat))
table(M$Subject.EmplmtStatus4cat, M$Subject.JobclassCat)
table(is.na(M$Subject.EmplmtStatus4cat), is.na(M$Subject.JobclassCat))

M$JobEmployment <- as.character(M$Subject.JobclassCat)
table(M$JobEmployment)
M[M$Subject.EmplmtStatus4cat %in% c("Not working"), ]$JobEmployment <- "Not working"
table(M$JobEmployment)
M[M$Subject.EmplmtStatus4cat %in% c("Retired"), ]$JobEmployment <- "Retired"
table(M$JobEmployment)
M[M$Subject.EmplmtStatus4cat %in% c("Student"), ]$JobEmployment <- "Student"
table(M$JobEmployment)
table(is.na(M$JobEmployment))

M[M$JobEmployment %in% c("None") & M$Subject.EmplmtStatus4cat %in% c("Working"), ]$JobEmployment <- "Other"
table(M$JobEmployment)
M[is.na(M$JobEmployment) & M$Subject.EmplmtStatus4cat %in% c("Working"), ]$JobEmployment <- "Other"
table(M$JobEmployment)
table(is.na(M$Subject.EmplmtStatus4cat), is.na(M$JobEmployment))
M$JobEmployment <- as.factor(M$JobEmployment)
levels(M$JobEmployment)
M$JobEmployment <- factor(M$JobEmployment, levels = c("Student", "Not working", "Other", "Manual worker", "Skilled manual worker", 
                                                      "Technician/Supervisor/Associate Professional", 
                                                      "Clerk/Sales", "Manager/Professional", "Retired"))

# Compare recoding with previous publications 
M <- merge(M, C[, c("subjectId", "TBISeverity")], by = "subjectId")
table(M[M$TBISeverity %in% c("Mild (Baseline GCS>=13)") & M$Subject.Age >= 16,]$JobEmployment, 
      M[M$TBISeverity %in% c("Mild (Baseline GCS>=13)") & M$Subject.Age >= 16,]$Subject.Sex  )
M$TBISeverity <- NULL

# Level of education - dichotomize: low = less than high-school diploma
table(M$Subject.EduLvlUSATyp) 
M$Subject.EduLvlUSATyp <- as.factor(M$Subject.EduLvlUSATyp)
levels(M$Subject.EduLvlUSATyp) <- c("None, not currently in school",
                                    "Currently in diploma or degree-oriented program",
                                    "Primary school",
                                    "Secondary school/High school",
                                    "Post-high school training (e.g. trade/technical certificate)",
                                    "College/University (diploma or degree)") 
table(M$Subject.EduLvlUSATyp)
table(is.na(M$Subject.EduLvlUSATyp))

M$Subject.EduLvlUSATyp2cat <- as.factor(M$Subject.EduLvlUSATyp)
levels(M$Subject.EduLvlUSATyp2cat) <- c("low",   
                                        "hs or higher",
                                        "low",
                                        "hs or higher",
                                        "hs or higher",
                                        "hs or higher") 
table(M$Subject.EduLvlUSATyp2cat)
table(is.na(M$Subject.EduLvlUSATyp2cat))
table(M$Subject.EduLvlUSATyp, M$Subject.EduLvlUSATyp2cat)

## KEEP RELEVANT VARIABLES FOR IMPUTATION 
M <- M[, c("subjectId", "MechanismHighvLowVelocity", "major_extracran_injury",
           "InjuryHx.NonAccInjCause", "Subject.Age", "Subject.Sex", 
           "med_hx", "MedHx.MedHxPreInjASAPSClass", "MedHx.MedHxPsychiatric", "MedHx.MedHxDevelopmental", 
           "MedHx.MedHxPreTBIConcussions", "JobEmployment", "Subject.EduLvlUSATyp2cat", "Subject.EduYrCt",
           "InjuryHx.InjCause", 
           "InjuryHx.CervicalSpineAIS", "InjuryHx.FaceAIS", "InjuryHx.ThoraxChestAIS", 
           "InjuryHx.ThoracicSpineAIS", "InjuryHx.AbdomenPelvicContentsAIS", "InjuryHx.LumbarSpineAIS", 
           "InjuryHx.UpperExtremitiesAIS", "InjuryHx.LowerExtremitiesAIS", "InjuryHx.PelvicGirdleAIS",
           "InjuryHx.ExternaAIS",
           "MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT",              
           "MedHx.MedHxGastro", "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", 
           "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro", "MedHx.MedHxNeuroPain", 
           "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal", "MedHx.MedHxOther",
           "Subject.EduLvlUSATyp")]

for (i in c("MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT",              
            "MedHx.MedHxGastro", "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", 
            "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro", "MedHx.MedHxNeuroPain", 
            "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal", "MedHx.MedHxOther")) {
  print(table(M[, i]))
  M[, i] <- as.factor(M[, i])
  print(table(M[, i]))
} 

### ENDPOINTS ----------------------------------------------------------------
# https://center-tbi.incf.org/_6600d0afab2e6d9f49883465
O <- read.csv("IMPORT/Endpoints.25.03.2024.csv")

## RECODE "" AS MISSING
O[, 3:length(O)][O[, 3:length(O)] == ""] <- NA

## RECODE CATEGORICAL VARIABLES
table(O$Subject.GOSE3monthEndpointDerived)
O$Subject.GOSE3monthEndpointDerived <- as.factor(O$Subject.GOSE3monthEndpointDerived)
levels(O$Subject.GOSE3monthEndpointDerived) <- c("1=Dead", "2=VS/3=LSD", "4=USD", "5=LMD", "6=UMD", "7=LGR", "8=UGR")
table(O$Subject.GOSE3monthEndpointDerived)

table(O$Subject.GOSE6monthEndpointDerived)
O$Subject.GOSE6monthEndpointDerived <- as.factor(O$Subject.GOSE6monthEndpointDerived)
levels(O$Subject.GOSE6monthEndpointDerived) <- c("1=Dead", "2=VS/3=LSD", "4=USD", "5=LMD", "6=UMD", "7=LGR", "8=UGR")
table(O$Subject.GOSE6monthEndpointDerived)

table(O$Subject.PatientType)
O$Subject.PatientType <- as.factor(O$Subject.PatientType)
levels(O$Subject.PatientType) <- c("ER", "Admission", "ICU")
table(O$Subject.PatientType)

## CREATE NEW VARIABLES

# unfavourable outcome (dichotomized GOSE < 5 vs. >= 5)
O$unfavourable_6 <- as.factor(ifelse(O$Subject.GOSE6monthEndpointDerived %in% c("1=Dead", "2=VS/3=LSD", "4=USD"), "Yes", ifelse(O$Subject.GOSE6monthEndpointDerived %in% c("5=LMD", "6=UMD", "7=LGR", "8=UGR"), "No", NA)))

# mortality (dichotomized GOSE = 1 vs. >= 1)
O$mortality_6 <- as.factor(ifelse(O$Subject.GOSE6monthEndpointDerived %in% c("1=Dead"), "Yes", ifelse(O$Subject.GOSE6monthEndpointDerived %in% c("2=VS/3=LSD", "4=USD", "5=LMD", "6=UMD", "7=LGR", "8=UGR"), "No", NA)))

# incomplete recovery (dichotomized GOSE < 8 vs. = 8)
O$incomplete_6 <- as.factor(ifelse(O$Subject.GOSE6monthEndpointDerived %in% c("1=Dead", "2=VS/3=LSD", "4=USD", "5=LMD", "6=UMD", "7=LGR"), "Yes", ifelse(O$Subject.GOSE6monthEndpointDerived %in% c("8=UGR"), "No", NA)))

# Hospital admission
O$Hosp <- O$Subject.PatientType
table(O$Hosp)
levels(O$Hosp) <- c("No", "Yes", "Yes")
table(O$Hosp)

# ICU admission
O$ICU <- O$Subject.PatientType
table(O$ICU)
levels(O$ICU) <- c("No", "No", "Yes")
table(O$ICU)

## ADD PRESENCE OF INTRACRANIAL ABNORMALITIES ON CT EARLY AS DIAGNOSTIC ENDPOINT
O <-  merge(O, I[, c("subjectId", "AnyIntracranAbnormality")], by = "subjectId", all.x = T)
I$AnyIntracranAbnormality <- NULL

## ADD ICP MONITORING VARIABLE
# https://center-tbi.incf.org/_6620f3f5f43dc41f11f91a58
ICP <- read.csv("IMPORT/ICPMonitor.18.04.2024.csv")

# Check scheduled at ER vs. actual monitor placement
table(ICP$InjuryHx.EDICPMonitoring, ICP$Hospital.ICPMonitorYes)

table(ICP$Hospital.ICUReasonNoICP)
ICP$Hospital.ICUReasonNoICP <- as.factor(ICP$Hospital.ICUReasonNoICP)
levels(ICP$Hospital.ICUReasonNoICP) <- c("GCS >8",
                                         "No radiological signs of raised ICP",
                                         "Risk of raised ICP considered low",
                                         "Patient considered unsalvageable",
                                         "Coagulopathy",
                                         "Use of anticoagulants or platelet aggregation inhibitors",
                                         "No device available",
                                         "Not local policy to monitor ICP",
                                         "Other")
table(ICP$Hospital.ICUReasonNoICP)

table(ICP$Hospital.ICPMonitorYes)
ICP$Hospital.ICPMonitorYes <- as.factor(ICP$Hospital.ICPMonitorYes)
levels(ICP$Hospital.ICPMonitorYes) <- c("No", "Yes")
table(ICP$Hospital.ICPMonitorYes)
table(ICP[ICP$InjuryHx.EDICPMonitoring %in% c(1) & ICP$Hospital.ICPMonitorYes %in% c("No", NA), ]$Hospital.ICUReasonNoICP)
table(ICP$Hospital.ICPMonitorYes)

O <- merge(O, ICP[, c("subjectId", "Hospital.ICPMonitorYes")], by = "subjectId")
table(O$Hospital.ICPMonitorYes)
rm(ICP)

## PROCESS and CURATE CRANIAL SURGERY DATA 
# https://center-tbi.incf.org/_661d05f10ee7f51b0004b828
cran <- read.csv("IMPORT/CranialSurgery.15.04.2024.csv")
cran[, 2:length(cran)][cran[, 2:length(cran)] == ""] <- NA

# Remove observation with all cranial surgery variables NA
for (i in 1:length(cran$subjectId)) {
  cran[i,"allNA"] <- all(is.na(cran[i, c("Surgeries.CranialSurgDone", "SurgeriesCranial.SurgeryStartDate", 
                                         "SurgeriesCranial.SurgeryStartTime", "SurgeriesCranial.SurgeryEndDate", 
                                         "SurgeriesCranial.SurgeryEndTime", "SurgeriesCranial.SurgeryDescCranial", 
                                         "SurgeriesCranial.SurgeryCranialReason", "Subject.SurgeriesNotes")]))
}
table(cran$allNA)
cran <- cran[cran$allNA == F, ]

# Remove observation with all cranial surgery variables NA and Surgeries.CranialSurgDone == 0
for (i in 1:length(cran$subjectId)) {
  cran[i,"allNA"] <- all(is.na(cran[i, c("SurgeriesCranial.SurgeryStartDate", 
                                         "SurgeriesCranial.SurgeryStartTime", "SurgeriesCranial.SurgeryEndDate", 
                                         "SurgeriesCranial.SurgeryEndTime", "SurgeriesCranial.SurgeryDescCranial", 
                                         "SurgeriesCranial.SurgeryCranialReason", "Subject.SurgeriesNotes")]))
}
table(cran$allNA, cran$Surgeries.CranialSurgDone)
cran <- cran[!(cran$allNA == T & cran$Surgeries.CranialSurgDone %in% c(0)), ]
# Unclear why some patients with recorded details of cranial surgery interventions have Surgeries.CranialSurgDone = 0/88 recorded
# Unclear why some patients with no recorded details of cranial surgery interventions have Surgeries.CranialSurgDone = 1 recorded -> will be treated as NA

# Some patients have no cranial surgery data recorded, only a surgical note
for (i in 1:length(cran$subjectId)) {
  cran[i,"allNA"] <- all(is.na(cran[i, c("SurgeriesCranial.SurgeryStartDate", 
                                         "SurgeriesCranial.SurgeryStartTime", "SurgeriesCranial.SurgeryEndDate", 
                                         "SurgeriesCranial.SurgeryEndTime", "SurgeriesCranial.SurgeryDescCranial", 
                                         "SurgeriesCranial.SurgeryCranialReason")]))
}
table(cran$allNA, cran$Surgeries.CranialSurgDone)
cran[cran$allNA == T & cran$Surgeries.CranialSurgDone %in% c(0), ]$Subject.SurgeriesNotes
write.xlsx(cran[cran$allNA == T & cran$Surgeries.CranialSurgDone %in% c(0), ], "EXPORT/CranialSurgeryQuestions.xlsx", sheetName = "Notes")
# Surgical notes were checked before removing observations with no cranial surgery data (almost all were notes for extracranial surgery)
# One patient with no recorded surgery data had a surgery note describing cranial surgery: 4hEk995 
cran <- cran[!(cran$allNA == T & cran$Surgeries.CranialSurgDone %in% c(0) & cran$subjectId != "4hEk995"), ]

# Recode Surgical Description 
table(cran$SurgeriesCranial.SurgeryDescCranial)
cran$SurgeriesCranial.SurgeryDescCranial <- as.factor(cran$SurgeriesCranial.SurgeryDescCranial)
levels(cran$SurgeriesCranial.SurgeryDescCranial) <- c("Aneurysm (non trauma)", "Acute subdural hematoma", "Contusion", 
                                                      "Craniofacial surgery", "CSF shunt", "Chronic subdural hematoma", 
                                                      "Decompressive craniectomy-hemicraniectomy", "Depressed skull fracture", "Epidural hematoma",
                                                      "Intracerebral hematoma", "Infection", "Optic nerve decompression", 
                                                      "Posterior fossa surgery", "Skull base fracture", "Ventriculostomy for CSF drainage",
                                                      "Debridement - minimal for penetrating injuries", "Debridement - extensive for penetrating injuries", "Foreign body removal", 
                                                      "Bone flap replacement", "Cranioplasty", "Other", 
                                                      "Decompressive craniectomy - bifrontal", "Decompressive craniectomy - removal previous bone flap")
table(cran$SurgeriesCranial.SurgeryDescCranial)
write.xlsx(as.data.frame(table(cran$SurgeriesCranial.SurgeryDescCranial)), "EXPORT/CranialSurgeryQuestions.xlsx", sheetName = "Indications", append = T)

# Define major cranial surgery 
cran$major_surgery <- ifelse(cran$SurgeriesCranial.SurgeryDescCranial %in% c(
  "Acute subdural hematoma", "Contusion", "Decompressive craniectomy-hemicraniectomy",
  "Epidural hematoma", "Intracerebral hematoma", "Decompressive craniectomy - bifrontal"), 1, 0)
table(cran$SurgeriesCranial.SurgeryDescCranial, cran$major_surgery)

# Surgical notes of patients with "Other" as indication were checked - some patients have multiple entries for the same intervention, and a different entry is major surgery
unique(cran[cran$SurgeriesCranial.SurgeryDescCranial %in% c("Other") & !is.na(cran$Subject.SurgeriesNotes),]$Subject.SurgeriesNotes)
cran[cran$Subject.SurgeriesNotes %in% c("Cranial surgeries - other is Evacuation of extra-dural haematoma",
                                        "Craniotomy for extradural haematoma"),]$major_surgery <- 1

# the patient with missing data had ASDH removal recorded in the surgical note
cran[cran$subjectId %in% c("4hEk995"),]$major_surgery <- 1

table(cran$major_surgery)
length(unique(cran[cran$major_surgery %in% c(1),]$subjectId))

# Calculate time from injury to cranial surgery
cran$surgery_dt <- as.POSIXct(paste(cran$SurgeriesCranial.SurgeryStartDate, cran$SurgeriesCranial.SurgeryStartTime), format="%Y-%m-%d %H:%M:%S")
# Select surgeries within first 72h from injury
cran <- merge(cran, DT[, c("subjectId", "injury_dt")], by = "subjectId")
cran$surgery_time <- as.numeric(difftime(cran$surgery_dt, cran$injury_dt, units = c("hours"))) 
summary(cran$surgery_time) 

# Some patients have missing surgery start time, but recorded start day
table(cran[is.na(cran$surgery_time), ]$SurgeriesCranial.SurgeryStartDate)
# Consider patients operated on 1970-01-01, 1970-01-02, 1970-01-03 as operated within 72h from injury
cran_72 <- cran[(!is.na(cran$surgery_time) & cran$surgery_time <= 72) | 
                  (is.na(cran$surgery_time) & cran$SurgeriesCranial.SurgeryStartDate %in% c("1970-01-01", "1970-01-02", "1970-01-03")), ]
length(unique(cran_72$subjectId))
table(cran_72$SurgeriesCranial.SurgeryDescCranial)
# Some interventions for Bone flap replacement, Cranioplasty, CSF shunt are recorded within 72h

## ANY CRANIAL SURGERY
# if Surgeries.CranialSurgDone == 1, but no cranial surgery data and no notes, recode as missing cranial surgery
O$any_cran_surg <- as.factor(ifelse(O$subjectId %in% cran[cran$Surgeries.CranialSurgDone %in% c(1) & cran$allNA %in% c(T), ]$subjectId, NA, 
                                    ifelse(O$subjectId %in% cran$subjectId, "Yes", "No")))
table(O$any_cran_surg)

## MAJOR CRANIAL SURGERY
O$major_cran_surg <- as.factor(ifelse(O$subjectId %in% cran[cran$Surgeries.CranialSurgDone %in% c(1) & cran$allNA %in% c(T), ]$subjectId, NA, 
                                      ifelse(O$subjectId %in% cran[cran$major_surgery %in% c(1), ]$subjectId, "Yes", "No")))
table(O$major_cran_surg)

## ANY CRANIAL SURGERY within first 72h from injury
O$any_cran_surg72 <- as.factor(ifelse(O$subjectId %in% cran[cran$Surgeries.CranialSurgDone %in% c(1) & cran$allNA %in% c(T), ]$subjectId | 
                                        O$subjectId %in% c("4hEk995"), NA, 
                                      ifelse(O$subjectId %in% cran_72$subjectId, "Yes", "No")))
table(O$any_cran_surg72)
table(O$any_cran_surg72, O$any_cran_surg)

## MAJOR CRANIAL SURGERY within first 72h from injury
O$major_cran_surg72 <- as.factor(ifelse(O$subjectId %in% cran[cran$Surgeries.CranialSurgDone %in% c(1) & cran$allNA %in% c(T), ]$subjectId | 
                                          O$subjectId %in% c("4hEk995"), NA, 
                                        ifelse(O$subjectId %in% cran_72[cran_72$major_surgery %in% c(1), ]$subjectId, "Yes", "No")))
table(O$major_cran_surg72)
table(O$major_cran_surg72, O$major_cran_surg)

# Check if any recorded decompressive craniectomies (different CRF) were missed

# https://center-tbi.incf.org/_663fbb75340a069536410ba3
DC <- read.csv("IMPORT/DCs.11.05.2024.csv")

table(O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId, ]$any_cran_surg)
table(O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId, ]$major_cran_surg)

# 3 patients with recorded Surgeries.DecompressiveCran == 1 have no cranial surgery data recorded
# 13 patients with recorded Surgeries.DecompressiveCran == 1 have cranial surgery data recorded, but the indication is not DC
# In some of these, even if Surgeries.DecompressiveCran == 1, there are no DC details (size, location, type)
# DC data does not contain timing of surgery, so unclear if interventions were performed within 72h of injury

# DECISION for the 3 patients with recorded Surgeries.DecompressiveCran == 1 and no cranial surgery data recorded
# When extensive DC details (size, location, type) are recorded:
# any_cran_surg <- 1 and major_cran_surg <- 1 
# any_cran_surg72 <- NA and major_cran_surg72 <- NA (unknown time)
# When no extensive DC details (size, location, type) are recorded, these might be entry errors, as no other cranial surgery data documents a DC:
# any_cran_surg <- NA and major_cran_surg <- NA 
# any_cran_surg72 <- NA and major_cran_surg72 <- NA 

O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId & O$any_cran_surg %in% c("No"), ]$subjectId
DC[DC$subjectId %in% O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId & O$any_cran_surg %in% c("No"), ]$subjectId,]

O[O$subjectId %in% c("5cWJ927"), ]$any_cran_surg <- "Yes"
O[O$subjectId %in% c("5cWJ927"), ]$major_cran_surg <- "Yes"
O[O$subjectId %in% c("5cWJ927"), ]$any_cran_surg72 <- NA
O[O$subjectId %in% c("5cWJ927"), ]$major_cran_surg72 <- NA

O[O$subjectId %in% c("2jfV575", "5MXJ828"), ]$any_cran_surg <- NA
O[O$subjectId %in% c("2jfV575", "5MXJ828"), ]$major_cran_surg <- NA
O[O$subjectId %in% c("2jfV575", "5MXJ828"), ]$any_cran_surg72 <- NA
O[O$subjectId %in% c("2jfV575", "5MXJ828"), ]$major_cran_surg72 <- NA

# DECISION for the 13 patients with recorded Surgeries.DecompressiveCran == 1, cranial surgery data recorded, but the indication is not DC
# When extensive DC details (size, location, type) are recorded, coroborate cranial surgery data with DC data to infer if same intervention
# adjust major_cran_surg and major_cran_surg72 accordingly
# If unclear, record as NA
# When no extensive DC details (size, location, type) are recorded, these might be entry errors, as no other cranial surgery data documents a DC:
# major_cran_surg <- NA 
# major_cran_surg72 <- NA 

O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId & O$major_cran_surg %in% c("No"), ]$subjectId
O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId & O$major_cran_surg %in% c("No"), ]$any_cran_surg72
O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId & O$major_cran_surg %in% c("No"), ]$major_cran_surg
O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId & O$major_cran_surg %in% c("No"), ]$major_cran_surg72

DC[DC$subjectId %in% O[O$subjectId %in% DC[DC$Surgeries.DecompressiveCran %in% c(1),]$subjectId & O$major_cran_surg %in% c("No"), ]$subjectId,]

O[O$subjectId %in% c("8pCt865", "4MbF392", "7tPj935", "8ptq735", "9Nxq794", "6TND297", "6aVB889", "9Ckj402"), ]$major_cran_surg <- "Yes"
O[O$subjectId %in% c("8pCt865", "4MbF392", "8ptq735", "9Nxq794", "6TND297", "6aVB889", "9Ckj402"), ]$major_cran_surg72 <- "Yes"

O[O$subjectId %in% c("3zJm265"), ]$any_cran_surg72 <- NA
O[O$subjectId %in% c("3zJm265", "5HqC593", "8pAJ686", "6aVa533", "7EXZ358"), ]$major_cran_surg <- NA
O[O$subjectId %in% c("3zJm265", "5HqC593", "8pAJ686", "6aVa533", "7EXZ358"), ]$major_cran_surg72 <- NA

## STORE DATE AND TIME OF FIRST CRANIAL SURGERY AND FIRST MAJOR CRANIAL SURGERY
# impute missing surgery start times with the latest possible value
cran[is.na(cran$SurgeriesCranial.SurgeryStartTime),]$SurgeriesCranial.SurgeryStartTime <- "23:59:59" 
cran$surgery_dt <- as.POSIXct(paste(cran$SurgeriesCranial.SurgeryStartDate, cran$SurgeriesCranial.SurgeryStartTime), format="%Y-%m-%d %H:%M:%S") 
cran$surgery_time <- as.numeric(difftime(cran$surgery_dt, cran$injury_dt, units = c("hours"))) 

# select first major surgery
cran_major <- cran[cran$subjectId %in% O[O$major_cran_surg %in% c("Yes"),]$subjectId, ]
cran_major[cran_major$subjectId %in% c("8pCt865", "4MbF392", "7tPj935", "8ptq735", "9Nxq794", "6TND297", "6aVB889", "9Ckj402"), c(1, 12, 7, 11, 9)]

cran_major[cran_major$subjectId %in% c("8pCt865", "8ptq735"), ]$major_surgery <- 1
cran_major[cran_major$subjectId %in% c("7tPj935") & cran_major$surgery_dt == "1970-01-08 11:14:00", ]$major_surgery <- 1
cran_major[cran_major$subjectId %in% c("4MbF392", "9Nxq794", "6TND297", "6aVB889", "9Ckj402") & 
             cran_major$SurgeriesCranial.SurgeryDescCranial %in% c("Decompressive craniectomy - removal previous bone flap", "Ventriculostomy for CSF drainage"), ]$major_surgery <- 1

cran_major <- cran_major[cran_major$major_surgery %in% c(1), ]
cran_major <- data.table(cran_major)
cran_major_first <- cran_major[, .SD[which.min(surgery_time)], by = subjectId]
cran_major_first$major_surgery_time <- cran_major_first$surgery_time
O <- merge(O, cran_major_first[, c("subjectId", "major_surgery_time")], by = "subjectId", all.x =T)

# select first surgery
cran <- data.table(cran)
cran_first <- cran[, .SD[which.min(surgery_time)], by = subjectId]
O <- merge(O, cran_first[, c("subjectId", "surgery_time")], by = "subjectId", all.x =T)

rm(cran, cran_72, cran_first, cran_major, cran_major_first, DC)

## ADD QOLIBRI OS
# https://center-tbi.incf.org/_669519084f12bd0241e5744d
QolibriOS <- read.csv("IMPORT/QolibriOS.15.07.2024.csv")
QolibriOS <- QolibriOS[!is.na(QolibriOS$Outcomes.QoLIBRIOSTotalScore),]
table(QolibriOS$Outcomes.Timepoint)
QolibriOS <- QolibriOS[QolibriOS$Outcomes.Timepoint == "6mo",]
length(unique(QolibriOS$subjectId))

O <- merge(O, QolibriOS[, c("subjectId", "Outcomes.QoLIBRIOSTotalScore")], by = "subjectId", all.x =T)
O$HRQOL_Impairment <- as.factor(ifelse(O$Outcomes.QoLIBRIOSTotalScore < 52, "Yes", "No"))
table(O$HRQOL_Impairment)
table(O$HRQOL_Impairment, O$Outcomes.QoLIBRIOSTotalScore)
rm(QolibriOS)

## Store surgery times
Cranial_Surgery_Times <- O[, c("subjectId", 
                               "any_cran_surg", "any_cran_surg72", "surgery_time", 
                               "major_cran_surg", "major_cran_surg72", "major_surgery_time")]

## KEEP RELEVANT VARIABLES FOR IMPUTATION 
O <- O[, c("subjectId", "AnyIntracranAbnormality", 
           "Hosp", "ICU", "Hospital.ICPMonitorYes", 
           "any_cran_surg", "any_cran_surg72",  
           "major_cran_surg", "major_cran_surg72", 
           "mortality_6", "unfavourable_6", "incomplete_6", 
           "Subject.GOSE6monthEndpointDerived", 
           "Outcomes.QoLIBRIOSTotalScore", "HRQOL_Impairment",
           "Subject.GOSE3monthEndpointDerived")]

### ADDITIONAL PREDICTORS FOR IMPACT-TBI AND CRASH-TBI -----------------------

## IMPACT-TBI CORE+CT+LAB: GCS 3-12 -> outcomes unfavourable_6; mortality_6

# Hypoxia	
extra <- read.csv("IMPORT/Baseline.11.05.2024.csv")[, c("subjectId", "InjuryHx.EDComplEventHypoxia", "InjuryHx.EDComplEventHypotension")]
table(extra$InjuryHx.EDComplEventHypoxia)
extra$InjuryHx.EDComplEventHypoxia <- as.factor(extra$InjuryHx.EDComplEventHypoxia)
# Definite hypoxia is defined as a documented PaO2 <8 kPa (60 mm Hg) and/or SaO2<90%; 
# "Suspected" was scored if the patient did not have documented hypoxia by PaO2 or SaO2, 
# but there was a clinical suspicion , as evidenced by for example cyanosis, apnoea or respiratory distress (treat as Yes)
levels(extra$InjuryHx.EDComplEventHypoxia) <- c("No", "Yes", "Yes", NA)

# Hypotension
table(extra$InjuryHx.EDComplEventHypotension)
extra$InjuryHx.EDComplEventHypotension <- as.factor(extra$InjuryHx.EDComplEventHypotension)
# Definite hypotension is defined as a documented systolic BP < 90 mm Hg (adults); 
# "Suspected" was scored if the patient did not have a documented blood pressure, 
# but was reported to be in shock or have an absent brachial pulse (not related to injury of the extremity) (treat as Yes)
levels(extra$InjuryHx.EDComplEventHypotension) <- c("No", "Yes", "Yes", NA)

# Epidural mass on CT
# ==individual EDH >= 25, not total EDH volume (when multiple EDHs present)
edh <- read.csv("IMPORT/JSON extraction script/Individual/epidural_hematoma.10.05.2024.csv")
edh <- edh[edh$Imaging.Timepoint == "CT Early", ]
edh <- edh[edh$epidural_hematoma_basic_observation %in% ("present"), ]
extra$EDH_mass <- as.factor(ifelse(extra$subjectId %in% edh[!is.na(edh$epidural_hematoma_descriptive_volume) & 
                                                              edh$epidural_hematoma_descriptive_volume >= 25,]$subjectId, "Yes", 
                                   ifelse(extra$subjectId %in% I$subjectId, "No", NA)))
table(extra$EDH_mass)
rm(edh)

# First Glucose (3-20 mmol/L)	
# https://center-tbi.incf.org/_664b7eba317763b978219cf3
# GlucoseOther curationRemarks: When sites used another unit than the preferred unit, the other value was converted by the Data Curation Task Force 
# to the preferred unit for uniformity in analyses. In principle, other units can be disregarded, since you will find 
# the converted equivalent in the preferred unit.
lab <- read.csv("IMPORT//Glucose.HB.20.05.2024.csv")
# remove entries where both glucose and Hb missing
table(is.na(lab$Labs.DLHemoglobingdL), is.na(lab$Labs.DLHemoglobinOther)) 
table(is.na(lab$Labs.DLGlucosemmolL), is.na(lab$Labs.DLHemoglobingdL))
lab <- lab[!(is.na(lab$Labs.DLGlucosemmolL) & is.na(lab$Labs.DLHemoglobingdL)), ]
# remove entries with missing date; unclear collection day
lab <- lab[lab$Labs.DLDate !="", ]
# check number of entries per patient for the ones with missing time 
lab[lab$subjectId %in% c(lab[lab$Labs.DLTime=="", ]$subjectId),]
# impute missing time with 23:59:59; otherwise a later sample will be selected
lab[lab$Labs.DLTime=="", ]$Labs.DLTime <- "23:59:59"
lab$sample_dt <- as.POSIXct(paste(lab$Labs.DLDate, lab$Labs.DLTime), format="%Y-%m-%d %H:%M:%S")
lab <- data.table(lab)
# select first measured glucose per patient
lab_notNA_glucose <- lab[!is.na(lab$Labs.DLGlucosemmolL), ]
first_glucose <- lab_notNA_glucose[, .SD[which.min(sample_dt)], by = subjectId]
# A few patients have first recorded glucose late -> NA (ignore entry errors)
table(first_glucose$Labs.DLDate)
first_glucose[first_glucose$Labs.DLDate %in% c("1970-01-04", "1970-01-05", "1970-01-06", "1970-01-07", "1970-01-19"), ]$Labs.DLGlucosemmolL <- NA
# merge
extra <- merge(extra, first_glucose[, c("subjectId", "Labs.DLGlucosemmolL")], by = "subjectId", all.x = T)
rm(first_glucose, lab_notNA_glucose)

# First Hb (6-17 g/dL)
# HB curationRemarks: 
# Situation: Hemoglobin Background: We checked whether Hemoglobin was converted correctly 
# Assessment: It appeared that not all values were converted correctly. We mainly saw problems with the convertion 
# of g/L to g/dL. Furthermore, not all values seemed to converted. 
# Recommendation: The convertion from g/L to g/dL should be checked again. 
# All units that were specified as 'Other' should be checked again which unit was used (probably g/L).
lab_notNA_hb <- lab[!is.na(lab$Labs.DLHemoglobingdL), ]
first_hb <- lab_notNA_hb[, .SD[which.min(sample_dt)], by = subjectId]

table(first_hb$Labs.DLHemoglobinOtherUnit)
first_hb$check_L_to_dl <- first_hb$Labs.DLHemoglobingdL == first_hb$Labs.DLHemoglobinOther/10
first_hb$check_mmolL_to_gdl <- round(first_hb$Labs.DLHemoglobingdL/first_hb$Labs.DLHemoglobinOther, 1) == 1.6
# replace "99.90" with Labs.DLHemoglobinOther/10
first_hb[first_hb$Labs.DLHemoglobingdL %in% c(99.90), ]$Labs.DLHemoglobingdL <- first_hb[first_hb$Labs.DLHemoglobingdL %in% c(99.90), ]$Labs.DLHemoglobinOther/10
# replace with other non-NA value
first_hb[first_hb$subjectId %in% c("6bJy778"), ]$Labs.DLHemoglobingdL <- first_hb[first_hb$subjectId %in% c("6bJy778"), ]$Labs.DLHemoglobingdL/10
first_hb[first_hb$subjectId %in% c("2VAp647"), ]$Labs.DLHemoglobingdL <- first_hb[first_hb$subjectId %in% c("2VAp647"), ]$Labs.DLHemoglobingdL*10
# A few patients have first recorded HB late -> NA (ignore entry errors)
table(first_hb$Labs.DLDate)
first_hb[first_hb$Labs.DLDate %in% c("1970-01-04", "1970-01-05", "1970-01-06", "1970-07-30"), ]$Labs.DLHemoglobingdL <- NA
# merge
extra <- merge(extra, first_hb[, c("subjectId", "Labs.DLHemoglobingdL")], by = "subjectId", all.x = T)
rm(first_hb, lab_notNA_hb, lab)

## CRASH-TBI: GCS 3-14 -> outcome unfavourable_6

# Obliteration of the third ventricle or basal cisterns	
ventr <- read.csv("IMPORT/JSON extraction script/Individual/ventricular_compression.10.05.2024.csv")
ventr <- ventr[ventr$Imaging.Timepoint == "CT Early", ]
ventr <- ventr[ventr$ventricular_compression_basic_observation %in% ("present"), ]
extra$third_ventr_oblit <- as.factor(ifelse(extra$subjectId %in% ventr[ventr$ventricular_compression_third_ventricle_shift %in% c("absent"),]$subjectId, "Yes", "No"))
table(extra$third_ventr_oblit)
cist <- read.csv("IMPORT/JSON extraction script/Individual/cisternal_compression.10.05.2024.csv")
cist <- cist[cist$Imaging.Timepoint == "CT Early", ]
cist <- cist[cist$cisternal_compression_basic_observation %in% ("present"), ]
extra$basal_cist_oblit <- as.factor(ifelse(extra$subjectId %in% cist[cist$cisternal_compression_suprasellar_cistern %in% c("absent") &
                                                                       cist$cisternal_compression_interpeduncular_prepontine %in% c("absent") &
                                                                       cist$cisternal_compression_ambient_cistern %in% c("absent") &
                                                                       cist$cisternal_compression_quadrigeminal %in% c("absent"),]$subjectId, "Yes", "No"))
table(extra$basal_cist_oblit)
table(extra$third_ventr_oblit, extra$basal_cist_oblit)
extra$oblit_3_bc <- as.factor(ifelse(extra$third_ventr_oblit %in% c("Yes") | extra$basal_cist_oblit %in% c("Yes"), "Yes", 
                                     ifelse(extra$subjectId %in% I$subjectId, "No", NA)))
extra$third_ventr_oblit <- NULL 
extra$basal_cist_oblit <- NULL 
rm(ventr, cist)

# Non-evacuated haematoma
I$hematoma <- as.factor(ifelse(I$epidural_hematoma_count_p %in% c("Yes") | 
                                 I$subdural_hematoma %in% c("Yes") |
                                 I$Contusion %in% c("Yes") | 
                                 I$ICH %in% c("Yes") | 
                                 I$extraaxial_hematoma_count_p %in% c("Yes"), "Yes", "No"))
table(I$hematoma)

### ADDITIONAL AUXILIARY VARIABLES FOR IMPUTATION ---------------------------
# https://center-tbi.incf.org/_669a8c8d4f12bd0241e57450
aux <- read.csv("IMPORT/AuxiliaryVars.19.07.2024.csv")

# Neurological disorders (neurodegenerative, stroke, MS, seizures) 
aux$any_stroke_tia  <- apply(aux[, c("MedHx.MedHxNeuroCerebrovascularAccident", "MedHx.MedHxNeuroTIA",
                                     "MedHx.AnticoagulantReasonCardiovasTIS")], 1, function(x) length(which(x %in% c(1))))
table(aux$any_stroke_tia)
aux$any_stroke_tia <-  as.factor(ifelse(aux$any_stroke_tia %in% c(0), "No", "Yes"))

aux$any_seizure_hx  <- apply(aux[, c("MedHx.MedHxNeuroFebrileSeizures", "MedHx.MedHxNeuroEpilepsyPartial",
                                     "MedHx.MedHxNeuroEpilepsyGeneralized", "MedHx.MedHxNeuroEpilepsyOther")], 1, function(x) length(which(x %in% c(1))))
table(aux$any_seizure_hx)
aux$any_seizure_hx <-  as.factor(ifelse(aux$any_seizure_hx %in% c(0), "No", "Yes"))

table(aux$InjuryHx.EDComplEventSeizures)
aux$InjuryHx.EDComplEventSeizures <- as.factor(aux$InjuryHx.EDComplEventSeizures)
levels(aux$InjuryHx.EDComplEventSeizures) <- c("No", "Partial/Focal", "Generalized", "Status epilepticus", NA)
table(aux$InjuryHx.EDComplEventSeizures)

# Search free text variables for MS, neurodegenerative disorders
aux$neuro_degen_MS <- NA
aux[aux$MedHx.MedHxNeuroOtherTxt %in% c("Progressive Supranuclear Palsy", "Parkinsons disease", "Parkinsons", "parkinsons",
                                        "Parkinsonism", "Parkinson disease; hydrocephalus", "Parkinson", "parkinson", "Multiple sclerosis",
                                        "MS", "M. Parkinson", "M Parkinson", "Early onset Parkinsons", "Idiopathic Parkinsons Disease",
                                        "dementia", "Degenerative disorders, neurolysis", "Cerebral vasculopaty  Parkinson disease",
                                        "Alzheimers disease", "- MS  - Trigeminus Neuralgie", "dementia early stage", "Alzheimers disease", "Alzheimer disease") |
      aux$MedHx.MedHxOtherTxt %in% c("Progressive Supranuclear Palsy", "Parkinsons disease", "Parkinsons", "parkinsons",
                                     "Parkinsonism", "Parkinson disease; hydrocephalus", "Parkinson", "parkinson", "Multiple sclerosis",
                                     "MS", "M. Parkinson", "M Parkinson", "Early onset Parkinsons", "Idiopathic Parkinsons Disease",
                                     "dementia", "Degenerative disorders, neurolysis", "Cerebral vasculopaty  Parkinson disease",
                                     "Alzheimers disease", "- MS  - Trigeminus Neuralgie", "dementia early stage", "Alzheimers disease", "Alzheimer disease"), ]$neuro_degen_MS <- "Yes"


aux[aux$MedHx.MedHxNeuroOtherTxt %in% c("TIA", "Nontraumatic Subarachnoid Hemorrhage", "ischemic encephalopathy", "cerebellar infarct, ", 
                                        "Brainstem infarct  n. trochlearis neuropathy", "Aneurysmatic SAH", "Two previous ischemic stroke without sequelae.",
                                        "stroke  deep vein thrombosis",  "old lacunar infarctations seen in CT") |
      aux$MedHx.MedHxOtherTxt %in% c("TIA", "Nontraumatic Subarachnoid Hemorrhage", "ischemic encephalopathy", "cerebellar infarct, ", 
                                     "Brainstem infarct  n. trochlearis neuropathy", "Aneurysmatic SAH", "Two previous ischemic stroke without sequelae.",
                                     "stroke  deep vein thrombosis",  "old lacunar infarctations seen in CT"), ]$any_stroke_tia <- "Yes"

aux[aux$MedHx.MedHxNeuroOtherTxt %in% c("phenomenon of absence of short duration, with spontaneaous recovery, without current diagnostic, appeared 2weeks before admission",
                                        "have had seizures ") |
      aux$MedHx.MedHxOtherTxt %in% c("phenomenon of absence of short duration, with spontaneaous recovery, without current diagnostic, appeared 2weeks before admission",
                                     "have had seizures "), ]$any_seizure_hx <- "Yes"
aux[is.na(aux$neuro_degen_MS), ]$neuro_degen_MS <- "No"
aux$neuro_degen_MS <- as.factor(aux$neuro_degen_MS)
table(aux$neuro_degen_MS)

# Ammend M table for patients with ADHD and ADHD/PDD NOS in free text field not recorded as developmental hx
M[M$subjectId %in% c("3Vct756"), ]$MedHx.MedHxDevelopmental <- "present" 
M[M$subjectId %in% c("7RnY232"), ]$MedHx.MedHxDevelopmental <- "present" 

# Melanoma (S100B) - no CDE; none in free text fields 

# Alcohol_intoxication
table(aux$InjuryHx.InjViolenceVictimAlcohol)
aux$InjuryHx.InjViolenceVictimAlcohol <- as.factor(aux$InjuryHx.InjViolenceVictimAlcohol)
levels(aux$InjuryHx.InjViolenceVictimAlcohol) <- c("No", "Definite", "Suspect", NA)
table(aux$InjuryHx.InjViolenceVictimAlcohol)

## KEEP RELEVANT VARIABLES FOR IMPUTATION 
names(aux)
aux <- aux[, c("subjectId", "any_stroke_tia", "any_seizure_hx", "InjuryHx.EDComplEventSeizures", "neuro_degen_MS", "InjuryHx.InjViolenceVictimAlcohol")]
extra <- merge(extra, aux, by = "subjectId")
rm(aux)

# MERGE and RENAME DATA ----------------------------------------

all <- merge(C, B, by = "subjectId", all = T)
all <- merge(all, I, by = "subjectId", all = T)
all <- merge(all, M, by = "subjectId", all = T)
all <- merge(all, O, by = "subjectId", all = T)
all <- merge(all, extra, by = "subjectId", all = T)

# Rename only variables used in analyses

all <- all %>% 
  rename(
    "TBI Severity" = TBISeverity,
    "Pupils" = InjuryHx.PupilsBaselineDerived, 
    "GCS Score" = InjuryHx.GCSScoreBaselineDerived,
    "GCS Eyes" = InjuryHx.GCSEyesBaselineDerived, 
    "GCS Motor" = InjuryHx.GCSMotorBaselineDerived,
    "GCS Verbal" = InjuryHx.GCSVerbalBaselineDerived,
    "log GFAP" = logGFAP, 
    "log UCHL1" = logUCHL1, 
    "log S100B" = logS100B,
    "GFAP" = zlogGFAP, 
    "UCHL1" = zlogUCHL1, 
    "S100B" = zlogS100B,
    "Time to sampling" = bbb_sampling_time,
    "Epidural hematoma volume" = epidural_hematoma_total_volume_p, 
    "Subdural hematoma volume" = subdural_hematoma_volume, 
    "Any abnormality" = AnyAbnormality, 
    "No abnormality" = NoAbnormality, 
    "Skull fracture" = skull_fracture_count_p, 
    "Epidural hematoma" = epidural_hematoma_count_p, 
    "Subdural hematoma" = subdural_hematoma, 
    "TSAH" = tsah_count_p,
    "IVH" = intraventricular_hemorrhage_ivh_count_p,
    "Mass effect" = mass_effect, 
    "Contusion or ICH" = intraparenchymal_hemorrhage_count_p, 
    "Cisternal compression" = cisternal_compression_count_p,
    "Ventricular compression" = ventricular_compression_count_p,
    "MLS" = mls_count_p, 
    "Brain herniation" = brain_herniation_count_p, 
    "Total lesion volume" = total_lesion_volume, 
    "Total lesion volume >= 25" = total_lesion_volume_25, 
    "Mechanism of injury" = MechanismHighvLowVelocity, 
    "Major extracranial injury" = major_extracran_injury, 
    "Accidental cause" = InjuryHx.NonAccInjCause, 
    "Age" = Subject.Age, 
    "Sex" = Subject.Sex, 
    "Medical history" = med_hx, 
    "ASAPS class" = MedHx.MedHxPreInjASAPSClass,
    "Psychiatric history" = MedHx.MedHxPsychiatric, 
    "Developmental history" = MedHx.MedHxDevelopmental,  
    "TBI history" = MedHx.MedHxPreTBIConcussions, 
    "Employment status Job classification" = JobEmployment,  
    "Highest level of education" = Subject.EduLvlUSATyp2cat, 
    "Years of education" = Subject.EduYrCt, 
    "Detectable intracranial injury on CT early" = AnyIntracranAbnormality, 
    "Hospital admission" = Hosp, 
    "ICU admission" = ICU, 
    "ICP monitoring" = Hospital.ICPMonitorYes,
    "Cranial surgery anytime" = any_cran_surg, 
    "Cranial surgery within 72h" = any_cran_surg72,
    "Major cranial surgery anytime" = major_cran_surg, 
    "Major cranial surgery within 72h" = major_cran_surg72, 
    "Mortality at 6 months" = mortality_6, 
    "Unfavorable outcome at 6 months" = unfavourable_6,                               
    "Incomplete recovery at 6 months" = incomplete_6, 
    "GOSE score at 6 months" = Subject.GOSE6monthEndpointDerived,
    "Impairment of HRQOL" = HRQOL_Impairment,
    "Seizures" = InjuryHx.EDComplEventSeizures, 
    "Hypoxia" = InjuryHx.EDComplEventHypoxia,
    "Hypotension" = InjuryHx.EDComplEventHypotension)

# Define pillars
clinical <- c("GCS Score", "GCS Eyes", "GCS Motor", "GCS Verbal", "Pupils")
biomarkers <- c("GFAP", "UCHL1", "S100B", "Time to sampling")
imaging <- c("Any abnormality", "Skull fracture", "Epidural hematoma", "Subdural hematoma",                            
             "TSAH", "Contusion or ICH", "TAMVI", "IVH", "Mass effect", "Total lesion volume >= 25")
modifiers <- c( "Mechanism of injury","Seizures","Major extracranial injury", "Hypoxia", "Hypotension", "Accidental cause",
                "Age", "Sex",                                      
                "Medical history", "Psychiatric history", "Developmental history",                        
                "TBI history")
endpoints <- c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",
               "Major cranial surgery within 72h", "Mortality at 6 months", "Unfavorable outcome at 6 months",                               
               "Incomplete recovery at 6 months", "Impairment of HRQOL",  "GOSE score at 6 months")
names_components <- c(clinical, biomarkers, imaging, modifiers)
names_endpoints <- endpoints
# Add redundant variable
all$mms <- all$`TBI Severity`
levels(all$mms) <- c("Mild", "Moderate", "Severe")

# Store original data
all_original_data <- all

# DESCRIPTIVES and MISSINGNESS --------------------------------------------
vrbls       <- c(clinical, "Biomarkers.GFAP", "Biomarkers.UCH.L1", "Biomarkers.S100B",
                 "log GFAP", "log UCHL1", "log S100B", biomarkers,"No abnormality", imaging, modifiers, endpoints)
nonnormvrbl <- c("GCS Score", "GCS Eyes", "GCS Motor", "GCS Verbal", 
                 "Biomarkers.GFAP", "Biomarkers.UCH.L1", "Biomarkers.S100B",
                 "log GFAP", "log UCHL1", "log S100B", biomarkers, "Age")
factorvrbl  <- setdiff(vrbls, nonnormvrbl)
tb1   <- print(CreateTableOne(vars = vrbls, data = all, factorVars = factorvrbl), nonnorm = nonnormvrbl, missing = TRUE, contDigits = 2)
tb2   <- print(CreateTableOne(vars = vrbls, data = all, strata = "TBI Severity", factorVars = factorvrbl), nonnorm = nonnormvrbl, missing = TRUE, contDigits = 2)

tb12 <- cbind(tb1, tb2)
write.csv(tb12, "EXPORT/Descriptives.csv")

rm(tb1, tb2, tb12, vrbls, nonnormvrbl, factorvrbl)

pdf("EXPORT/Missingness.pdf", width = 16, height = 10)
vis_miss(all[, c(clinical, biomarkers, imaging, modifiers, endpoints)], cluster = T) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("Observations\n")
naniar::gg_miss_var(all[, c(clinical, biomarkers, imaging, modifiers, endpoints)], show_pct = TRUE) + ylab("\n% Missing")  + xlab("Variables\n")  
vis_miss(all[, c(clinical, biomarkers, imaging, modifiers, endpoints, "mms")], facet = mms) + 
  theme(axis.text.x = element_text(angle = 90)) + ylab("Observations\n") +
  ggtitle("Variable missingness, by TBI severity\n")
dev.off()

# CORRELATION HEATMAP (Available cases; Fig 1, SFig1) ------------------------------------------------------------

# temporarily set Impairment of HRQOL as Yes when dead for correlation heatmap
table(all$`Impairment of HRQOL`, all$`Mortality at 6 months`)
all[is.na(all$Outcomes.QoLIBRIOSTotalScore) & all$`Mortality at 6 months` %in% c("Yes"), ]$`Impairment of HRQOL` <- "Yes"

# ICP monitoring is No for ER and Ward stratum
summary(all$`ICP monitoring`)
length(all[is.na(all$`ICP monitoring`) & all$ICU %in% c("No"), ]$`ICP monitoring`)
all[is.na(all$`ICP monitoring`) & all$ICU %in% c("No"), ]$`ICP monitoring` <- "No"
summary(all$`ICP monitoring`)

mixed_assoc2 = function(df, cor_method="spearman"){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use spearman's on numerically transformed variables
      correlation = cor(as.numeric(x), as.numeric(y), method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      correlation = cor(x, as.numeric(y), method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_nominal(x) && is_numeric(y)){
      correlation = cor(as.numeric(x), y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}

# Recode for directional of coef
str(all[, c(clinical, biomarkers, imaging, modifiers, endpoints)])
cor_all <- all
cor_all <- cor_all %>% 
  rename(
    "Unreactive pupil(s)" = Pupils, 
    "Low velocity injury mechanism" = `Mechanism of injury`, 
    "Non-accidental cause" = `Accidental cause`, 
    "Male sex" = Sex)

# Define pillars
clinical <- c("GCS Score", "GCS Eyes", "GCS Motor", "GCS Verbal", "Pupils")
cor_clinical <- c("GCS Score", "GCS Eyes", "GCS Motor", "GCS Verbal", "Unreactive pupil(s)")
biomarkers <- c("GFAP", "UCHL1", "S100B", "Time to sampling")
imaging <- c("Any abnormality", "Skull fracture", "Epidural hematoma", "Subdural hematoma",                            
             "TSAH", "Contusion or ICH", "TAMVI", "IVH", "Mass effect", "Total lesion volume >= 25")
modifiers <- c("Mechanism of injury","Seizures","Major extracranial injury", "Hypoxia", "Hypotension", "Accidental cause",
               "Age", "Sex",                                      
               "Medical history", "Psychiatric history", "Developmental history",                        
               "TBI history")
cor_modifiers <- c("Low velocity injury mechanism","Seizures","Major extracranial injury", "Hypoxia", "Hypotension", "Non-accidental cause",
                   "Age", "Male sex",                                      
                   "Medical history", "Psychiatric history", "Developmental history",                        
                   "TBI history")

assoc <- mixed_assoc2(cor_all[, c(cor_clinical, biomarkers, imaging, cor_modifiers, endpoints)])
assoc$abs_value <- abs(assoc$assoc)

assoc$x <- factor(assoc$x, levels = c(cor_clinical, biomarkers, imaging, cor_modifiers, endpoints))
assoc$y <- factor(assoc$y, levels = c(cor_clinical, biomarkers, imaging, cor_modifiers, endpoints))
assoc$y <- fct_rev(assoc$y)

assoc$pillarx <- as.factor(ifelse(assoc$x %in% cor_clinical, "Clinical", 
                                  ifelse(assoc$x %in% biomarkers, "Biomarkers",
                                         ifelse(assoc$x %in% imaging, "Imaging", 
                                                ifelse(assoc$x %in% cor_modifiers, "Modifiers", "End-points")))))
assoc$pillarx <- factor(assoc$pillarx, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "End-points"))
assoc$pillary <- as.factor(ifelse(assoc$y %in% cor_clinical, "Clinical", 
                                  ifelse(assoc$y %in% biomarkers, "Biomarkers",
                                         ifelse(assoc$y %in% imaging, "Imaging", 
                                                ifelse(assoc$y %in% cor_modifiers, "Modifiers", "End-points")))))
assoc$pillary <- factor(assoc$pillary, levels = c("End-points", "Modifiers", "Imaging", "Biomarkers", "Clinical"))

# Heatmap with no coefs
Assoc_plot2 <- ggplot(assoc, 
                      aes(x = interaction(x, pillarx, lex.order = F), 
                          y = interaction(y, pillary, lex.order = F), 
                          fill = assoc)) +
  geom_tile()+ #geom_text(aes(label = format(round(abs_value, 2), nsmall = 2)), size = 2.5) +
  scale_fill_gradient2(low = M_COLOR, high = I_COLOR, mid = "white",
                       midpoint = 0, limits=c(-1, 1))+
  scale_x_discrete(name = "", position = "top", guide = "axis_nested") + 
  scale_y_discrete(name = "", guide = "axis_nested")+ 
  labs(fill="Spearman's rho\n", tag = paste0("CENTER-TBI\nn=",
                                             min(assoc$complete_obs_pairs), "-", max(assoc$complete_obs_pairs)))+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), 
        axis.text.x = element_text(angle=90, hjust=0, size = 10),
        axis.text.y = element_text(size = 10), plot.margin=unit(c(0, 0, 0, 0), 'cm'),
        ggh4x.axis.nestline.x = element_line(size = 2),
        ggh4x.axis.nestline.y = element_line(size = 2),
        ggh4x.axis.nesttext.x = element_text(angle = 0, vjust = 1, hjust = 0.5, face = "bold"),
        ggh4x.axis.nesttext.y = element_text(angle = 90, vjust = 1, hjust = 0.5, face = "bold"),
        legend.position = "bottom") +
  geom_rect(aes(xmin = 1 - 0.5, xmax = 6 - 0.5, ymin = 42 - 0.5, ymax = 37 - 0.5), fill = "transparent", color = "black", size = 1)+
  geom_rect(aes(xmin = 6 - 0.5, xmax = 10 - 0.5, ymin = 37 - 0.5, ymax = 33 - 0.5), fill = "transparent", color = "black", size = 1)+
  geom_rect(aes(xmin = 10 - 0.5, xmax = 20 -0.5, ymin = 33 - 0.5, ymax = 23 - 0.5), fill = "transparent", color = "black", size = 1)+
  geom_rect(aes(xmin = 20 - 0.5, xmax = 32 - 0.5, ymin = 23 - 0.5, ymax = 11 - 0.5), fill = "transparent", color = "black", size = 1) +
  geom_rect(aes(xmin = 32 - 0.5, xmax = 42 - 0.5, ymin = 11 - 0.5, ymax = 1 - 0.5), fill = "transparent", color = "black", size = 1)+
  theme(legend.position=c(-0.325,1.25), 
        plot.tag.location = "plot", 
        plot.tag =  element_text( hjust=0.3, vjust = 0.1))
Assoc_plot2 

pdf("EXPORT/Fig1A.pdf", height = 9, width = 9)
Assoc_plot2
dev.off()

# Heatmap with coefs
Assoc_plot <- ggplot(assoc, 
                     aes(x = interaction(x, pillarx, lex.order = F), 
                         y = interaction(y, pillary, lex.order = F), 
                         fill = assoc)) +
  geom_tile()+ geom_text(aes(label = format(round(assoc, 2), nsmall = 2)), size = 2.5, fontface = "bold") +
  scale_fill_gradient2(low = M_COLOR, high = I_COLOR, mid = "white",
                       midpoint = 0, limits=c(-1, 1))+
  scale_x_discrete(name = "", position = "top", guide = "axis_nested") + scale_y_discrete(name = "", guide = "axis_nested")+ 
  labs(fill="Spearman's rho\n", tag = paste0("CENTER-TBI\nn=",
                                             min(assoc$complete_obs_pairs), "-", max(assoc$complete_obs_pairs)))+
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), 
        axis.text.x = element_text(angle=90, hjust=0, size = 10, face="bold"),
        axis.text.y = element_text(size = 10, face="bold"), plot.margin=unit(c(0, 0, 0, 0), 'cm'),
        ggh4x.axis.nestline.x = element_line(size = 2),
        ggh4x.axis.nestline.y = element_line(size = 2),
        ggh4x.axis.nesttext.x = element_text(angle = 0, vjust = 1, hjust = 0.5, face = "bold"),
        ggh4x.axis.nesttext.y = element_text(angle = 90, vjust = 1, hjust = 0.5, face = "bold"),
        legend.position = "bottom") +
  geom_rect(aes(xmin = 1 - 0.5, xmax = 6 - 0.5, ymin = 42 - 0.5, ymax = 37 - 0.5), fill = "transparent", color = "black", size = 1)+
  geom_rect(aes(xmin = 6 - 0.5, xmax = 10 - 0.5, ymin = 37 - 0.5, ymax = 33 - 0.5), fill = "transparent", color = "black", size = 1)+
  geom_rect(aes(xmin = 10 - 0.5, xmax = 20 -0.5, ymin = 33 - 0.5, ymax = 23 - 0.5), fill = "transparent", color = "black", size = 1)+
  geom_rect(aes(xmin = 20 - 0.5, xmax = 32 - 0.5, ymin = 23 - 0.5, ymax = 11 - 0.5), fill = "transparent", color = "black", size = 1) +
  geom_rect(aes(xmin = 32 - 0.5, xmax = 42 - 0.5, ymin = 11 - 0.5, ymax = 1 - 0.5), fill = "transparent", color = "black", size = 1)+ 
  theme(legend.position=c(-0.18,1.18), legend.text = element_text(face="bold"), 
        plot.tag.location = "plot", 
        plot.tag =  element_text(hjust=0.3, vjust = 0.1, face="bold"))
Assoc_plot

pdf("EXPORT/FigS1.pdf", height = 10, width = 14)
Assoc_plot
dev.off()

emf(file = "EXPORT/FigS1.emf", height = 10, width = 14)
Assoc_plot
dev.off()

# CC: UNIVARIABLE ANALYSIS OF INDIVIDUAL PILLAR COMPONENTS --------------------------------------------
Univar <- data.frame(Variable = NA, RCS = NA, Levels = NA,
                     Outcome = NA, n = NA, Effect = NA, SE = NA, AUC = NA, R2 = NA, LRT = NA, 
                     n_Mild = NA, R2_Mild = NA, 
                     n_Moderate = NA, R2_Moderate = NA,
                     n_Severe = NA, R2_Severe = NA)

Linearity_Univar <- data.frame(Variable = NA, 
                               Outcome = NA, n = NA, 
                               AUC_RCS = NA, R2_RCS = NA, 
                               AUC_LIN = NA, R2_LIN = NA, 
                               LRT_RCS_NULL = NA, LRT_RCS_LIN = NA)

### TEST LINEARITY, FIT, EXTRACT INFO, SUBGROUP FITS  ----------------------------------------------------------------
# tol = 1e-10

for (j in endpoints) {
  print(j)
  # select complete cases for each endpoint
  all_cc <- all_original_data[complete.cases(all_original_data[, c(clinical, biomarkers, imaging, modifiers, j)]),]
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  for (i in c("TBI Severity", clinical, biomarkers, imaging, modifiers)) {
    
    if(!(i %in% imaging & j == "Detectable intracranial injury on CT early")) {
      
      rcs_temp <- 0
      df_temp <- 0
      
      # test linearity of continuous predictors
      if(i %in% c("GCS Score", "GFAP", "UCHL1", "S100B", "Time to sampling", "Age", "Years of education")) {
        
        model_linear <- lrm(as.formula(paste0("`", j, "` ~ `", i, "`")), data = all_cc, x = T, y = T)
        model_rcs <- lrm(as.formula(paste0("`", j, "` ~ rcs(`", i, "`, 3)")), data = all_cc, x = T, y = T)
        
        lrt_rcs_lin_null <- anova(model_rcs, test = 'LR')
        
        Linearity_Univar_temp <- data.frame(Variable = i, Outcome = j, 
                                            n = model_rcs$stats[["Obs"]],
                                            AUC_RCS = round(model_rcs$stats[["C"]], 2), 
                                            R2_RCS = round(model_rcs$stats[["R2"]], 3), 
                                            AUC_LIN = round(model_linear$stats[["C"]], 2), 
                                            R2_LIN = round(model_linear$stats[["R2"]], 3), 
                                            LRT_RCS_NULL = p_format(lrt_rcs_lin_null["TOTAL", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T), 
                                            LRT_RCS_LIN = p_format(lrt_rcs_lin_null[" Nonlinear", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T))
        
        rownames(Linearity_Univar_temp) <- NULL
        Linearity_Univar <- rbind(Linearity_Univar, Linearity_Univar_temp)
        
        plot_odds <- ggplot(all_cc, aes(x=all_cc[, i], y=model_linear$linear.predictors, color = "Linear")) +
          geom_line(size=1) + labs(x = paste0("\n", i, " (non-linearity LRT ", p_format(lrt_rcs_lin_null[" Nonlinear", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T), ")"), 
                                   y = paste0("Predicted log odds of\n ", j, "\n")) + theme_minimal()+
          geom_line(aes(x=all_cc[, i] , y=model_rcs$linear.predictors, colour = "RCS"), size = 1)+
          scale_colour_manual(name = "Model", values = cols)
        plot(plot_odds)
        
        plot_probs <- ggplot(all_cc, aes(x=all_cc[, i], y=plogis(model_linear$linear.predictors), color = "Linear")) +
          geom_line(size=1) + labs(x = paste0("\n", i, " (non-linearity LRT ", p_format(lrt_rcs_lin_null[" Nonlinear", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T), ")"), 
                                   y = paste0("Predicted probability of\n ", j, "\n")) + theme_minimal()+
          ylim(0, 1) + geom_line(aes(x=all_cc[, i] , y=plogis(model_rcs$linear.predictors), colour = "RCS"), size = 1)+
          scale_colour_manual(name = "Model", values = cols)
        plot(plot_probs)
        
        if(lrt_rcs_lin_null["TOTAL", "P"] < 0.05 & lrt_rcs_lin_null[" Nonlinear", "P"] < 0.05) {
          rcs_temp <- 1
          model_selected <- model_rcs
        } else {
          model_selected <- model_linear
        }
      }
      
      if(!i %in% c("GCS Score", "GFAP", "UCHL1", "S100B", "Time to sampling", "Age", "Years of education")) {
        all_cc$temp <- all_cc[, i]  # otherwise Error in X[, mmcolnames, drop = FALSE] : subscript out of bounds
        model_selected <- lrm(as.formula(paste0("`", j, "` ~ temp")), data = all_cc, x = T, y = T)
      }
      
      df_temp <- ifelse(j %in% "GOSE score at 6 months", length(model_selected$coefficients) - 6, length(model_selected$coefficients) - 1)
      
      if(df_temp > 1) {lr <- anova(model_selected, test = 'LR')}
      
      # subgroup analyses
      
      all_cc$lp <- model_selected$linear.predictors
      perform_subgroup <- F
      
      if(! i %in% "TBI Severity" & (j %in% "GOSE score at 6 months" | all(table(all_cc[, c("mms", j)]) > 10))) {
        
        perform_subgroup <- T
        rm(refit_mild, refit_moderate, refit_severe)
        try({
          refit_mild <- lrm(as.formula(paste0("`", j, "` ~ lp")), data = all_cc[all_cc$mms %in% c("Mild"),], x = T, y = T)
          print(i)
        }, silent = T)
        
        try({
          refit_moderate <- lrm(as.formula(paste0("`", j, "` ~ lp")), data = all_cc[all_cc$mms %in% c("Moderate"),], x = T, y = T)
          print(i)
        }, silent = T)
        
        try({
          refit_severe <- lrm(as.formula(paste0("`", j, "` ~ lp")), data = all_cc[all_cc$mms %in% c("Severe"),], x = T, y = T)
          print(i)
        }, silent = T)
        
      }
      
      
      # store results
      Univar_temp <- data.frame(Variable = rep(i, df_temp), 
                                RCS = rep(rcs_temp, df_temp),
                                Levels = model_selected$Design$colnames,
                                Outcome = rep(j, df_temp), 
                                n = rep(model_selected$stats[["Obs"]], df_temp),
                                Effect = model_selected$coefficients[model_selected$Design$colnames], 
                                SE = sqrt(diag(model_selected$var))[model_selected$Design$colnames], 
                                
                                AUC = rep(round(model_selected$stats[["C"]], 2), df_temp), 
                                R2 = rep(round(model_selected$stats[["R2"]], 3), df_temp), 
                                LRT = ifelse(df_temp == 1, NA, rep(p_format(lr["TOTAL", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T), df_temp)),
                                
                                R2_Mild =     ifelse(perform_subgroup == F | !exists("refit_mild"), NA, ifelse(refit_mild$fail == F, rep(round(refit_mild$stats[["R2"]], 3), df_temp), NA)), 
                                R2_Moderate = ifelse(perform_subgroup == F | !exists("refit_moderate"), NA, ifelse(refit_moderate$fail == F, rep(round(refit_moderate$stats[["R2"]], 3), df_temp), NA)), 
                                R2_Severe =   ifelse(perform_subgroup == F | !exists("refit_severe"), NA, ifelse(refit_severe$fail == F, rep(round(refit_severe$stats[["R2"]], 3), df_temp), NA)),
                                
                                n_Mild =      ifelse(perform_subgroup == F | !exists("refit_mild"), NA, ifelse(refit_mild$fail == F, rep(round(refit_mild$stats[["Obs"]], 3), df_temp), NA)), 
                                n_Moderate =  ifelse(perform_subgroup == F | !exists("refit_moderate"), NA, ifelse(refit_moderate$fail == F, rep(round(refit_moderate$stats[["Obs"]], 3), df_temp), NA)),  
                                n_Severe =    ifelse(perform_subgroup == F | !exists("refit_severe"), NA, ifelse(refit_severe$fail == F, rep(round(refit_severe$stats[["Obs"]], 3), df_temp), NA))) 
      
      rownames(Univar_temp) <- NULL
      Univar <- rbind.fill(Univar, Univar_temp)
    }
  }
}

# CC: PILLAR and CUMULATIVE MODELS -----------------------------------------------------------

### RCS DEPENDING ON LINEARITY -----------------------------------------------------------
RCS <- unique(Univar[, c("Outcome", "Variable", "RCS")])
RCS <- RCS[RCS$Variable %in% c("GCS Score", "GFAP", "UCHL1", "S100B", "Time to sampling", "Age", "Years of education"),]
table(RCS$Variable, RCS$RCS) # none is linear throughout

## Code alternative formulas

c_formula_gcs_rcs <- "rcs(`GCS Score`, 3)"
c_formula_gcs_lin <- "`GCS Score`"
c_formula_gcs_emv <- "`GCS Eyes` + `GCS Motor` + `GCS Verbal`"

gfap_formula_rcs <- "rcs(`GFAP`, 3)"
gfap_formula_lin <- "`GFAP`"
uchl1_formula_rcs <- "rcs(`UCHL1`, 3)"
uchl1_formula_lin <- "`UCHL1`"
s100b_formula_rcs <- "rcs(`S100B`, 3)"
s100b_formula_lin <- "`S100B`"
tts_formula_rcs <- "rcs(`Time to sampling`, 3)"
tts_formula_lin <- "`Time to sampling`"

age_formula_rcs <- "rcs(`Age`, 3)"
age_formula_lin <- "`Age`"
yrsedu_formula_rcs <- "rcs(`Years of education`, 3)"
yrsedu_formula_lin <- "`Years of education`"

### FIT, EXTRACT INFO, SUBGROUP FITS -----------------------------------------------------------------
Multi <- data.frame(Model = NA, Levels = NA, Outcome = NA,  n = NA, AUC = NA, R2 = NA)
list_plots_r2 <- list()
list_plots_auc <- list()
counter_r2 <- 1

for (j in endpoints) {
  
  # select complete cases for each endpoint
  all_cc <- all_original_data[complete.cases(all_original_data[, c(clinical, biomarkers, imaging, modifiers, j)]),]
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  # rename columns to avoid Error in X[, mmcolnames, drop = FALSE] : subscript out of bounds
  
  all_cc <- all_cc %>% 
    rename(
      "Any_abnormality" = "Any abnormality", 
      "Skull_fracture" = "Skull fracture", 
      "Epidural_hematoma" = "Epidural hematoma", 
      "Subdural_hematoma" = "Subdural hematoma", 
      "Contusion_or_ICH" = "Contusion or ICH",
      "Mass_effect" = "Mass effect", 
      "Total_lesion_volume_25" = "Total lesion volume >= 25",
      "Mechanism_of_injury" = "Mechanism of injury", 
      "Major_extracranial_injury" = "Major extracranial injury", 
      "Accidental_cause" = "Accidental cause",
      "Medical_history" = "Medical history",
      "ASAPS_class" = "ASAPS class", 
      "Psychiatric_history" = "Psychiatric history",
      "Developmental_history" = "Developmental history",
      "TBI_history" = "TBI history",
      "Employment_status_Job_classification" = "Employment status Job classification",
      "Highest_level_of_education" = "Highest level of education")
  
  # C PILLAR
  model_clinical <- lrm(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == j, ]$RCS == 1, 
                                                                            c_formula_gcs_rcs, c_formula_gcs_lin))), data = all_cc, x = T, y = T)
  all_cc$temp_c <- model_clinical$linear.predictors
  
  # B PILLAR
  model_biomarkers <- lrm(as.formula(paste0("`", j, "` ~ (", 
                                            ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                            ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, x = T, y = T)
  all_cc$temp_b <- model_biomarkers$linear.predictors
  
  # I PILLAR
  if (!j == "Detectable intracranial injury on CT early") {
    
    model_imaging <- lrm(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), data = all_cc, x = T, y = T)
    all_cc$temp_i <- model_imaging$linear.predictors
  } else {
    model_imaging <- NULL
    all_cc$temp_i <- NA
  }
  
  # M PILLAR
  model_modifiers <- lrm(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+`Seizures`+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                           ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == j, ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                           "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), data = all_cc, x = T, y = T)
  all_cc$temp_m <- model_modifiers$linear.predictors
  
  # CB 
  model_cb <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_b")), data = all_cc, x = T, y = T)
  all_cc$temp_cb <- model_cb$linear.predictors
  # CM
  model_cm <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_m")), data = all_cc, x = T, y = T)
  all_cc$temp_cm <- model_cm$linear.predictors
  # CBM
  model_cbm <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), data = all_cc, x = T, y = T)
  all_cc$temp_cbm <- model_cbm$linear.predictors
  # CBM with M interactions
  model_cbm_int_m <- lrm(as.formula(paste0("`", j, "` ~ (temp_c + temp_b)*temp_m")), data = all_cc, x = T, y = T)
  all_cc$temp_cbm_int_m <- model_cbm_int_m$linear.predictors
  
  if (!j == "Detectable intracranial injury on CT early") {
    # CI
    model_ci <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_i")), data = all_cc, x = T, y = T)
    all_cc$temp_ci <- model_ci$linear.predictors
    # CBI 
    model_cbi <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i")), data = all_cc, x = T, y = T)
    all_cc$temp_cbi <- model_cbi$linear.predictors
    # CIM
    model_cim <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_i + temp_m")), data = all_cc, x = T, y = T)
    all_cc$temp_cim <- model_cim$linear.predictors
    
    # CBIM 
    model_cbim <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), data = all_cc, x = T, y = T)
    all_cc$temp_cbim <- model_cbim$linear.predictors

    all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Imaging", "Modifiers"), 2)), 
                                   N = rep(length(all_cc$subjectId), 8), 
                                   Color = c("Clinical", "Biomarkers", "Imaging", "Modifiers", rep("Cumulative", 4)),
                                   R2 = c(model_clinical$stats[["R2"]], 
                                          model_biomarkers$stats[["R2"]],
                                          model_imaging$stats[["R2"]],
                                          model_modifiers$stats[["R2"]],
                                          model_clinical$stats[["R2"]],
                                          model_cb$stats[["R2"]],
                                          model_cbi$stats[["R2"]],
                                          model_cbim$stats[["R2"]]))
    
    list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                          aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers")), y = R2, 
                                              fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(j, " (n = ", unique(all_models_rsqrs$N), ")"))+  
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, I_COLOR, M_COLOR, "grey60"))+
      labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
      geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(1), vjust = 0, size = 5)
    
    
    all_models_aucs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Imaging", "Modifiers"), 2)), 
                                  N = rep(length(all_cc$subjectId), 8), 
                                  Color = c("Clinical", "Biomarkers", "Imaging", "Modifiers", rep("Cumulative", 4)),
                                  AUC = c(model_clinical$stats[["C"]], 
                                          model_biomarkers$stats[["C"]],
                                          model_imaging$stats[["C"]],
                                          model_modifiers$stats[["C"]],
                                          model_clinical$stats[["C"]],
                                          model_cb$stats[["C"]],
                                          model_cbi$stats[["C"]],
                                          model_cbim$stats[["C"]]))
    
    list_plots_auc[[counter_r2]] <- ggplot(all_models_aucs, 
                                           aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers")), y = AUC, 
                                               fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(j, " (n = ", unique(all_models_aucs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, I_COLOR, M_COLOR, "grey60"))+
      labs(y = "AUC", x ="") + coord_cartesian(ylim=c(0.5, 1))+
      geom_text(aes(label = format(round(AUC, 2), nsmall = 2), y = AUC + 0.05, fontface = "bold"), position = position_dodge(1), vjust = 0, size = 5)
    counter_r2 <- counter_r2 + 1
    
    if(j %in% "GOSE score at 6 months" | all(table(all_cc[, c("mms", j)]) > 10)) {
      
      for(k in c("Mild", "Moderate", "Severe")) {
        
        refit_clinical <- lrm(as.formula(paste0("`", j, "` ~ temp_c")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_biomarkers <- lrm(as.formula(paste0("`", j, "` ~ temp_b")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_modifiers <- lrm(as.formula(paste0("`", j, "` ~ temp_m")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_imaging <- lrm(as.formula(paste0("`", j, "` ~ temp_i")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_cb <- lrm(as.formula(paste0("`", j, "` ~ temp_cb")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_cbi <- lrm(as.formula(paste0("`", j, "` ~ temp_cbi")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_cbim <- lrm(as.formula(paste0("`", j, "` ~ temp_cbim")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)

        all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Imaging", "Modifiers"), 2)), 
                                       N = rep(length(all_cc[all_cc$mms %in% k,]$subjectId), 8), 
                                       Color = c("Clinical", "Biomarkers", "Imaging", "Modifiers", rep("Cumulative", 4)),
                                       R2 = c(refit_clinical$stats[["R2"]], 
                                              refit_biomarkers$stats[["R2"]],
                                              refit_imaging$stats[["R2"]],
                                              refit_modifiers$stats[["R2"]],
                                              refit_clinical$stats[["R2"]],
                                              refit_cb$stats[["R2"]],
                                              refit_cbi$stats[["R2"]],
                                              refit_cbim$stats[["R2"]]))
        
        list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                              aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers")), y = R2, 
                                                  fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
          geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none")  + ggtitle(paste0(j, " in ", k, " TBI subgroup (n = ", unique(all_models_rsqrs$N), ")")) + 
          scale_fill_manual("", values = c(C_COLOR, B_COLOR, I_COLOR, M_COLOR, "grey60"))+
          labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
          geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
        
        all_models_aucs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Imaging", "Modifiers"), 2)), 
                                      N = rep(length(all_cc[all_cc$mms %in% k,]$subjectId), 8), 
                                      Color = c("Clinical", "Biomarkers", "Imaging", "Modifiers", rep("Cumulative", 4)),
                                      AUC = c(refit_clinical$stats[["C"]], 
                                              refit_biomarkers$stats[["C"]],
                                              refit_imaging$stats[["C"]],
                                              refit_modifiers$stats[["C"]],
                                              refit_clinical$stats[["C"]],
                                              refit_cb$stats[["C"]],
                                              refit_cbi$stats[["C"]],
                                              refit_cbim$stats[["C"]]))
        
        list_plots_auc[[counter_r2]] <- ggplot(all_models_aucs, 
                                               aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers")), y = AUC, 
                                                   fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
          geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none")  + ggtitle(paste0(j, " in ", k, " TBI subgroup (n = ", unique(all_models_aucs$N), ")")) + 
          scale_fill_manual("", values = c(C_COLOR, B_COLOR, I_COLOR, M_COLOR, "grey60"))+
          labs(y = "AUC", x ="") + coord_cartesian(ylim=c(0.5, 1))+
          geom_text(aes(label = format(round(AUC, 2), nsmall = 2), y = AUC + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
        counter_r2 <- counter_r2 + 1   
      }
    }else {
      print(counter_r2)
    }
    
  
    
  } else {
    all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Modifiers"), 2)), 
                                   N = rep(length(all_cc$subjectId), 6), 
                                   Color = c("Clinical", "Biomarkers", "Modifiers", rep("Cumulative", 3)),
                                   R2 = c(model_clinical$stats[["R2"]], 
                                          model_biomarkers$stats[["R2"]],
                                          model_modifiers$stats[["R2"]],
                                          model_clinical$stats[["R2"]],
                                          model_cb$stats[["R2"]],
                                          model_cbm$stats[["R2"]]))
    
    print(anova(model_cbm_int_m, test = 'LR'))
    
    list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                          aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Modifiers")), y = R2, 
                                              fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(j, " (n = ", unique(all_models_rsqrs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, M_COLOR, "grey60"))+
      labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
      geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(1), vjust = 0, size = 5)
    
    all_models_aucs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Modifiers"), 2)), 
                                  N = rep(length(all_cc$subjectId), 6), 
                                  Color = c("Clinical", "Biomarkers", "Modifiers", rep("Cumulative", 3)),
                                  AUC = c(model_clinical$stats[["C"]], 
                                          model_biomarkers$stats[["C"]],
                                          model_modifiers$stats[["C"]],
                                          model_clinical$stats[["C"]],
                                          model_cb$stats[["C"]],
                                          model_cbm$stats[["C"]]))
    
    list_plots_auc[[counter_r2]] <- ggplot(all_models_aucs, 
                                           aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Modifiers")), y = AUC, 
                                               fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(j, " (n = ", unique(all_models_aucs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, M_COLOR, "grey60"))+
      labs(y = "AUC", x ="") + coord_cartesian(ylim=c(0.5, 1))+
      geom_text(aes(label = format(round(AUC, 2), nsmall = 2), y = AUC + 0.05, fontface = "bold"), position = position_dodge(1), vjust = 0, size = 5)
    counter_r2 <- counter_r2 + 1
    
    if(all(table(all_cc[, c("mms", j)]) > 10)) {
      for(k in c("Mild", "Moderate", "Severe")) {
        refit_clinical <- lrm(as.formula(paste0("`", j, "` ~ temp_c")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_biomarkers <- lrm(as.formula(paste0("`", j, "` ~ temp_b")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_modifiers <- lrm(as.formula(paste0("`", j, "` ~ temp_m")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_cb <- lrm(as.formula(paste0("`", j, "` ~ temp_cb")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_cbm <- lrm(as.formula(paste0("`", j, "` ~ temp_cbm")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        refit_cbm_int_m <- lrm(as.formula(paste0("`", j, "` ~ temp_cbm_int_m")), data = all_cc[all_cc$mms %in% k,], x = T, y = T, tol = 1e-10)
        
        all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Modifiers"), 2)), 
                                       N = rep(length(all_cc[all_cc$mms %in% k,]$subjectId), 6), 
                                       Color = c("Clinical", "Biomarkers", "Modifiers", rep("Cumulative", 3)),
                                       R2 = c(refit_clinical$stats[["R2"]], 
                                              refit_biomarkers$stats[["R2"]],
                                              refit_modifiers$stats[["R2"]],
                                              refit_clinical$stats[["R2"]],
                                              refit_cb$stats[["R2"]],
                                              refit_cbm$stats[["R2"]]))
        
        list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                              aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Modifiers")), y = R2, 
                                                  fill = factor(Color, levels = c("Clinical", "Biomarkers", "Modifiers", "Cumulative")))) +  
          geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none")  + ggtitle(paste0(j, " in ", k, " TBI subgroup (n = ", unique(all_models_rsqrs$N), ")")) + 
          scale_fill_manual("", values = c(C_COLOR, B_COLOR, M_COLOR, "grey60"))+
          labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
          geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
        
        all_models_aucs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Modifiers"), 2)), 
                                      N = rep(length(all_cc[all_cc$mms %in% k,]$subjectId), 6), 
                                      Color = c("Clinical", "Biomarkers", "Modifiers", rep("Cumulative", 3)),
                                      AUC = c(refit_clinical$stats[["C"]], 
                                              refit_biomarkers$stats[["C"]],
                                              refit_modifiers$stats[["C"]],
                                              refit_clinical$stats[["C"]],
                                              refit_cb$stats[["C"]],
                                              refit_cbm$stats[["C"]]))
        
        list_plots_auc[[counter_r2]] <- ggplot(all_models_aucs, 
                                               aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Modifiers")), y = AUC, 
                                                   fill = factor(Color, levels = c("Clinical", "Biomarkers", "Modifiers", "Cumulative")))) +  
          geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none")  + ggtitle(paste0(j, " in ", k, " TBI subgroup (n = ", unique(all_models_aucs$N), ")")) + 
          scale_fill_manual("", values = c(C_COLOR, B_COLOR, M_COLOR, "grey60"))+
          labs(y = "AUC", x ="") + coord_cartesian(ylim=c(0.5, 1))+
          geom_text(aes(label = format(round(AUC, 2), nsmall = 2), y = AUC + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
        counter_r2 <- counter_r2 + 1
      }
    }else {
      print(counter_r2)
    }
    
  }
}

emf(file = "EXPORT/FigS4_Panel1.emf", emfPlus = FALSE, width = 6, height = 15)
plot(ggarrange(plotlist = list_plots_r2[c(1, 5, 6, 7, 11)], common.legend = T, ncol = 1, legend = "none"))
dev.off()

emf(file = "EXPORT/FigS4_Panel3.emf", emfPlus = FALSE, width = 6, height = 12)
plot(ggarrange(plotlist = list_plots_r2[c(15, 19, 23, 27)], common.legend = T, ncol = 1, legend = "none"))
dev.off()

pdf(paste0("EXPORT/FigS4.pdf"), width = 6, height = 12)
plot(ggarrange(plotlist = list_plots_r2[c(1, 5, 6, 7, 11)], common.legend = T, ncol = 1, legend = "none"))
plot(ggarrange(plotlist = list_plots_r2[c(15, 19, 23, 27)], common.legend = T, ncol = 1, legend = "none"))
dev.off()

### FIT, EXTRACT INFO + LRT -----------------------------------------------------------------

list_plots_r2 <- list()
counter_r2 <- 1

for (j in endpoints[1:9]) {
  
  # select complete cases for each endpoint
  all_cc <- all_original_data[complete.cases(all_original_data[, c(clinical, biomarkers, imaging, modifiers, j)]),]
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  # rename columns to avoid Error in X[, mmcolnames, drop = FALSE] : subscript out of bounds
  
  all_cc <- all_cc %>% 
    rename(
      "Any_abnormality" = "Any abnormality", 
      "Skull_fracture" = "Skull fracture", 
      "Epidural_hematoma" = "Epidural hematoma", 
      "Subdural_hematoma" = "Subdural hematoma", 
      "Contusion_or_ICH" = "Contusion or ICH",
      "Mass_effect" = "Mass effect", 
      "Total_lesion_volume_25" = "Total lesion volume >= 25",
      "Mechanism_of_injury" = "Mechanism of injury", 
      "Major_extracranial_injury" = "Major extracranial injury", 
      "Accidental_cause" = "Accidental cause",
      "Medical_history" = "Medical history",
      "ASAPS_class" = "ASAPS class", 
      "Psychiatric_history" = "Psychiatric history",
      "Developmental_history" = "Developmental history",
      "TBI_history" = "TBI history",
      "Employment_status_Job_classification" = "Employment status Job classification",
      "Highest_level_of_education" = "Highest level of education")
  
  # C PILLAR
  model_clinical <- lrm(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == j, ]$RCS == 1, 
                                                                            c_formula_gcs_rcs, c_formula_gcs_lin))), data = all_cc, x = T, y = T)
  all_cc$temp_c <- model_clinical$linear.predictors
  
  # B PILLAR
  model_biomarkers <- lrm(as.formula(paste0("`", j, "` ~ (", 
                                            ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                            ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, x = T, y = T)
  all_cc$temp_b <- model_biomarkers$linear.predictors
  
  # I PILLAR
  if (!j == "Detectable intracranial injury on CT early") {
    
    model_imaging <- lrm(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), data = all_cc, x = T, y = T)
    all_cc$temp_i <- model_imaging$linear.predictors
  } else {
    model_imaging <- NULL
    all_cc$temp_i <- NA
  }
  
  # M PILLAR
  model_modifiers <- lrm(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+`Seizures`+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                           ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == j, ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                           "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), data = all_cc, x = T, y = T)
  all_cc$temp_m <- model_modifiers$linear.predictors
  
  ## CUMULATIVE MODELS, plot redistribution of predicted rish
  
  # CB 
  model_cb <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_b")), data = all_cc, x = T, y = T)
  all_cc$temp_cb <- model_cb$linear.predictors
  # CBM
  model_cbm <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), data = all_cc, x = T, y = T)
  all_cc$temp_cbm <- model_cbm$linear.predictors
  
  if (!j == "Detectable intracranial injury on CT early") {
    # CBI 
    model_cbi <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i")), data = all_cc, x = T, y = T)
    all_cc$temp_cbi <- model_cbi$linear.predictors
    # CBIM 
    model_cbim <- lrm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), data = all_cc, x = T, y = T)
    all_cc$temp_cbim <- model_cbim$linear.predictors
    
    all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Imaging", "Modifiers"), 2)), 
                                   N = rep(length(all_cc$subjectId), 8), 
                                   Color = c("Clinical", "Biomarkers", "Imaging", "Modifiers", rep("Cumulative", 4)),
                                   R2 = c(model_clinical$stats[["R2"]], 
                                          model_biomarkers$stats[["R2"]],
                                          model_imaging$stats[["R2"]],
                                          model_modifiers$stats[["R2"]],
                                          model_clinical$stats[["R2"]],
                                          model_cb$stats[["R2"]],
                                          model_cbi$stats[["R2"]],
                                          model_cbim$stats[["R2"]]))
    all_models_rsqrs$LRT <- c("",  "", "", "", "",
                              ifelse(anova(model_cb, test = 'LR')[2,3] <0.05, "*", ""),
                              ifelse(anova(model_cbi, test = 'LR')[3,3] <0.05, "*", ""),
                              ifelse(anova(model_cbim, test = 'LR')[4,3] <0.05, "*", ""))
    print(anova(model_cb, test = 'LR'))
    print(anova(model_cbi, test = 'LR'))
    print(anova(model_cbim, test = 'LR'))
    
    list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                          aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers")), y = R2, 
                                              fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(j, " (n = ", unique(all_models_rsqrs$N), ")"))+  
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, I_COLOR, M_COLOR, "grey60"))+
      labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
      geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0, size = 5)+
      geom_text(aes(label = LRT, y = R2+0.1, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
    counter_r2 <- counter_r2 + 1
  } else {
    
    all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Modifiers"), 2)), 
                                   N = rep(length(all_cc$subjectId), 6), 
                                   Color = c("Clinical", "Biomarkers", "Modifiers", rep("Cumulative", 3)),
                                   R2 = c(model_clinical$stats[["R2"]], 
                                          model_biomarkers$stats[["R2"]],
                                          model_modifiers$stats[["R2"]],
                                          model_clinical$stats[["R2"]],
                                          model_cb$stats[["R2"]],
                                          model_cbm$stats[["R2"]]))
    
    all_models_rsqrs$LRT <- c("", "", "", "",
                              ifelse(anova(model_cb, test = 'LR')[2,3] <0.05, "*", ""),
                              ifelse(anova(model_cbm, test = 'LR')[3,3] <0.05, "*", ""))
    print(anova(model_cb, test = 'LR'))
    print(anova(model_cbm, test = 'LR'))
    
    list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                          aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Modifiers")), y = R2, 
                                              fill = factor(Color, levels = c("Clinical", "Biomarkers", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(j, " (n = ", unique(all_models_rsqrs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, M_COLOR, "grey60"))+
      labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
      geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0, size = 5)+
      geom_text(aes(label = LRT, y = R2+0.1, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
    
    counter_r2 <- counter_r2 + 1
    
  }
}

emf(file = "EXPORT/FigS4_Panel1.emf", emfPlus = FALSE, width = 6, height = 15)
plot(ggarrange(plotlist = list_plots_r2[c(1:5)], common.legend = T, ncol = 1, legend = "none"))
dev.off()

emf(file = "EXPORT/FigS4_Panel3.emf", emfPlus = FALSE, width = 6, height = 12)
plot(ggarrange(plotlist = list_plots_r2[c(6:9)], common.legend = T, ncol = 1, legend = "none"))
dev.off()


### SUNBURST PLOTS ----------------------------------------------------------

list_plots_sun <- list()
counter <- 1
supp_table <- data.frame(Outcome = c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
                                     "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL"), 
                         CBIM = NA, C = NA, B = NA,  I = NA, M = NA)

for (j in c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
            "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL")) {
  
  # select complete cases for each endpoint
  all_cc <- all_original_data[complete.cases(all_original_data[, c(clinical, biomarkers, imaging, modifiers, j)]),]
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  # rename columns to avoid Error in X[, mmcolnames, drop = FALSE] : subscript out of bounds
  all_cc <- all_cc %>% 
    rename(
      "Any_abnormality" = "Any abnormality", 
      "Skull_fracture" = "Skull fracture", 
      "Epidural_hematoma" = "Epidural hematoma", 
      "Subdural_hematoma" = "Subdural hematoma", 
      "Contusion_or_ICH" = "Contusion or ICH",
      "Mass_effect" = "Mass effect", 
      "Total_lesion_volume_25" = "Total lesion volume >= 25",
      "Mechanism_of_injury" = "Mechanism of injury", 
      "Major_extracranial_injury" = "Major extracranial injury", 
      "Accidental_cause" = "Accidental cause",
      "Medical_history" = "Medical history",
      "ASAPS_class" = "ASAPS class", 
      "Psychiatric_history" = "Psychiatric history",
      "Developmental_history" = "Developmental history",
      "TBI_history" = "TBI history",
      "Employment_status_Job_classification" = "Employment status Job classification",
      "Highest_level_of_education" = "Highest level of education")
  
  # C PILLAR
  model_clinical <- glm(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == j, ]$RCS == 1, 
                                                                            c_formula_gcs_rcs, c_formula_gcs_lin))), data = all_cc, family = "binomial")
  all_cc$temp_c <- model_clinical$linear.predictors
  
  # B PILLAR
  model_biomarkers <- glm(as.formula(paste0("`", j, "` ~ (", 
                                            ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                            ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial")
  all_cc$temp_b <- model_biomarkers$linear.predictors
  
  # M PILLAR
  model_modifiers <- glm(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+`Seizures`+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                           ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == j, ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                           "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), data = all_cc, family = "binomial")
  all_cc$temp_m <- model_modifiers$linear.predictors
  
  # I PILLAR
  if (!j == "Detectable intracranial injury on CT early") {
    model_imaging <- glm(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), data = all_cc, family = "binomial")
    all_cc$temp_i <- model_imaging$linear.predictors
    
    model_cbim <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), data = all_cc, family = "binomial")
    print(round(rsq.partial(model_cbim, type = 'n')$partial.rsq, 4))
    data <- data.frame(
      name = c(" ", "CBIM", "C", "B", "I", "M", " "),
      value = c(1-rsq::rsq(model_cbim, type = 'n'), 
                rsq::rsq(model_cbim, type = 'n'), 
                rsq::rsq.partial(model_cbim, type = 'n')$partial.rsq, 
                1 - sum(rsq::rsq.partial(model_cbim, type = 'n')$partial.rsq)),
      level = c(rep(1, 2), rep(2, 5)), 
      fill = c(" ", "CBIM", "C", "B", "I", "M", " ")
    )
    data$level <- as.factor(data$level)
    data$fill  <- factor(data$fill, levels = c(" ", "CBIM", "M", "I", "B", "C"))
    data$name <- as.factor(data$name)
    data[data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
    
    
    
    
    list_plots_sun[[counter]] <- data %>%  
      ggplot(aes(x = level, y = value, fill = fill)) +
      geom_col(width = c(rep(1.4, 2), rep(0.6, 5)), position = position_stack()) +
      geom_text(aes(label = name), size = 4, color = "white", position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y", direction = 1, clip = "off") +
      scale_fill_manual(values = c(NA, "black", M_COLOR, I_COLOR, B_COLOR, C_COLOR), na.translate = F) +
      theme_minimal() + 
      labs(x = NULL, y = NULL) +
      theme(axis.text.y = element_blank(), legend.position = "none", axis.text.x = element_text(face = "bold"))
    
    supp_table[counter, "CBIM"] <- round(data[data$fill =="CBIM", "value"], 4)
    supp_table[counter, "C"] <- round(data[data$fill =="C", "value"], 4)
    supp_table[counter, "B"] <- round(data[data$fill =="B", "value"], 4)
    supp_table[counter, "I"] <- round(data[data$fill =="I", "value"], 4)
    supp_table[counter, "M"] <- round(data[data$fill =="M", "value"], 4)
    
    counter <- counter + 1
    
  } else {
    model_cbm <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), data = all_cc, family = "binomial")
    print(round(rsq.partial(model_cbm, type = 'n')$partial.rsq, 4))
    data <- data.frame(
      name = c(" ", "CBIM", "C", "B", "I", "M", " "),
      value = c(1-rsq::rsq(model_cbm, type = 'n'), rsq::rsq(model_cbm, type = 'n'), 
                rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq[1:2], 0,
                rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq[3],
                1 - sum(rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq)),
      level = c(1, 1, 2, 2, 2, 2, 2), 
      fill = c(" ", "CBIM", "C", "B", "I", "M", " ")
    )
    data$level <- as.factor(data$level)
    data$fill  <- factor(data$fill, levels = c(" ", "CBIM", "M", "I", "B", "C"))
    data$name <- as.factor(data$name)
    data[data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
    
    list_plots_sun[[counter]] <- data %>%  
      ggplot(aes(x = level, y = value, fill = fill)) +
      geom_col(width = c(rep(1.4, 2), rep(0.6, 5)), position = position_stack()) +
      geom_text(aes(label = name), size = 4, color = "white", position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y", direction = 1, clip = "off") +
      scale_fill_manual(values = c(NA, "black", M_COLOR, I_COLOR, B_COLOR, C_COLOR), na.translate = F) +
      theme_minimal() + 
      labs(x = NULL, y = NULL) +
      theme(axis.text.y = element_blank(), legend.position = "none", axis.text.x = element_text(face = "bold"))
    
    supp_table[counter, "CBIM"] <- round(data[data$fill =="CBIM", "value"], 4)
    supp_table[counter, "C"] <- round(data[data$fill =="C", "value"], 4)
    supp_table[counter, "B"] <- round(data[data$fill =="B", "value"], 4)
    supp_table[counter, "M"] <- round(data[data$fill =="M", "value"], 4)
    
    counter <- counter + 1
    
  }
}

write.xlsx(supp_table, "EXPORT/Supp_tb_2_CC.xlsx")

list_plots_sun_p <- list()
counter <- 1

# WITHIN PILLARS
for (j in c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
            "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL")) {
  
  # select complete cases for each endpoint
  all_cc <- all_original_data[complete.cases(all_original_data[, c(clinical, biomarkers, imaging, modifiers, j)]),]
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  # rename columns to avoid Error in X[, mmcolnames, drop = FALSE] : subscript out of bounds
  all_cc <- all_cc %>% 
    rename(
      "Any_abnormality" = "Any abnormality", 
      "Skull_fracture" = "Skull fracture", 
      "Epidural_hematoma" = "Epidural hematoma", 
      "Subdural_hematoma" = "Subdural hematoma", 
      "Contusion_or_ICH" = "Contusion or ICH",
      "Mass_effect" = "Mass effect", 
      "Total_lesion_volume_25" = "Total lesion volume >= 25",
      "Mechanism_of_injury" = "Mechanism of injury", 
      "Major_extracranial_injury" = "Major extracranial injury", 
      "Accidental_cause" = "Accidental cause",
      "Medical_history" = "Medical history",
      "ASAPS_class" = "ASAPS class", 
      "Psychiatric_history" = "Psychiatric history",
      "Developmental_history" = "Developmental history",
      "TBI_history" = "TBI history",
      "Employment_status_Job_classification" = "Employment status Job classification",
      "Highest_level_of_education" = "Highest level of education")
  
  # C PILLAR
  model_clinical <- glm(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == j, ]$RCS == 1, 
                                                                            c_formula_gcs_rcs, c_formula_gcs_lin))), data = all_cc, family = "binomial")
  
  data <- data.frame(
    name = c(" ", "C", "Pupils", "GCS", " "),
    value = c(1-rsq::rsq(model_clinical, type = 'n'), 
              rsq::rsq(model_clinical, type = 'n'), 
              rsq::rsq.partial(model_clinical, type = 'n')$partial.rsq, 
              1 - sum(rsq::rsq.partial(model_clinical, type = 'n')$partial.rsq)),
    level = c(rep(1, 2), rep(2, 3)), 
    fill = c(" ", "C", "C", "C", " "))
  data$level <- as.factor(data$level)
  data$fill  <- factor(data$fill, levels = c(" ", "C"))
  data$name <- as.factor(data$name)
  data[data$level == "1" | data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
  
  list_plots_sun_p[[counter]] <- data %>%  
    ggplot(aes(x = level, y = value, fill = fill, alpha = level)) +
    geom_col(width = c(rep(1.4, 2), rep(0.6, 3)), color = "white", position = position_stack()) +
    geom_label_repel(aes(label = name), size = 4, color = "black", fontface = "bold",
                     position = position_stacknudge(vjust = 0), label.size = NA) +
    coord_polar(theta = "y", direction = 1, clip = "off") + 
    scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = 0.5), guide = F) +
    scale_fill_manual(values = c(NA, C_COLOR), na.translate = F, guide = F) +
    theme_minimal() + 
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank(), legend.position = "none", axis.text.x = element_text(face = "bold"))
  counter <- counter + 1
  
  # B PILLAR
  model_biomarkers <- glm(as.formula(paste0("`", j, "` ~ (", 
                                            ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                            ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial")
  
  data <- data.frame(
    name = c(" ", "B", "GFAP", "UCHL1", "S100B", "Time", " "),
    value = c(1-rsq(model_biomarkers, type = 'n'), 
              rsq(model_biomarkers, type = 'n'), 
              rsq.partial(model_biomarkers, type = 'n', objR = glm(as.formula(paste0("`", j, "` ~ (", 
                                                                                     ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                                                     ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                                                     ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial"))$partial.rsq,
              rsq.partial(model_biomarkers, type = 'n', objR = glm(as.formula(paste0("`", j, "` ~ (", 
                                                                                     ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                                                     ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                                                     ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial"))$partial.rsq,
              rsq.partial(model_biomarkers, type = 'n', objR = glm(as.formula(paste0("`", j, "` ~ (", 
                                                                                     ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                                                     ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), ") *",
                                                                                     ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial"))$partial.rsq,
              rsq.partial(model_biomarkers, type = 'n', objR = glm(as.formula(paste0("`", j, "` ~ ", 
                                                                                     ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                                                     ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                                                     ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin))), data = all_cc, family = "binomial"))$partial.rsq,
              1 - sum(rsq.partial(model_biomarkers, type = 'n', objR = glm(as.formula(paste0("`", j, "` ~ (", 
                                                                                             ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                                                             ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                                                             ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial"))$partial.rsq,
                      rsq.partial(model_biomarkers, type = 'n', objR = glm(as.formula(paste0("`", j, "` ~ (", 
                                                                                             ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                                                             ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                                                             ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial"))$partial.rsq,
                      rsq.partial(model_biomarkers, type = 'n', objR = glm(as.formula(paste0("`", j, "` ~ (", 
                                                                                             ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                                                             ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), ") *",
                                                                                             ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial"))$partial.rsq,
                      rsq.partial(model_biomarkers, type = 'n', objR = glm(as.formula(paste0("`", j, "` ~ ", 
                                                                                             ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                                                             ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                                                             ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin))), data = all_cc, family = "binomial"))$partial.rsq)),
    level = c(rep(1, 2), rep(2, 5)), 
    fill = c(" ", "B", "B", "B", "B", "B", " "))
  data$level <- as.factor(data$level)
  data$fill  <- factor(data$fill, levels = c(" ", "B"))
  data$name <- as.factor(data$name)
  data[data$level == "1" | data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
  
  list_plots_sun_p[[counter]] <- data %>%  
    ggplot(aes(x = level, y = value, fill = fill, alpha = level)) +
    geom_col(width = c(rep(1.4, 2), rep(0.6, 5)), color = "white", position = position_stack()) +
    geom_label_repel(aes(label = name), size = 4, color = "black", fontface = "bold",
                     position = position_stacknudge(vjust = 0), label.size = NA) +
    coord_polar(theta = "y", direction = 1, clip = "off") + 
    scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = 0.5), guide = F) +
    scale_fill_manual(values = c(NA, B_COLOR), na.translate = F) +
    theme_minimal() + 
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank(), legend.position = "none", axis.text.x = element_text(face = "bold"))
  counter <- counter + 1
  
  # I PILLAR
  if (!j == "Detectable intracranial injury on CT early") {
    model_imaging <- glm(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), data = all_cc, family = "binomial")
    
    data <- data.frame(
      name = c(" ", "I", "Any", "Fr", "EDH", "SDH",  "TSAH", "Con", "TAMVI", "IVH", "Mass", "Vol", " "),
      value = c(1-rsq::rsq(model_imaging, type = 'n'), 
                rsq::rsq(model_imaging, type = 'n'), 
                rsq::rsq.partial(model_imaging, type = 'n')$partial.rsq, 
                1 - sum(rsq::rsq.partial(model_imaging, type = 'n')$partial.rsq)),
      level = c(rep(1, 2), rep(2, 11)), 
      fill = c(" ", rep("I", 11), " "))
    data$level <- as.factor(data$level)
    data$fill  <- factor(data$fill, levels = c(" ", "I"))
    data$name <- as.factor(data$name)
    data[data$level == "1" | data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
    
    list_plots_sun_p[[counter]] <- data %>% ggplot(aes(x = level, y = value, fill = fill, alpha = level)) +
      geom_col(width = c(rep(1.4, 2), rep(0.6, 11)), color = "white", position = position_stack()) +
      coord_polar(theta = "y", direction = 1, clip = "off") + 
      geom_label_repel(aes(label = name), size = 4, color = "black", fontface = "bold",
                       position = position_stacknudge(vjust = 0),label.size = NA) +
      scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = 0.5), guide = F) +
      scale_fill_manual(values = c(NA, I_COLOR), na.translate = F, guide = F) +
      theme_minimal() + 
      labs(x = NULL, y = NULL) +
      theme(axis.text.y = element_blank(), legend.position = "none", axis.text.x = element_text(face = "bold"))
    counter <- counter + 1
  } else {    list_plots_sun_p[[counter]] <- NA
  counter <- counter + 1
  
  }
  
  # M PILLAR
  model_modifiers <- glm(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+`Seizures`+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                           ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == j, ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                           "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), data = all_cc, family = "binomial")
  
  data <- data.frame(
    name = c(" ", "M", "Mech", "Seiz", "MEI", "HypO2", "HypoT", "Int", "Age", "Sex", "MedHx", "Psych", "Dev", "TBIHx", " "),
    value = c(1-rsq::rsq(model_modifiers, type = 'n'), 
              rsq::rsq(model_modifiers, type = 'n'), 
              rsq::rsq.partial(model_modifiers, type = 'n')$partial.rsq, 
              1 - sum(rsq::rsq.partial(model_modifiers, type = 'n')$partial.rsq)),
    level = c(rep(1, 2), rep(2, 13)), 
    fill = c(" ", rep("M", 13), " "))
  data$level <- as.factor(data$level)
  data$fill  <- factor(data$fill, levels = c(" ", "M"))
  data$name <- as.factor(data$name)
  data[data$level == "1" | data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
  
  list_plots_sun_p[[counter]] <- data %>% ggplot(aes(x = level, y = value, fill = fill, alpha = level)) +
    geom_col(width = c(rep(1.4, 2), rep(0.6, 13)), color = "white", position = position_stack()) +
    coord_polar(theta = "y", direction = 1, clip = "off") + 
    geom_label_repel(aes(label = name), size = 4, color = "black", fontface = "bold",
                     position = position_stacknudge(vjust = 0),label.size = NA) +
    scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = 0.5), guide = F) +
    scale_fill_manual(values = c(NA, M_COLOR), na.translate = F, guide = F) +
    theme_minimal() + 
    labs(x = NULL, y = NULL) +
    theme(axis.text.y = element_blank(), legend.position = "none", axis.text.x = element_text(face = "bold"))
  counter <- counter + 1
  
}

pdf("EXPORT/FigS5_Panel1.pdf", height = 20, width = 9)
ggarrange(plotlist = list_plots_sun_p[c(1:36)], ncol = 4, nrow = 9)
dev.off()

emf(file = "EXPORT/FigS5_Panel1.emf", emfPlus = T, height = 20, width = 9)
ggarrange(plotlist = list_plots_sun_p[c(1:36)], ncol = 4, nrow = 9)
dev.off()

# IMPUTATION --------------------------------------------------------------
dti_for_imp <- all_original_data

# Fix names for mice
names(dti_for_imp) <- gsub(" ", "_", names(dti_for_imp))
dti_for_imp$`Total_lesion_volume_25` <- dti_for_imp$`Total_lesion_volume_>=_25`
dti_for_imp$`Total_lesion_volume_>=_25` <- NULL

# define ordered factors
dti_for_imp$Seizures <- factor(dti_for_imp$Seizures, levels = levels(dti_for_imp$Seizures), ordered = T) 
dti_for_imp$lesions_classification_morris_marshall_classification <- factor(dti_for_imp$lesions_classification_morris_marshall_classification, levels = levels(dti_for_imp$lesions_classification_morris_marshall_classification), ordered = T) 
dti_for_imp$lesions_classification_marshall_ct_classification <- factor(dti_for_imp$lesions_classification_marshall_ct_classification, levels = levels(dti_for_imp$lesions_classification_marshall_ct_classification), ordered = T) 
dti_for_imp$ASAPS_class <- factor(dti_for_imp$ASAPS_class, levels = levels(dti_for_imp$ASAPS_class), ordered = T) 
dti_for_imp$Subject.GOSE3monthEndpointDerived <- factor(dti_for_imp$Subject.GOSE3monthEndpointDerived, levels = levels(dti_for_imp$Subject.GOSE3monthEndpointDerived), ordered = T) 
dti_for_imp$GOSE_score_at_6_months <- factor(dti_for_imp$GOSE_score_at_6_months, levels = levels(dti_for_imp$GOSE_score_at_6_months), ordered = T) 

# study missingness
naniar::gg_miss_var(dti_for_imp, show_pct = TRUE)  
#vis_miss(dti_for_imp, cluster = T)
flux(dti_for_imp) # if very low outflux (<0.5) - don't use in imputation, keep in dataset
fluxplot(dti_for_imp)

#### AUXILIARY VARIABLES WITH HIGH MISSINGNESS, LOW OUTFLUX -> Remove from dataset 
row.names(flux(dti_for_imp))[flux(dti_for_imp)$outflux < 0.5]
# remove the next biomarker measurement 
dti_for_imp <- dti_for_imp[, c(setdiff(names(dti_for_imp), 
                                       c("next_earliest_bbb_sampling_time", 
                                         "lognext_earliest_GFAP", "lognext_earliest_S100B", 
                                         "lognext_earliest_UCH.L1", "lognext_earliest_NFL", 
                                         "lognext_earliest_NSE", "lognext_earliest_Tau")))]
fluxplot(dti_for_imp)

#### REMOVE REDUNDANT VARIABLES
dti_for_imp$TBI_Severity <- NULL

table(dti_for_imp$lesions_classification_fisher_classication, dti_for_imp$TSAH)
dti_for_imp$lesions_classification_fisher_classication <- NULL

table(dti_for_imp$lesions_classification_morris_marshall_classification, dti_for_imp$TSAH)

#### UNDEFINED VARIABLES 

## ICP monitoring if not ICU
summary(dti_for_imp$ICP_monitoring)
length(dti_for_imp[is.na(dti_for_imp$ICP_monitoring) & dti_for_imp$ICU %in% c("No"), ]$ICP_monitoring)
dti_for_imp[is.na(dti_for_imp$ICP_monitoring) & dti_for_imp$ICU %in% c("No"), ]$ICP_monitoring <- "No"
summary(dti_for_imp$ICP_monitoring)

## HRQoL for deceased -> exclude from imputation
fluxplot(dti_for_imp)

# Reorder columns (continuous before binary, raw before derived)
dti_for_imp <- dti_for_imp[, c("subjectId", "Pupils", "GCS_Score", "mms", "GCS_Eyes", "GCS_Motor", "GCS_Verbal", "InjuryHx.LOCGCSSumDet",     
                               "any_stroke_tia", "any_seizure_hx", "Seizures", 
                               "neuro_degen_MS", "InjuryHx.InjViolenceVictimAlcohol",
                               "Hypoxia", "Hypotension", 
                               "Labs.DLGlucosemmolL", "Labs.DLHemoglobingdL", 
                               "Time_to_sampling", "log_GFAP", "log_UCHL1", "log_S100B", "logNFL", "logNSE", "logTau",                                               
                               "scan_time", "Skull_fracture", 
                               "Epidural_hematoma_volume", "Epidural_hematoma",                                    
                               "subdural_hematoma_acute_total_volume_p", "subdural_hematoma_acute_count_p",                      
                               "subdural_hematoma_subacute_total_volume_p", "subdural_hematoma_subacute_count_p",
                               "subdural_hematoma_mixed_density_total_volume_p", "subdural_hematoma_mixed_density_count_p",
                               "Contusion_volume", "Contusion", 
                               "ICH_volume", "ICH", 
                               "extraaxial_hematoma_total_volume_p", "extraaxial_hematoma_count_p", 
                               "lesions_classification_morris_marshall_classification", "TSAH", "IVH", "Cisternal_compression", "Ventricular_compression", "MLS", "Brain_herniation",                                     
                               "penetrating_injury_count_p", "dai_count_p", "tai_count_p", "edema_hyperemia_ischemia_count",                       
                               "lesions_classification_marshall_ct_classification", 
                               "TAMVI", "Subdural_hematoma_volume", "Subdural_hematoma", "Total_lesion_volume", "Total_lesion_volume_25",
                               "Mass_effect", "hematoma", "EDH_mass", "oblit_3_bc", "Any_abnormality", 
                               "Mechanism_of_injury", 
                               "InjuryHx.CervicalSpineAIS", "InjuryHx.FaceAIS", "InjuryHx.ThoraxChestAIS", "InjuryHx.ThoracicSpineAIS", 
                               "InjuryHx.AbdomenPelvicContentsAIS", "InjuryHx.LumbarSpineAIS", "InjuryHx.UpperExtremitiesAIS", "InjuryHx.LowerExtremitiesAIS", 
                               "InjuryHx.PelvicGirdleAIS", "InjuryHx.ExternaAIS", "Major_extracranial_injury",
                               "InjuryHx.InjCause", "Accidental_cause", "Age", "Sex", 
                               "MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT", "MedHx.MedHxGastro",
                               "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro",
                               "MedHx.MedHxNeuroPain", "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal", 
                               "MedHx.MedHxOther", "Medical_history", "ASAPS_class", "Psychiatric_history", "Developmental_history",
                               "TBI_history", "Employment_status_Job_classification", "Years_of_education", "Subject.EduLvlUSATyp", "Highest_level_of_education", 
                               "Detectable_intracranial_injury_on_CT_early", "Hospital_admission",
                               "ICU_admission", "ICP_monitoring", 
                               "Cranial_surgery_anytime", "Cranial_surgery_within_72h",
                               "Major_cranial_surgery_anytime", "Major_cranial_surgery_within_72h",
                               "Subject.GOSE3monthEndpointDerived", "GOSE_score_at_6_months", 
                               "Mortality_at_6_months", "Unfavorable_outcome_at_6_months", "Incomplete_recovery_at_6_months", 
                               "Outcomes.QoLIBRIOSTotalScore", "Impairment_of_HRQOL")]

meth <- make.method(dti_for_imp)
pred <- make.predictorMatrix(dti_for_imp)
post <- make.post(dti_for_imp)

## HRQoL for deceased 
meth[c("subjectId", "Impairment_of_HRQOL", "Outcomes.QoLIBRIOSTotalScore", 
       "InjuryHx.CervicalSpineAIS", "InjuryHx.FaceAIS", "InjuryHx.ThoraxChestAIS", "InjuryHx.ThoracicSpineAIS", 
       "InjuryHx.AbdomenPelvicContentsAIS", "InjuryHx.LumbarSpineAIS", "InjuryHx.UpperExtremitiesAIS", "InjuryHx.LowerExtremitiesAIS", 
       "InjuryHx.PelvicGirdleAIS", "InjuryHx.ExternaAIS", "Major_extracranial_injury")] <- ""
pred[, c("subjectId", "Impairment_of_HRQOL", "Outcomes.QoLIBRIOSTotalScore")] <- 0

#### DERIVED/DEPENDENT VARIABLES                                         
## impute continuous lesion volume variables first, then binary variable: SDHs, EDH, EAH, Contusion, ICH
# mms
meth["mms"] <- paste("~", expression(ifelse((GCS_Score >= 13), "Mild", ifelse((GCS_Score <= 8), "Severe", "Moderate"))))
pred[c("GCS_Score"), "mms"] <- 0

# imaging
meth["Epidural_hematoma"] <- paste("~", expression(ifelse(Epidural_hematoma_volume == 0, "No", "Yes")))
pred[c("Epidural_hematoma_volume"), "Epidural_hematoma"] <- 0
meth["subdural_hematoma_acute_count_p"] <- paste("~", expression(ifelse(subdural_hematoma_acute_total_volume_p == 0, "No", "Yes")))
pred[c("subdural_hematoma_acute_total_volume_p"), "subdural_hematoma_acute_count_p"] <- 0
meth["subdural_hematoma_subacute_count_p"] <- paste("~", expression(ifelse(subdural_hematoma_subacute_total_volume_p == 0, "No", "Yes")))
pred[c("subdural_hematoma_subacute_total_volume_p"), "subdural_hematoma_subacute_count_p"] <- 0
meth["subdural_hematoma_mixed_density_count_p"] <- paste("~", expression(ifelse(subdural_hematoma_mixed_density_total_volume_p == 0, "No", "Yes")))
pred[c("subdural_hematoma_mixed_density_total_volume_p"), "subdural_hematoma_mixed_density_count_p"] <- 0
meth["extraaxial_hematoma_count_p"] <- paste("~", expression(ifelse(extraaxial_hematoma_total_volume_p == 0, "No", "Yes")))
pred[c("extraaxial_hematoma_total_volume_p"), "extraaxial_hematoma_count_p"] <- 0
meth["Contusion"] <- paste("~", expression(ifelse(Contusion_volume == 0, "No", "Yes")))
pred[c("Contusion_volume"), "Contusion"] <- 0
meth["ICH"] <- paste("~", expression(ifelse(ICH_volume == 0, "No", "Yes")))
pred[c("ICH_volume"), "ICH"] <- 0
meth["TSAH"] <- paste("~", expression(ifelse(lesions_classification_morris_marshall_classification %in% c("1", "2", "3", "4"), "Yes", "No")))
pred[c("lesions_classification_morris_marshall_classification"), "TSAH"] <- 0

## SDH
meth["Subdural_hematoma"] <- paste("~", expression(ifelse(subdural_hematoma_acute_count_p %in% "Yes" |                      
                                                            subdural_hematoma_subacute_count_p %in% "Yes" |                    
                                                            subdural_hematoma_mixed_density_count_p %in% "Yes", "Yes", "No")))
## SDH volume
meth["Subdural_hematoma_volume"] <- "~ I(subdural_hematoma_acute_total_volume_p + subdural_hematoma_subacute_total_volume_p + subdural_hematoma_mixed_density_total_volume_p)"
## TAMVI
meth["TAMVI"] <- paste("~", expression(ifelse(dai_count_p %in% "Yes" | tai_count_p %in% "Yes", "Yes", "No")))
## Mass Effect
meth["Mass_effect"] <- paste("~", expression(ifelse(Cisternal_compression %in% "Yes" | Ventricular_compression %in% "Yes" | MLS %in% "Yes" | Brain_herniation %in% "Yes", "Yes", "No")))
# impute as block (otherwise overparametrized -> all tend to 1)
pred[c("Cisternal_compression", "Ventricular_compression", "MLS", "Brain_herniation"), 
     c("Cisternal_compression", "Ventricular_compression", "MLS", "Brain_herniation")] <- 0
## Total volume
meth["Total_lesion_volume"] <- "~ I(subdural_hematoma_acute_total_volume_p + subdural_hematoma_subacute_total_volume_p + subdural_hematoma_mixed_density_total_volume_p +
                               Epidural_hematoma_volume + Contusion_volume + ICH_volume + extraaxial_hematoma_total_volume_p)"
## total volume >= 25
meth["Total_lesion_volume_25"] <- paste("~", expression(ifelse(Total_lesion_volume >= 25, "Yes", "No")))
## EDH mass (although technically this is at lesion level)
meth["EDH_mass"] <- paste("~", expression(ifelse(Epidural_hematoma_volume >= 25, "Yes", "No")))
## hematoma
meth["hematoma"] <- paste("~", expression(ifelse(Epidural_hematoma %in% "Yes" | Subdural_hematoma %in% "Yes" |  
                                                   Contusion %in% "Yes" | ICH %in% "Yes" | extraaxial_hematoma_count_p %in% "Yes", "Yes", "No")))

pred[, c("hematoma", "EDH_mass", "Total_lesion_volume_25", "Total_lesion_volume", "Mass_effect", 
         "TAMVI", "Subdural_hematoma_volume", "Subdural_hematoma")] <- 0
# any abnormality
meth["Any_abnormality"] <- paste("~", expression(ifelse(Skull_fracture == "Yes" | Epidural_hematoma == "Yes" | Contusion == "Yes" | ICH == "Yes" | IVH == "Yes" | 
                                                          subdural_hematoma_acute_count_p == "Yes" | subdural_hematoma_subacute_count_p == "Yes" | 
                                                          subdural_hematoma_mixed_density_count_p == "Yes" |extraaxial_hematoma_count_p == "Yes" | TSAH == "Yes" | 
                                                          Cisternal_compression == "Yes" | MLS == "Yes" | Ventricular_compression == "Yes" | Brain_herniation == "Yes" | 
                                                          penetrating_injury_count_p == "Yes" | TAMVI == "Yes" | edema_hyperemia_ischemia_count == "Yes" , "Yes", "No")))
# any intracranial abnormality
meth["Detectable_intracranial_injury_on_CT_early"] <- paste("~", expression(ifelse(Epidural_hematoma == "Yes" | Contusion == "Yes" | ICH == "Yes" | IVH == "Yes" | 
                                                                                     subdural_hematoma_acute_count_p == "Yes" | subdural_hematoma_subacute_count_p == "Yes" | 
                                                                                     subdural_hematoma_mixed_density_count_p == "Yes" |extraaxial_hematoma_count_p == "Yes" | TSAH == "Yes" | 
                                                                                     Cisternal_compression == "Yes" | MLS == "Yes" | Ventricular_compression == "Yes" | Brain_herniation == "Yes" | 
                                                                                     penetrating_injury_count_p == "Yes" | TAMVI == "Yes" | edema_hyperemia_ischemia_count == "Yes" , "Yes", "No")))
pred[c("Skull_fracture", "Epidural_hematoma_volume", "Epidural_hematoma",                                    
       "subdural_hematoma_acute_total_volume_p", "subdural_hematoma_acute_count_p",                      
       "subdural_hematoma_subacute_total_volume_p", "subdural_hematoma_subacute_count_p",
       "subdural_hematoma_mixed_density_total_volume_p", "subdural_hematoma_mixed_density_count_p",
       "Contusion_volume", "Contusion", 
       "ICH_volume", "ICH", 
       "extraaxial_hematoma_total_volume_p", "extraaxial_hematoma_count_p", 
       "TSAH", "IVH", "Cisternal_compression", "Ventricular_compression", "MLS", "Brain_herniation",                                     
       "penetrating_injury_count_p", "dai_count_p", "tai_count_p", "edema_hyperemia_ischemia_count",                       
       "lesions_classification_marshall_ct_classification", 
       "lesions_classification_morris_marshall_classification", 
       "TAMVI", "Subdural_hematoma_volume", "Subdural_hematoma", "Total_lesion_volume", "Total_lesion_volume_25",
       "Mass_effect", "hematoma", "EDH_mass", "oblit_3_bc", "Any_abnormality", "Detectable_intracranial_injury_on_CT_early"),
     c("Any_abnormality", "Detectable_intracranial_injury_on_CT_early")] <- 0

# Medical history
meth["Medical_history"] <- paste("~",  expression(ifelse(MedHx.MedHxCardio == "1" | MedHx.MedHxEndocrine == "1" | MedHx.MedHxENT == "1" | 
                                                           MedHx.MedHxGastro == "1" | MedHx.MedHxHematologic == "1" | MedHx.MedHxHepatic == "1" |  
                                                           MedHx.MedHxMusculoskeletal == "1" | MedHx.MedHxNeuro == "1" | MedHx.MedHxNeuroPain == "1" | 
                                                           MedHx.MedHxOncologic == "1" | MedHx.MedHxPulmonary == "1" |  MedHx.MedHxRenal == "1", "Yes", "No")))
pred[c("MedHx.MedHxCardio", "MedHx.MedHxEndocrine", "MedHx.MedHxENT", "MedHx.MedHxGastro",
       "MedHx.MedHxHematologic", "MedHx.MedHxHepatic", "MedHx.MedHxMusculoskeletal", "MedHx.MedHxNeuro",
       "MedHx.MedHxNeuroPain", "MedHx.MedHxOncologic", "MedHx.MedHxPulmonary", "MedHx.MedHxRenal", 
       "MedHx.MedHxOther"), "Medical_history"] <- 0

# Non-accidental cause
meth["Accidental_cause"] <- paste("~", expression(ifelse(InjuryHx.InjCause %in% c("Violence/assault", "Act of mass violence", "Suicide attempt"), "Intentional", "Accidental")))

# Highest level of education
meth["Highest_level_of_education"] <- paste("~", expression(ifelse(Subject.EduLvlUSATyp %in% c("None, not currently in school", "Primary school"), "low", "hs or higher")))

# GOSE subscales
meth["Mortality_at_6_months"] <- paste("~", expression(ifelse(GOSE_score_at_6_months == "1=Dead", "Yes", "No")))
meth["Unfavorable_outcome_at_6_months"] <- paste("~", expression(ifelse(GOSE_score_at_6_months %in% c("1=Dead", "2=VS/3=LSD", "4=USD"), "Yes", "No")))
meth["Incomplete_recovery_at_6_months"] <- paste("~", expression(ifelse(GOSE_score_at_6_months %in% c("1=Dead", "2=VS/3=LSD", "4=USD", "5=LMD", "6=UMD", "7=LGR"), "Yes", "No")))
pred[, c("Accidental_cause", "Highest_level_of_education", "Mortality_at_6_months", "Unfavorable_outcome_at_6_months", "Incomplete_recovery_at_6_months")] <- 0

## CONDITIONAL 
# oblit_3_bc
post["oblit_3_bc"] <- paste("imp[[j]][data$Ventricular_compression[!r[, j]] ==\"No\" & data$Cisternal_compression[!r[, j]] == \"No\", i] <- \"No\"")
pred[, "oblit_3_bc"] <- 0  

# cranial surgery
post["Cranial_surgery_within_72h"] <- paste("imp[[j]][data$Cranial_surgery_anytime[!r[, j]] ==\"No\", i] <- \"No\"")
post["Major_cranial_surgery_within_72h"] <- paste("imp[[j]][data$Major_cranial_surgery_anytime[!r[, j]] ==\"No\" | data$Cranial_surgery_anytime[!r[, j]] == \"No\" | data$Cranial_surgery_within_72h[!r[, j]] == \"No\", i] <- \"No\"")
post["Major_cranial_surgery_anytime"] <- paste("imp[[j]][data$Cranial_surgery_anytime[!r[, j]] ==\"No\", i] <- \"No\"")
# impute as block (otherwise overparametrized -> all tend to 1)
pred[c("Cranial_surgery_anytime", "Cranial_surgery_within_72h",
       "Major_cranial_surgery_anytime", "Major_cranial_surgery_within_72h"), 
     c("Cranial_surgery_anytime", "Cranial_surgery_within_72h",
       "Major_cranial_surgery_anytime", "Major_cranial_surgery_within_72h")] <- 0

pdf("EXPORT/PredictionMatrix.pdf", height = 15, width = 15)
ggmice::plot_pred(pred, rotate = T)
dev.off()

meth
post

# RUN imputation
if (F) {
  set.seed(21)
  dti_new <- mice(dti_for_imp, meth = meth, pred = pred, post = post, m=5, maxit = 20, printFlag = T)
  warnings()
  save(dti_new, file = "IMPORT/Imputed_NEW.RData")
  dti_new$loggedEvents  
}

load("IMPORT/Imputed_NEW.RData")
dti <- dti_new

# Recreate combined contusion + ICH variable
all_dats <- complete(dti, action = "long", include = TRUE)
table(all_dats$Contusion, all_dats$ICH)
all_dats$`Contusion_or_ICH` <- as.factor(ifelse(all_dats$Contusion %in% c("Yes") | all_dats$ICH %in% c("Yes"), "Yes", 
                                                ifelse(all_dats$Contusion %in% c("No") & all_dats$ICH %in% c("No"), "No", NA)))
table(all_dats$`Contusion_or_ICH`)

all_dats$GFAP <- NA
all_dats$UCHL1 <- NA
all_dats$S100B <- NA

for(i in 0:5){
  all_dats[all_dats$.imp  == i, ]$GFAP <- scale(all_dats[all_dats$.imp  == i, ]$log_GFAP)
  all_dats[all_dats$.imp  == i, ]$UCHL1 <- scale(all_dats[all_dats$.imp  == i, ]$log_UCHL1)
  all_dats[all_dats$.imp  == i, ]$S100B <- scale(all_dats[all_dats$.imp  == i, ]$log_S100B)
}

summary(all_dats[all_dats$.imp  == 0, ]$GFAP)
summary(all_original_data$GFAP)

combine_dats <- list()
for(i in 0:max(all_dats$.imp)) {
  combine_dats[[i+1]] <- 
    all_dats %>%
    subset(.imp == i)%>%
    arrange(.id)
}
dti <- as.mids(do.call(rbind, combine_dats))

if (F){
  dti$loggedEvents 
  ggmice::plot_pred(dti$pred, rotate = T)
  plot(dti)
  densityplot(dti, data = ~GCS_Score+GCS_Eyes+GCS_Motor+GCS_Verbal+Labs.DLGlucosemmolL+Labs.DLHemoglobingdL+Time_to_sampling+log_GFAP+log_UCHL1+                                            
                log_S100B+Epidural_hematoma_volume+subdural_hematoma_acute_total_volume_p+subdural_hematoma_subacute_total_volume_p+
                subdural_hematoma_mixed_density_total_volume_p+Contusion_volume+ICH_volume+                                        
                extraaxial_hematoma_total_volume_p+lesions_classification_morris_marshall_classification+lesions_classification_marshall_ct_classification+Subdural_hematoma_volume+                             
                Total_lesion_volume+Years_of_education+GOSE_score_at_6_months)
  
  # Sanity checks
  test_1 <- complete(dti, 1)
  test_1 <- test_1 %>% 
    rename(
      "Seizures" = "InjuryHx.EDComplEventSeizures", 
      "Hypoxia" = "InjuryHx.EDComplEventHypoxia",
      "Hypotension" = "InjuryHx.EDComplEventHypotension")
  table(test_1$Seizures)
  table(test_1$Hypoxia)
  table(test_1$Hypotension)
  table(test_1$GCS_Score, test_1$mms)
  table(test_1$InjuryHx.InjCause, test_1$Accidental_cause)
  table(test_1$Subject.EduLvlUSATyp, test_1$Highest_level_of_education)
  table(test_1$GOSE_score_at_6_months, test_1$Unfavorable_outcome_at_6_months)
  table(test_1$Cisternal_compression, test_1$oblit_3_bc)
  
  # Visual checks
  bwplot(dti, GCS_Motor)
  bwplot(dti, GCS_Score)
  bwplot(dti, log_GFAP)
  bwplot(dti, Total_lesion_volume)
  bwplot(dti, subdural_hematoma_acute_total_volume_p)
  
  xyplot(dti, subdural_hematoma_acute_total_volume_p  ~ Epidural_hematoma_volume)
  xyplot(dti, log_GFAP  ~ log_S100B)
  xyplot(dti, log_GFAP  ~ log_UCHL1)
  xyplot(dti, Total_lesion_volume  ~ log_GFAP)
  
  
  # Descriptives
  write.xlsx(print(CreateTableOne(vars = names(test_1[, -1]), data = dti_for_imp), missing = TRUE, contDigits = 1), 
             file = "EXPORT/IMP_Descriptives.xlsx", sheetName = "Original")
  
  write.xlsx(print(CreateTableOne(vars = names(test_1[, -1]), data = test_1), missing = TRUE, contDigits = 1), 
             file = "EXPORT/IMP_Descriptives.xlsx", sheetName = "IMPUTED 1", append = T)
  
  # Correlations
  # temporarily set Impairment of HRQOL as Yes when dead
  table(test_1$Impairment_of_HRQOL, test_1$Mortality_at_6_months)
  test_1[is.na(test_1$Outcomes.QoLIBRIOSTotalScore) & test_1$Mortality_at_6_months %in% c("Yes"), ]$Impairment_of_HRQOL <- "Yes"
  test_1$ASAPS_class <- factor(test_1$ASAPS_class, levels = levels(test_1$ASAPS_class), ordered = F) 
  test_1$GOSE_score_at_6_months <- factor(test_1$GOSE_score_at_6_months, levels = levels(test_1$GOSE_score_at_6_months), ordered = F) 
  
  
  # rename columns
  clinical <- c("GCS_Score", "GCS_Eyes", "GCS_Motor", "GCS_Verbal", "Pupils")
  biomarkers <- c("GFAP", "UCHL1", "S100B", "Time_to_sampling")
  imaging <- c("Any_abnormality", "Skull_fracture", "Epidural_hematoma", "Subdural_hematoma",                            
               "TSAH", "Contusion_or_ICH", "TAMVI", "IVH", "Mass_effect", "Total_lesion_volume_25")
  modifiers <- c("Mechanism_of_injury", "Seizures", "Major_extracranial_injury", "Hypoxia", "Hypotension", "Accidental_cause", "Age", "Sex",                                      
                 "Medical_history", "Psychiatric_history", "Developmental_history",                        
                 "TBI_history")
  endpoints <- c("Detectable_intracranial_injury_on_CT_early", "Hospital_admission", "ICU_admission", "ICP_monitoring",
                 "Major_cranial_surgery_within_72h", "Mortality_at_6_months", "Unfavorable_outcome_at_6_months",                               
                 "Incomplete_recovery_at_6_months", "Impairment_of_HRQOL",  "GOSE_score_at_6_months")
  gsub("_", " ",  clinical)
  
  require(tidyverse)
  require(rcompanion)
  
  table(test_1$Employment_status_Job_classification)
  test_1$Employment_status_Job_classification <- factor(test_1$Employment_status_Job_classification, levels = c("Student", "Not working", "Other", "Manual worker", "Skilled manual worker", 
                                                                                                                "Technician/Supervisor/Associate Professional", 
                                                                                                                "Clerk/Sales", "Manager/Professional", "Retired"))
  
  table(test_1$Seizures)
  test_1$Seizures <- factor(test_1$Seizures, levels = c("No", "Partial/Focal", "Generalized", "Status epilepticus"), ordered = F)
  str(test_1)
  class(test_1$Seizures) %in% c("factor", "character")
  
  mixed_assoc2 = function(df, cor_method="spearman"){
    df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
    
    is_nominal = function(x) class(x) %in% c("factor", "character")
    # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
    # https://github.com/r-lib/rlang/issues/781
    is_numeric <- function(x) { is.integer(x) || is_double(x)}
    
    f = function(xName,yName) {
      x =  pull(df, xName)
      y =  pull(df, yName)
      
      result = if(is_nominal(x) && is_nominal(y)){
        # use spearman's on numerically transformed variables
        correlation = cor(as.numeric(x), as.numeric(y), method=cor_method, use="complete.obs")
        data.frame(xName, yName, assoc=correlation, type="correlation")
        
      }else if(is_numeric(x) && is_numeric(y)){
        correlation = cor(x, y, method=cor_method, use="complete.obs")
        data.frame(xName, yName, assoc=correlation, type="correlation")
        
      }else if(is_numeric(x) && is_nominal(y)){
        correlation = cor(x, as.numeric(y), method=cor_method, use="complete.obs")
        data.frame(xName, yName, assoc=correlation, type="correlation")
        
      }else if(is_nominal(x) && is_numeric(y)){
        correlation = cor(as.numeric(x), y, method=cor_method, use="complete.obs")
        data.frame(xName, yName, assoc=correlation, type="correlation")
        
      }else {
        warning(paste("unmatched column type combination: ", class(x), class(y)))
      }
      
      # finally add complete obs number and ratio to table
      result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
    }
    
    # apply function to each variable combination
    map2_df(df_comb$X1, df_comb$X2, f)
    
  }
  imp_assoc <- mixed_assoc2(test_1[, c(clinical, biomarkers, imaging, modifiers, endpoints)])
  imp_assoc$abs_value <- abs(imp_assoc$assoc)
  
  # rename
  imp_assoc[imp_assoc$x == "Total_lesion_volume_25", ]$x <- "Total lesion volume >= 25"
  imp_assoc[imp_assoc$y == "Total_lesion_volume_25", ]$y <- "Total lesion volume >= 25"
  imaging <- c("Any_abnormality", "Skull_fracture", "Epidural_hematoma", "Subdural_hematoma",                            
               "TSAH", "Contusion_or_ICH", "TAMVI", "IVH", "Mass_effect", "Total lesion volume >= 25")
  
  imp_assoc$x <- factor(imp_assoc$x, levels = c(clinical, biomarkers, imaging, modifiers, endpoints))
  levels(imp_assoc$x) <- gsub("_", " ",  levels(imp_assoc$x))
  imp_assoc$y <- factor(imp_assoc$y, levels = c(clinical, biomarkers, imaging, modifiers, endpoints))
  levels(imp_assoc$y) <- gsub("_", " ",  levels(imp_assoc$y))
  imp_assoc$y <- fct_rev(imp_assoc$y)
  
  imp_assoc$pillarx <- as.factor(ifelse(imp_assoc$x %in% gsub("_", " ",  clinical), "Clinical", 
                                        ifelse(imp_assoc$x %in% gsub("_", " ",  biomarkers), "Biomarkers",
                                               ifelse(imp_assoc$x %in% gsub("_", " ",  imaging), "Imaging", 
                                                      ifelse(imp_assoc$x %in% gsub("_", " ",  modifiers), "Modifiers", "End-points")))))
  imp_assoc$pillarx <- factor(imp_assoc$pillarx, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "End-points"))
  imp_assoc$pillary <- as.factor(ifelse(imp_assoc$y %in% gsub("_", " ",  clinical), "Clinical", 
                                        ifelse(imp_assoc$y %in% gsub("_", " ",  biomarkers), "Biomarkers",
                                               ifelse(imp_assoc$y %in% gsub("_", " ",  imaging), "Imaging", 
                                                      ifelse(imp_assoc$y %in% gsub("_", " ",  modifiers), "Modifiers", "End-points")))))
  imp_assoc$pillary <- factor(imp_assoc$pillary, levels = c("End-points", "Modifiers", "Imaging", "Biomarkers", "Clinical"))
  
  
  pdf("EXPORT/IMP_CorrelationsSpearman.pdf", height = 10, width = 17)
  IMP_Assoc_plot <- ggplot(imp_assoc, 
                           aes(x = interaction(x, pillarx, lex.order = F), 
                               y = interaction(y, pillary, lex.order = F), 
                               fill = abs_value)) +
    geom_tile()+ geom_text(aes(label = format(round(abs_value, 2), nsmall = 2)), size = 2.5) +
    scale_fill_gradient2(low = "white", high = M_COLOR,
                         midpoint = 0.1, limits=c(0, 1))+
    scale_x_discrete(name = "", position = "top", guide = "axis_nested") + scale_y_discrete(name = "", guide = "axis_nested")+ 
    labs(fill="Absolute value\nSpearman's rho\n")+
    theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
          panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank(),
          panel.background=element_rect(fill="white"), 
          axis.text.x = element_text(angle=90, hjust=0, size = 10),
          axis.text.y = element_text(size = 10), plot.margin=unit(c(1, 1, 1, 1), 'cm'),
          ggh4x.axis.nestline.x = element_line(size = 2),
          ggh4x.axis.nestline.y = element_line(size = 2),
          ggh4x.axis.nesttext.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
    geom_rect(aes(xmin = 1 - 0.5, xmax = 6 - 0.5, ymin = 42 - 0.5, ymax = 37 - 0.5), fill = "transparent", color = "black", size = 1)+
    geom_rect(aes(xmin = 6 - 0.5, xmax = 10 - 0.5, ymin = 37 - 0.5, ymax = 33 - 0.5), fill = "transparent", color = "black", size = 1)+
    geom_rect(aes(xmin = 10 - 0.5, xmax = 20 -0.5, ymin = 33 - 0.5, ymax = 23 - 0.5), fill = "transparent", color = "black", size = 1)+
    geom_rect(aes(xmin = 20 - 0.5, xmax = 32 - 0.5, ymin = 23 - 0.5, ymax = 11 - 0.5), fill = "transparent", color = "black", size = 1) +
    geom_rect(aes(xmin = 32 - 0.5, xmax = 42 - 0.5, ymin = 11 - 0.5, ymax = 1 - 0.5), fill = "transparent", color = "black", size = 1)+ 
    
    
    ggtitle(paste0("Heatmap of Pairwise Correlation Coefficients between Pillar Components and End-points in CENTER-TBI\n(imputed data, n = ",
                   max(assoc$complete_obs_pairs), ")"))
  IMP_Assoc_plot
  dev.off()
  
}
# IMPUTED DATASET: UNIVARIABLE ANALYSIS OF INDIVIDUAL PILLAR COMPONENTS ---------------------------------------------------------------
IMP_Univar <- data.frame(Variable = NA, RCS = NA, Levels = NA,
                         Outcome = NA, n = NA, Effect = NA, SE = NA, AUC = NA, R2 = NA, LRT = NA, 
                         n_Mild = NA, R2_Mild = NA, 
                         n_Moderate = NA, R2_Moderate = NA,
                         n_Severe = NA, R2_Severe = NA)

IMP_Linearity_Univar <- data.frame(Variable = NA, 
                                   Outcome = NA, n = NA, 
                                   AUC_RCS = NA, R2_RCS = NA, 
                                   AUC_LIN = NA, R2_LIN = NA, 
                                   LRT_RCS_NULL = NA, LRT_RCS_LIN = NA)

clinical <- c("GCS_Score", "GCS_Eyes", "GCS_Motor", "GCS_Verbal", "Pupils")
biomarkers <- c("GFAP", "UCHL1", "S100B", "Time_to_sampling")
imaging <- c("Any_abnormality", "Skull_fracture", "Epidural_hematoma", "Subdural_hematoma",                            
             "TSAH", "Contusion_or_ICH", "TAMVI", "IVH", "Mass_effect", "Total_lesion_volume_25")
modifiers <- c("Mechanism_of_injury", "Seizures", "Major_extracranial_injury", "Hypoxia", "Hypotension", "Accidental_cause", "Age", "Sex",                                      
               "Medical_history", "Psychiatric_history", "Developmental_history",                        
               "TBI_history")
endpoints <- c("Detectable_intracranial_injury_on_CT_early", "Hospital_admission", "ICU_admission", "ICP_monitoring",
               "Major_cranial_surgery_within_72h", "Mortality_at_6_months", "Unfavorable_outcome_at_6_months",                               
               "Incomplete_recovery_at_6_months", "Impairment_of_HRQOL",  "GOSE_score_at_6_months")

dti$data <- dti$data %>% 
  rename(
    "Seizures" = InjuryHx.EDComplEventSeizures,
    "Hypoxia" = InjuryHx.EDComplEventHypoxia, 
    "Hypotension" = InjuryHx.EDComplEventHypotension)
long <- mice::complete(dti, "long", include = T)

### TEST LINEARITY, FIT, EXTRACT INFO, SUBGROUP FITS ---------------------------------------------------------------
for (j in endpoints) {
  
  # select only cases with observed endpoint
  obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]), ]$subjectId
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP_monitoring")) {obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & all_original_data$`ICU admission` %in% c("Yes"), ]$subjectId} 
  
  for (i in c("mms", clinical, biomarkers, imaging, modifiers)) {
    
    if(!(i %in% imaging & j == "Detectable_intracranial_injury_on_CT_early")) {
      
      rcs_temp <- 0
      df_temp <- 0
      
      # test linearity of continuous predictors
      if(i %in% c("GCS_Score", "GFAP", "UCHL1", "S100B", "Time_to_sampling", "Age", "Years_of_education")) {
        
        model_linear <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `", i, "`")), fitter = lrm, 
                                        xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                        fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
        model_rcs <- fit.mult.impute(as.formula(paste0("`", j, "` ~ rcs(`", i, "`, 3)")), fitter = lrm, 
                                     xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                     fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
        
        lrt_rcs_lin_null <- anova(model_rcs, test = 'LR')
        
        Linearity_Univar_temp <- data.frame(Variable = gsub("_", " ", i), Outcome = gsub("_", " ", j), 
                                            n = model_rcs$stats[["Obs"]],
                                            AUC_RCS = round(model_rcs$stats[["C"]], 2), 
                                            R2_RCS = round(model_rcs$stats[["R2"]], 3), 
                                            AUC_LIN = round(model_linear$stats[["C"]], 2), 
                                            R2_LIN = round(model_linear$stats[["R2"]], 3), 
                                            LRT_RCS_NULL = p_format(lrt_rcs_lin_null["TOTAL", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T), 
                                            LRT_RCS_LIN = p_format(lrt_rcs_lin_null[" Nonlinear", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T))
        
        rownames(Linearity_Univar_temp) <- NULL
        IMP_Linearity_Univar <- rbind(IMP_Linearity_Univar, Linearity_Univar_temp)
        
        plot_odds <- ggplot() +
          geom_line(aes(x=model_linear$fits[[1]]$x, y=model_linear$fits[[1]]$linear.predictors, color = "Linear"), size=1) + 
          labs(x = paste0("\n", gsub("_", " ", i), " (non-linearity LRT ", p_format(lrt_rcs_lin_null[" Nonlinear", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T), ")"), 
               y = paste0("Predicted log odds of\n ", gsub("_", " ", j), "\n")) + theme_minimal()+
          geom_line(aes(x=model_rcs$fits[[1]]$x[, 1], y=model_rcs$fits[[1]]$linear.predictors, colour = "RCS"), size = 1)+
          scale_colour_manual(name = "Model", values = cols)
        plot(plot_odds)
        
        plot_probs <- ggplot() +
          geom_line(aes(x=model_linear$fits[[1]]$x, y=plogis(model_linear$fits[[1]]$linear.predictors), color = "Linear"), size=1) + 
          labs(x = paste0("\n", gsub("_", " ", i), " (non-linearity LRT ", p_format(lrt_rcs_lin_null[" Nonlinear", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T), ")"), 
               y = paste0("Predicted probability of\n ", gsub("_", " ", j), "\n")) + theme_minimal()+ ylim(0, 1) +
          geom_line(aes(x=model_rcs$fits[[1]]$x[, 1], y=plogis(model_rcs$fits[[1]]$linear.predictors), colour = "RCS"), size = 1)+
          scale_colour_manual(name = "Model", values = cols)
        plot(plot_probs)
        
        if(lrt_rcs_lin_null["TOTAL", "P"] < 0.05 & lrt_rcs_lin_null[" Nonlinear", "P"] < 0.05) {
          rcs_temp <- 1
          model_selected <- model_rcs
        } else {
          model_selected <- model_linear
        }
      }
      
      if(!i %in% c("GCS_Score", "GFAP", "UCHL1", "S100B", "Time_to_sampling", "Age", "Years_of_education")) {
        model_selected <- fit.mult.impute(as.formula(paste0("`", j, "` ~ catg(`", i, "`)")), fitter = lrm, 
                                          xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                          pr = F, fitargs = list(x=TRUE, y=TRUE))
      }
      
      df_temp <- ifelse(j %in% "GOSE_score_at_6_months", length(model_selected$coefficients) - 6, length(model_selected$coefficients) - 1)
      
      if(df_temp > 1) {lr <- anova(model_selected, test = 'LR')}
      
      # subgroup analyses
      
      long$lp <- predict(model_selected, newdata=long)
      dti.lp <- as.mids(long[, c(".imp", ".id", "subjectId", i, j, "lp", "mms")])
      
      perform_subgroup <- F
      
      if(! i %in% "mms" & (j %in% "GOSE_score_at_6_months" | all(table(all_original_data[, c("mms", gsub("_", " ", j))]) > 10))) {
        print(j)
        perform_subgroup <- T
        rm(refit_mild, refit_moderate, refit_severe)
        try({
          refit_mild <- fit.mult.impute(as.formula(paste0("`", j, "` ~ lp")), fitter = lrm, 
                                        xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end & dti.lp$data$mms %in% c("Mild"), 
                                        fit.reps = T, pr = F)
          print(i)
        }, silent = T)
        
        try({
          refit_moderate <- fit.mult.impute(as.formula(paste0("`", j, "` ~ lp")), fitter = lrm, 
                                            xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end & dti.lp$data$mms %in% c("Moderate"), 
                                            fit.reps = T, pr = F)
          print(i)
        }, silent = T)
        
        try({
          refit_severe <- fit.mult.impute(as.formula(paste0("`", j, "` ~ lp")), fitter = lrm, 
                                          xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end & dti.lp$data$mms %in% c("Severe"), 
                                          fit.reps = T, pr = F)
          print(i)
        }, silent = T)
        
      }
      
      # store results
      Univar_temp <- data.frame(Variable = rep(gsub("_", " ", i), df_temp), 
                                RCS = rep(rcs_temp, df_temp),
                                Levels = model_selected$Design$colnames,
                                Outcome = rep(gsub("_", " ", j), df_temp), 
                                n = rep(model_selected$stats[["Obs"]], df_temp),
                                Effect = model_selected$coefficients[model_selected$Design$colnames], 
                                SE = sqrt(diag(model_selected$var))[model_selected$Design$colnames], 
                                
                                AUC = rep(round(model_selected$stats[["C"]], 2), df_temp), 
                                R2 = rep(round(model_selected$stats[["R2"]], 3), df_temp), 
                                LRT = ifelse(df_temp == 1, NA, rep(p_format(lr["TOTAL", "P"], accuracy = 0.001, add.p = TRUE, digits = 2, space = T), df_temp)),
                                
                                R2_Mild =     ifelse(perform_subgroup == F | !exists("refit_mild"), NA, ifelse(refit_mild$fail == F, rep(round(refit_mild$stats[["R2"]], 3), df_temp), NA)), 
                                R2_Moderate = ifelse(perform_subgroup == F | !exists("refit_moderate"), NA, ifelse(refit_moderate$fail == F, rep(round(refit_moderate$stats[["R2"]], 3), df_temp), NA)), 
                                R2_Severe =   ifelse(perform_subgroup == F | !exists("refit_severe"), NA, ifelse(refit_severe$fail == F, rep(round(refit_severe$stats[["R2"]], 3), df_temp), NA)),
                                
                                n_Mild =      ifelse(perform_subgroup == F | !exists("refit_mild"), NA, ifelse(refit_mild$fail == F, rep(round(refit_mild$stats[["Obs"]], 3), df_temp), NA)), 
                                n_Moderate =  ifelse(perform_subgroup == F | !exists("refit_moderate"), NA, ifelse(refit_moderate$fail == F, rep(round(refit_moderate$stats[["Obs"]], 3), df_temp), NA)),  
                                n_Severe =    ifelse(perform_subgroup == F | !exists("refit_severe"), NA, ifelse(refit_severe$fail == F, rep(round(refit_severe$stats[["Obs"]], 3), df_temp), NA))) 
      
      
      rownames(Univar_temp) <- NULL
      IMP_Univar <- rbind.fill(IMP_Univar, Univar_temp)
    }
  }
}


# IMPUTED DATASET: PILLAR and CUMULATIVE MODELS ---------------------------

### RCS DEPENDING ON LINEARITY ---------------------------------------------------------------
IMP_Univar[IMP_Univar$Variable %in% "Total lesion volume 25", ]$Variable <- "Total lesion volume >= 25"
IMP_Univar[IMP_Univar$Variable %in% "mms", ]$Variable <- "TBI Severity"

RCS <- unique(IMP_Univar[, c("Outcome", "Variable", "RCS")])
RCS <- RCS[RCS$Variable %in% c("GCS Score", "GFAP", "UCHL1", "S100B", "Time to sampling", "Age", "Years of education"),]
table(RCS$Variable, RCS$RCS) # none is linear throughout

## Code alternative formulas

c_formula_gcs_rcs <- "rcs(`GCS_Score`, 3)"
c_formula_gcs_lin <- "`GCS_Score`"
c_formula_gcs_emv <- "`GCS_Eyes` + `GCS_Motor` + `GCS_Verbal`"

gfap_formula_rcs <- "rcs(`GFAP`, 3)"
gfap_formula_lin <- "`GFAP`"
uchl1_formula_rcs <- "rcs(`UCHL1`, 3)"
uchl1_formula_lin <- "`UCHL1`"
s100b_formula_rcs <- "rcs(`S100B`, 3)"
s100b_formula_lin <- "`S100B`"
tts_formula_rcs <- "rcs(`Time_to_sampling`, 3)"
tts_formula_lin <- "`Time_to_sampling`"

age_formula_rcs <- "rcs(`Age`, 3)"
age_formula_lin <- "`Age`"
yrsedu_formula_rcs <- "rcs(`Years_of_education`, 3)"
yrsedu_formula_lin <- "`Years_of_education`"

### FIT, EXTRACT INFO + CIs -----------------------------------------------------------------
####function for Bootstrap intervals
pooled_boot_CI <- function(m_fit, mids_obj, subset_mask = rep(TRUE, nrow(mids_obj$data)),
                           B = 200, seed = 153, verbose = TRUE) {
  set.seed(seed)
  stopifnot(inherits(mids_obj, "mids"))
  M <- mids_obj$m
  n_sub <- sum(subset_mask)
  if (n_sub < 5) stop("too few observations")
  
  # vectors to collect per-imputation baseline (from stored LP) and pooled bootstrap draws
  C_baseline <- numeric(M)
  R2_baseline <- numeric(M)
  C_boot_all <- numeric(0)
  R2_boot_all <- numeric(0)
  
  formula_used <- m_fit$sformula
  y_vec <- m_fit$y   # should be length n_sub
  
  for (j in 1:M) {
    if (verbose) message("Imp ", j, "/", M)
    dat_j <- complete(mids_obj, j)[subset_mask, , drop = FALSE]
    
    # baseline from stored LP (same logic you used)
    lp <- m_fit$fits[[j]]$linear.predictors
    tmp <- lrm(y_vec ~ lp)
    C_baseline[j] <- as.numeric(tmp$stats["C"])
    R2_baseline[j] <- as.numeric(tmp$stats["R2"])
    
    for (b in 1:B) {
      idx <- sample.int(n_sub, n_sub, replace = TRUE)
      dat_b <- dat_j[idx, , drop = FALSE]
      y_b <- y_vec[idx]
      
      # try to fit in bootstrap sample; skip if it errors
      fit_b <- try(lrm(formula_used, data = dat_b), silent = TRUE)
      if (inherits(fit_b, "try-error")) next
      
      # compute performance on bootstrap sample via lp
      lp_b <- try(fit_b$linear.predictors, silent = TRUE)
      if (inherits(lp_b, "try-error")) next
      perf <- try(lrm(y_b ~ lp_b), silent = TRUE)
      if (inherits(perf, "try-error")) next
      
      C_boot_all <- c(C_boot_all, as.numeric(perf$stats["C"]))
      R2_boot_all <- c(R2_boot_all, as.numeric(perf$stats["R2"]))
    }
  }
  
  # pooled point estimates = mean across imputations (baseline)
  C_point <- mean(C_baseline, na.rm = TRUE)
  R2_point <- mean(R2_baseline, na.rm = TRUE)
  
  # pooled percentile CIs from pooled bootstrap draws
  C_ci <- quantile(C_boot_all, probs = c(0.025, 0.975), na.rm = TRUE)
  R2_ci <- quantile(R2_boot_all, probs = c(0.025, 0.975), na.rm = TRUE)
  
  list(C = list(est = C_point, lower = C_ci[1], upper = C_ci[2]),
       R2 = list(est = R2_point, lower = R2_ci[1], upper = R2_ci[2]))
}

ConfInterv2 <- data.frame(Model = rep(c("C", "B", "I", "M", "CB", "CBI", "CBIM"), 9), 
                          Outcome = c(rep(endpoints[1], 7),
                                      rep(endpoints[2], 7),
                                      rep(endpoints[3], 7),
                                      rep(endpoints[4], 7),
                                      rep(endpoints[5], 7),
                                      rep(endpoints[6], 7),
                                      rep(endpoints[7], 7),
                                      rep(endpoints[8], 7),
                                      rep(endpoints[9], 7)),  
                          Events=NA, n = NA, 
                          C=NA, R=NA, 
                          AUC = NA, AUC_ll = NA, AUC_ul = NA, 
                          R2 = NA , R2_ll = NA, R2_ul = NA)

long <- mice::complete(dti, "long", include = T)
long <- long[long$subjectId != "6DYY996",]

for (j in endpoints[1:9]) {
  
  # select only cases with observed endpoint
  obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]), ]$subjectId
  obs_end <- obs_end[!obs_end == "6DYY996"]
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP_monitoring")) {obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & all_original_data$`ICU admission` %in% c("Yes"), ]$subjectId
  obs_end <- obs_end[!obs_end == "6DYY996"]} 
  
  # C PILLAR
  model_clinical <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, 
                                                                                        c_formula_gcs_rcs, c_formula_gcs_lin))), fitter = lrm, 
                                    xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                    fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  long$temp_c <- predict(model_clinical, newdata=long)
  
  ConfInterv2[ConfInterv2$Outcome == j, 3] <- sum(model_clinical$y %in% c("Yes", "1"))
  ConfInterv2[ConfInterv2$Outcome == j, 4] <- length(model_clinical$y)
  
  Store_cis <- pooled_boot_CI(model_clinical, dti, dti$data$subjectId %in% obs_end, B = 200, seed = 153)
  ConfInterv2[ConfInterv2$Model == "C" & ConfInterv2$Outcome == j, 5:12] <- c(model_clinical$stats[["C"]], model_clinical$stats[["R2"]], Store_cis$C$est, Store_cis$C$lower, Store_cis$C$upper, 
                                                                              Store_cis$R2$est, Store_cis$R2$lower, Store_cis$R2$upper)
  print(Store_cis)
  
  # B PILLAR
  model_biomarkers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ (", 
                                                        ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                        ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                        ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                        ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), fitter = lrm, 
                                      xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                      fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  long$temp_b <- predict(model_biomarkers, newdata=long) 
  Store_cis <- pooled_boot_CI(model_biomarkers, dti, dti$data$subjectId %in% obs_end, B = 200, seed = 153)
  ConfInterv2[ConfInterv2$Model == "B" & ConfInterv2$Outcome == j, 5:12] <- c(model_biomarkers$stats[["C"]], model_biomarkers$stats[["R2"]], Store_cis$C$est, Store_cis$C$lower, Store_cis$C$upper, 
                                                                              Store_cis$R2$est, Store_cis$R2$lower, Store_cis$R2$upper)
  
  # I PILLAR
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    
    model_imaging <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), fitter = lrm, 
                                     xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                     fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_i <- predict(model_imaging, newdata=long) 
    Store_cis <- pooled_boot_CI(model_imaging, dti, dti$data$subjectId %in% obs_end, B = 200, seed = 153)
    ConfInterv2[ConfInterv2$Model == "I" & ConfInterv2$Outcome == j, 5:12] <- c(model_imaging$stats[["C"]], model_imaging$stats[["R2"]], Store_cis$C$est, Store_cis$C$lower, Store_cis$C$upper, 
                                                                                Store_cis$R2$est, Store_cis$R2$lower, Store_cis$R2$upper)
    
  } else {
    model_imaging <- NULL
    long$temp_i <- NA
  }
  
  # M PILLAR
  model_modifiers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+catg(`Seizures`)+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                                       ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                                       "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), fitter = lrm, 
                                     xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                     fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  long$temp_m <- predict(model_modifiers, newdata=long)  
  Store_cis <- pooled_boot_CI(model_modifiers, dti, dti$data$subjectId %in% obs_end, B = 200, seed = 153)
  ConfInterv2[ConfInterv2$Model == "M" & ConfInterv2$Outcome == j, 5:12] <- c(model_modifiers$stats[["C"]], model_modifiers$stats[["R2"]], Store_cis$C$est, Store_cis$C$lower, Store_cis$C$upper, 
                                                                              Store_cis$R2$est, Store_cis$R2$lower, Store_cis$R2$upper)
  
  
  ## CUMULATIVE MODELS, plot redistribution of predicted risk
  dti.lp <- as.mids(long[, c(".imp", ".id", "subjectId", j, "temp_c", "temp_b", "temp_i", "temp_m")])
  
  # CB 
  model_cb <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b")), fitter = lrm, 
                              xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  Store_cis <- pooled_boot_CI(model_cb, dti.lp, dti.lp$data$subjectId %in% obs_end, B = 200, seed = 153)
  ConfInterv2[ConfInterv2$Model == "CB" & ConfInterv2$Outcome == j, 5:12] <- c(model_cb$stats[["C"]], model_cb$stats[["R2"]], Store_cis$C$est, Store_cis$C$lower, Store_cis$C$upper, 
                                                                               Store_cis$R2$est, Store_cis$R2$lower, Store_cis$R2$upper)
  print(Store_cis)
  
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    # CBI 
    model_cbi <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i")), fitter = lrm, 
                                 xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    
    Store_cis <- pooled_boot_CI(model_cbi, dti.lp, dti.lp$data$subjectId %in% obs_end, B = 200, seed = 153)
    ConfInterv2[ConfInterv2$Model == "CBI" & ConfInterv2$Outcome == j, 5:12] <- c(model_cbi$stats[["C"]], model_cbi$stats[["R2"]], Store_cis$C$est, Store_cis$C$lower, Store_cis$C$upper, 
                                                                                  Store_cis$R2$est, Store_cis$R2$lower, Store_cis$R2$upper)
    
    print(Store_cis)
    # CBIM 
    model_cbim <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), fitter = lrm, 
                                  xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    
    Store_cis <- pooled_boot_CI(model_cbim, dti.lp, dti.lp$data$subjectId %in% obs_end, B = 200, seed = 153)
    ConfInterv2[ConfInterv2$Model == "CBIM" & ConfInterv2$Outcome == j, 5:12] <- c(model_cbim$stats[["C"]], model_cbim$stats[["R2"]], Store_cis$C$est, Store_cis$C$lower, Store_cis$C$upper, 
                                                                                   Store_cis$R2$est, Store_cis$R2$lower, Store_cis$R2$upper)
    
    print(Store_cis)
  }else{
    # CBM
    model_cbm <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), fitter = lrm, 
                                 xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    
    Store_cis <- pooled_boot_CI(model_cbm, dti.lp, dti.lp$data$subjectId %in% obs_end, B = 200, seed = 153)
    ConfInterv2[ConfInterv2$Model == "CBIM" & ConfInterv2$Outcome == j, 5:12] <- c(model_cbm$stats[["C"]], model_cbm$stats[["R2"]], Store_cis$C$est, Store_cis$C$lower, Store_cis$C$upper, 
                                                                                   Store_cis$R2$est, Store_cis$R2$lower, Store_cis$R2$upper)
    
    
    print(Store_cis)
  }
}


EditedConfInterv2 <- ConfInterv2

EditedConfInterv2[, 5:12] <- format(round(EditedConfInterv2[, 5:12], 2), nsmall=2)
EditedConfInterv2$AUC_CI <- paste0(EditedConfInterv2$AUC, " (", EditedConfInterv2$AUC_ll, " - ", 
                                   EditedConfInterv2$AUC_ul, ")") 
EditedConfInterv2$R2_CI <- paste0(EditedConfInterv2$R2, " (", EditedConfInterv2$R2_ll, " - ", 
                                  EditedConfInterv2$R2_ul, ")")
EditedConfInterv2$EventN <- paste0(EditedConfInterv2$Events, "/", EditedConfInterv2$n) 
EditedConfInterv2$Outcome <- gsub("_", " ", EditedConfInterv2$Outcome)

EditedConfIntervC2 <- EditedConfInterv2[, c(1, 2, 15, 13)]
EditedConfIntervC2 <- reshape(EditedConfIntervC2, idvar = "Outcome", timevar = "Model", direction = "wide")
EditedConfIntervC2 <- EditedConfIntervC2[, c(1:3, 5, 7, 9, 11, 13, 15)]

EditedConfIntervR2 <- EditedConfInterv2[, c(1, 2, 15, 14)]
EditedConfIntervR2 <- reshape(EditedConfIntervR2, idvar = "Outcome", timevar = "Model", direction = "wide")
EditedConfIntervR2 <- EditedConfIntervR2[, c(1:3, 5, 7, 9, 11, 13, 15)]

write.xlsx(EditedConfIntervC2, "EXPORT/AUC_R2s.xlsx", sheetName = "AUC")
write.xlsx(EditedConfIntervR2, "EXPORT/AUC_R2s.xlsx", sheetName = "R2", append = T)

PlotCIs <- ConfInterv2
PlotCIs$Model <- as.factor(PlotCIs$Model)
levels(PlotCIs$Model) <- c("C", "C+B", "C+B+I", "CBI-M")
PlotCIs$Endpoint <- gsub("_", " ", PlotCIs$Outcome)
PlotCIs$Endpoint <- factor(PlotCIs$Endpoint, levels = c(gsub("_", " ", endpoints)))

PlotCIs[PlotCIs$Model=="C+B+I" & PlotCIs$Endpoint=="Detectable intracranial injury on CT early",]$C <- PlotCIs[PlotCIs$Model=="CBI-M" & PlotCIs$Endpoint=="Detectable intracranial injury on CT early",]$C
PlotCIs[PlotCIs$Model=="C+B+I" & PlotCIs$Endpoint=="Detectable intracranial injury on CT early",]$R <- PlotCIs[PlotCIs$Model=="CBI-M" & PlotCIs$Endpoint=="Detectable intracranial injury on CT early",]$R

PLOT_all_AUC <- ggplot(PlotCIs, 
                       aes(x=Model, y=C, group=Endpoint)) +
  geom_line(aes(color=Endpoint), linewidth = 1.2)+
  geom_point(aes(color=Endpoint, shape=Endpoint), size = 2.5)+theme_minimal()+
  scale_shape_manual(values=c(1:9))+
  scale_color_manual(values=c("grey", "#5c1726", "#a12943", "#e63b60", "#f29daf", 
                              "#0d1750", "#1b2fa0", "#4e62d3", "#bcc4ee"))+
  labs(y = "AUC", x ="")
PLOT_all_AUC 

PLOT_all_R <- ggplot(PlotCIs, 
                     aes(x=Model, y=R, group=Endpoint)) +
  geom_line(aes(color=Endpoint), linewidth = 1.2)+
  geom_point(aes(color=Endpoint, shape=Endpoint), size = 2.5)+theme_minimal()+
  scale_shape_manual(values=c(1:9))+
  scale_color_manual(values=c("grey", "#5c1726", "#a12943", "#e63b60", "#f29daf", 
                              "#0d1750", "#1b2fa0", "#4e62d3", "#bcc4ee"))+
  labs(y = expression(paste("Pseudo-", R^2)), x ="")+
  labs(tag = "CENTER-TBI")+
  theme(plot.tag.location = "plot", 
        plot.tag =  element_text( hjust=0.5, vjust = 0.1))
PLOT_all_R

pdf(paste0("EXPORT/Fig2A.pdf"), width = 9, height = 4)
plot(ggarrange(plotlist = list(PLOT_all_R, PLOT_all_AUC), common.legend = T, ncol = 2, nrow = 1, legend = "right"))
dev.off()

pdf(paste0("EXPORT/Fig21.pdf"), width = 9, height = 4)
plot(ggarrange(plotlist = list(PLOT_all_R, PLOT_all_AUC), common.legend = T, ncol = 2, nrow = 1, legend = "right"))
dev.off()

### FIT, EXTRACT INFO + LRT ---------------------------------------------------------------
list_plots_r2 <- list()
list_plots_auc <- list()
counter_r2 <- 1
long <- mice::complete(dti, "long", include = T)

for (j in endpoints[1:9]) {
  print(j)
  # select only cases with observed endpoint
  obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]), ]$subjectId
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP_monitoring")) {obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & all_original_data$`ICU admission` %in% c("Yes"), ]$subjectId} 
  obs_end <- obs_end[!obs_end == "6DYY996"]
  
  # C PILLAR
  model_clinical <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, 
                                                                                        c_formula_gcs_rcs, c_formula_gcs_lin))), fitter = lrm, 
                                    xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                    fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  long$temp_c <- predict(model_clinical, newdata=long)
  
  # B PILLAR
  model_biomarkers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ (", 
                                                        ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                        ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                        ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                        ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), fitter = lrm, 
                                      xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                      fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  
  long$temp_b <- predict(model_biomarkers, newdata=long) 
  
  # I PILLAR
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    
    model_imaging <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), fitter = lrm, 
                                     xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                     fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_i <- predict(model_imaging, newdata=long) 
  } else {
    model_imaging <- NULL
    long$temp_i <- NA
  }
  
  # M PILLAR
  model_modifiers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+catg(`Seizures`)+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                                       ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                                       "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), fitter = lrm, 
                                     xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                     fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  long$temp_m <- predict(model_modifiers, newdata=long)  
  
  
  ## CUMULATIVE MODELS, plot redistribution of predicted risk
  dti.lp <- as.mids(long[, c(".imp", ".id", "subjectId", j, "temp_c", "temp_b", "temp_i", "temp_m", "mms")])
  
  # CB 
  model_cb <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b")), fitter = lrm, 
                              xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  long$temp_cb <- predict(model_cb, newdata=long)
  
  # CBM
  model_cbm <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), fitter = lrm, 
                               xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  long$temp_cbm <- predict(model_cbm, newdata=long)
  
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    # CBI 
    model_cbi <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i")), fitter = lrm, 
                                 xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_cbi <- predict(model_cbi, newdata=long)
    
    # CBIM 
    model_cbim <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), fitter = lrm, 
                                  xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_cbim <- predict(model_cbim, newdata=long)
    
    all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Imaging", "Modifiers"), 2)), 
                                   N = rep(model_cbim$stats[["Obs"]], 8), 
                                   Color = c("Clinical", "Biomarkers", "Imaging", "Modifiers", rep("Cumulative", 4)),
                                   R2 = c(model_clinical$stats[["R2"]], 
                                          model_biomarkers$stats[["R2"]],
                                          model_imaging$stats[["R2"]],
                                          model_modifiers$stats[["R2"]],
                                          model_clinical$stats[["R2"]],
                                          model_cb$stats[["R2"]],
                                          model_cbi$stats[["R2"]],
                                          model_cbim$stats[["R2"]]))
    
    all_models_rsqrs$LRT <- c("",  "", "", "", "",
                              ifelse(anova(model_cb, test = 'LR')[2,3] <0.05, "*", ""),
                              ifelse(anova(model_cbi, test = 'LR')[3,3] <0.05, "*", ""),
                              ifelse(anova(model_cbim, test = 'LR')[4,3] <0.05, "*", ""))
    
    list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                          aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers")), y = R2, 
                                              fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(gsub("_", " ", j), " (n = ", unique(all_models_rsqrs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, I_COLOR, M_COLOR, "grey60"))+
      labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
      geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0)+
      geom_text(aes(label = LRT, y = R2+0.1, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
    
    
    all_models_aucs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Imaging", "Modifiers"), 2)), 
                                  N = rep(model_cbim$stats[["Obs"]], 8),
                                  Color = c("Clinical", "Biomarkers", "Imaging", "Modifiers", rep("Cumulative", 4)),
                                  AUC = c(model_clinical$stats[["C"]], 
                                          model_biomarkers$stats[["C"]],
                                          model_imaging$stats[["C"]],
                                          model_modifiers$stats[["C"]],
                                          model_clinical$stats[["C"]],
                                          model_cb$stats[["C"]],
                                          model_cbi$stats[["C"]],
                                          model_cbim$stats[["C"]]))
    
    list_plots_auc[[counter_r2]] <- ggplot(all_models_aucs, 
                                           aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers")), y = AUC, 
                                               fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(gsub("_", " ", j), " (n = ", unique(all_models_aucs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, I_COLOR, M_COLOR, "grey60"))+
      labs(y = "AUC", x ="") + coord_cartesian(ylim=c(0.5, 1))+
      geom_text(aes(label = format(round(AUC, 2), nsmall = 2), y = AUC + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
    counter_r2 <- counter_r2 + 1
    
    
  } else {
    all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Modifiers"), 2)), 
                                   N = rep(model_cbm$stats[["Obs"]], 6), 
                                   Color = c("Clinical", "Biomarkers", "Modifiers", rep("Cumulative", 3)),
                                   R2 = c(model_clinical$stats[["R2"]], 
                                          model_biomarkers$stats[["R2"]],
                                          model_modifiers$stats[["R2"]],
                                          model_clinical$stats[["R2"]],
                                          model_cb$stats[["R2"]],
                                          model_cbm$stats[["R2"]]))
    
    all_models_rsqrs$LRT <- c("", "", "", "",
                              ifelse(anova(model_cb, test = 'LR')[2,3] <0.05, "*", ""),
                              ifelse(anova(model_cbm, test = 'LR')[3,3] <0.05, "*", ""))
    
    
    list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                          aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Modifiers")), y = R2, 
                                              fill = factor(Color, levels = c("Clinical", "Biomarkers", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(gsub("_", " ", j), " (n = ", unique(all_models_rsqrs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, M_COLOR, "grey60"))+
      labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
      geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0)+
      geom_text(aes(label = LRT, y = R2+0.1, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
    
    all_models_aucs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Modifiers"), 2)), 
                                  N = rep(model_cbm$stats[["Obs"]], 6), 
                                  Color = c("Clinical", "Biomarkers", "Modifiers", rep("Cumulative", 3)),
                                  AUC = c(model_clinical$stats[["C"]], 
                                          model_biomarkers$stats[["C"]],
                                          model_modifiers$stats[["C"]],
                                          model_clinical$stats[["C"]],
                                          model_cb$stats[["C"]],
                                          model_cbm$stats[["C"]]))
    
    list_plots_auc[[counter_r2]] <- ggplot(all_models_aucs, 
                                           aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Modifiers")), y = AUC, 
                                               fill = factor(Color, levels = c("Clinical", "Biomarkers", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 12, face = "bold"), axis.title.y = element_text(face = "bold"))  + ggtitle(paste0(gsub("_", " ", j), " (n = ", unique(all_models_aucs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, M_COLOR, "grey60"))+
      labs(y = "AUC", x ="") + coord_cartesian(ylim=c(0.5, 1))+
      geom_text(aes(label = format(round(AUC, 2), nsmall = 2), y = AUC + 0.05, fontface = "bold"), position = position_dodge(0.8), vjust = 0)
    counter_r2 <- counter_r2 + 1
  }
}

emf(file = "EXPORT/FigS2_Panel1.emf", emfPlus = T, width = 5.5, height = 15)
plot(ggarrange(plotlist = list_plots_r2[c(1:5)], common.legend = T, ncol = 1, legend = "none"))
dev.off()

emf(file = "EXPORT/FigS2_Panel3.emf", emfPlus = T, width = 5.5, height = 15)
plot(ggarrange(plotlist = list_plots_auc[c(1:5)], common.legend = T, ncol = 1, legend = "none"))
dev.off()

emf(file = "EXPORT/FigS3_Panel1.emf", emfPlus = T, width = 5.5, height = 15)
plot(ggarrange(plotlist = list_plots_r2[c(6:9)], common.legend = T, ncol = 1, legend = "none"))
dev.off()

emf(file = "EXPORT/FigS3_Panel3.emf", emfPlus = T, width = 5.5, height = 15)
plot(ggarrange(plotlist = list_plots_auc[c(6:9)], common.legend = T, ncol = 1, legend = "none"))
dev.off()
### SUBGROUP FITS ------------------------------------------------------------
Multi <- data.frame(Model = NA, Levels = NA, Outcome = NA,  n = NA, AUC = NA, R2 = NA)
list_plots_r2 <- list()
counter_r2 <- 1
long <- mice::complete(dti, "long", include = T)

# subgroup analyses
table(dti.lp$data$mms)
levels(dti.lp$data$mms)<- c("Mild", "Mod-Severe", "Mod-Severe" )
table(dti.lp$data$mms)

for (j in c("Detectable_intracranial_injury_on_CT_early",                    
            "ICU_admission", "ICP_monitoring",
            "Major_cranial_surgery_within_72h", "Mortality_at_6_months",
            "Unfavorable_outcome_at_6_months", "Incomplete_recovery_at_6_months",
            "Impairment_of_HRQOL")) {
  
  # select only cases with observed endpoint
  obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]), ]$subjectId
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP_monitoring")) {obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & all_original_data$`ICU admission` %in% c("Yes"), ]$subjectId} 
  
  for (k in c("Mild", "Mod-Severe")){
    
    # C PILLAR
    model_clinical <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Pupils` + `GCS_Score`")), fitter = lrm, 
                                      xtrans = dti, subset = dti$data$subjectId %in% obs_end & dti.lp$data$mms %in% k, 
                                      fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_c <- predict(model_clinical, newdata=long)
    
    # B PILLAR
    model_biomarkers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ (", 
                                                          ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                          ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                          ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                          ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), fitter = lrm, 
                                        xtrans = dti, subset = dti$data$subjectId %in% obs_end & dti.lp$data$mms %in% k, 
                                        fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    
    long$temp_b <- predict(model_biomarkers, newdata=long) 
    
    # I PILLAR
    if (!j == "Detectable_intracranial_injury_on_CT_early") {
      
      model_imaging <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), fitter = lrm, 
                                       xtrans = dti, subset = dti$data$subjectId %in% obs_end & dti.lp$data$mms %in% k, 
                                       fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
      long$temp_i <- predict(model_imaging, newdata=long) 
    } else {
      model_imaging <- NULL
      long$temp_i <- NA
    }
    
    # M PILLAR
    model_modifiers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+catg(`Seizures`)+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                                         ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                                         "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), fitter = lrm, 
                                       xtrans = dti, subset = dti$data$subjectId %in% obs_end & dti.lp$data$mms %in% k, 
                                       fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_m <- predict(model_modifiers, newdata=long)  
    
    
    ## CUMULATIVE MODELS, plot redistribution of predicted risk
    dti.lp <- as.mids(long[, c(".imp", ".id", "subjectId", j, "temp_c", "temp_b", "temp_i", "temp_m", "mms")])
    print(table(dti.lp$data$mms))
    levels(dti.lp$data$mms)<- c("Mild", "Mod-Severe", "Mod-Severe" )
    print(table(dti.lp$data$mms))
    
    # CB 
    model_cb <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b")), fitter = lrm, 
                                xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end & dti.lp$data$mms %in% k, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_cb <- predict(model_cb, newdata=long)
    # CBM
    model_cbm <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), fitter = lrm, 
                                 xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end & dti.lp$data$mms %in% k, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_cbm <- predict(model_cbm, newdata=long)
    
    if (!j == "Detectable_intracranial_injury_on_CT_early") {
      # CBI 
      model_cbi <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i")), fitter = lrm, 
                                   xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end & dti.lp$data$mms %in% k, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
      long$temp_cbi <- predict(model_cbi, newdata=long)
      # CBIM 
      model_cbim <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), fitter = lrm, 
                                    xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end & dti.lp$data$mms %in% k, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
      long$temp_cbim <- predict(model_cbim, newdata=long)
      
      all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Imaging", "Modifiers"), 2)), 
                                     N = rep(model_cbim$stats[["Obs"]], 8), 
                                     Color = c("Clinical", "Biomarkers", "Imaging", "Modifiers", rep("Cumulative", 4)),
                                     R2 = c(model_clinical$stats[["R2"]], 
                                            model_biomarkers$stats[["R2"]],
                                            model_imaging$stats[["R2"]],
                                            model_modifiers$stats[["R2"]],
                                            model_clinical$stats[["R2"]],
                                            model_cb$stats[["R2"]],
                                            model_cbi$stats[["R2"]],
                                            model_cbim$stats[["R2"]]))
      
      list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                            aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers")), y = R2, 
                                                fill = factor(Color, levels = c("Clinical", "Biomarkers", "Imaging", "Modifiers", "Cumulative")))) +  
        geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 16, face = "bold"))  + ggtitle(paste0(gsub("_", " ", j), " in ", k, " TBI subgroup (n = ", unique(all_models_rsqrs$N), ")")) + 
        scale_fill_manual("", values = c(C_COLOR, B_COLOR, I_COLOR, M_COLOR, "grey60"))+
        labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
        geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(1), vjust = 0, size = 10)
      counter_r2 <- counter_r2+1
      
    } else {all_models_rsqrs <- data.frame(Model = c(rep(c("Clinical", "Biomarkers", "Modifiers"), 2)), 
                                           N = rep(model_cbm$stats[["Obs"]], 6), 
                                           Color = c("Clinical", "Biomarkers", "Modifiers", rep("Cumulative", 3)),
                                           R2 = c(model_clinical$stats[["R2"]], 
                                                  model_biomarkers$stats[["R2"]],
                                                  model_modifiers$stats[["R2"]],
                                                  model_clinical$stats[["R2"]],
                                                  model_cb$stats[["R2"]],
                                                  model_cbm$stats[["R2"]]))
    
    list_plots_r2[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                          aes(x = factor(Model, levels = c("Clinical", "Biomarkers", "Modifiers")), y = R2, 
                                              fill = factor(Color, levels = c("Clinical", "Biomarkers", "Modifiers", "Cumulative")))) +  
      geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none", text = element_text(size = 16, face = "bold"))  + ggtitle(paste0(gsub("_", " ", j), " in ", k, " TBI subgroup (n = ", unique(all_models_rsqrs$N), ")")) + 
      scale_fill_manual("", values = c(C_COLOR, B_COLOR, M_COLOR, "grey60"))+
      labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
      geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05, fontface = "bold"), position = position_dodge(1), vjust = 0, size = 10)
    counter_r2 <- counter_r2+1
    } 
  }
}

emf(file = "EXPORT/FigS6.emf", emfPlus = T, width = 22, height =30)
plot(ggarrange(plotlist = list_plots_r2, common.legend = T, ncol = 2, nrow = 8, legend = "none"))
dev.off()

pdf(file = "EXPORT/FigS6.pdf", width = 22, height =30)
plot(ggarrange(plotlist = list_plots_r2, common.legend = T, ncol = 2, nrow = 8, legend = "none"))
dev.off()

### COMPARISON with CRASH, IMPACT ---------------------------------------------------------------
list_plots_impact_crash <- list()
counter_r2 <- 1
long <- mice::complete(dti, "long", include = T)

for (validatein in c("IMPACT", "CRASH")){
  
  for (j in c("Mortality_at_6_months", "Unfavorable_outcome_at_6_months")) {
    
    # select only cases with observed endpoint, in target validation cohort
    if (validatein == "IMPACT"){
      # select IMPACT target population
      obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & 
                                     all_original_data$Age >= 14 & all_original_data$`GCS Score` %in% c(3:12), ]$subjectId
      print(table(all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & 
                                      all_original_data$Age >= 14 & all_original_data$`GCS Score` %in% c(3:12), ][, gsub("_", " ", j)]))
    } else {
      # select CRASH target population
      obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & 
                                     all_original_data$Age >= 16 & all_original_data$`GCS Score` %in% c(3:14), ]$subjectId
      print(table(all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & 
                                      all_original_data$Age >= 16 & all_original_data$`GCS Score` %in% c(3:14), ][, gsub("_", " ", j)]))
    }
    
    
    
    # IMPACT MODEL
    model_impact <- fit.mult.impute(as.formula(paste0("`", j, "` ~ ", ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == j, ]$RCS == 1, age_formula_rcs, age_formula_lin),
                                                      " + `GCS_Motor` + `Pupils` + `Hypoxia` + `Hypotension` + catg(`lesions_classification_marshall_ct_classification`)  + `TSAH` + `EDH_mass` + `Labs.DLGlucosemmolL` + `Labs.DLHemoglobingdL`")), 
                                    fitter = lrm, 
                                    xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                    fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    R2_CI_impact <- pooled_boot_CI(model_impact, dti, dti$data$subjectId %in% obs_end, B = 200, seed = 153)
    
    # CRASH MODEL
    model_crash <- fit.mult.impute(as.formula(paste0("`", j, "` ~ ", ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == j, ]$RCS == 1, age_formula_rcs, age_formula_lin),
                                                     " + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == j, ]$RCS == 1, c_formula_gcs_rcs, c_formula_gcs_lin),
                                                     " + `Pupils` + `Major_extracranial_injury` + `TAMVI` + `oblit_3_bc`  + `TSAH` + `MLS` + `hematoma`")), 
                                   fitter = lrm, 
                                   xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                   fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    
    R2_CI_crash <- pooled_boot_CI(model_crash, dti, dti$data$subjectId %in% obs_end, B = 200, seed = 153)
    
    # C PILLAR
    model_clinical <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, 
                                                                                          c_formula_gcs_rcs, c_formula_gcs_lin))), fitter = lrm, 
                                      xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                      fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_c <- predict(model_clinical, newdata=long)
    
    R2_CI_clinical <- pooled_boot_CI(model_clinical, dti, dti$data$subjectId %in% obs_end, B = 200, seed = 153)
    
    
    # B PILLAR
    model_biomarkers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ (", 
                                                          ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                          ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                          ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                          ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), fitter = lrm, 
                                        xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                        fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    
    long$temp_b <- predict(model_biomarkers, newdata=long) 
    
    
    # I PILLAR
    model_imaging <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), fitter = lrm, 
                                     xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                     fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_i <- predict(model_imaging, newdata=long) 
    
    
    # M PILLAR
    model_modifiers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+catg(`Seizures`)+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                                         ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                                         "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), fitter = lrm, 
                                       xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                       fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    long$temp_m <- predict(model_modifiers, newdata=long)  
    
    
    ## CUMULATIVE MODELS, plot redistribution of predicted risk
    dti.lp <- as.mids(long[, c(".imp", ".id", "subjectId", j, "temp_c", "temp_b", "temp_i", "temp_m")])
    
    # CB 
    model_cb <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b")), fitter = lrm, 
                                xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    R2_CI_cb <- pooled_boot_CI(model_cb, dti.lp, dti.lp$data$subjectId %in% obs_end, B = 200, seed = 153)
    
    # CBI 
    model_cbi <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i")), fitter = lrm, 
                                 xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    R2_CI_cbi <- pooled_boot_CI(model_cbi, dti.lp, dti.lp$data$subjectId %in% obs_end, B = 200, seed = 153)
    
    # CBIM 
    model_cbim <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), fitter = lrm, 
                                  xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
    R2_CI_cbim <- pooled_boot_CI(model_cbim, dti.lp, dti.lp$data$subjectId %in% obs_end, B = 200, seed = 153)
    
    if (validatein == "IMPACT"){
      all_models_rsqrs <- data.frame(Model = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab"), 
                                     N = rep(length(obs_end), 5), 
                                     Color = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab"),
                                     R2 = c(model_clinical$stats[["R2"]],
                                            model_cb$stats[["R2"]],
                                            model_cbi$stats[["R2"]],
                                            model_cbim$stats[["R2"]],
                                            model_impact$stats[["R2"]]),
                                     R2_lower = c(R2_CI_clinical$R2$lower,
                                                  R2_CI_cb$R2$lower,
                                                  R2_CI_cbi$R2$lower,
                                                  R2_CI_cbim$R2$lower,
                                                  R2_CI_impact$R2$lower),
                                     R2_upper = c(R2_CI_clinical$R2$upper,
                                                  R2_CI_cb$R2$upper,
                                                  R2_CI_cbi$R2$upper,
                                                  R2_CI_cbim$R2$upper,
                                                  R2_CI_impact$R2$upper),
                                     C = c(model_clinical$stats[["C"]],
                                           model_cb$stats[["C"]],
                                           model_cbi$stats[["C"]],
                                           model_cbim$stats[["C"]],
                                           model_impact$stats[["C"]]),
                                     C_lower = c(R2_CI_clinical$C$lower,
                                                 R2_CI_cb$C$lower,
                                                 R2_CI_cbi$C$lower,
                                                 R2_CI_cbim$C$lower,
                                                 R2_CI_impact$C$lower),
                                     C_upper = c(R2_CI_clinical$C$upper,
                                                 R2_CI_cb$C$upper,
                                                 R2_CI_cbi$C$upper,
                                                 R2_CI_cbim$C$upper,
                                                 R2_CI_impact$C$upper))
      write.csv(all_models_rsqrs, paste0("EXPORT/Impact_", j, ".csv"))
      
      list_plots_impact_crash[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                                      aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")), y = R2, 
                                                          fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")))) + 
        geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none") + ggtitle(paste0(gsub("_", " ", j), " (n = ", unique(all_models_rsqrs$N), ")")) + 
        scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "black"))+
        labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
        geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05), position = position_dodge(0.8), vjust = 0)
      counter_r2 <- counter_r2 + 1
      
    } else {
      all_models_rsqrs <- data.frame(Model = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT"), 
                                     N = rep(length(obs_end), 5), 
                                     Color = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT"),
                                     R2 = c(model_clinical$stats[["R2"]],
                                            model_cb$stats[["R2"]],
                                            model_cbi$stats[["R2"]],
                                            model_cbim$stats[["R2"]],
                                            model_crash$stats[["R2"]]),
                                     R2_lower = c(R2_CI_clinical$R2$lower,
                                                  R2_CI_cb$R2$lower,
                                                  R2_CI_cbi$R2$lower,
                                                  R2_CI_cbim$R2$lower,
                                                  R2_CI_crash$R2$lower),
                                     R2_upper = c(R2_CI_clinical$R2$upper,
                                                  R2_CI_cb$R2$upper,
                                                  R2_CI_cbi$R2$upper,
                                                  R2_CI_cbim$R2$upper,
                                                  R2_CI_crash$R2$upper),
                                     C = c(model_clinical$stats[["C"]],
                                           model_cb$stats[["C"]],
                                           model_cbi$stats[["C"]],
                                           model_cbim$stats[["C"]],
                                           model_crash$stats[["C"]]),
                                     C_lower = c(R2_CI_clinical$C$lower,
                                                 R2_CI_cb$C$lower,
                                                 R2_CI_cbi$C$lower,
                                                 R2_CI_cbim$C$lower,
                                                 R2_CI_crash$C$lower),
                                     C_upper = c(R2_CI_clinical$C$upper,
                                                 R2_CI_cb$C$upper,
                                                 R2_CI_cbi$C$upper,
                                                 R2_CI_cbim$C$upper,
                                                 R2_CI_crash$C$upper))
      write.csv(all_models_rsqrs, paste0("EXPORT/Crash_", j, ".csv"))
      
      list_plots_impact_crash[[counter_r2]] <- ggplot(all_models_rsqrs, 
                                                      aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")), y = R2, 
                                                          fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")))) + 
        geom_col(position = "dodge") + theme_minimal() + theme(legend.position = "none") + ggtitle(paste0(gsub("_", " ", j), " (n = ", unique(all_models_rsqrs$N), ")")) + 
        scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "black"))+
        labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
        geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05), position = position_dodge(0.8), vjust = 0)
      counter_r2 <- counter_r2 + 1
    }}}

impact_mort <- read.csv("EXPORT/Impact_Mortality_at_6_months.csv")
impact_unfav <- read.csv("EXPORT/Impact_Unfavorable_outcome_at_6_months.csv")
crash_mort <- read.csv("EXPORT/Crash_Mortality_at_6_months.csv")
crash_unfav <- read.csv("EXPORT/Crash_Unfavorable_outcome_at_6_months.csv")

impact_mort_R <- ggplot(impact_mort, 
                        aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")), y = R2, 
                            fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")))) + 
  geom_col(position = "dodge") + theme_minimal() + 
  scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "grey80"))+
  ggtitle("IMPACT | Mortality at 6 months", 
          subtitle = paste0("CENTER-TBI age ≥ 14, GCS score ≤ 12 (n = ", unique(impact_mort$N), ")")) + 
  labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
  geom_text(aes(label = c(
    paste0("[",format(round(impact_mort[impact_mort$Model == "C", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "C", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_mort[impact_mort$Model == "C+B", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "C+B", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_mort[impact_mort$Model == "C+B+I", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "C+B+I", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_mort[impact_mort$Model == "Full CBI-M", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "Full CBI-M", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_mort[impact_mort$Model == "IMPACT Lab", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "IMPACT Lab", ]$R2_upper, 2), nsmall = 2), "]")), 
    y = R2 - 0.1), vjust = 0, size = 1.75) + 
  theme(legend.position = "none", title = element_text(size=6), axis.text = element_text(size=5)) + 
  geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05), position = position_dodge(0.8), vjust = 0, size = 3)

impact_mort_C <- ggplot(impact_mort, 
                        aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")), y = C, 
                            fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")))) + 
  geom_col(position = "dodge") + theme_minimal() + 
  scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "grey80"))+
  ggtitle("IMPACT | Mortality at 6 months", 
          subtitle = paste0("CENTER-TBI age ≥ 14, GCS score ≤ 12 (n = ", unique(impact_mort$N), ")")) + 
  labs(y = "AUC", x ="")+ 
  geom_text(aes(label = c(
    paste0("[",format(round(impact_mort[impact_mort$Model == "C", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "C", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_mort[impact_mort$Model == "C+B", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "C+B", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_mort[impact_mort$Model == "C+B+I", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "C+B+I", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_mort[impact_mort$Model == "Full CBI-M", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "Full CBI-M", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_mort[impact_mort$Model == "IMPACT Lab", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_mort[impact_mort$Model == "IMPACT Lab", ]$C_upper, 2), nsmall = 2), "]")), 
    y = C - 0.05), vjust = 0, size = 1.75) + 
  theme(legend.position = "none", title = element_text(size=6), axis.text = element_text(size=5)) + 
  geom_text(aes(label = format(round(C, 2), nsmall = 2), y = C + 0.025), position = position_dodge(0.8), vjust = 0, size = 3)+
  coord_cartesian(ylim = c(0.5, 1))


impact_unfav_R <- ggplot(impact_unfav, 
                         aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")), y = R2, 
                             fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")))) + 
  geom_col(position = "dodge") + theme_minimal() + 
  scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "grey80"))+
  ggtitle("IMPACT | Unfavorable outcome at 6 months", 
          subtitle = paste0("CENTER-TBI age ≥ 14, GCS score ≤ 12 (n = ", unique(impact_unfav$N), ")")) + 
  labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
  geom_text(aes(label = c(
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "C", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "C", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "C+B", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "C+B", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "C+B+I", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "C+B+I", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "Full CBI-M", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "Full CBI-M", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "IMPACT Lab", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "IMPACT Lab", ]$R2_upper, 2), nsmall = 2), "]")), 
    y = R2 - 0.1), vjust = 0, size = 1.75) + 
  theme(legend.position = "none", title = element_text(size=6), axis.text = element_text(size=5)) + 
  geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05), position = position_dodge(0.8), vjust = 0, size = 3)

impact_unfav_C <- ggplot(impact_unfav, 
                         aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")), y = C, 
                             fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "IMPACT Lab")))) + 
  geom_col(position = "dodge") + theme_minimal() + 
  scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "grey80"))+
  ggtitle("IMPACT | Unfavorable outcome at 6 months",  
          subtitle = paste0("CENTER-TBI age ≥ 14, GCS score ≤ 12 (n = ", unique(impact_unfav$N), ")")) + 
  labs(y = "AUC", x ="")+ 
  geom_text(aes(label = c(
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "C", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "C", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "C+B", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "C+B", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "C+B+I", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "C+B+I", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "Full CBI-M", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "Full CBI-M", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(impact_unfav[impact_unfav$Model == "IMPACT Lab", ]$C_lower, 2), nsmall = 2), "-",
           format(round(impact_unfav[impact_unfav$Model == "IMPACT Lab", ]$C_upper, 2), nsmall = 2), "]")), 
    y = C - 0.05), vjust = 0, size = 1.75) + 
  theme(legend.position = "none", title = element_text(size=6), axis.text = element_text(size=5)) + 
  geom_text(aes(label = format(round(C, 2), nsmall = 2), y = C + 0.025), position = position_dodge(0.8), vjust = 0, size = 3)+
  coord_cartesian(ylim = c(0.5, 1))


crash_mort_R <- ggplot(crash_mort, 
                       aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")), y = R2, 
                           fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")))) + 
  geom_col(position = "dodge") + theme_minimal() + 
  scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "grey80"))+
  ggtitle("CRASH | Mortality at 6 months", 
          subtitle = paste0("CENTER-TBI age ≥ 16, GCS score ≤ 14 (n = ", unique(crash_mort$N), ")")) + 
  labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
  geom_text(aes(label = c(
    paste0("[",format(round(crash_mort[crash_mort$Model == "C", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "C", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_mort[crash_mort$Model == "C+B", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "C+B", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_mort[crash_mort$Model == "C+B+I", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "C+B+I", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_mort[crash_mort$Model == "Full CBI-M", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "Full CBI-M", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_mort[crash_mort$Model == "CRASH CT", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "CRASH CT", ]$R2_upper, 2), nsmall = 2), "]")), 
    y = R2 - 0.1), vjust = 0, size = 1.75) + 
  theme(legend.position = "none", title = element_text(size=6), axis.text = element_text(size=5)) + 
  geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05), position = position_dodge(0.8), vjust = 0, size = 3)

crash_mort_C <- ggplot(crash_mort, 
                       aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")), y = C, 
                           fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")))) + 
  geom_col(position = "dodge") + theme_minimal() + 
  scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "grey80"))+
  ggtitle("CRASH | Mortality at 6 months", 
          subtitle = paste0("CENTER-TBI age ≥ 16, GCS score ≤ 14 (n = ", unique(crash_mort$N), ")")) + 
  labs(y = "AUC", x ="")+ 
  geom_text(aes(label = c(
    paste0("[",format(round(crash_mort[crash_mort$Model == "C", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "C", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_mort[crash_mort$Model == "C+B", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "C+B", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_mort[crash_mort$Model == "C+B+I", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "C+B+I", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_mort[crash_mort$Model == "Full CBI-M", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "Full CBI-M", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_mort[crash_mort$Model == "CRASH CT", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_mort[crash_mort$Model == "CRASH CT", ]$C_upper, 2), nsmall = 2), "]")), 
    y = C - 0.05), vjust = 0, size = 1.75) + 
  theme(legend.position = "none", title = element_text(size=6), axis.text = element_text(size=5)) + 
  geom_text(aes(label = format(round(C, 2), nsmall = 2), y = C + 0.025), position = position_dodge(0.8), vjust = 0, size = 3)+
  coord_cartesian(ylim = c(0.5, 1))


crash_unfav_R <- ggplot(crash_unfav, 
                        aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")), y = R2, 
                            fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")))) + 
  geom_col(position = "dodge") + theme_minimal() + 
  scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "grey80"))+
  ggtitle("CRASH | Unfavorable outcome at 6 months",  
          subtitle = paste0("CENTER-TBI age ≥ 16, GCS score ≤ 14 (n = ", unique(crash_unfav$N), ")")) + 
  labs(y = expression(paste("Pseudo-", R^2)), x ="")+ ylim(0, 1)+
  geom_text(aes(label = c(
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "C", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "C", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "C+B", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "C+B", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "C+B+I", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "C+B+I", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "Full CBI-M", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "Full CBI-M", ]$R2_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "CRASH CT", ]$R2_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "CRASH CT", ]$R2_upper, 2), nsmall = 2), "]")), 
    y = R2 - 0.1), vjust = 0, size = 1.75) + 
  theme(legend.position = "none", title = element_text(size=6), axis.text = element_text(size=5)) + 
  geom_text(aes(label = format(round(R2, 2), nsmall = 2), y = R2 + 0.05), position = position_dodge(0.8), vjust = 0, size = 3)

crash_unfav_C <- ggplot(crash_unfav, 
                        aes(x = factor(Model, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")), y = C, 
                            fill = factor(Color, levels = c("C", "C+B", "C+B+I", "Full CBI-M", "CRASH CT")))) + 
  geom_col(position = "dodge") + theme_minimal() + 
  scale_fill_manual("", values = c("lightgreen", "lightgreen", "lightgreen", "lightgreen", "grey80"))+
  ggtitle("CRASH | Unfavorable outcome at 6 months",  
          subtitle = paste0("CENTER-TBI age ≥ 16, GCS score ≤ 14 (n = ", unique(crash_unfav$N), ")")) + 
  labs(y = "AUC", x ="")+
  geom_text(aes(label = c(
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "C", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "C", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "C+B", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "C+B", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "C+B+I", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "C+B+I", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "Full CBI-M", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "Full CBI-M", ]$C_upper, 2), nsmall = 2), "]"), 
    paste0("[",format(round(crash_unfav[crash_unfav$Model == "CRASH CT", ]$C_lower, 2), nsmall = 2), "-",
           format(round(crash_unfav[crash_unfav$Model == "CRASH CT", ]$C_upper, 2), nsmall = 2), "]")), 
    y = C - 0.05), vjust = 0, size = 1.75) + 
  theme(legend.position = "none", title = element_text(size=6), axis.text = element_text(size=5)) + 
  geom_text(aes(label = format(round(C, 2), nsmall = 2), y = C + 0.025), position = position_dodge(0.8), vjust = 0, size = 3)+
  coord_cartesian(ylim = c(0.5, 1))

tiff("EXPORT/Fig4_right.tiff", res = 600, width = 2.6, height = 7, units = "in")
ggarrange(impact_mort_R, impact_unfav_R, crash_mort_R, crash_unfav_R, ncol = 1)
dev.off()

tiff("EXPORT/IMPACT_CRASH_AUC_right.tiff", res = 600, width = 2.6, height = 7, units = "in")
ggarrange(impact_mort_C, impact_unfav_C, crash_mort_C, crash_unfav_C, ncol = 1)
dev.off()

emf(file = "EXPORT/IMPACT_CRASH_AUC_right.emf", emfPlus = T, width = 2.6, height = 7)
ggarrange(impact_mort_C, impact_unfav_C, crash_mort_C, crash_unfav_C, ncol = 1)
dev.off()

### SUNBURST PLOTS in 1 IMPUTED DATASET ----------------------------------------------
list_plots_sun <- list()
counter <- 1
supp_table <- data.frame(Outcome = c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
                                     "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL"), 
                         CBIM = NA, C = NA, B = NA,  I = NA, M = NA)

for (j in gsub(" ", "_", c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
                           "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL"))) {
  
  titles <- c("Intracranial injury\non CT early\n", "Hospital\nadmission\n", "ICU\nadmission\n", "ICP\nmonitoring\n",  "Major cranial surgery\nwithin 72h\n", "Mortality\nat 6 months\n", 
              "Unfavorable\noutcome at 6 months\n", "Incomplete recovery\nat 6 months\n", "Impairment\nof HRQOL\n")
  # select complete cases for each endpoint
  all_cc <- complete(dti, 5)
  all_cc <- all_cc[all_cc$subjectId %in% all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]), ]$subjectId &
                     !all_cc$subjectId %in% c("6DYY996"), ]
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  # C PILLAR
  model_clinical <- glm(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, 
                                                                            c_formula_gcs_rcs, c_formula_gcs_lin))), data = all_cc, family = "binomial")
  all_cc$temp_c <- model_clinical$linear.predictors
  
  # B PILLAR
  model_biomarkers <- glm(as.formula(paste0("`", j, "` ~ (", 
                                            ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                            ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial")
  all_cc$temp_b <- model_biomarkers$linear.predictors
  
  # M PILLAR
  model_modifiers <- glm(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+`Seizures`+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                           ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                           "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), data = all_cc, family = "binomial")
  all_cc$temp_m <- model_modifiers$linear.predictors
  
  # I PILLAR
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    model_imaging <- glm(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), data = all_cc, family = "binomial")
    all_cc$temp_i <- model_imaging$linear.predictors
    
    model_cbim <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), data = all_cc, family = "binomial")
    print(round(rsq.partial(model_cbim, type = 'n')$partial.rsq, 4))
    data <- data.frame(
      name = c(" ", "CBI-M", "Clinical", "Biomarkers", "Imaging", "Modifiers", " "),
      value = c(1-rsq::rsq(model_cbim, type = 'n'), 
                rsq::rsq(model_cbim, type = 'n'), 
                rsq::rsq.partial(model_cbim, type = 'n')$partial.rsq, 
                1 - sum(rsq::rsq.partial(model_cbim, type = 'n')$partial.rsq)),
      level = c(rep(1, 2), rep(2, 5)), 
      fill = c(" ", "CBI-M", "Clinical", "Biomarkers", "Imaging", "Modifiers", " ")
    )
    data$level <- as.factor(data$level)
    data$fill  <- factor(data$fill, levels = c(" ", "CBI-M", "Modifiers", "Imaging", "Biomarkers", "Clinical"))
    data$name <- as.factor(data$name)
    data[data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
    
    list_plots_sun[[counter]] <- data %>%  
      ggplot(aes(x = level, y = value, fill = fill)) +
      geom_col(width = c(rep(1.4, 2), rep(0.7, 5)), position = position_stack()) +
      coord_polar(theta = "y", direction = 1, clip = "off") +
      scale_fill_manual(values = c(NA, "grey60", M_COLOR, I_COLOR, B_COLOR, C_COLOR), na.translate = F, 
                        guide = guide_legend(reverse = TRUE, nrow=1)) +
      theme_minimal() + 
      labs(x = titles[counter]) +
      theme(axis.text.y = element_blank(), axis.title.y = element_text(face = "bold", size = 10),
            axis.title.x = element_blank(),
            legend.position = "none", 
            plot.margin = margin(0, 0, 0, 0)) 
    
    supp_table[counter, "CBIM"] <- round(data[data$fill =="CBI-M", "value"], 4)
    supp_table[counter, "C"] <- round(data[data$fill =="Clinical", "value"], 4)
    supp_table[counter, "B"] <- round(data[data$fill =="Biomarkers", "value"], 4)
    supp_table[counter, "I"] <- round(data[data$fill =="Imaging", "value"], 4)
    supp_table[counter, "M"] <- round(data[data$fill =="Modifiers", "value"], 4)
    counter <- counter + 1
    
  } else {
    model_cbm <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), data = all_cc, family = "binomial")
    print(round(rsq.partial(model_cbm, type = 'n')$partial.rsq, 4))
    data <- data.frame(
      name = c(" ", "CBIM", "Clinical", "Biomarkers", "Imaging", "Modifiers", " "),
      value = c(1-rsq::rsq(model_cbm, type = 'n'), rsq::rsq(model_cbm, type = 'n'), 
                rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq[1:2], 0,
                rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq[3],
                1 - sum(rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq)),
      level = c(1, 1, 2, 2, 2, 2, 2), 
      fill = c(" ", "CBIM", "Clinical", "Biomarkers", "Imaging", "Modifiers", " ")
    )
    data$level <- as.factor(data$level)
    data$fill  <- factor(data$fill, levels = c(" ", "CBIM", "Modifiers", "Imaging", "Biomarkers", "Clinical"))
    data$name <- as.factor(data$name)
    data[data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
    
    list_plots_sun[[counter]] <- data %>%  
      ggplot(aes(x = level, y = value, fill = fill)) +
      geom_col(width = c(rep(1.4, 2), rep(0.7, 5)), position = position_stack()) +
      coord_polar(theta = "y", direction = 1, clip = "off") +
      scale_fill_manual(values = c(NA, "grey60", M_COLOR, I_COLOR, B_COLOR, C_COLOR), na.translate = F, 
                        guide = guide_legend(reverse = TRUE, nrow=1)) +
      theme_minimal() + 
      labs(x = titles[counter]) +
      theme(axis.text.y = element_blank(), axis.title.y = element_text(face = "bold", size = 10),
            axis.title.x = element_blank(),
            legend.position = "none", 
            plot.margin = margin(0, 0, 0, 0)) 
    
    supp_table[counter, "CBIM"] <- round(data[data$fill =="CBIM", "value"], 4)
    supp_table[counter, "C"] <- round(data[data$fill =="Clinical", "value"], 4)
    supp_table[counter, "B"] <- round(data[data$fill =="Biomarkers", "value"], 4)
    supp_table[counter, "M"] <- round(data[data$fill =="Modifiers", "value"], 4)
    counter <- counter + 1
    
  }
  
}
write.xlsx(supp_table, "EXPORT/Supp_tb_2_Imputed.xlsx")


grid1 <- ggarrange(plotlist = list_plots_sun[c(1:5)], nrow = 5) 

tiff("EXPORT/Fig3_1.tiff", res = 600, width = 2, height = 7.5, units = "in")
annotate_figure(grid1, top = text_grob("CENTER-TBI\n", face = "bold", size = 14, hjust = 0.3))
dev.off()

grid2 <- ggarrange(plotlist = list_plots_sun[c(6:9)], nrow = 5) 

tiff("EXPORT/Fig3_3.tiff", res = 600, width = 2, height = 7.5, units = "in")
annotate_figure(grid2, top = text_grob("CENTER-TBI\n", face = "bold", size = 14, hjust = 0.3))
dev.off()


list_plots_sun2 <- list()
counter <- 1
supp_table <- data.frame(Outcome = c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
                                     "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL"), 
                         CBIM = NA, C = NA, B = NA,  I = NA, M = NA)

for (j in gsub(" ", "_", c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
                           "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL"))) {
  
  # select complete cases for each endpoint
  all_cc <- complete(dti, 5)
  all_cc <- all_cc[all_cc$subjectId %in% all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]), ]$subjectId &
                     !all_cc$subjectId %in% c("6DYY996"), ]
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  # C PILLAR
  model_clinical <- glm(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, 
                                                                            c_formula_gcs_rcs, c_formula_gcs_lin))), data = all_cc, family = "binomial")
  all_cc$temp_c <- model_clinical$linear.predictors
  
  # B PILLAR
  model_biomarkers <- glm(as.formula(paste0("`", j, "` ~ (", 
                                            ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                            ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial")
  all_cc$temp_b <- model_biomarkers$linear.predictors
  
  # M PILLAR
  model_modifiers <- glm(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+`Seizures`+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                           ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                           "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), data = all_cc, family = "binomial")
  all_cc$temp_m <- model_modifiers$linear.predictors
  
  # I PILLAR
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    model_imaging <- glm(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), data = all_cc, family = "binomial")
    all_cc$temp_i <- model_imaging$linear.predictors
    
    model_cbim <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), data = all_cc, family = "binomial")
    print(round(rsq.partial(model_cbim, type = 'n')$partial.rsq, 4))
    data <- data.frame(
      name = c(" ", "CBI-M", "Clinical", "Biomarkers", "Imaging", "Modifiers", " "),
      value = c(1-rsq::rsq(model_cbim, type = 'n'), 
                rsq::rsq(model_cbim, type = 'n'), 
                rsq::rsq.partial(model_cbim, type = 'n')$partial.rsq, 
                1 - sum(rsq::rsq.partial(model_cbim, type = 'n')$partial.rsq)),
      level = c(rep(1, 2), rep(2, 5)), 
      fill = c(" ", "CBI-M", "Clinical", "Biomarkers", "Imaging", "Modifiers", " ")
    )
    data$level <- as.factor(data$level)
    data$fill  <- factor(data$fill, levels = c(" ", "CBI-M", "Modifiers", "Imaging", "Biomarkers", "Clinical"))
    data$name <- as.factor(data$name)
    data[data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
    
    list_plots_sun2[[counter]] <- data %>%  
      ggplot(aes(x = level, y = value, fill = fill)) +
      geom_col(width = c(rep(1.4, 2), rep(0.7, 5)), position = position_stack()) +
      coord_polar(theta = "y", direction = 1, clip = "off") +
      scale_fill_manual(values = c(NA, "grey60", M_COLOR, I_COLOR, B_COLOR, C_COLOR), na.translate = F, 
                        guide = guide_legend(reverse = TRUE, nrow=1)) +
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none", 
            plot.margin = margin(0, 0, 0, 0)) 
    
    supp_table[counter, "CBIM"] <- round(data[data$fill =="CBI-M", "value"], 4)
    supp_table[counter, "C"] <- round(data[data$fill =="Clinical", "value"], 4)
    supp_table[counter, "B"] <- round(data[data$fill =="Biomarkers", "value"], 4)
    supp_table[counter, "I"] <- round(data[data$fill =="Imaging", "value"], 4)
    supp_table[counter, "M"] <- round(data[data$fill =="Modifiers", "value"], 4)
    counter <- counter + 1
    
  } else {
    model_cbm <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), data = all_cc, family = "binomial")
    print(round(rsq.partial(model_cbm, type = 'n')$partial.rsq, 4))
    data <- data.frame(
      name = c(" ", "CBIM", "Clinical", "Biomarkers", "Imaging", "Modifiers", " "),
      value = c(1-rsq::rsq(model_cbm, type = 'n'), rsq::rsq(model_cbm, type = 'n'), 
                rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq[1:2], 0,
                rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq[3],
                1 - sum(rsq::rsq.partial(model_cbm, type = 'n')$partial.rsq)),
      level = c(1, 1, 2, 2, 2, 2, 2), 
      fill = c(" ", "CBIM", "Clinical", "Biomarkers", "Imaging", "Modifiers", " ")
    )
    data$level <- as.factor(data$level)
    data$fill  <- factor(data$fill, levels = c(" ", "CBIM", "Modifiers", "Imaging", "Biomarkers", "Clinical"))
    data$name <- as.factor(data$name)
    data[data$value < 0.05 | data$name %in% c(" "), ]$name <- NA
    
    list_plots_sun2[[counter]] <- data %>%  
      ggplot(aes(x = level, y = value, fill = fill)) +
      geom_col(width = c(rep(1.4, 2), rep(0.7, 5)), position = position_stack()) +
      coord_polar(theta = "y", direction = 1, clip = "off") +
      scale_fill_manual(values = c(NA, "grey60", M_COLOR, I_COLOR, B_COLOR, C_COLOR), na.translate = F, 
                        guide = guide_legend(reverse = TRUE, nrow=1)) +
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none", 
            plot.margin = margin(0, 0, 0, 0)) 
    
    supp_table[counter, "CBIM"] <- round(data[data$fill =="CBIM", "value"], 4)
    supp_table[counter, "C"] <- round(data[data$fill =="Clinical", "value"], 4)
    supp_table[counter, "B"] <- round(data[data$fill =="Biomarkers", "value"], 4)
    supp_table[counter, "M"] <- round(data[data$fill =="Modifiers", "value"], 4)
    counter <- counter + 1
    
  }
  if (counter==10){
    placeholder <- data %>%  
      ggplot(aes(x = level, y = value, fill = fill)) +
      geom_col(width = c(rep(1.4, 2), rep(0.7, 5)), position = position_stack()) +
      coord_polar(theta = "y", direction = 1, clip = "off") +
      scale_fill_manual(values = c(NA, "grey60", M_COLOR, I_COLOR, B_COLOR, C_COLOR), na.translate = F, 
                        guide = guide_legend(reverse = TRUE)) +
      theme_minimal() + 
      labs(x = titles[counter]) +
      theme(axis.text.y = element_blank(), axis.title.y = element_text(face = "bold", size = 10),
            axis.title.x = element_blank(),
            legend.position = "bottom", legend.direction = "vertical",
            legend.title = element_blank(), axis.text.x = element_text(face = "bold", size = 4),
            plot.margin = margin(0, 0, 0, 0)) 
    legend <- cowplot::get_legend(list_plots_sun[[9]])
    list_plots_sun2[[counter]] <- as_ggplot(get_legend(placeholder))
  }
}

grid1 <- ggarrange(plotlist = list_plots_sun2[c(1:5)], nrow = 5) 

tiff("EXPORT/Fig3_2MOCKUP.tiff", res = 600, width = 1.5, height = 7.5, units = "in")
annotate_figure(grid1, top = text_grob("TRACK-TBI\n", face = "bold", size = 14, hjust = 0.5))
dev.off()

grid2 <- ggarrange(plotlist = list_plots_sun2[c(6:10)], nrow = 5) 

tiff("EXPORT/Fig3_4_legendMOCKUPp.tiff", res = 600, width = 1.5, height = 7.5, units = "in")
annotate_figure(grid2, top = text_grob("TRACK-TBI\n", face = "bold", size = 14, hjust = 0.5))
dev.off()

### INTERNAL VALIDATION -----------------------------------------------

AppVal <- data.frame(Model = rep(c("C", "B", "I", "M", "CB", "CBI", "CBIM"), 9), 
                     Outcome = c(rep(endpoints[1], 7),
                                 rep(endpoints[2], 7),
                                 rep(endpoints[3], 7),
                                 rep(endpoints[4], 7),
                                 rep(endpoints[5], 7),
                                 rep(endpoints[6], 7),
                                 rep(endpoints[7], 7),
                                 rep(endpoints[8], 7),
                                 rep(endpoints[9], 7)),  
                     Events=NA, n = NA, App_C=NA, Opt_C=NA, Int_C=NA, App_R=NA, Opt_R=NA, Int_R=NA)
R2 <- function(y,p)
{
  p[p==1] <- 0.9999999   
  min.2.loglik <- -2*sum((y*log(p)+(1-y)*log(1-p)))
  min.2.loglik.null <- -2*sum((y*log(mean(y))+(1-y)*log(1-mean(y))))
  n <- length(y)
  R2 <- (1-exp((min.2.loglik-min.2.loglik.null)/n))/(1-exp(-min.2.loglik.null/n))
}

for (j in endpoints[1:9]) {
  
  long <- mice::complete(dti, "long", include = T)
  
  # Select only cases with observed endpoint
  obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]), ]$subjectId
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP_monitoring")) {obs_end <- all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]) & all_original_data$`ICU admission` %in% c("Yes"), ]$subjectId} 
  obs_end <- obs_end[!obs_end == "6DYY996"]
  
  # FIT PILLAR AND CUMULATIVE MODELS IN ORIGINAL SAMPLE
  model_clinical <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, 
                                                                                        c_formula_gcs_rcs, c_formula_gcs_lin))), fitter = lrm, 
                                    xtrans = dti, subset = dti$data$subjectId %in% obs_end, fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  long$temp_c <- predict(model_clinical, newdata=long)
  
  print(table(model_clinical$y))
  AppVal[AppVal$Outcome == j, 3] <- sum(model_clinical$y %in% c("Yes", "1"))
  AppVal[AppVal$Outcome == j, 4] <- length(model_clinical$y)
  
  model_biomarkers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ (", 
                                                        ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                        ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                        ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                        ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), fitter = lrm, 
                                      xtrans = dti, subset = dti$data$subjectId %in% obs_end, pr = F)
  long$temp_b <- predict(model_biomarkers, newdata=long) 
  
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    model_imaging <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), fitter = lrm, 
                                     xtrans = dti, subset = dti$data$subjectId %in% obs_end, pr = F)
    long$temp_i <- predict(model_imaging, newdata=long) 
  } else {
    model_imaging <- NULL
    long$temp_i <- NA
  }
  model_modifiers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+catg(`Seizures`)+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                                       ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                                       "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), fitter = lrm, 
                                     xtrans = dti, subset = dti$data$subjectId %in% obs_end, pr = F)
  long$temp_m <- predict(model_modifiers, newdata=long)  
  
  ## Store pillar model LPs to create the cumulative models in original sample
  dti.lp <- as.mids(long[, c(".imp", ".id", "subjectId", j, "temp_c", "temp_b", "temp_i", "temp_m")])
  
  model_cb <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b")), fitter = lrm, xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, pr = F)
  
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    model_cbi <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i")), fitter = lrm, xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, pr = F)
    model_cbim <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), fitter = lrm, xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, pr = F)
  }else{model_cbm <- fit.mult.impute(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), fitter = lrm, xtrans = dti.lp, subset = dti.lp$data$subjectId %in% obs_end, pr = F)}
  
  set.seed(21)
  B <- 10
  optC_C <- optC_B <- optC_I <- optC_M <- optC_CB <- optC_CBI <- optC_CBIM <- optC_CBM <- rep(NA, B)
  optR_C <- optR_B <- optR_I <- optR_M <- optR_CB <- optR_CBI <- optR_CBIM <- optR_CBM <- rep(NA, B)
  
  for (i in 1:B){
    ### select bootstrap sample from subjects w observed endpoint
    boot <- sample(as.numeric(names(model_clinical$y)), replace=TRUE)
    print(paste0(j, " ", i, " ", all(dti$data[boot, ]$subjectId %in% obs_end)))
    
    # Refit pillar models in bootstrap sample -> store LPs -> fit cumulative models in bootstrap sample 
    clinical.boot   <- fit.mult.impute(model_clinical$sformula, fitter = lrm, xtrans = dti, subset = boot, pr=F)
    biomarker.boot  <- fit.mult.impute(model_biomarkers$sformula, fitter = lrm, xtrans = dti, subset = boot, pr=F)
    modifiers.boot <- fit.mult.impute(model_modifiers$sformula, fitter = lrm, xtrans = dti, subset = boot, pr=F)
    
    long$boot_c <- predict(clinical.boot, long)
    long$boot_b <- predict(biomarker.boot, long)
    long$boot_m <- predict(modifiers.boot, long)
    
    if (!j == "Detectable_intracranial_injury_on_CT_early") {
      imaging.boot    <- fit.mult.impute(model_imaging$sformula, fitter = lrm, xtrans = dti, subset = boot, pr=F)
      long$boot_i <- predict(imaging.boot, long)
    } else {long$boot_i <- NA}
    
    # Store bootstrap sample pillar models LPs
    dti.lp <- as.mids(long[, c(".imp", ".id", "subjectId", j, "boot_c", "boot_b", "boot_i", "boot_m")])
    # Fit cumulative models in bootstrap sample based on pillar models LPs
    cb.boot   <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_c + boot_b")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F)
    long$boot_cb <- predict(cb.boot, long)
    
    if (!j == "Detectable_intracranial_injury_on_CT_early") {
      cbi.boot   <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_c + boot_b + boot_i")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F)
      cbim.boot   <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_c + boot_b + boot_i + boot_m")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F)
      long$boot_cbi <- predict(cbi.boot, long)
      long$boot_cbim <- predict(cbim.boot, long)
      long$boot_cbm <- NA
    } else {
      cbm.boot   <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_c + boot_b + boot_m")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F)
      long$boot_cbi <- long$boot_cbim <- NA
      long$boot_cbm <- predict(cbm.boot, long)
    }
    
    # Store pillar + cumulative models in bootstrap sample LPs
    dti.lp <- as.mids(long[, c(".imp", ".id", "subjectId", j, "boot_c", "boot_b", "boot_i", "boot_m", "boot_cb", "boot_cbi", "boot_cbm", "boot_cbim")])
    
    ### Validation in bootstrap and original sample
    clinical.boot   <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_c")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F, fit.reps = TRUE, fitargs = list(x=TRUE, y=TRUE))
    biomarker.boot <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_b")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F, fit.reps = TRUE, fitargs = list(x=TRUE, y=TRUE))
    modifiers.boot <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_m")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F, fit.reps = TRUE, fitargs = list(x=TRUE, y=TRUE))
    cb.boot <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_cb")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F, fit.reps = TRUE, fitargs = list(x=TRUE, y=TRUE))
    
    if (!j == "Detectable_intracranial_injury_on_CT_early") {
      imaging.boot <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_i")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F, fit.reps = TRUE, fitargs = list(x=TRUE, y=TRUE))
      cbi.boot <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_cbi")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F, fit.reps = TRUE, fitargs = list(x=TRUE, y=TRUE))
      cbim.boot <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_cbim")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F, fit.reps = TRUE, fitargs = list(x=TRUE, y=TRUE))
    } else {cbm.boot <- fit.mult.impute(as.formula(paste0("`", j, "` ~ boot_cbm")), fitter = lrm, xtrans = dti.lp, subset = boot, pr=F, fit.reps = TRUE, fitargs = list(x=TRUE, y=TRUE))}
    
    c.val_C <- c.val_B <- c.val_I <- c.val_M <- c.val_CB <- c.val_CBI <- c.val_CBIM <- c.val_CBM <- rep(0,5)
    r.val_C <- r.val_B <- r.val_I <- r.val_M <- r.val_CB <- r.val_CBI <- r.val_CBIM <- r.val_CBM <- rep(0,5)
    c.orig_C <- c.orig_B <- c.orig_I <- c.orig_M <- c.orig_CB <- c.orig_CBI <- c.orig_CBIM <- c.orig_CBM <- rep(0,5)
    r.orig_C <- r.orig_B <- r.orig_I <- r.orig_M <- r.orig_CB <- r.orig_CBI <- r.orig_CBIM <- r.orig_CBM <- rep(0,5)
    
    for (k in 1:5) { 
      # C
      c.val_C[k] <- concordance(clinical.boot$y ~ predict(clinical.boot$fits[[k]], newdata=clinical.boot$fits[[k]]$x))$concordance 
      r.val_C[k] <- R2(1*(clinical.boot$y=="Yes"), plogis(predict(clinical.boot$fits[[k]], newdata=clinical.boot$fits[[k]]$x)))
      
      c.orig_C[k] <- concordance(model_clinical$y ~ predict(clinical.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ]))$concordance 
      r.orig_C[k] <- R2(1*(model_clinical$y=="Yes"), plogis(predict(clinical.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ])))
      
      # B
      c.val_B[k] <- concordance(clinical.boot$y ~ predict(biomarker.boot$fits[[k]], newdata=biomarker.boot$fits[[k]]$x))$concordance 
      r.val_B[k] <- R2(1*(clinical.boot$y=="Yes"), plogis(predict(biomarker.boot$fits[[k]], newdata=biomarker.boot$fits[[k]]$x)))
      
      c.orig_B[k] <- concordance(model_clinical$y ~ predict(biomarker.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ]))$concordance 
      r.orig_B[k] <- R2(1*(model_clinical$y=="Yes"), plogis(predict(biomarker.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ])))
      
      # M
      c.val_M[k] <- concordance(clinical.boot$y ~ predict(modifiers.boot$fits[[k]], newdata=modifiers.boot$fits[[k]]$x))$concordance 
      r.val_M[k] <- R2(1*(clinical.boot$y=="Yes"), plogis(predict(modifiers.boot$fits[[k]], newdata=modifiers.boot$fits[[k]]$x)))
      
      c.orig_M[k] <- concordance(model_clinical$y ~ predict(modifiers.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ]))$concordance 
      r.orig_M[k] <- R2(1*(model_clinical$y=="Yes"), plogis(predict(modifiers.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ])))
      
      # CB
      c.val_CB[k] <- concordance(clinical.boot$y ~ predict(cb.boot$fits[[k]], newdata=cb.boot$fits[[k]]$x))$concordance 
      r.val_CB[k] <- R2(1*(clinical.boot$y=="Yes"), plogis(predict(cb.boot$fits[[k]], newdata=cb.boot$fits[[k]]$x)))
      
      c.orig_CB[k] <- concordance(model_clinical$y ~ predict(cb.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ]))$concordance 
      r.orig_CB[k] <- R2(1*(model_clinical$y=="Yes"), plogis(predict(cb.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ])))
      
      
      if (!j == "Detectable_intracranial_injury_on_CT_early") {
        # I
        c.val_I[k] <- concordance(clinical.boot$y ~ predict(imaging.boot$fits[[k]], newdata=imaging.boot$fits[[k]]$x))$concordance 
        r.val_I[k] <- R2(1*(clinical.boot$y=="Yes"), plogis(predict(imaging.boot$fits[[k]], newdata=imaging.boot$fits[[k]]$x)))
        
        c.orig_I[k] <- concordance(model_clinical$y ~ predict(imaging.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ]))$concordance 
        r.orig_I[k] <- R2(1*(model_clinical$y=="Yes"), plogis(predict(imaging.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ])))
        
        # CBI
        c.val_CBI[k] <- concordance(clinical.boot$y ~ predict(cbi.boot$fits[[k]], newdata=cbi.boot$fits[[k]]$x))$concordance 
        r.val_CBI[k] <- R2(1*(clinical.boot$y=="Yes"), plogis(predict(cbi.boot$fits[[k]], newdata=cbi.boot$fits[[k]]$x)))
        
        c.orig_CBI[k] <- concordance(model_clinical$y ~ predict(cbi.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ]))$concordance 
        r.orig_CBI[k] <- R2(1*(model_clinical$y=="Yes"), plogis(predict(cbi.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ])))
        
        # CBIM
        c.val_CBIM[k] <- concordance(clinical.boot$y ~ predict(cbim.boot$fits[[k]], newdata=cbim.boot$fits[[k]]$x))$concordance 
        r.val_CBIM[k] <- R2(1*(clinical.boot$y=="Yes"), plogis(predict(cbim.boot$fits[[k]], newdata=cbim.boot$fits[[k]]$x)))
        
        c.orig_CBIM[k] <- concordance(model_clinical$y ~ predict(cbim.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ]))$concordance 
        r.orig_CBIM[k] <- R2(1*(model_clinical$y=="Yes"), plogis(predict(cbim.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ])))
        
      } else {
        # CBM
        c.val_CBM[k] <- concordance(clinical.boot$y ~ predict(cbm.boot$fits[[k]], newdata=cbm.boot$fits[[k]]$x))$concordance 
        r.val_CBM[k] <- R2(1*(clinical.boot$y=="Yes"), plogis(predict(cbm.boot$fits[[k]], newdata=cbm.boot$fits[[k]]$x)))
        
        c.orig_CBM[k] <- concordance(model_clinical$y ~ predict(cbm.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ]))$concordance 
        r.orig_CBM[k] <- R2(1*(model_clinical$y=="Yes"), plogis(predict(cbm.boot$fits[[k]], newdata=long[long$.imp == k & long$subjectId %in% obs_end, ])))
      }
    }
    
    ### Store C and R2 Diff between bootstrap and alternative
    optC_C[i] <- (sum(c.val_C)/5)-(sum(c.orig_C)/5)
    optR_C[i] <- (sum(r.val_C)/5)-(sum(r.orig_C)/5)
    
    optC_B[i] <- (sum(c.val_B)/5)-(sum(c.orig_B)/5)
    optR_B[i] <- (sum(r.val_B)/5)-(sum(r.orig_B)/5)
    
    optC_M[i] <- (sum(c.val_M)/5)-(sum(c.orig_M)/5)
    optR_M[i] <- (sum(r.val_M)/5)-(sum(r.orig_M)/5)
    
    optC_CB[i] <- (sum(c.val_CB)/5)-(sum(c.orig_CB)/5)
    optR_CB[i] <- (sum(r.val_CB)/5)-(sum(r.orig_CB)/5)
    
    if (!j == "Detectable_intracranial_injury_on_CT_early") {
      
      optC_I[i] <- (sum(c.val_I)/5)-(sum(c.orig_I)/5)
      optR_I[i] <- (sum(r.val_I)/5)-(sum(r.orig_I)/5)
      
      optC_CBI[i] <- (sum(c.val_CBI)/5)-(sum(c.orig_CBI)/5)
      optR_CBI[i] <- (sum(r.val_CBI)/5)-(sum(r.orig_CBI)/5)
      
      optC_CBIM[i] <- (sum(c.val_CBIM)/5)-(sum(c.orig_CBIM)/5)
      optR_CBIM[i] <- (sum(r.val_CBIM)/5)-(sum(r.orig_CBIM)/5)
    } else {
      
      optC_CBM[i] <- (sum(c.val_CBM)/5)-(sum(c.orig_CBM)/5)
      optR_CBM[i] <- (sum(r.val_CBM)/5)-(sum(r.orig_CBM)/5)
    }
    
  }
  
  AppVal[AppVal$Model == "C" & AppVal$Outcome == j, 5:10] <- round(c(model_clinical$stats[["C"]], mean(optC_C), model_clinical$stats[["C"]] - mean(optC_C), 
                                                                     model_clinical$stats[["R2"]], mean(optR_C), model_clinical$stats[["R2"]] - mean(optR_C)), 3)
  AppVal[AppVal$Model == "B" & AppVal$Outcome == j, 5:10] <- round(c(model_biomarkers$stats[["C"]], mean(optC_B), model_biomarkers$stats[["C"]] - mean(optC_B), 
                                                                     model_biomarkers$stats[["R2"]], mean(optR_B), model_biomarkers$stats[["R2"]] - mean(optR_B)), 3)
  AppVal[AppVal$Model == "M" & AppVal$Outcome == j, 5:10] <- round(c(model_modifiers$stats[["C"]], mean(optC_M),  model_modifiers$stats[["C"]] - mean(optC_M), 
                                                                     model_modifiers$stats[["R2"]], mean(optR_M), model_modifiers$stats[["R2"]] - mean(optR_M)), 3)
  AppVal[AppVal$Model == "CB" & AppVal$Outcome == j, 5:10] <- round(c(model_cb$stats[["C"]], mean(optC_CB), model_cb$stats[["C"]] - mean(optC_CB), 
                                                                      model_cb$stats[["R2"]], mean(optR_CB), model_cb$stats[["R2"]] - mean(optR_CB)), 3)
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    AppVal[AppVal$Model == "I" & AppVal$Outcome == j, 5:10] <- round(c(model_imaging$stats[["C"]], mean(optC_I), model_imaging$stats[["C"]] - mean(optC_I), 
                                                                       model_imaging$stats[["R2"]], mean(optR_I), model_imaging$stats[["R2"]] - mean(optR_I)), 3)
    AppVal[AppVal$Model == "CBI" & AppVal$Outcome == j, 5:10] <- round(c(model_cbi$stats[["C"]], mean(optC_CBI), model_cbi$stats[["C"]] - mean(optC_CBI), 
                                                                         model_cbi$stats[["R2"]], mean(optR_CBI), model_cbi$stats[["R2"]] - mean(optR_CBI)), 3)
    AppVal[AppVal$Model == "CBIM" & AppVal$Outcome == j, 5:10] <- round(c(model_cbim$stats[["C"]], mean(optC_CBIM), model_cbim$stats[["C"]] - mean(optC_CBIM), 
                                                                          model_cbim$stats[["R2"]], mean(optR_CBIM), model_cbim$stats[["R2"]] - mean(optR_CBIM)), 3)
  }else{ 
    AppVal[AppVal$Model == "CBIM" & AppVal$Outcome == j, 5:10] <- round(c(model_cbm$stats[["C"]], mean(optC_CBM), model_cbm$stats[["C"]] - mean(optC_CBM), 
                                                                          model_cbm$stats[["R2"]], mean(optR_CBM), model_cbm$stats[["R2"]] - mean(optR_CBM)), 3)
  }
}

write.csv(AppVal, "appval1000.csv")

AppVal <- read.csv("appval1000.csv")
AppVal$EN <- paste0(AppVal$Events, "/", AppVal$n)
AppVal$AUC <- paste0(format(round(AppVal$App_C, 2), nsmall = 2, trim=T, scientific = F), " (", format(round(AppVal$Int_C, 2), nsmall = 2, trim=T, scientific = F), ")")
AppVal$R2 <- paste0(format(round(AppVal$App_R, 2), nsmall = 2, trim=T, scientific = F), " (", format(round(AppVal$Int_R, 2), nsmall = 2, trim=T, scientific = F), ")")

EditAppVal <- reshape(AppVal[, c(2:3, 12, 7, 13, 10, 14)], idvar = "Outcome", timevar = "Model", direction = "wide")
EditAppVal$Outcome <- gsub("_", " ", EditAppVal$Outcome)
EditAppVal[, c("EN.B", "EN.I", "EN.M", "EN.CB", "EN.CBI", "EN.CBIM")] <- NULL
EditAppVal <- EditAppVal %>% 
  mutate_at(c(2:30), as.character())
EditAppVal[EditAppVal == "0"] <- "0.000"
EditAppVal[10, ] <- c("Endpoint", "N events/sample",
                      "C", rep("", 3), 
                      "B", rep("", 3), 
                      "I", rep("", 3), 
                      "M", rep("", 3), 
                      "C+B", rep("", 3), 
                      "C+B+I", rep("", 3), 
                      "Full CBI-M", rep("", 3))
EditAppVal[11, ] <- c("", "", 
                      rep(c("AUC", "", "Pseudo-R2", ""), 7))
EditAppVal[12, ] <- c("", "", 
                      rep(c("Opt", "Apparent (Corrected)"), 14))


EditAppVal <- EditAppVal[c(10:12, 1:9),]
write.xlsx(EditAppVal, "EXPORT/STb7_IntValidation.xlsx")

# REVISION: Biomarkers sampled before/after surgery -----------------------------------------------------------
table(Cranial_Surgery_Times$major_cran_surg72)
Cranial_Surgery_Times_B <- merge(Cranial_Surgery_Times, all[, c("subjectId", "Time to sampling")], by= "subjectId")
table(is.na(Cranial_Surgery_Times_B[Cranial_Surgery_Times_B$major_cran_surg72 %in% c("Yes"), ]$`Time to sampling`))
PercTable(Cranial_Surgery_Times_B[Cranial_Surgery_Times_B$major_cran_surg72 %in% c("Yes") & 
                                    !is.na(Cranial_Surgery_Times_B$`Time to sampling`), ]$`Time to sampling` < Cranial_Surgery_Times_B[Cranial_Surgery_Times_B$major_cran_surg72 %in% c("Yes") & 
                                                                                                                                         !is.na(Cranial_Surgery_Times_B$`Time to sampling`), ]$surgery_time)
sub_bbb <- data.frame(Group = c("Observed biomarker levels and sampling time, regardless of sampling time", 
                                "Observed biomarker levels and sampling time, sampling before surgery", 
                                "Observed biomarker levels and sampling time, sampling after surgery"),
                      n = NA, Major_cranial_surgery_within_72h = NA, No_Major_cranial_surgery_within_72h = NA, AUC = NA, R2 = NA)

long <- mice::complete(dti, "long", include = T)
long <- merge(long, Cranial_Surgery_Times[, c("subjectId", "surgery_time")], by = "subjectId")

# For 574 pts with major surgery, check if sampling before surgery
table(long[long$.imp == 0, ]$Major_cranial_surgery_within_72h)

# observed time to sampling: 414 total, sampling after surgery 330, sampling before surgery 84 
long$sampling_before_surgery <- NA
long[long$.imp == 0 & long$Major_cranial_surgery_within_72h %in% c("Yes"), ]$sampling_before_surgery <- long[long$.imp == 0 & long$Major_cranial_surgery_within_72h %in% c("Yes"), ]$Time_to_sampling < long[long$.imp == 0 & long$Major_cranial_surgery_within_72h %in% c("Yes"), ]$surgery_time
table(long$sampling_before_surgery)

j <- "Major_cranial_surgery_within_72h"

for (group in sub_bbb$Group){
  
  if (group == "Observed biomarker levels and sampling time, regardless of sampling time"){
    obs_end <- long[long$.imp == 0 & !is.na(long$Major_cranial_surgery_within_72h) & !is.na(long$Time_to_sampling), ]$subjectId
    obs_end <- obs_end[!obs_end == "6DYY996"]
    print(length(obs_end))
  }
  
  if (group == "Observed biomarker levels and sampling time, sampling before surgery"){
    obs_end <- long[long$.imp == 0 & !is.na(long$Major_cranial_surgery_within_72h) & !is.na(long$Time_to_sampling) & 
                      (long$Major_cranial_surgery_within_72h %in% c("No") | long$sampling_before_surgery %in% c(TRUE)), ]$subjectId
    obs_end <- obs_end[!obs_end == "6DYY996"]
    print(length(obs_end))
  }
  
  if (group == "Observed biomarker levels and sampling time, sampling after surgery"){
    obs_end <- long[long$.imp == 0 & !is.na(long$Major_cranial_surgery_within_72h) & !is.na(long$Time_to_sampling) & 
                      (long$Major_cranial_surgery_within_72h %in% c("No") | long$sampling_before_surgery %in% c(FALSE)), ]$subjectId
    obs_end <- obs_end[!obs_end == "6DYY996"]
    print(length(obs_end))
  }
  
  model_biomarkers <- fit.mult.impute(as.formula(paste0("`", j, "` ~ (", 
                                                        ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                                        ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                                        ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                                        ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), fitter = lrm, 
                                      xtrans = dti, subset = dti$data$subjectId %in% obs_end, 
                                      fit.reps = T, fitargs = list(x=TRUE, y=TRUE), pr = F)
  
  sub_bbb[sub_bbb$Group == group, ]$n <- model_biomarkers$stats[["Obs"]]
  sub_bbb[sub_bbb$Group == group, ]$Major_cranial_surgery_within_72h <- sum(model_biomarkers$y == "Yes")
  sub_bbb[sub_bbb$Group == group, ]$No_Major_cranial_surgery_within_72h <- sum(model_biomarkers$y == "No")
  sub_bbb[sub_bbb$Group == group, ]$AUC <- round(model_biomarkers$stats[["C"]], 2)
  sub_bbb[sub_bbb$Group == group, ]$R2 <- round(model_biomarkers$stats[["R2"]], 2)
  
}

write.xlsx(sub_bbb, "EXPORT/subgroup_bbb_surg.xlsx")

### Compare pre/post-surgery BBBs-----------------------------------------------------------------

# https://center-tbi.incf.org/_6699106e4f12bd0241e5744e
B <- read.csv("IMPORT/Biomarkers.18.07.2024.csv")
length(unique(B$subjectId)) 

## SELECT PATIENT SUBSET WITH AT LEAST 1/6 BIOMARKERS SAMPLED (GFAP | S100B | UCH-L1 | NFL | NSE | Tau)

# Exclude patients with no samples
table(B$Biomarkers.SampleId != "")
B <- B[B$Biomarkers.SampleId != "",]
length(unique(B$subjectId))   # 706 patients with no samples collected -> excluded

# Exclude patients with samples in which none of the 6 biomarkers were measured
B$number_of_biomearkers_measured <- rowSums(!is.na(B[, c("Biomarkers.GFAP", "Biomarkers.S100B", "Biomarkers.UCH.L1",
                                                         "Biomarkers.NFL", "Biomarkers.NSE", "Biomarkers.Tau")]))
table(B$number_of_biomearkers_measured)   # remove 15 samples with all 6 biomarker values missing
B <- B[B$number_of_biomearkers_measured != 0,]
length(unique(B$subjectId)) # 1 patient excluded

## CALCULATE TIME FROM INJURY TO SAMPLING
B$bbb_collection_dt <- as.POSIXct(paste(B$Biomarkers.CollectionDate, B$Biomarkers.CollectionTime), format="%Y-%m-%d %H:%M:%S")

# Add date and time of injury
DT <- read.csv("IMPORT/InjuryDT.6.05.2024.csv")
DT$injury_dt <- as.POSIXct(paste(DT$Subject.DateInj, DT$Subject.TimeInj), format="%Y-%m-%d %H:%M:%S")

# Impute 4 missing times of injury to earliest possible time
DT[is.na(DT$injury_dt), ]
DT[is.na(DT$injury_dt), ]$injury_dt <- "1970-01-01 00:00:00 CET"

B <- merge(B, DT[, c("subjectId", "injury_dt")], by = "subjectId", all.x = T)
B$bbb_sampling_time <- as.numeric(difftime(B$bbb_collection_dt, B$injury_dt, units = c("hours"))) 
summary(B$bbb_sampling_time) 

B <- B[B$subjectId %in% c(long[long$.imp == 0 & long$Major_cranial_surgery_within_72h %in% c("Yes"), ]$subjectId),]
B <- merge(B, Cranial_Surgery_Times[, c("subjectId", "surgery_time")], by = "subjectId")
B$sampling_before_surg <- B$bbb_sampling_time < B$surgery_time

B <- B[B$subjectId %in% B[B$sampling_before_surg %in% c(TRUE),]$subjectId, ]
length(unique(B$subjectId))
B <- B[!is.na(B$bbb_sampling_time) & B$bbb_sampling_time <= 72,]

B$sampling_before_and_after <- NA
for (i in B$subjectId){
  B[B$subjectId == i, ]$sampling_before_and_after <- T %in% B[B$subjectId == i, ]$sampling_before_surg &  F %in% B[B$subjectId == i, ]$sampling_before_surg
}
B <- B[B$sampling_before_and_after %in% c(TRUE), ]

first_after_24 <- B[B$sampling_before_surg == T & B$bbb_sampling_time > 24,]$subjectId
B <- B[!B$subjectId %in% first_after_24, ]
length(unique(B$subjectId))

B <- B %>%
  arrange(subjectId, bbb_sampling_time) %>%
  group_by(subjectId) %>%
  slice_head(n = 2) %>%
  ungroup()

B_paired <- B %>%
  arrange(subjectId, bbb_sampling_time) %>%
  group_by(subjectId) %>%
  slice_head(n = 2) %>%
  mutate(sample = c("early", "late")) %>%
  ungroup() %>%
  select(subjectId, sample, Biomarkers.GFAP, Biomarkers.S100B, Biomarkers.UCH.L1) %>%
  pivot_wider(
    names_from = sample,
    values_from = c(Biomarkers.GFAP, Biomarkers.S100B, Biomarkers.UCH.L1)
  )
wilcox.test(log(B_paired$Biomarkers.GFAP_early), log(B_paired$Biomarkers.GFAP_late), paired = TRUE)
wilcox.test(log(B_paired$Biomarkers.S100B_early), log(B_paired$Biomarkers.S100B_late), paired = TRUE)
wilcox.test(log(B_paired$Biomarkers.UCH.L1_early), log(B_paired$Biomarkers.UCH.L1_late), paired = TRUE)

B_paired$delta_GFAP <- B_paired$Biomarkers.GFAP_late - B_paired$Biomarkers.GFAP_early
summary(B_paired$delta_GFAP)
B_paired$delta_S100B <- B_paired$Biomarkers.S100B_late - B_paired$Biomarkers.S100B_early
summary(B_paired$delta_S100B)
B_paired$delta_UCHL1 <- B_paired$Biomarkers.UCH.L1_late - B_paired$Biomarkers.UCH.L1_early
summary(B_paired$delta_UCHL1)


### 95% CIs for partial R2s - available cases -------------------------------------------------

# run after line 2499 (for variable names without space, before imputation)

supp_table_cc <- data.frame(Outcome = c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
                                        "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL"), 
                            CBIM = NA, C = NA, B = NA,  I = NA, M = NA)

for (j in c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
            "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL")) {
  
  # select complete cases for each endpoint
  all_cc <- all_original_data[complete.cases(all_original_data[, c(names_components, j)]),]
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  # rename columns to avoid Error in X[, mmcolnames, drop = FALSE] : subscript out of bounds
  all_cc <- all_cc %>% 
    rename(
      "Any_abnormality" = "Any abnormality", 
      "Skull_fracture" = "Skull fracture", 
      "Epidural_hematoma" = "Epidural hematoma", 
      "Subdural_hematoma" = "Subdural hematoma", 
      "Contusion_or_ICH" = "Contusion or ICH",
      "Mass_effect" = "Mass effect", 
      "Total_lesion_volume_25" = "Total lesion volume >= 25",
      "Mechanism_of_injury" = "Mechanism of injury", 
      "Major_extracranial_injury" = "Major extracranial injury", 
      "Accidental_cause" = "Accidental cause",
      "Medical_history" = "Medical history",
      "ASAPS_class" = "ASAPS class", 
      "Psychiatric_history" = "Psychiatric history",
      "Developmental_history" = "Developmental history",
      "TBI_history" = "TBI history",
      "Employment_status_Job_classification" = "Employment status Job classification",
      "Highest_level_of_education" = "Highest level of education")
  
  # C PILLAR
  model_clinical <- glm(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == j, ]$RCS == 1, 
                                                                            c_formula_gcs_rcs, c_formula_gcs_lin))), data = all_cc, family = "binomial")
  all_cc$temp_c <- model_clinical$linear.predictors
  
  # B PILLAR
  model_biomarkers <- glm(as.formula(paste0("`", j, "` ~ (", 
                                            ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == j, ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == j, ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == j, ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                            ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == j, ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial")
  all_cc$temp_b <- model_biomarkers$linear.predictors
  
  # M PILLAR
  model_modifiers <- glm(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+`Seizures`+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                           ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == j, ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                           "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), data = all_cc, family = "binomial")
  all_cc$temp_m <- model_modifiers$linear.predictors
  
  # I PILLAR
  if (!j == "Detectable intracranial injury on CT early") {
    model_imaging <- glm(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), data = all_cc, family = "binomial")
    all_cc$temp_i <- model_imaging$linear.predictors
    
    model_cbim <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), data = all_cc, family = "binomial")
    dat <- model.frame(model_cbim)
    print(nrow(dat))
    boot_r2 <- data.frame(CBIM = rep(NA, 1000), C = rep(NA, 1000), B = rep(NA, 1000), I = rep(NA, 1000), M = rep(NA, 1000))
    
    set.seed(21)
    # Bootstrap
    for (row_i in c(1:1000)) {
      # Resample rows
      ind <- sample(seq_len(nrow(dat)), size = nrow(dat), replace = TRUE)
      d <- dat[ind, , drop = FALSE]
      # Refit model
      fit <- tryCatch(update(model_cbim, data = d), error = function(e) NULL)
      if (is.null(fit)) next
      # Partial R2
      r2 <- tryCatch(rsq.partial(fit, type = "n")$partial.rsq, error = function(e) NULL)
      if (is.null(r2)) next
      boot_r2[row_i, ] <- c(rsq(fit, type = "n"), r2)
    }
    
    for(pillar in 1:5){
      supp_table_cc[supp_table_cc$Outcome == gsub("_", " ", j), pillar+1] <- paste0(format(round(c(rsq(model_cbim, type = "n"), rsq.partial(model_cbim, type = 'n')$partial.rsq), 2), nsmall=2)[pillar], " (",
                                                                                    format(round(apply(boot_r2, 2, quantile, probs = 0.025, na.rm = TRUE), 2), nsmall=2)[pillar], " - ",
                                                                                    format(round(apply(boot_r2, 2, quantile, probs = 0.975, na.rm = TRUE), 2), nsmall=2)[pillar], ")")
    }
  } else {
    model_cbm <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), data = all_cc, family = "binomial")
    dat <- model.frame(model_cbm)
    print(nrow(dat))
    boot_r2 <- data.frame(CBM = rep(NA, 1000), C = rep(NA, 1000), B = rep(NA, 1000), M = rep(NA, 1000))
    
    set.seed(21)
    # Bootstrap
    for (row_i in c(1:1000)) {
      # Resample rows
      ind <- sample(seq_len(nrow(dat)), size = nrow(dat), replace = TRUE)
      d <- dat[ind, , drop = FALSE]
      # Refit model
      fit <- tryCatch(update(model_cbm, data = d), error = function(e) NULL)
      if (is.null(fit)) next
      # Partial R2
      r2 <- tryCatch(rsq.partial(fit, type = "n")$partial.rsq, error = function(e) NULL)
      if (is.null(r2)) next
      boot_r2[row_i, ] <- c(rsq(fit, type = "n"), r2)
    }
    
    for(pillar in c(1:4)){
      supp_table_cc[supp_table_cc$Outcome == gsub("_", " ", j), pillar+1] <- paste0(format(round(c(rsq(model_cbm, type = "n"), rsq.partial(model_cbm, type = 'n')$partial.rsq), 2), nsmall=2)[pillar], " (",
                                                                                    format(round(apply(boot_r2, 2, quantile, probs = 0.025, na.rm = TRUE), 2), nsmall=2)[pillar], " - ",
                                                                                    format(round(apply(boot_r2, 2, quantile, probs = 0.975, na.rm = TRUE), 2), nsmall=2)[pillar], ")")
    }
    
  }
}

supp_table_cc[supp_table_cc$Outcome == "Detectable intracranial injury on CT early", "M"] <- supp_table_cc[supp_table_cc$Outcome == "Detectable intracranial injury on CT early", "I"] 
supp_table_cc[supp_table_cc$Outcome == "Detectable intracranial injury on CT early", "I"] <- NA

write.xlsx(supp_table_cc, "EXPORT/S5_Table.xlsx", sheetName = "CC")


### 95% CIs for partial R2s - imputed data ----------------------------------

# run at the end of the script
supp_table_imp <- data.frame(Outcome = c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
                                         "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL"), 
                             CBIM = NA, C = NA, B = NA,  I = NA, M = NA)

for (j in gsub(" ", "_", c("Detectable intracranial injury on CT early", "Hospital admission", "ICU admission", "ICP monitoring",  "Major cranial surgery within 72h", "Mortality at 6 months", 
                           "Unfavorable outcome at 6 months", "Incomplete recovery at 6 months", "Impairment of HRQOL"))) {
  
  # select complete cases for each endpoint
  all_cc <- complete(dti, 5)
  all_cc <- all_cc[all_cc$subjectId %in% all_original_data[!is.na(all_original_data[, gsub("_", " ", j)]), ]$subjectId &
                     !all_cc$subjectId %in% c("6DYY996"), ]
  
  # ICP monitoring analyzed only in ICU subgroup
  if(j %in% c("ICP monitoring")) {all_cc <- all_cc[all_cc$`ICU admission` %in% c("Yes"), ]} 
  
  # C PILLAR
  model_clinical <- glm(as.formula(paste0("`", j, "` ~ `Pupils` + ", ifelse(RCS[RCS$Variable == "GCS Score" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, 
                                                                            c_formula_gcs_rcs, c_formula_gcs_lin))), data = all_cc, family = "binomial")
  all_cc$temp_c <- model_clinical$linear.predictors
  
  # B PILLAR
  model_biomarkers <- glm(as.formula(paste0("`", j, "` ~ (", 
                                            ifelse(RCS[RCS$Variable == "GFAP" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, gfap_formula_rcs, gfap_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "UCHL1" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, uchl1_formula_rcs, uchl1_formula_lin), " + ",
                                            ifelse(RCS[RCS$Variable == "S100B" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, s100b_formula_rcs, s100b_formula_lin), ") *",
                                            ifelse(RCS[RCS$Variable == "Time to sampling" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, tts_formula_rcs, tts_formula_lin))), data = all_cc, family = "binomial")
  all_cc$temp_b <- model_biomarkers$linear.predictors
  
  # M PILLAR
  model_modifiers <- glm(as.formula(paste0("`", j, "` ~ `Mechanism_of_injury`+`Seizures`+`Major_extracranial_injury`+`Hypoxia`+`Hypotension`+`Accidental_cause`+", 
                                           ifelse(RCS[RCS$Variable == "Age" & RCS$Outcome == gsub("_", " ", j), ]$RCS == 1, age_formula_rcs, age_formula_lin), 
                                           "+`Sex`+`Medical_history`+`Psychiatric_history`+`Developmental_history`+`TBI_history`")), data = all_cc, family = "binomial")
  all_cc$temp_m <- model_modifiers$linear.predictors
  
  # I PILLAR
  if (!j == "Detectable_intracranial_injury_on_CT_early") {
    model_imaging <- glm(as.formula(paste0("`", j, "` ~ `Any_abnormality`+`Skull_fracture`+`Epidural_hematoma`+`Subdural_hematoma`+`TSAH`+`Contusion_or_ICH`+`TAMVI`+`IVH`+`Mass_effect`+`Total_lesion_volume_25`")), data = all_cc, family = "binomial")
    all_cc$temp_i <- model_imaging$linear.predictors
    
    model_cbim <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_i + temp_m")), data = all_cc, family = "binomial")
    
    dat <- model.frame(model_cbim)
    print(nrow(dat))
    boot_r2 <- data.frame(C = rep(NA, 1000), B = rep(NA, 1000), I = rep(NA, 1000), M = rep(NA, 1000))
    
    set.seed(21)
    # Bootstrap
    for (row_i in c(1:1000)) {
      # Resample rows
      ind <- sample(seq_len(nrow(dat)), size = nrow(dat), replace = TRUE)
      d <- dat[ind, , drop = FALSE]
      # Refit model
      fit <- tryCatch(update(model_cbim, data = d), error = function(e) NULL)
      if (is.null(fit)) next
      # Partial R2
      r2 <- tryCatch(rsq.partial(fit, type = "n")$partial.rsq, error = function(e) NULL)
      if (is.null(r2)) next
      boot_r2[row_i, ] <- r2
    }
    
    for(pillar in 1:4){
      supp_table_imp[supp_table_imp$Outcome == gsub("_", " ", j), pillar+2] <- paste0(format(round(rsq.partial(model_cbim, type = 'n')$partial.rsq, 2), nsmall=2)[pillar], " (",
                                                                                      format(round(apply(boot_r2, 2, quantile, probs = 0.025, na.rm = TRUE), 2), nsmall=2)[pillar], " - ",
                                                                                      format(round(apply(boot_r2, 2, quantile, probs = 0.975, na.rm = TRUE), 2), nsmall=2)[pillar], ")")
    }
    
  } else {
    model_cbm <- glm(as.formula(paste0("`", j, "` ~ temp_c + temp_b + temp_m")), data = all_cc, family = "binomial")
    
    dat <- model.frame(model_cbm)
    print(nrow(dat))
    boot_r2 <- data.frame(C = rep(NA, 1000), B = rep(NA, 1000), M = rep(NA, 1000))
    
    set.seed(21)
    # Bootstrap
    for (row_i in c(1:1000)) {
      # Resample rows
      ind <- sample(seq_len(nrow(dat)), size = nrow(dat), replace = TRUE)
      d <- dat[ind, , drop = FALSE]
      # Refit model
      fit <- tryCatch(update(model_cbm, data = d), error = function(e) NULL)
      if (is.null(fit)) next
      # Partial R2
      r2 <- tryCatch(rsq.partial(fit, type = "n")$partial.rsq, error = function(e) NULL)
      if (is.null(r2)) next
      boot_r2[row_i, ] <- r2
    }
    
    for(pillar in c(1:3)){
      supp_table_imp[supp_table_imp$Outcome == gsub("_", " ", j), pillar+2] <- paste0(format(round(rsq.partial(model_cbm, type = 'n')$partial.rsq, 2), nsmall=2)[pillar], " (",
                                                                                      format(round(apply(boot_r2, 2, quantile, probs = 0.025, na.rm = TRUE), 2), nsmall=2)[pillar], " - ",
                                                                                      format(round(apply(boot_r2, 2, quantile, probs = 0.975, na.rm = TRUE), 2), nsmall=2)[pillar], ")")
    }
    
  }
  
}

supp_table_imp[supp_table_imp$Outcome == "Detectable intracranial injury on CT early", "M"] <- supp_table_imp[supp_table_imp$Outcome == "Detectable intracranial injury on CT early", "I"] 
supp_table_imp[supp_table_imp$Outcome == "Detectable intracranial injury on CT early", "I"] <- NA

supp_table_imp[, 2] <- EditedConfIntervR2[, "R2_CI.CBIM"]

write.xlsx(supp_table_imp, "EXPORT/S5_Table.xlsx", sheetName = "IMP", append = T)


