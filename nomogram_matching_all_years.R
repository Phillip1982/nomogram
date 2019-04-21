#Muffly, Liss, Alston, Raffaelli, Jelovsek  ###DRAFT###

##################################################################
#Objective:  We sought to construct and validate a model that predict a medical students chances of matching into an obstetrics and gynecology residency.  

#Install and Load packages
if(!require(pacman))install.packages("pacman")
pacman::p_load('caret', 'readxl', 'XML', 'reshape2', 'devtools', 'purrr', 'readr', 'ggplot2', 'dplyr', 'magick', 'janitor', 'lubridate', 'hms', 'tidyr', 'stringr', 'readr', 'openxlsx', 'forcats', 'RcppRoll', 'tibble', 'bit64', 'munsell', 'scales', 'rgdal', 'tidyverse', "foreach", "PASWR", "rms", "pROC", "ROCR", "nnet", "janitor", "packrat", "DynNom", "export", "caTools", "mlbench", "randomForest", "ipred", "xgboost", "Metrics", "RANN", "AppliedPredictiveModeling", "nomogramEx", "shiny", "earth", "fastAdaboost", "Boruta", "glmnet", "ggforce", "tidylog", "InformationValue", "pscl", "scoring", "DescTools", "gbm", "Hmisc", "arsenal", "pander", "moments", "leaps", "MatchIt", "car", "mice", "rpart", "beepr", "fansi", "utf8", "zoom")
#.libPaths("/Users/tylermuffly/.exploratory/R/3.5")  # Set libPaths.
packrat::init(infer.dependencies = TRUE)
packrat_mode(on = TRUE)
set.seed(123456)

##################################################################
#### Set data file locations ####
setwd("~/Dropbox/Nomogram/nomogram")  #Set working directory

################################################################
#Load Data
download.file("https://www.dropbox.com/s/d4dk1v73d92pjwz/all_years_mutate_83.rds?raw=1",destfile=paste0("all_years_mutate_83.rds"), method = "auto", cacheOK = TRUE)
all_data <- read_rds("~/Dropbox/Nomogram/nomogram/data/all_years_mutate_83.rds")  #Bring in years 2015, 2016, 2017, and 2018 data
glimpse(all_data)
dim(all_data)
colnames(all_data)
all_data$Match_Status_Dichot

################################################################
# Place nicer labels for the data
#label(data$Self_Identify)    <- 'Race/Ethnicity'
label(all_data$Age)    <- 'Age'
units(all_data$Age) <- 'years'
label(all_data$Alpha_Omega_Alpha) <- 'AOA Member'
label(all_data$USMLE_Step_1_Score) <- 'USMLE Step 1 Score'
label(all_data$Gender) <- 'Gender'
label(all_data$Couples_Match) <- 'Couples Matching'
label(all_data$Visa_Status_Expected) <- 'Expected Visa Status'
label(all_data$Medical_School_Type) <- 'Medical School Type'
label(all_data$Medical_Education_or_Training_Interrupted) <- 'Medical School Interrupted'
label(all_data$Misdemeanor_Conviction) <- 'Misdemeanor Conviction'
#label(all_data$USMLE_Step_2_CK_Score) <- 'USMLE Step 2 CK Score'
#label(all_data$USMLE_Step_2_CS_Score) <- 'USMLE Step 2 CS Score'
#label(all_data$USMLE_Step_3_Score) <- 'USMLE Step 3 Score'
label(all_data$US_or_Canadian_Applicant) <- 'US or Canadian Applicant'
label(all_data$Gold_Humanism_Honor_Society) <- 'Gold Humanism Honors Society'
label(all_data$Military_Service_Obligation) <- 'Military Service Obligation'
label(all_data$Count_of_Oral_Presentation) <- 'Count of Oral Presentations'
label(all_data$Count_of_Peer_Reviewed_Book_Chapter) <- 'Count of Peer-Reviewed Book Chapters'
label(all_data$Count_of_Poster_Presentation) <- 'Count of Poster Presentations'
label(all_data$Other_Service_Obligation) <- 'Other Service Obligation'
#label(all_data$Med_school_condensed) <- 'Medical School Condensed' 
label(all_data$white_non_white) <- 'Race' 
label(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts) <- 'Count of Peer-Reviewed Journal Articles'
label(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published) <-'Count of Peer-Reviewed Research Not Published'
label(all_data$Match_Status_Dichot) <- 'Matching Status'
label(all_data$Match_Status) <- 'Matching Status'
label(all_data) #Check labels for the data set
all_data$Match_Status_Dichot
####