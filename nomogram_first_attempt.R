#https://lengyueyang.github.io/Research/Nomogram-rms.html
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#https://campus.datacamp.com/courses/multiple-and-logistic-regression/logistic-regression-4?ex=1
#https://www.kaggle.com/sindhuee/r-caret-example


#Pull clerkship grades by hand.  
#Create column of CU students vs. universe
#Create column of CU students who did not match vs. all students who did not match.  

#Install and Load packages
#rm(list=ls())
#remotes::install_github("topepo/caret")
if(!require(pacman))install.packages("pacman")
pacman::p_load('Hmisc', 'readxl', 'XML', 'reshape2', 'devtools', 'plyr', 'packrat', 'highcharter', 'purrr', 'readr', 'htmlwidgets', 'RColorBrewer', 'leaflet', 'rgdal', 'dygraphs', 'quantmod', 'DT', 'formattable', 'ggplot2',  'idbr', 'genderizeR', 'animation', 'dplyr', 'magick', 'tidycensus', 'ggthemes', 'stringr', 'geosphere', 'ggmap', 'grid', 'gmapsdistance', 'zipcode', 'janitor', 'lubridate', 'hms', 'tidyr', 'stringr', 'readr', 'openxlsx', 'forcats', 'RcppRoll', 'tibble', 'bit64', 'munsell', 'scales', 'leaflet', 'rgdal', 'htmltools', 'mapview', 'htmlwidgets', 'sf', 'sp', 'tidyverse', 'viridis', 'fansi', 'webshot', 'geosphere', 'zipcode', 'leaflet.extras', 'raster',  'spData','spDataLarge', 'stplanr', 'tmap', 'osmdata', 'arsenal', 'doMC', "wesanderson", "fasterize", "USAboundaries", "RANN", "tidycensus", "geofacet", "extrafont", "shiny", "ParallelLogger", "parallel", "RSelenium", "humaniformat", "visdat", "skimr", "assertr", "tidylog", "doParallel", "DiagrammeR", "DiagrammeRsvg", "rsvg", "iterators", "parallel", "foreach", "PASWR", "rms", "pROC", "ROCR", "nnet", "janitor", "packrat", "DynNom", "rsconnect", "party", "recipes", "caret", "export", "caTools", "mlbench", "randomForest")
.libPaths("/Users/tylermuffly/.exploratory/R/3.5")  # Set libPaths.
#packrat::init(infer.dependencies = TRUE)
set.seed(123456)
registerDoMC(cores = detectCores()-1)

##################################################################
#### Set data file locations ####
# Set path to data and filenames as "constants" and use CAPS to denote.
setwd("~/Dropbox/Nomogram/nomogram")     #Set working directory

#1)  Create Table 1 of matched vs. unmatched applicants
#2)  Create logistic regression
#3)  Create nomogram

################################################################
#Load Data
data <- as.data.frame(read_rds("data/CU_Obgyn_Residency_Applicants_select_59.rds")) #40 has filled data so all fields are complete
#Carat gets confused by tibbles so convert to data.frame  

 
 ################################################################
 #Data cleaning, Place nicer labels for the data
 #label(data$Self_Identify)    <- 'Race/Ethnicity'
 label(data$Age)    <- 'Age'
 label(data$Alpha_Omega_Alpha) <- 'AOA Member'
 label(data$USMLE_Step_1_Score) <- 'USMLE Step 1 Score'
 label(data$Gender) <- 'Gender'
 label(data$Couples_Match) <- 'Couples Matching'
 label(data$Expected_Visa_Status_Dichotomized) <- 'Expected Visa Status'
 label(data$Medical_School_Type) <- 'Medical School Type'
 label(data$Medical_Education_or_Training_Interrupted) <- 'Medical School Interrupted'
 label(data$Misdemeanor_Conviction) <- 'Misdemeanor Conviction'
 #label(data$USMLE_Step_2_CK_Score) <- 'USMLE Step 2 CK Score'
 #label(data$USMLE_Step_2_CS_Score) <- 'USMLE Step 2 CS Score'
 #label(data$USMLE_Step_3_Score) <- 'USMLE Step 3 Score'
 label(data$US_or_Canadian_Applicant) <- 'US or Canadian Applicant'
 label(data$Gold_Humanism_Honor_Society) <- 'Gold Humanism Honors Society'
 label(data$Military_Service_Obligation) <- 'Military Service Obligation'
 label(data$Count_of_Oral_Presentation) <- 'Count of Oral Presentations'
 label(data$Count_of_Peer_Reviewed_Book_Chapter) <- 'Count of Peer-Reviewed Book Chapters'
 label(data$Count_of_Poster_Presentation) <- 'Count of Poster Presentations'
 label(data$Other_Service_Obligation) <- 'Other Service Obligation'
 #label(data$Med_school_condensed) <- 'Medical School Condensed' 
 label(data$white_non_white) <- 'Race' 
 label(data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts) <- 'Count of Peer-Reviewed Journal Articles'
 label(data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published) <-'Count of Peer-Reviewed Journal Articles Abstracts Other than Published'
 label(data) #Check labels for the data set

 ##plot relevant features for lots of variables
 colnames(data)
 features <-colnames(data)
 features_rel<-features [3:35]   
 
 for( i in features_rel ){
   
   p<-ggplot(data = data, aes_string(x=i,fill="Match_Status")) + geom_bar(alpha=0.8,colour='black', show.legend = TRUE, stat = "count") + theme(legend.position = "top") +
     guides(fill = guide_legend(nrow = 4, byrow = T) + 
    geom_text (aes(label = y), position = position_stack(vjust = 0.5), size = 10, angle = 45, check_overlap = TRUE) +
    geom_label(fontface = "bold"))
   print(p) }
   #ggsave("p.png", plot = last_plot(), device = "png", scale = 1, width = 12, height = 10, units = c("cm"), dpi = 500,  bg = "transparent")
  
 
 ################################################################
 #### Building Table 1 ####
 #Bring in the full data set
full_data <- as.data.frame(read_rds("data/CU_Obgyn_Residency_Applicants_fill_48.rds")) 
 #Use the arsenal package to create a table one with p-values.  I changed the stats so I get median, IQR.   
colnames(full_data)
table1 <- tableby(Match_Status ~ 
                    Self_Identify + 
                    white_non_white + 
                    Age + 
                    Gender + 
                    Couples_Match + 
                    Expected_Visa_Status_Dichotomized + 
                    US_or_Canadian_Applicant + 
                    Medical_School_Type + 
                    Medical_Education_or_Training_Interrupted + 
                    Misdemeanor_Conviction + 
                    Alpha_Omega_Alpha + 
                    Gold_Humanism_Honor_Society + 
                    Military_Service_Obligation + 
                    USMLE_Step_1_Score + 
                    Military_Service_Obligation + 
                    Count_of_Poster_Presentation + 
                    Count_of_Oral_Presentation + 
                    Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                    Count_of_Peer_Reviewed_Book_Chapter + 
                    Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + 
                    Count_of_Peer_Reviewed_Online_Publication + 
                    Misdemeanor_Conviction  + 
                    Expected_Visa_Status_Dichotomized + 
                    Visa_Sponsorship_Needed +
                    OBGYN_Grade +
                    Medical_Degree +
                    Medical_School_Country, 
                  data=full_data, control = tableby.control(test = TRUE, total = F, digits = 1L, digits.p = 2L, digits.count = 0L, numeric.simplify = F, numeric.stats = c("meansd", "median", "q1q3"), cat.stats = c("Nmiss","countpct"), stats.labels = list(Nmiss = "N Missing", Nmiss2 ="N Missing", meansd = "Mean (SD)", medianrange = "Median (Range)", median ="Median", medianq1q3 = "Median (Q1, Q3)", q1q3 = "Q1, Q3", iqr = "IQR",range = "Range", countpct = "Count (Pct)", Nevents = "Events", medSurv ="Median Survival", medTime = "Median Follow-Up")))
 
 #labels
 labels(table1)
 
 #show the table
 summary(table1, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology in 2019', pfootnote=TRUE)
 
 #Adjust for Bonferroni for multiple p-values
 padjust(table1, method = "bonferroni")
 summary(table1, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology in 2019', pfootnote=TRUE)
 
 #Write to HTML
 arsenal::write2html(table1, paste0("~/Dropbox/Nomogram/nomogram/results/table1.html"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")
 
 #Write to word
 arsenal::write2word(table1, paste0("~/Dropbox/Nomogram/nomogram/results/table1.doc"))
 
 #######################################################################################
 #Keep predictors in the binary logistic regression model that have a p<0.10 a priori to create nomogram
 colnames(data)
 t.test(data$Age~data$Match_Status) #SS # where y is numeric and x is a binary factor
 chisq.test(data$Match_Status, data$white_non_white) #SS
 chisq.test(data$Match_Status, data$Gender) #SS
 chisq.test(data$Match_Status, data$Couples_Match) #SS
 t.test(data$USMLE_Step_1_Score~data$Match_Status) #SS
 chisq.test(data$Match_Status, data$Expected_Visa_Status_Dichotomized) #SS
 #chisq.test(data$Match_Status, data$Med_school_condensed)
 chisq.test(data$Match_Status, data$Medical_Education_or_Training_Interrupted) #SS
 chisq.test(data$Match_Status, data$Misdemeanor_Conviction) #SS
 chisq.test(data$Match_Status, data$Alpha_Omega_Alpha) #SS
 chisq.test(data$Match_Status, data$US_or_Canadian_Applicant) #SS
 chisq.test(data$Match_Status, data$Gold_Humanism_Honor_Society) #SS
 chisq.test(data$Match_Status, data$Military_Service_Obligation)  #Not significant at all
 chisq.test(data$Match_Status, data$Visa_Sponsorship_Needed) #SS
 chisq.test(data$Match_Status, data$white_non_white) #SS
 chisq.test(data$Match_Status, data$Expected_Visa_Status) #SS
 chisq.test(data$Match_Status, data$Partner_Match) #NS
 t.test(data$Count_of_Poster_Presentation ~ data$Match_Status) #SS
 t.test(data$Count_of_Oral_Presentation ~ data$Match_Status)  #NS
 t.test(data$Count_of_Peer_Reviewed_Book_Chapter ~ data$Match_Status) #NS
 t.test(data$Count_of_Peer_Reviewed_Online_Publication ~ data$Match_Status) #NS
 chisq.test(data$Match_Status, data$OBGYN_Grade) #Failes because too many NS
 #######################################################################################

#For Logistic regression we use the binomial family.  Creation of model using format of:  log(odds)=β0+β1∗x1+...+βn∗xn
#mod_fit has all variables included
colnames(data)
 data$Match_Status <- as.factor(data$Match_Status)
 mod_fit <- glm(Match_Status ~ 
                  Age + 
                  white_non_white + 
                  Gender + 
                  Couples_Match + 
                  Expected_Visa_Status_Dichotomized +
                  US_or_Canadian_Applicant + 
                  #Medical_School_Type + 
                  Alpha_Omega_Alpha + 
                  Gold_Humanism_Honor_Society + 
                  USMLE_Step_1_Score +  
                  Military_Service_Obligation + 
                  Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                  Count_of_Peer_Reviewed_Book_Chapter + 
                  Count_of_Poster_Presentation + 
                  Other_Service_Obligation + 
                  Visa_Sponsorship_Needed + 
                  Count_of_Non_Peer_Reviewed_Online_Publication + 
                  Medical_Education_or_Training_Interrupted + 
                  Misdemeanor_Conviction,
                  #Medical_Degree +
                  #Medical_School_Country, 
                data=data, family="binomial")
 print(mod_fit)

 #mod_fit_two has only the univariate statistically significant values 
mod_fit_two <- glm(Match_Status ~ 
                   Age +  #Univariate SS
                   Gender + #Univariate SS
                   white_non_white + #Univariate SS
                   Couples_Match + #Univariate SS
                   USMLE_Step_1_Score + #Univariate SS
                   Expected_Visa_Status_Dichotomized + #Univariate SS
                   Medical_Education_or_Training_Interrupted + #Univariate SS
                   Misdemeanor_Conviction + #Univariate SS
                   Alpha_Omega_Alpha + #Univariate SS
                   US_or_Canadian_Applicant + #Univariate SS
                   Gold_Humanism_Honor_Society + #Univariate SS
                   Visa_Sponsorship_Needed + #Univariate SS
                   Count_of_Poster_Presentation +  #Univariate SS
                     Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published,
                   data=data, family="binomial")
print(mod_fit_two)
 
####Pseudo R^2
library(pscl)
pR2(mod_fit)  # look for 'McFadden', values closer to zero indicating that the model has no predictive power
pR2(mod_fit_two) 

####Wald Test - Individual Variable Importance
#  The idea is to test the hypothesis that the coefficient of an independent variable in the model is significantly different from zero. If the test fails to reject the null hypothesis, this suggests that removing the variable from the model will not substantially harm the fit of that model.
library(survey)
#Keep p<0.3
regTermTest(mod_fit_two, "Age") #Wald Test p=0.30, good
regTermTest(mod_fit_two, "Gender") #Wald Test p=0.30, good
regTermTest(mod_fit_two, "white_non_white") # Wald Test p=0.30
regTermTest(mod_fit_two, "Couples_Match") # Wald Test p=0.048, KEEP
regTermTest(mod_fit_two, "Expected_Visa_Status_Dichotomized") #Wald Testp=0.35
regTermTest(mod_fit_two, "Medical_Education_or_Training_Interrupted") #Wald Test p=0.75, DROP??
regTermTest(mod_fit_two, "Alpha_Omega_Alpha") # Wald Test p=0.5
regTermTest(mod_fit_two, "USMLE_Step_1_Score") #Wald Test p=0.025
regTermTest(mod_fit_two, "US_or_Canadian_Applicant") #Wald Test p0.98
regTermTest(mod_fit_two, "Gold_Humanism_Honor_Society") #Wald Test p=0.01
regTermTest(mod_fit_two, "Count_of_Oral_Presentation") # Wald Test p=0.47, KEEP
regTermTest(mod_fit_two, "Count_of_Peer_Reviewed_Journal_Articles_Abstracts") #Wald Test p=0.64, DROP
regTermTest(mod_fit_two, "Count_of_Peer_Reviewed_Book_Chapter") #Wald Test p=0.64, DROP
regTermTest(mod_fit_two, "Count_of_Poster_Presentation") #Wald Test  KEEP!
regTermTest(mod_fit_two, "Misdemeanor_Conviction")  #Wald Test Drop it because Wald test show p=0.641
regTermTest(mod_fit_two, "Count_of_Peer_Reviewed_Book_Chapter") #Wald Test Drop it p=0.7
regTermTest(mod_fit_two, "Military_Service_Obligation") #Wald Test Drop it, p=0.43
regTermTest(mod_fit_two, "Other_Service_Obligation") #Wald Test Drop
regTermTest(mod_fit_two, "Visa_Sponsorship_Needed") #Wald Test p=0.20

#To assess the relative importance of individual predictors in the model, we can also look at the absolute value of the t-statistic for each model parameter. 
caret::varImp(mod_fit_two)
 
mod_fit_three <- glm(Match_Status ~ 
                     Age + #Univariate SS  #Wald Test p=0.40, fair
                     Gender + #Univariate SS  #Wald Test p=0.30, good
                     white_non_white + #Univariate SS,  # p=0.30
                     Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                     USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                     Expected_Visa_Status_Dichotomized + #Univariate SS, #Wald Testp=0.35
                     Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                     Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                     Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
                     Count_of_Poster_Presentation +
                       Medical_Education_or_Training_Interrupted + #Univariate SS
                       Misdemeanor_Conviction + #Univariate SS
                       US_or_Canadian_Applicant + #Univariate SS
                       Visa_Sponsorship_Needed + #Univariate SS
                       Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published,
                   data=data, family="binomial")
print(mod_fit_three)
plot(mod_fit_three)
 
###### Now that we have picked the variables and the best model.  
colnames(data)
ddist <- datadist(data)
ddist
options (datadist = 'ddist')

# Logistic Regression Model #
# Recalculated (mod_fit_three) using rms::lrm so that I can use that package for the nomogram
model.binomial.significant <- rms::lrm(Match_Status ~ 
                                         Age + #Univariate SS  #Wald Test p=0.40, fair
                                         Gender + #Univariate SS  #Wald Test p=0.30, good
                                         white_non_white + #Univariate SS,  # p=0.30
                                         Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                                         USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                                         Expected_Visa_Status_Dichotomized + #Univariate SS, #Wald Testp=0.35
                                         Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                                         Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                                         Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
                                         Count_of_Poster_Presentation +
                                         Medical_Education_or_Training_Interrupted + #Univariate SS
                                         Misdemeanor_Conviction + #Univariate SS
                                         US_or_Canadian_Applicant + #Univariate SS
                                         Visa_Sponsorship_Needed + #Univariate SS
                                         Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published,
             data = data, x=TRUE, y=TRUE)
print(model.binomial.significant)  #Check the C-statistic which is the same as ROC area for binary logistic regression

####Test Area Under the Curve
library(ROCR)
# Compute AUC for predicting Match_Status_Dichot with the model
prob <- predict(mod_fit_three, newdata=data, type="response")
pred <- prediction(prob, data$Match_Status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc  #81% AUC

  #######################################################################################
  ###NOMOGRAM 
  ##Nomogram for a binary outcome (matching into residency), https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5451623/
  #fun.at - Demarcations on the function axis: "Matching into obgyn"
  #lp=FALSE so we don't have the logistic progression
  
  nomo_from_model.binomial.significant <- rms::nomogram(model.binomial.significant, 
                          #lp.at = seq(-3,4,by=0.5),
                          fun = plogis, 
                          fun.at = c(0.001, 0.01, 0.05, seq(0.2, 0.8, by = 0.2), 0.95, 0.99, 0.999), 
                          funlabel = "Chance of Matching in OBGYN, 2019", 
                          lp =FALSE,
                          #conf.int = c(0.1,0.7), 
                          abbrev = F,
                          minlength = 9)
  
  nomo_final <- plot(nomo_from_model.binomial.significant, lplabel="Linear Predictor",
       cex.sub = 0.8, cex.axis=0.8, cex.main=1, cex.lab=1, ps=10, xfrac=.7,
       #fun.side=c(3,3,1,1,3,1,3,1,1,1,1,1,3),
       #col.conf=c('red','green'),
       #conf.space=c(0.1,0.5),
       label.every=1,
       col.grid = gray(c(0.8, 0.95)),
       which="Match_Status")
  print(nomo_from_model.binomial.significant)
  #legend.nomabbrev(nom.bi, which='Alpha_Omega_Alpha', x=.5, y=5)
  
  
  #######################################################################################
  beepr::beep(sound = 4)
  DynNom::DynNom.lrm(model.binomial.significant, data, clevel = 0.95, m.summary = "formatted")
  #If we could look at the clerkship honors, hp, pass would allow us to decrease our step 1 cut off or put at mean because need to review by hand.  
  
  #Hurts student if they do not get a grade at Stanford clerkship.  
  
  #Academic score from CU could be a proxy for clerkship and sub-i grades.  These people were reviewed by Meredith to determine if they should get a CU interview.  All these people have a step 1 score of >233.  National average was 229 because it is due to time.  This is Perfect score is 10 for A or Honors.  
  
  #At APGO/CREOG talk about removing step 1 score and then you can't do any sort of cut off.  
  
  
  #######################################################################################
  ####  Model Calibration
  #resampling internal validation
  rms::validate(model.binomial.significant, method = "boot", B=300, estimates = T, type="residual")
  rms::validate(model.binomial.significant, method = "boot", B=300, group = y)  #NOT WORKING
  
  #cal <- calibrate(model.binomial.significant, kint=2, predy=seq(.2, .8, length=60), group=y)
  # group= does k-sample validation: make resamples have same 
  # numbers of subjects in each level of y as original sample
  
  caret::calibration(Match_Status_Dichot ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation, data = data)
  
  #https://campus.datacamp.com/courses/machine-learning-toolbox/regression-models-fitting-them-and-evaluating-their-performance?ex=3
  # Fit lm model: model
  model <- rms::lrm(Match_Status ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation, data = data, x=TRUE, y=TRUE)
  model
  # Predict on full data: p
  p <- predict(model, data)
  p
  
  #Next, you use the sample() function to shuffle the row indices of the diamonds dataset. You can later use these indices to reorder the dataset.
  # Set seed
  set.seed(123456)
  
  # Shuffle row indices: rows
  rows <- sample(nrow(data))
  
  # Randomly order data
  shuffled_data <- data[rows, ]
  
  ###Now that your dataset is randomly ordered, you can split the first 80% of it into a training set, and the last 20% into a test set. You can do this by choosing a split point approximately 80% of the way through your data
  # Determine row to split on: split
  split <- round(nrow(data) * 0.80)
  
  # Create train
  train <- data[1:split, ]
  
  # Create test
  test <- data[(split + 1):nrow(data), ]
  
  ###Now that you have a randomly split training set and test set, you can use the lm() function as you did in the first exercise to fit a model to your training set, rather than the entire dataset.
  # Fit lm model on train: model
  model <- rms::lrm(Match_Status ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation, data = train, x=TRUE, y=TRUE)
  
  # Predict on test: p
  p <- predict(model, test)
  
  
  ####Kaggle example
  #Read in the data
  #data <- as.data.frame(read_rds("~/Dropbox/Nomogram/nomogram/data/CU_Obgyn_Residency_Applicants_mutate_43.rds"))
  vis_miss(data, warn_large_data = FALSE)  #looks for missing data
  dim(data)
  data <- na.omit(data)  #removed any rows with NAs
  dim(data)
  vis_miss(data, warn_large_data = FALSE)  #looks for missing data
  
  #Creating training and test data 70-30 split
  set.seed(123456)
  trainIndex <- createDataPartition(data$Match_Status_Dichot, p = .7, 
                                    list = FALSE, 
                                    times = 1)
  dtrain<-data[trainIndex,]
  dtest<-data[-trainIndex,]
  
  #Modeling: Logistic regression and decision trees with 10 fold cross validation
  fitControl <- trainControl(## 10-fold CV
    method = "cv",
    number = 10,
    savePredictions = TRUE
  )
  
  ## Logistic regression
  lreg<-caret::train(Match_Status ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation,data=dtrain,method="glm",family=binomial(), trControl=fitControl)
  
  ## decision tree 
  dtree<-caret::train(Match_Status ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation,data=dtrain,method="ctree", trControl=fitControl)
  lreg
  dtree
  
  #feature importance
  varImp(lreg)
  varImp(dtree)
  
  ### predict on test dataset
  lreg_pred<-predict(lreg,dtest)
  dtree_pred<-predict(dtree,dtest)
  
  ##results
  confusionMatrix(lreg_pred,data$Match_Status)
  confusionMatrix(dtree_pred,data$Match_Status)
####

  
#######################################
#Datacamp chapter
  #https://campus.datacamp.com/courses/machine-learning-toolbox/classification-models-fitting-them-and-evaluating-their-performance?ex=2
  model.binomial.significant <- rms::lrm(Match_Status ~ 
                                           Age + #Univariate SS  #Wald Test p=0.40, fair
                                           Gender + #Univariate SS  #Wald Test p=0.30, good
                                           white_non_white + #Univariate SS,  # p=0.30
                                           Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                                           USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                                           #Expected_Visa_Status_Dichotomized + #Univariate SS, #Wald Testp=0.35, #lrm p-value of 0.92
                                           Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                                           Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                                           #Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20,          #lrm p-value of 0.21
                                           Count_of_Poster_Presentation,#Univariate SS, #Wald Test  KEEP!
                                         data = data, x=TRUE, y=TRUE)
  print(model.binomial.significant)  #Check the C-statistic which is the same as ROC area for binary logistic regression

  ################################################################################
  data <- as.data.frame(read_rds("data/CU_Obgyn_Residency_Applicants_select_58.rds"))
  
  vis_miss(data, warn_large_data = FALSE)  #looks for missing data
  dim(data)
  data <- na.omit(data)  #removed any rows with NAs
  dim(data)
  vis_miss(data, warn_large_data = FALSE)  #looks for missing data
  str(data)
  
  test_model <- glm(Match_Status~., family = "binomial", data=train)  
  summary(test_model)
  require(pROC)
  pred <- predict(test_model, type='response')
  tmp <- pROC::roc(data$Match_Status[!is.na(data$Match_Status)]~ pred, plot=TRUE, percent=TRUE)
  
  smaller_model <- glm(formula = Match_Status ~  Couples_Match + USMLE_Step_1_Score + Gold_Humanism_Honor_Society + Visa_Sponsorship_Needed + Count_of_Poster_Presentation + Age + Gender + Expected_Visa_Status_Dichotomized + Medical_Education_or_Training_Interrupted + US_or_Canadian_Applicant + white_non_white + Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, family = "binomial", data=train)  
  summary(smaller_model)
  
  a <- arsenal::modelsum(Match_Status ~  Couples_Match + USMLE_Step_1_Score + Gold_Humanism_Honor_Society + Visa_Sponsorship_Needed + Count_of_Poster_Presentation + Age + Gender + Expected_Visa_Status_Dichotomized + Medical_Education_or_Training_Interrupted + US_or_Canadian_Applicant + white_non_white + Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, data=train, family = "binomial")
  summary(a, text=T, show.intercept = F)
  
  fitall <- modelsum(Match_Status ~  Couples_Match + USMLE_Step_1_Score + Gold_Humanism_Honor_Society + Visa_Sponsorship_Needed + Count_of_Poster_Presentation + Age + Gender + Expected_Visa_Status_Dichotomized + Medical_Education_or_Training_Interrupted + US_or_Canadian_Applicant + white_non_white + Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, data=data, family=binomial, binomial.stats=c("Nmiss2","OR","p.value"))
  summary(fitall)
  
  
  ################################################################################
  # Random Forest in R YouTube video by Bharatendra Rai
  rf <- randomForest(Match_Status ~ Couples_Match + USMLE_Step_1_Score + Gold_Humanism_Honor_Society + Visa_Sponsorship_Needed + Count_of_Poster_Presentation, data=train)
rf  
attributes(rf)
af$confusion
plot(rf) #Looking at confusion matrix the class.error shows that the model is not as good at predicting students who will not match.  




###################################################################################
data <- as.data.frame(read_rds("data/CU_Obgyn_Residency_Applicants_select_59.rds"))

vis_miss(data, warn_large_data = FALSE)  #looks for missing data
dim(data)
data <- na.omit(data)  #removed any rows with NAs
dim(data)
vis_miss(data, warn_large_data = FALSE)  #looks for missing data
str(data)

# Get the number of observations
n_obs <- nrow(data)
n_obs

# Shuffle row indices: permuted_rows
permuted_rows <- sample(n_obs)
permuted_rows
# Randomly order data: Sonar
Sonar_shuffled <- data[permuted_rows, ]

# Identify row to split on: split
split <- round(n_obs * 0.6)

# Create train
train <- Sonar_shuffled[1:split, ]

# Create test
test <- Sonar_shuffled[(split + 1):n_obs, ]

#Check to make sure all variables are int or Factors
str(data)

# Fit glm model: model
model <- glm(Match_Status ~ .,
             family = "binomial", train)
summary(model)

# Predict on test: p
p <- predict(model, test, type = "response")
summary (p)
#Confusion matrix - This is where there is confusion in the model and it predicts one outcome rather than the other.  
# If p exceeds threshold of 0.5, M else R: m_or_r
match_or_no_match <- ifelse(p > 0.5, "No_Match", "Student_Matched")

# Convert to factor: p_class
p_class <- factor(match_or_no_match, levels = levels(test[["Match_Status"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Match_Status"]])  #Look at Accuracy statistic that is not very impressive at 0.31

# Make ROC curve
colAUC(p, test[["Match_Status"]], plotROC = TRUE)  #Models that are 50% accurate follows the diagnal line

#Area under the curve of 0.5 is a model with a random guess and 1.0 is perfect
#AUC can be thought of as a letter grade with A of 0.9
# Create trainControl object: myControl

myControl <- trainControl(
  method = "cv",
  number = 10,          #10-fold cross-validation 
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
model <- train(
  Match_Status ~ ., 
  data, 
  method = "glm",
  trControl = myControl, 
  preProcess = c("medianImpute", "center", "scale")
)

# Print model to console
model



# Fit random forest: model
model <- train(
  Match_Status ~ .,
  tuneLength = 3,
  data = data, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
model  
plot(model)  

# Define the tuning grid: tuneGrid
tuneGrid <- data.frame(
  .mtry = c(25, 13, 4, 5),
  .splitrule = "variance",
  .min.node.size = 5
)    

# Hand tune the model
model <- train(
  Match_Status ~ .,
  tuneGrid = tuneGrid, 
  data = data, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
model  
plot(model)   #output shows mtry=2


##glmnet model can pick out predictors for you
# Create custom trainControl: myControl
# Fit glmnet model: model_glmnet
myControl <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Fit glmnet model: model_glmnet
model_glmnet <- train(
  x = model_x, 
  y = model_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)
# Print model to console
model
attributes(model)

# Print maximum ROC statistic
max(model[["results"]][["ROC"]])  

