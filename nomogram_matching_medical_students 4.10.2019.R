#Muffly, Liss, Alston, Raffaelli, Jelovsek  ###DRAFT###

##################################################################
#Objective:  We sought to construct and validate a model that predict a medical students chances of matching into an obstetrics and gynecology residency.  

#Install and Load packages
if(!require(pacman))install.packages("pacman")
pacman::p_load('caret', 'readxl', 'XML', 'reshape2', 'devtools', 'purrr', 'readr', 'ggplot2', 'dplyr', 'magick', 'janitor', 'lubridate', 'hms', 'tidyr', 'stringr', 'readr', 'openxlsx', 'forcats', 'RcppRoll', 'tibble', 'bit64', 'munsell', 'scales', 'rgdal', 'tidyverse', "foreach", "PASWR", "rms", "pROC", "ROCR", "nnet", "janitor", "packrat", "DynNom", "export", "caTools", "mlbench", "randomForest", "ipred", "xgboost", "Metrics", "RANN", "AppliedPredictiveModeling", "nomogramEx", "shiny", "earth", "fastAdaboost", "Boruta", "glmnet", "ggforce", "tidylog", "InformationValue", "pscl", "scoring", "DescTools", "gbm", "Hmisc", "arsenal", "pander", "moments", "leaps", "MatchIt", "car", "mice", "rpart", "beepr", "fansi", "utf8")
#.libPaths("/Users/tylermuffly/.exploratory/R/3.5")  # Set libPaths.
packrat::init(infer.dependencies = TRUE)
packrat_mode(on = TRUE)
set.seed(123456)

##################################################################
#### Set data file locations ####
setwd("~/Dropbox/Nomogram/nomogram")  #Set working directory

################################################################
#Load Data
download.file("https://www.dropbox.com/s/b2vqurq5575dbxf/CU_Obgyn_Residency_Applicants_rename_61.rds?raw=1",destfile=paste0("CU_Obgyn_Residency_Applicants_rename_61.rds"), method = "auto", cacheOK = TRUE)
data <- read_rds("CU_Obgyn_Residency_Applicants_rename_61.rds")
str(data)

download.file("https://www.dropbox.com/s/845h7ixrjoz4h5m/CU_Obgyn_Residency_Applicants_fill_48.rds?raw=1",destfile=paste0("CU_Obgyn_Residency_Applicants_fill_48.rds"), method = "auto", cacheOK = TRUE)
full_data <- read_rds("CU_Obgyn_Residency_Applicants_fill_48.rds")  #Bring in the full data set

################################################################
####  Plot variable characteristics
 colnames(data)
 features <-colnames(data)
 features_rel<-features [2:24]   
 
for( i in features_rel ){
   temp_plot<-ggplot(data = data, aes_string(x=i,fill="Match_Status")) + geom_bar(alpha=0.8,colour='black', show.legend = TRUE, stat = "count") + theme(legend.position = "top") + 
 ggtitle(paste0("Match Statistics for 2019 OBGYN: \n",i)) + 
    guides(fill = guide_legend(nrow = 4, byrow = T) + 
    geom_text (aes(label = y), position = position_stack(vjust = 0.5), size = 10, angle = 45, check_overlap = TRUE) + 
    geom_label(fontface = "bold"))
   print(temp_plot) 
   ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm", dpi = 500,  bg = "transparent")
 }
 print(temp_plot)

################################################################
#### Building Table 1 ####
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
 
 labels(table1)  #labels
 
 summary(table1, text=T, title='Table 1:  Demographics of CU Applicants to Obstetrics and Gynecology in 2019', pfootnote=TRUE)   #show the table
 
 padjust(table1, method = "bonferroni")   #Adjust for Bonferroni for multiple p-values
 summary(table1, text=T, title='Table 1:  Demographics of CU Applicants to Obstetrics and Gynecology in 2019', pfootnote=TRUE)
 
 arsenal::write2html(table1, ("~/Dropbox/Nomogram/nomogram/results/table1.html"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")   #Write to HTML
 
 arsenal::write2word(table1, paste0("~/Dropbox/Nomogram/nomogram/results/table1.doc"))  #Write to Word 
 
#######################################################################################
 #  Explore Data
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
 chisq.test(data$Match_Status, data$OBGYN_Grade) #Fails because too many NA

#=================================================================
# Data Wrangling: Look at the full data set for NAs
#=================================================================
 nrow(data)
 ncol(data)
 sum(is.na(data))
 data <- na.omit(data)
 sum(is.na(data))
str(data)

#=================================================================
# Split Full Data Set for Creating Model
#=================================================================

# Use caret to create a 70/30% split of the training data, keeping the proportions of the Match_Status class label the same across splits.
indexes <- caret::createDataPartition(y=data$Match_Status,   #Create the training set from the whole data
                               times = 1,
                               p = 0.7,
                               list = FALSE)
match.train <- data[indexes,]
match.train <- na.omit(match.train)
nrow(match.train)
sum(is.na(match.train))
match.test <- data[-indexes,]
match.test <- na.omit(match.test)
sum(is.na(match.test))
nrow(match.test)

# Examine the proportions of the Match_Status class lable across the datasets.
prop.table(table(data$Match_Status))       #Original data set proportion 
prop.table(table(match.train$Match_Status)) #Train data set proportion
prop.table(table(match.test$Match_Status))  #Test data set proportion

#=================================================================
# Data Visualization of Split Data (match.train)
#=================================================================
dim(match.train)
colnames(match.train)
skimmed <- skim_to_wide(match.train)
skimmed[, c(2:15)]
str(match.train)
sum(is.na(match.train))  # Check for NAs in data

#=================================================================
#  Factor Selection
#=================================================================
indexes <- caret::createDataPartition(y = match.train$Match_Status,  #Divide the data into train and test sets
                                      times = 1,
                                      p = 0.7,
                                      list = FALSE)

#Split the data so that we cna run a model and find best factors.  
train <- data[indexes,]
nrow(train)
test <- data[-indexes,]
nrow(test)

#Method 1:  Principal Component Analysis - Use principal components analysis to pick what predictors to use 
train_pca <- preProcess(select(data, - Match_Status), 
                        method = c("center", "scale", "nzv", "pca"))
train_pca
.train_pca$method
train_pca$rotation
#PCA chose Count_of_Other_Articles and Count_of_Peer_Reviewed_Online_Publication

#Method 2: Recursive Feature Elimination
options(warn=-1)
subsets <- c(2:24)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 2,
                   verbose = FALSE)

lmProfile <- rfe(x=train[, 2:24], y=train$Match_Status,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile  
#Recursive feature elimination picked USMLE_Step_1_Score, Count_of_Poster_Presentation, US_or_Canadian_Applicant, Alpha_Omega_Alpha, Gold_Humanism_Honor_Society

#Method 3:  Boruta search
#You'll see how you can use it to perform a top-down search for relevant features by comparing original attributes' importance with importance achievable at random, estimated using their permuted copies, and progressively elliminating irrelevant features.
boruta_output <- Boruta::Boruta(Match_Status ~ ., data=na.omit(train), doTrace=0)  
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <-Boruta::getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  
roughFixMod <- TentativeRoughFix(boruta_output)  # Do a tentative rough fix
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
plot(boruta_output, cex.axis=0.35, las=2, xlab="", main="Variable Importance")  # Plot variable importance
#Boruta chose Age, AOA, USMLE Step 1 Score, US or Canadian Applicant, Count of Poster presentations, White or non-White

#Method 4:  Variable important from ML algorithm 
modelLookup('earth')

# Train the model using randomForest and predict on the training data itself.
model_mars = train(Match_Status ~ ., data=train, method='earth')
fitted <- predict(model_mars)
model_mars
plot(model_mars, main="Model Accuracies with MARS") #Iterations of hyperparameter search performed.
varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS") #Visual of the most important factors
#MARS only predicted that Step 1 score was important

#Model 5: Backwards Stepwise Regression
model.binomial.significant <- rms::lrm(Match_Status ~ 
                                         Age + #Univariate SS  #Wald Test p=0.40, fair
                                         Gender + #Univariate SS  #Wald Test p=0.30, good
                                         white_non_white + #Univariate SS,  # p=0.30
                                         #Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                                         USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                                         Visa_Status_Expected + #Univariate SS, #Wald Testp=0.35
                                         #Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                                         Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                                         Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
                                         Count_of_Poster_Presentation +
                                         Medical_Education_or_Training_Interrupted + #Univariate SS
                                         #Misdemeanor_Conviction + #Univariate SS
                                         #US_or_Canadian_Applicant + #Univariate SS
                                         #Count_of_Non_Peer_Reviewed_Online_Publication,
                                       Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published,
                                       data = train, x=TRUE, y=TRUE)

print(model.binomial.significant)  #Check the C-statistic which is the same as ROC
rms::fastbw(model.binomial.significant, type=c("residual", "individual", "total"), rule=c("aic", "p")) 
#Factors selected by fast backwards step-wise regression for: white_non_white, USMLE Step 1, Count of Poster Presentations.  This is quick and dirty.  

#=================================================================
#  Create a Logistic Regression Model: log(odds)=β0+β1∗x1+...+βn∗xn
#=================================================================
colnames(train)
ddist <- datadist(train)
ddist
options (datadist = 'ddist')

sum(is.na(train$Alpha_Omega_Alpha))
levels(train$Alpha_Omega_Alpha)
str(train$Alpha_Omega_Alpha)
sapply(sapply(train, unique), length)
train$Alpha_Omega_Alpha <- as.factor(train$Alpha_Omega_Alpha)
table(train$Alpha_Omega_Alpha)


# Recalculated using rms::lrm so that I can use that package for the nomogram
model.binomial.significant <- rms::lrm(Match_Status ~ 
                                         Age + #Univariate SS  #Wald Test p=0.40, fair
                                         Gender + #Univariate SS  #Wald Test p=0.30, good
                                         white_non_white + #Univariate SS,  # p=0.30
                                         #Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                                         USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                                         Visa_Status_Expected + #Univariate SS, #Wald Testp=0.35
                                         #Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                                         Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                                         Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
                                         Count_of_Poster_Presentation +
                                         Medical_Education_or_Training_Interrupted + #Univariate SS
                                         #Misdemeanor_Conviction + #Univariate SS
                                         US_or_Canadian_Applicant + #Univariate SS
                                         Count_of_Non_Peer_Reviewed_Online_Publication,
                                         Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published,
                                       data = train, x=TRUE, y=TRUE)

print(model.binomial.significant)  #Check the C-statistic which is the same as ROC area for binary logistic regression
fastbw(model.binomial.significant) #stepdown by p-values, AIC

plot(summary(model.binomial.significant))

model2 <- rms::lrm(Match_Status ~ Age +
                     white_non_white +
                     USMLE_Step_1_Score + 
                     Count_of_Poster_Presentation + 
                     US_or_Canadian_Applicant +
                     Alpha_Omega_Alpha +
                     Gold_Humanism_Honor_Society, data = train, x=TRUE, y=TRUE)
print(model2)
validate <- rms::validate(model2, B=1000, set.seed(123456), sls=0.2)
validate
model2plot <- rms::calibrate(model2)
dev.off()
plot(model2plot)

library(Hmisc) #Evaluate predictors without looking at the outcome variable
hoeffding <- Hmisc::varclus(~ Age + #Univariate SS  #Wald Test p=0.40, fair
                 Gender + #Univariate SS  #Wald Test p=0.30, good
                 white_non_white + #Univariate SS,  # p=0.30
                 Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                 USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                 Visa_Status_Expected + #Univariate SS, #Wald Testp=0.35
                 Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                 Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                 Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
                 Count_of_Poster_Presentation +
                 Medical_Education_or_Training_Interrupted + #Univariate SS
                 Misdemeanor_Conviction + #Univariate SS
                 US_or_Canadian_Applicant + #Univariate SS
                 Count_of_Non_Peer_Reviewed_Online_Publication, data = train, sim="hoeffding")  #only predictors
plot(hoeffding)

#Redundancy analysis
redun <- Hmisc::redun(~ Age + #Univariate SS  #Wald Test p=0.40, fair
             Gender + #Univariate SS  #Wald Test p=0.30, good
             white_non_white + #Univariate SS,  # p=0.30
             Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
             USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
             Visa_Status_Expected + #Univariate SS, #Wald Testp=0.35
             Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
             Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
             Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
             Count_of_Poster_Presentation +
             Medical_Education_or_Training_Interrupted + #Univariate SS
             Misdemeanor_Conviction + #Univariate SS
             US_or_Canadian_Applicant + #Univariate SS
             Count_of_Non_Peer_Reviewed_Online_Publication, data = train, type="adjusted", r2 = 0.3, pr=TRUE)    #only predictors
print(redun, digits=3, long=TRUE)

#=================================================================
#Look for Co-linearity with Variance Inflation Factors
#=================================================================
rms::vif(model.binomial.significant) #Should be <4

#=================================================================
#  Table 2 of Odds Ratios and CIs for Predictors of Matching into OBGYN
#=================================================================
oddsratios <- as.data.frame(exp(cbind("Odds ratio" = coef(model.binomial.significant), confint.default(model.binomial.significant, level = 0.95))))
print(oddsratios)

#Write Table 2 to HTML
arsenal::write2html(oddsratios, ("oddratios_table2.html"), total=FALSE, title = "Table 2", quiet = FALSE, theme = "yeti")

#Write to Table 2 word
arsenal::write2word(oddsratios, paste0("oddsratios_table2.doc"))

#=================================================================
#  Prepare the test dataset and predict on NEW DATA
#=================================================================
dummy.vars <- caret::dummyVars(~ ., data = match.train[, -1])
trainData <- predict(dummy.vars, match.train[, -1])  #Had to remove spaces from all variables and values
pre.process <- caret::preProcess(match.train, method = "bagImpute") # Now, impute!
imputed.data <- predict(pre.process, match.train)
sum(is.na(imputed.data))
testData2 <- predict(pre.process, test)  

#=================================================================
# Predict and Check Confusion Matrix
#=================================================================
predicted <- predict(model_mars, testData2)
head(predicted)
caret::confusionMatrix(reference = test$Match_Status, data = predicted, mode='everything') 

#=================================================================
# Check the quality of the new model
#=================================================================
####Pseudo R^2
pscl::pR2(model.binomial.significant)  # look for 'McFadden', values closer to zero indicating that the model has no predictive power

model.binomial.significant2 <- glm(Match_Status ~ USMLE_Step_1_Score + Alpha_Omega_Alpha + Count_of_Poster_Presentation + US_or_Canadian_Applicant, data = test, family = "binomial")

# Compute AUC for predicting Match_Status_Dichot with the model
prob <- predict(model.binomial.significant2, newdata=test, type="response")  #Must use GLM model
pred <- prediction(prob, test$Match_Status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc  

#######################################################################################
###NOMOGRAM 
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
nomogramEx(nomo=nomo_from_model.binomial.significant,np=1,digit=2)  #Gives the polynomial formula

################################################################
# Place nicer labels for the data
#label(data$Self_Identify)    <- 'Race/Ethnicity'
label(data$Age)    <- 'Age'
label(data$Alpha_Omega_Alpha) <- 'AOA Member'
label(data$USMLE_Step_1_Score) <- 'USMLE Step 1 Score'
label(data$Gender) <- 'Gender'
label(data$Couples_Match) <- 'Couples Matching'
label(data$Visa_Status_Expected) <- 'Expected Visa Status'
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

#dev.off()  #Run this until null device = 1
nomo_final <- plot(nomo_from_model.binomial.significant, lplabel="Linear Predictor",
                   cex.sub = 0.8, cex.axis=0.8, cex.main=1, cex.lab=1, ps=10, xfrac=.7,
                   #fun.side=c(3,3,1,1,3,1,3,1,1,1,1,1,3),
                   #col.conf=c('red','green'),
                   #conf.space=c(0.1,0.5),
                   label.every=1,
                   col.grid = gray(c(0.8, 0.95)),
                   which="Match_Status")
print(nomo_from_model.binomial.significant)

# Check Brier Score
DescTools::BrierScore(model.binomial.significant2)

# Calibration
calib <- rms::calibrate(model.binomial.significant, boot=1000, data = test)  #Plot test data set
#AUC and calibration matters

plot(calib)
calib
#######################################################################################
beepr::beep(sound = 4)
#DynNom::DynNom.lrm(model.binomial.significant, data, clevel = 0.95, m.summary = "formatted")

#######################################################################################
#Decision Curve
colnames(all_data)
dim(all_data) #3,441 applicants
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################


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
label(all_data) #Check labels for the data set
all_data$Match_Status_Dichot
####

Hmisc::describe(all_data) # A little better than summary.  Gives proportions for categorical variables. Amen!


####
#Look at the data in one graph.  Nice.  Page 292 in Harrell's book
dev.off()
par("mar")
par(mar=c(1,1,1,1))

all_data$Match_Status_Dichot <- as.numeric(all_data$Match_Status_Dichot)
all_data$Match_Status_Dichot <- (all_data$Match_Status_Dichot - 1)
all_data$Match_Status_Dichot#Fucking outcome must be numeric
v <- c('Match_Status_Dichot', 'Age', 'Gender', 'Alpha_Omega_Alpha','USMLE_Step_1_Score', 'Couples_Match', 'Medical_Education_or_Training_Interrupted', 'Misdemeanor_Conviction', 'US_or_Canadian_Applicant', 'Gold_Humanism_Honor_Society',  'Military_Service_Obligation', 'Count_of_Oral_Presentation', 'Count_of_Peer_Reviewed_Book_Chapter', 'Count_of_Poster_Presentation', 'white_non_white','Count_of_Peer_Reviewed_Journal_Articles_Abstracts', 'Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published')
t3 <- all_data[,v]

dd <- datadist(t3)
options(datadist='dd')
s <- summary(Match_Status_Dichot ~ cut2(Age, 30:30) + Gender + Alpha_Omega_Alpha + cut2(USMLE_Step_1_Score, 245:245) + Couples_Match + Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Military_Service_Obligation + Count_of_Oral_Presentation + cut2(Count_of_Peer_Reviewed_Book_Chapter, 0:3) + cut2(Count_of_Poster_Presentation, 0:3) + white_non_white + cut2(Count_of_Peer_Reviewed_Journal_Articles_Abstracts, 0:3) + cut2(Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, 0:3), data = t3)
dev.off()
plot(s, main= "Univariate Analysis", cex.sub = 0.5, cex.axis=0.5, cex.main=0.6, cex.lab=0.6, subtitles = FALSE, xlab = "Chance of Matching into OBGYN Residency")

#####
colnames(t3)
t3$Match_Status_Dichot
t3_clean <- na.omit(t3)
sum(is.na(t3_clean))
#install.packages("ggplot2")
library(ggplot2)
b <- scale_size_discrete(range=c(0.1, 0.85))
y1 <- ylab(NULL)
p1 <- ggplot(t3, aes(x=Age, y= Match_Status_Dichot, color = Gender)) +
  Hmisc::histSpikeg(Match_Status_Dichot ~ Age + Gender, lowess=T, data=t3_clean) + 
  ylim (0,1) + y1
plot(p1)

###Original 
colnames(data)
features <-colnames(data)
features_rel<-features [2:24]   

for( i in features_rel ){
  temp_plot<-ggplot(data = data, aes_string(x=i,fill="Match_Status")) + geom_bar(alpha=0.8,colour='black', show.legend = TRUE, stat = "count") + theme(legend.position = "top") + 
    ggtitle(paste0("Match Statistics for 2019 OBGYN: \n",i)) + 
    guides(fill = guide_legend(nrow = 4, byrow = T) + 
             geom_text (aes(label = y), position = position_stack(vjust = 0.5), size = 10, angle = 45, check_overlap = TRUE) + 
             geom_label(fontface = "bold"))
  print(temp_plot) 
  ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm", dpi = 500,  bg = "transparent")
}
print(temp_plot)

################################################################
#Look at Missing Data
#Page 302 of Harrell book
na.patterns <- Hmisc::naclus(all_data)
na.patterns
require(rpart)

who.na <- rpart(is.na(Gold_Humanism_Honor_Society) ~ Match_Status + Medical_Education_or_Training_Interrupted + USMLE_Step_1_Score + white_non_white + US_or_Canadian_Applicant, data = all_data, minbucket = 15)

naplot(na.patterns, 'na per var')
dev.off()
plot(who.na, margin = 0.1); test(who.na)
plot(na.patterns) #Cool!! this shows who has the most missing data.  

m <- lrm(is.na(Gold_Humanism_Honor_Society) ~ Match_Status + Medical_Education_or_Training_Interrupted + USMLE_Step_1_Score + white_non_white + US_or_Canadian_Applicant, data=all_data) #Wald statistics for is.na)Gold_Humanism. Shows that students not matching are not more likely to have Gold_Humanism.  The higher the step 1 score the less likely that Gold_Humanism to be missing.  
anova(m)

################################################################
#Impute data in so there are no NAs
#Page 55 of Harrell book
set.seed(123456)
mi <- Hmisc::aregImpute(~Gender + 
                  white_non_white + 
                  Couples_Match + 
                  USMLE_Step_1_Score + 
                  Alpha_Omega_Alpha + 
                  Gold_Humanism_Honor_Society + 
                  Visa_Sponsorship_Needed + 
                  Count_of_Poster_Presentation +
                  Medical_Education_or_Training_Interrupted + 
                  Misdemeanor_Conviction + 
                  US_or_Canadian_Applicant + 
                  Count_of_Non_Peer_Reviewed_Online_Publication + Match_Status_Dichot, data = all_data, 
                  n.impute = 20, nk = 4, pr = FALSE)
mi

#Page 306 Harrell text for fit.mult.impute, Fits five models and examines the within and between imputation variances.  The output is used to make odds ratios.  
f.mi <- fit.mult.impute(Match_Status_Dichot ~ (Age + Gender + 
                       white_non_white + 
                       Couples_Match + 
                       USMLE_Step_1_Score + 
                       Alpha_Omega_Alpha + 
                       Gold_Humanism_Honor_Society + 
                       Visa_Sponsorship_Needed + 
                       Count_of_Poster_Presentation +
                       Medical_Education_or_Training_Interrupted + 
                       Misdemeanor_Conviction + 
                       US_or_Canadian_Applicant + 
                       Count_of_Non_Peer_Reviewed_Online_Publication), lrm, mi, 
                       data=all_data, pr = FALSE)

################################################################
### Examination of skewness and kurtosis for numeric values, Zhang book page 65
colnames(all_data)
par(mfrow=c(1,2))
hist(all_data$Age)  #There is skew in age
hist(all_data$USMLE_Step_1_Score) #No skew with USMLE
moments::agostino.test(all_data$Age) #D'Agostino skewness test is positive for skewness
moments::anscombe.test(all_data$USMLE_Step_1_Score)  #There is kurtosis for the Step 1 score data.  
#Therefore only use medians.  
 
################################################################
#### Building Table 1 ####
colnames(all_data)
table1_all_data <- arsenal::tableby(Match_Status ~
                    white_non_white + 
                    Age + 
                    Gender + 
                    Couples_Match + 
                    #Expected_Visa_Status_Dichotomized + 
                    US_or_Canadian_Applicant + 
                    #Medical_School_Type + 
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
                    Visa_Sponsorship_Needed +
                    #OBGYN_Grade +
                    Medical_Degree,
    
                  data=all_data, control = tableby.control(test = TRUE, total = F, digits = 1L, digits.p = 2L, digits.count = 0L, numeric.simplify = F, numeric.stats = c("median", "q1q3"), cat.stats = c("Nmiss","countpct"), stats.labels = list(Nmiss = "N Missing", Nmiss2 ="N Missing", meansd = "Mean (SD)", medianrange = "Median (Range)", median ="Median", medianq1q3 = "Median (Q1, Q3)", q1q3 = "Q1, Q3", iqr = "IQR",range = "Range", countpct = "Count (Pct)", Nevents = "Events", medSurv ="Median Survival", medTime = "Median Follow-Up")))

labels(table1_all_data)  #labels

summary(table1_all_data, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology from 2015 to 2018', pfootnote=TRUE)   #show the table

#padjust(table1, method = "bonferroni")   #Adjust for Bonferroni for multiple p-values
summary(table1_all_data, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology from 2015 to 2018', pfootnote=TRUE)

arsenal::write2html(table1_all_data, ("~/Dropbox/Nomogram/nomogram/results/all_data_table1.html"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")   #Write to HTML
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.html")

arsenal::write2word(table1_all_data, paste0("~/Dropbox/Nomogram/nomogram/results/all_data_table1.doc"))  #Write to Word 
#pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.doc")

#######################################################################################
#  Univariate Analysis.  Explore all_data

colnames(all_data)
prop.table(table(all_data$Match_Status)) #Percentage of students who matched
####  Plot variable characteristics
colnames(all_data)
features <-colnames(all_data)
features_rel<-features [2:31]   

for( i in features_rel ){
  temp_plot<-ggplot(data = all_data, aes_string(x=i,fill="Match_Status")) + geom_bar(alpha=0.8,colour='black', show.legend = TRUE, stat = "count") + theme(legend.position = "top") + 
    ggtitle(paste0("Match Statistics for 2019 OBGYN: \n",i)) + 
    guides(fill = guide_legend(nrow = 4, byrow = T) + 
             geom_text (aes(label = y), position = position_stack(vjust = 0.5), size = 10, angle = 45, check_overlap = TRUE) + 
             geom_label(fontface = "bold"))
  print(temp_plot) 
  ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm", dpi = 500,  bg = "transparent")
}
print(temp_plot)

#######################################################################################
#  Bivariate Analysis.  Explore all_data
wilcox.test(all_data$Age~all_data$Match_Status) #SS 
chisq.test(all_data$Match_Status, all_data$white_non_white) #SS
chisq.test(all_data$Match_Status, all_data$Gender) #SS
chisq.test(all_data$Match_Status, all_data$Couples_Match) #SS
t.test(all_data$USMLE_Step_1_Score~all_data$Match_Status) #SS
chisq.test(all_data$Match_Status, all_data$Visa_Sponsorship_Needed) #SS
#chisq.test(all_data$Match_Status, all_data$Med_school_condensed)
chisq.test(all_data$Match_Status, all_data$Medical_Education_or_Training_Interrupted) #SS
chisq.test(all_data$Match_Status, all_data$Misdemeanor_Conviction) #NOT Significant
chisq.test(all_data$Match_Status, all_data$Alpha_Omega_Alpha) #SS
chisq.test(all_data$Match_Status, all_data$US_or_Canadian_Applicant) #SS
chisq.test(all_data$Match_Status, all_data$Gold_Humanism_Honor_Society) #SS
chisq.test(all_data$Match_Status, all_data$Military_Service_Obligation)  #Not significant at all
chisq.test(all_data$Match_Status, all_data$ACLS) #NS
chisq.test(all_data$Match_Status, all_data$Malpractice_Cases_Pending) #NS
chisq.test(all_data$Match_Status, all_data$Citizenship) #SS
chisq.test(all_data$Match_Status, all_data$Sigma_Sigma_Phi) #NS
chisq.test(all_data$Match_Status, all_data$Medical_Degree) #SS
chisq.test(all_data$Match_Status, all_data$Year) #SS
chisq.test(all_data$Match_Status, all_data$Positions_offered) #SS

#Use Wilcox test as data is assumed not to be normally distributed
wilcox.test(all_data$Count_of_Poster_Presentation ~ all_data$Match_Status) #SS
wilcox.test(all_data$Count_of_Oral_Presentation ~ all_data$Match_Status)  #NS
wilcox.test(all_data$Count_of_Peer_Reviewed_Book_Chapter ~ all_data$Match_Status) #SS
wilcox.test(all_data$Count_of_Peer_Reviewed_Online_Publication ~ all_data$Match_Status) #NS
wilcox.test(all_data$USMLE_Step_1_Score ~ all_data$Match_Status) #SS
wilcox.test(all_data$Count_of_Scientific_Monograph ~ all_data$Match_Status) #NS
wilcox.test(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published ~ all_data$Match_Status) #NS
wilcox.test(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts ~ all_data$Match_Status) #SS
wilcox.test(all_data$Count_of_Non_Peer_Reviewed_Online_Publication ~ all_data$Match_Status) #SS


#=================================================================
# Data Wrangling: Look at the full data set for NAs
#=================================================================
nrow(all_data)
ncol(all_data)
#sum(is.na(all_data))
#all_data <- na.omit(all_data)
#sum(is.na(all_data))
str(all_data)

#=================================================================
# Split Full Data Set for Creating Model
#=================================================================
#indexes <- caret::createDataPartition(y = all_data$Match_Status,  #Divide the data into train and test sets
#                                      times = 1,
#                                      p = 0.7,
#                                      list = FALSE)

#Split the data so that we cna run a model and find best factors.  
train <- filter(all_data, Year < 2018)  #Train on years 2015, 2016, 2017
nrow(train)
test <- filter(all_data, Year == c(2018))
nrow(test)

# Examine the proportions of the Match_Status class lable across the datasets.
prop.table(table(all_data$Match_Status))       #Original data set proportion 
prop.table(table(train$Match_Status)) #Train data set proportion
prop.table(table(test$Match_Status))  #Test data set proportion
nrow(all_data)


#=================================================================
# Variable selection with stepwise and best subset approaches, Zhang book page 78
#=================================================================
#First plot the full model
full <- glm(Match_Status ~ white_non_white + 
              Age + 
              Gender + 
              Couples_Match + 
              US_or_Canadian_Applicant + 
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
              Visa_Sponsorship_Needed +
              Medical_Degree, family=binomial, data = train)
summary(full) 
#Final model: white_non_whiteWhite, US_or_Canadian_ApplicantYes, USMLE_Step_1_Score, and Medical_DegreeMD were all significant at the conventional level

#Begin stepwise selection procedure
backward <- stepAIC(full, direction = "backward", trace=FALSE)
backward$anova 
# Final Model:
#   Match_Status ~ white_non_white + Age + Gender + Couples_Match + 
#   US_or_Canadian_Applicant + Medical_Education_or_Training_Interrupted + 
#   Gold_Humanism_Honor_Society + Military_Service_Obligation + 
#   USMLE_Step_1_Score + Visa_Sponsorship_Needed + Medical_Degree

#Create model using stepAIC not backwards regression
scope <- stepAIC(full, scope = list(lower = ~US_or_Canadian_Applicant + USMLE_Step_1_Score, upper = full), trace = FALSE)
scope$anova 
# Final Model:
#   Match_Status ~ white_non_white + Age + Gender + Couples_Match + 
#   US_or_Canadian_Applicant + Medical_Education_or_Training_Interrupted + 
#   Gold_Humanism_Honor_Society + Military_Service_Obligation + 
#   USMLE_Step_1_Score + Visa_Sponsorship_Needed + Medical_Degree

#step2 <- stepAIC(full, ~.^2 +I(scale(Age)^2) + I(scale(USMLE_Step_1_Score)^2) + I(scale(Count_of_Oral_Presentation)^2) + I(scale(Count_of_Peer_Reviewed_Book_Chapter)^2), trace= FALSE) #Looks for interactions between numeric variables of Age, Step 1 score, Count of oral presentations, and count of peer_review_book chapters.
#step2$anova
#Interactions between Gender and USMLE Step 1 remain in the model + Gender:USMLE_Step_1_Score, + Gender:Count_of_Peer_Reviewed_Journal_Articles_Abstracts,  + Age:Count_of_Peer_Reviewed_Online_Publication, + Gender:Count_of_Peer_Reviewed_Online_Publication, + white_non_white:Military_Service_Obligation, + Gold_Humanism_Honor_Society:Count_of_Peer_Reviewed_Online_Publication, + Gender:Gold_Humanism_Honor_Society, + Age:Count_of_Poster_Presentation, + Medical_Education_or_Training_Interrupted:Count_of_Peer_Reviewed_Book_Chapter,  + Count_of_Peer_Reviewed_Online_Publication:Medical_Degree, + white_non_white:Medical_Degree, + Age:US_or_Canadian_Applicant, + US_or_Canadian_Applicant:Medical_Education_or_Training_Interrupted, + Age:Medical_Degree

###Best subset regression, Zhang book page 80, could not figure out dummy variables...

###Recursive partitioning
fit <- rpart::rpart(Match_Status ~ Age + #Univariate SS  #Wald Test p=0.40, fair
                      Gender + #Univariate SS  #Wald Test p=0.30, good
                      white_non_white + #Univariate SS,  # p=0.30
                      Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                      USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                      #Visa_Status_Expected + #Univariate SS, #Wald Testp=0.35
                      Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                      Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                      Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
                      Count_of_Poster_Presentation +
                      Medical_Education_or_Training_Interrupted + #Univariate SS
                      Misdemeanor_Conviction + #Univariate SS
                      US_or_Canadian_Applicant + #Univariate SS
                      Count_of_Non_Peer_Reviewed_Online_Publication, data=train)

fit2 <- rpart(Match_Status ~ Age + #Univariate SS  #Wald Test p=0.40, fair
                Gender + #Univariate SS  #Wald Test p=0.30, good
                white_non_white + #Univariate SS,  # p=0.30
                Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                #Visa_Status_Expected + #Univariate SS, #Wald Testp=0.35
                Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
                Count_of_Poster_Presentation +
                Medical_Education_or_Training_Interrupted + #Univariate SS
                Misdemeanor_Conviction + #Univariate SS
                US_or_Canadian_Applicant + #Univariate SS
                Count_of_Non_Peer_Reviewed_Online_Publication, data = train,
              parms = list(prior = c(.65,.35), split = "information"))

fit3 <- rpart(Match_Status ~ Age + #Univariate SS  #Wald Test p=0.40, fair
                Gender + #Univariate SS  #Wald Test p=0.30, good
                white_non_white + #Univariate SS,  # p=0.30
                Couples_Match + #Univariate SS, # Wald Test p=0.048, KEEP
                USMLE_Step_1_Score + #Univariate SS, #Wald Test p=0.025
                #Visa_Status_Expected + #Univariate SS, #Wald Testp=0.35
                Alpha_Omega_Alpha + #Univariate SS, #Wald Test p=0.5
                Gold_Humanism_Honor_Society + #Univariate SS, #Wald Test p=0.01
                Visa_Sponsorship_Needed + #Univariate SS, #Wald Test p=0.20
                Count_of_Poster_Presentation +
                Medical_Education_or_Training_Interrupted + #Univariate SS
                Misdemeanor_Conviction + #Univariate SS
                US_or_Canadian_Applicant + #Univariate SS
                Count_of_Non_Peer_Reviewed_Online_Publication, data = train,
              control = rpart.control(cp = 0.05))
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)

#=================================================================
#  Residuals and regression diagnostics
#=================================================================
scope.model <- glm(Match_Status ~ white_non_white + Age + Gender + Couples_Match +
                       US_or_Canadian_Applicant + Medical_Education_or_Training_Interrupted +
                       Gold_Humanism_Honor_Society + Military_Service_Obligation +
                       USMLE_Step_1_Score + Visa_Sponsorship_Needed + Medical_Degree, family = binomial, train)



car::residualPlots(scope.model) #All variables are linear except age and Step 1 scores.  Therefore we need to check the trend between predictors and age or Step 1 score.  Better than looking is checking the Test state and they are not significant suggesting a properly specified model.  Zhang book page 90.  

                                      # Test stat Pr(>|Test stat|)
# white_non_white                                               
# Age                                    0.0307           0.8609
# Gold_Humanism_Honor_Society                                   
# USMLE_Step_1_Score                     2.0060           0.1567
# Count_of_Oral_Presentation             0.0761           0.7826
# Count_of_Peer_Reviewed_Book_Chapter    0.2740           0.6007
# Medical_Degree 

marginalModelPlots(scope.model)

#Check for outliers
outlierTest(scope.model)  #There are no outliers as judged by Bonferonni p.  

#Check for leverage
influenceIndexPlot(scope.model, id.n=3) #Identifies observations that are the farthest away from the mean.  Look at the largest studentized residuals for observations that are most likely to be outliers.  

#Check for influence of the outliers
influencePlot(scope.model, col="red", id.n=3)
#Identify the change of coefficient by removing these influential observations
scope.model572 <- update(scope.model, subset= c(-572))
compareCoefs(scope.model, scope.model572) #Coefficients are changed minimally so that observation 572 is not influential

#Identify the change of coefficient by removing these influential observations
scope.model375 <- update(scope.model, subset= c(-375))
compareCoefs(scope.model, scope.model375) #Coefficients are changed minimally so that observation 572 is not influential

#=================================================================
#  Propensity Score, Zhang book page 97 #Matching methods balance the treatment group with the control group so that they are more identical.
#=================================================================

#YOU NEED TO IMPUTE THE DATA BY HAND USING ZHANG BOOK page 43 BEFORE DOING BELOW
#all_data.na.omit <- na.omit(all_data)
#sum(is.na(all_data.na.omit))
#str(all_data.na.omit)
#class(all_data.na.omit)
#all_data.na.omit <- as.data.frame(all_data.na.omit)

#library(MatchIt)
#m.out <- matchit(Match_Status ~ white_non_white, data=all_data.na.omit)

#Zhang book, page 51
nrow(all_data)
sum(is.na(all_data))
imp <- mice::mice(all_data, seed=123456)
imp #5 datasets were imputed
head(complete(imp, action = 4))

#Now run stats on each of the 5 database
ttest <- with(imp.t.test(BLS.miss))

#=================================================================
#  Factor Selection
#=================================================================

#Method 1:  Principal Component Analysis - Use principal components analysis to pick what predictors to use 
train_pca <- preProcess(select(all_data, - Match_Status), 
                        method = c("center", "scale", "nzv", "pca"))
train_pca
train_pca$method
train_pca$rotation
#PCA needed 8 components to capture 95% of the variance:
# [1] "USMLE_Step_1_Score"                                                    
# [2] "Count_of_Oral_Presentation"                                            
# [3] "Count_of_Other_Articles"                                               
# [4] "Count_of_Peer_Reviewed_Journal_Articles_Abstracts"                     
# [5] "Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published"
# [6] "Count_of_Peer_Reviewed_Online_Publication"                             
# [7] "Count_of_Poster_Presentation"                                          
# [8] "Age"  

#Method 2: Recursive Feature Elimination
options(warn=-1)
colnames(all_data)
subsets <- c(2:31)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 2,
                   verbose = FALSE)

lmProfile <- rfe(x=train[, 2:31], y=train$Match_Status,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile  
#Recursive feature elimination picked USMLE_Step_1_Score, Count_of_Poster_Presentation, US_or_Canadian_Applicant, Alpha_Omega_Alpha, Gold_Humanism_Honor_Society

#Method 3:  Boruta search
#You'll see how you can use it to perform a top-down search for relevant features by comparing original attributes' importance with importance achievable at random, estimated using their permuted copies, and progressively elliminating irrelevant features.
boruta_output <- Boruta::Boruta(Match_Status ~ ., data=na.omit(train), doTrace=0)  
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <-Boruta::getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  
roughFixMod <- TentativeRoughFix(boruta_output)  # Do a tentative rough fix
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# [1] "Medical_Education_or_Training_Interrupted" "ACLS"                                     
# [3] "PALS"                                      "Citizenship"                              
# [5] "Gender"                                    "US_or_Canadian_Applicant"                 
# [7] "Visa_Sponsorship_Needed"                   "USMLE_Step_1_Score"                       
# [9] "Count_of_Poster_Presentation"              "Age"                                      
# [11] "BLS"                                       "Gold_Humanism_Honor_Society"   

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
plot(boruta_output, cex.axis=0.35, las=2, xlab="", main="Variable Importance")  # Plot variable importance
#Boruta chose:
# meanImp  decision
# USMLE_Step_1_Score       20.265448 Confirmed
# Age                      17.253973 Confirmed
# Citizenship              15.657170 Confirmed
# US_or_Canadian_Applicant 14.254795 Confirmed
# PALS                     10.006216 Confirmed
# Visa_Sponsorship_Needed   6.898714 Confirmed

#Method 4:  Variable important from ML algorithm 
modelLookup('earth')

# Train the model using randomForest and predict on the training data itself.
model_mars = train(Match_Status ~ ., data=train, method='earth')
fitted <- predict(model_mars)
model_mars
plot(model_mars, main="Model Accuracies with MARS") #Iterations of hyperparameter search performed.
varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS") #Visual of the most important factors
#MARS only predicted that Step 1 score was important

#Model 5: Backwards Stepwise Regression
model.binomial.significant <- rms::lrm(Match_Status ~ 
                                         white_non_white + 
                                         Age + 
                                         Gender + 
                                         Couples_Match + 
                                         #Expected_Visa_Status_Dichotomized + 
                                         US_or_Canadian_Applicant + 
                                         #Medical_School_Type + 
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
                                         Visa_Sponsorship_Needed +
                                         #OBGYN_Grade +
                                         Medical_Degree,
                                       data = train, x=TRUE, y=TRUE)

print(model.binomial.significant)  #Check the C-statistic which is the same as ROC
rms::fastbw(model.binomial.significant, type=c("residual", "individual", "total"), rule=c("aic", "p")) 
#Factors selected by fast backwards step-wise regression for: white_non_white, USMLE Step 1, Count of Poster Presentations.  This is quick and dirty.  

#=================================================================
#  Create a Logistic Regression Model: log(odds)=β0+β1∗x1+...+βn∗xn
#=================================================================
colnames(train)
ddist <- datadist(train)
ddist
options (datadist = 'ddist')

sum(is.na(train$Alpha_Omega_Alpha))
levels(train$Alpha_Omega_Alpha)
str(train$Alpha_Omega_Alpha)
sapply(sapply(train, unique), length)
train$Alpha_Omega_Alpha <- as.factor(train$Alpha_Omega_Alpha)
table(train$Alpha_Omega_Alpha)


# Recalculated using rms::lrm so that I can use that package for the nomogram
model.binomial.significant <- rms::lrm(Match_Status ~ 
                                         white_non_white + 
                                         Age + 
                                         Gender + 
                                         Couples_Match + 
                                         #Expected_Visa_Status_Dichotomized + 
                                         US_or_Canadian_Applicant + 
                                         #Medical_School_Type + 
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
                                         Visa_Sponsorship_Needed +
                                         #OBGYN_Grade +
                                         Medical_Degree,
                                       data = train, x=TRUE, y=TRUE)

print(model.binomial.significant)  #Check the C-statistic which is the same as ROC area for binary logistic regression
anova(model.binomial.significant) #Harrell book page 298, The Wald Anova indicates especially strong age, US or Canadian applicants, USMLE score effects.  

f <- update(model.binomial.significant, x=TRUE, y=TRUE)
validate(model.binomial.significant, B=200)


fastbw(model.binomial.significant) #stepdown by p-values, AIC

model2 <- rms::lrm(Match_Status ~ Age +
                     white_non_white +
                     USMLE_Step_1_Score + 
                     Count_of_Poster_Presentation + 
                     US_or_Canadian_Applicant +
                     Alpha_Omega_Alpha +
                     Gold_Humanism_Honor_Society, data = train, x=TRUE, y=TRUE)
print(model2)
validate <- rms::validate(model2, B=1000, set.seed(123456), sls=0.2)
validate
model2plot <- rms::calibrate(model2)
dev.off()
plot(model2plot)

library(Hmisc) #Evaluate predictors without looking at the outcome variable
hoeffding <- Hmisc::varclus(~                     white_non_white + 
                              Age + 
                              Gender + 
                              Couples_Match + 
                              #Expected_Visa_Status_Dichotomized + 
                              US_or_Canadian_Applicant + 
                              #Medical_School_Type + 
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
                              Visa_Sponsorship_Needed +
                              #OBGYN_Grade +
                              Medical_Degree, data = train, sim="hoeffding")  #only predictors
plot(hoeffding)

#Redundancy analysis
redun <- Hmisc::redun(~                     white_non_white + 
                        Age + 
                        Gender + 
                        Couples_Match + 
                        #Expected_Visa_Status_Dichotomized + 
                        US_or_Canadian_Applicant + 
                        #Medical_School_Type + 
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
                        Visa_Sponsorship_Needed +
                        #OBGYN_Grade +
                        Medical_Degree, data = train, type="adjusted", r2 = 0.3, pr=TRUE)    #only predictors
print(redun, digits=3, long=TRUE)

install.packages("rpart")
library(rpart)
rpart::
  
#=================================================================
#Look for Co-linearity with Variance Inflation Factors
#=================================================================
rms::vif(model.binomial.significant) #Should be <4

#=================================================================
#  Table 2 of Odds Ratios and CIs for Predictors of Matching into OBGYN
#=================================================================
oddsratios <- as.data.frame(exp(cbind("Odds ratio" = coef(model.binomial.significant), confint.default(model.binomial.significant, level = 0.95))))
print(oddsratios)

#Write Table 2 to HTML
arsenal::write2html(oddsratios, ("~/Dropbox/Nomogram/nomogram/results/all_data_oddratios_table2.html"), total=FALSE, title = "Table 2", quiet = FALSE, theme = "yeti")
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_oddratios_table2.html")

#Write to Table 2 word
arsenal::write2word(oddsratios, ("~/Dropbox/Nomogram/nomogram/results/all_data_oddsratios_table2.doc"))

#Another way to create odds ratios, page 308 Harrell
#Use Hmisc to plot out odds ratios that are alot clearer than Table 2
dd <- datadist(all_data); options(datadist='dd')
dd <- datadist
s <- summary(f.mi)

#=================================================================
#  Prepare the test dataset and predict on NEW DATA
#=================================================================
#dummy.vars <- caret::dummyVars(~ ., data = match.train[, -1])
#trainData <- predict(dummy.vars, match.train[, -1])  #Had to remove spaces from all variables and values
pre.process <- caret::preProcess(train, method = "bagImpute") # Now, impute!
imputed.data <- predict(pre.process, train)
sum(is.na(imputed.data))
testData2 <- predict(pre.process, test)  

#=================================================================
# Predict and Check Confusion Matrix
#=================================================================
predicted <- predict(model_mars, testData2)
head(predicted)
caret::confusionMatrix(reference = test$Match_Status, data = predicted, mode='everything') 

#=================================================================
# Check the quality of the new model
#=================================================================
####Pseudo R^2
pscl::pR2(model.binomial.significant)  # look for 'McFadden', values closer to zero indicating that the model has no predictive power

model.binomial.significant2 <- glm(Match_Status ~ USMLE_Step_1_Score + Alpha_Omega_Alpha + Count_of_Poster_Presentation + US_or_Canadian_Applicant, data = test, family = "binomial")

# Compute AUC for predicting Match_Status_Dichot with the model
prob <- predict(model.binomial.significant2, newdata=test, type="response")  #Must use GLM model
pred <- prediction(prob, test$Match_Status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc  

#######################################################################################
###NOMOGRAM 
#fun.at - Demarcations on the function axis: "Matching into obgyn"
#lp=FALSE so we don't have the logistic progression

nomo_from_model.binomial.significant <- rms::nomogram(model.binomial.significant, 
                                                      #lp.at = seq(-3,4,by=0.5),
                                                      fun = plogis, 
                                                      fun.at = c(0.001, 0.01, 0.05, seq(0.2, 0.8, by = 0.2), 0.95, 0.99, 0.999), 
                                                      funlabel = "Chance of Matching in OBGYN", 
                                                      lp =FALSE,
                                                      #conf.int = c(0.1,0.7), 
                                                      abbrev = F,
                                                      minlength = 9)
nomogramEx(nomo=nomo_from_model.binomial.significant,np=1,digit=2)  #Gives the polynomial formula


nomo_final <- plot(nomo_from_model.binomial.significant, lplabel="Linear Predictor",
                   cex.sub = 0.8, cex.axis=0.8, cex.main=1, cex.lab=1, ps=10, xfrac=.7,
                   #fun.side=c(3,3,1,1,3,1,3,1,1,1,1,1,3),
                   #col.conf=c('red','green'),
                   #conf.space=c(0.1,0.5),
                   label.every=1,
                   col.grid = gray(c(0.8, 0.95)),
                   which="Match_Status")
print(nomo_from_model.binomial.significant)

# Check Brier Score
DescTools::BrierScore(model.binomial.significant2)

# Calibration
calib <- rms::calibrate(model.binomial.significant, boot=1000, data = test)  #Plot test data set
#AUC and calibration matters

plot(calib)
calib
#######################################################################################
beepr::beep(sound = 4)
#DynNom::DynNom.lrm(model.binomial.significant, data, clevel = 0.95, m.summary = "formatted")

#######################################################################################
#Decision Curve

#### REFERENCES
#https://lengyueyang.github.io/Research/Nomogram-rms.html
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#https://campus.datacamp.com/courses/multiple-and-logistic-regression/logistic-regression-4?ex=1
#https://www.kaggle.com/sindhuee/r-caret-example
#https://github.com/datasciencedojo/meetup/blob/master/intro_to_ml_with_r_and_caret/IntroToMachineLearning.R
#https://www.machinelearningplus.com/machine-learning/caret-package/
#https://www.machinelearningplus.com/machine-learning/feature-selection/
#http://r-statistics.co/Logistic-Regression-With-R.html
#http://r-statistics.co/Variable-Selection-and-Importance-With-R.html#7.%20Information%20value%20and%20Weight%20of%20evidence
#https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
#https://www.meetup.com/data-science-dojo/events/239730653/
#dev.off()  #https://towardsdatascience.com/visual-overview-of-the-data-frame-4c6186a69697
#https://www.analyticsvidhya.com/blog/2016/12/introduction-to-feature-selection-methods-with-an-example-or-how-to-select-the-right-variables/
### Use recursive feature elimination (rfe), https://www.machinelearningplus.com/machine-learning/caret-package/ 
##Nomogram for a binary outcome (matching into residency), https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5451623/
#####https://rdrr.io/cran/caret/man/calibration.html  ####
#http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/

#Pull clerkship grades by hand.  
#Create column of CU students vs. universe
#Create column of CU students who did not match vs. all students who did not match.  
#If we could look at the clerkship honors, hp, pass would allow us to decrease our step 1 cut off or put at mean because need to review by hand.  
#Hurts student if they do not get a grade at Stanford clerkship.  
#Academic score from CU could be a proxy for clerkship and sub-i grades.  These people were reviewed by Meredith to determine if they should get a CU interview.  All these people have a step 1 score of >233.  National average was 229 because it is due to time.  This is Perfect score is 10 for A or Honors.  
#At APGO/CREOG talk about removing step 1 score and then you can't do any sort of cut off.  


#Modifiable Factors 

model.binomial.significant <- rms::lrm(Match_Status ~ 
                                         
                                         Alpha_Omega_Alpha + 
                                         Gold_Humanism_Honor_Society + 
                                         #Military_Service_Obligation + 
                                         USMLE_Step_1_Score + 
                                         #Military_Service_Obligation + 
                                         Count_of_Poster_Presentation + 
                                         Count_of_Oral_Presentation + 
                                         Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                                         Count_of_Peer_Reviewed_Book_Chapter + 
                                         Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + 
                                         Count_of_Peer_Reviewed_Online_Publication,
                                         #Misdemeanor_Conviction  + 
                                         #Visa_Sponsorship_Needed +
                                         #OBGYN_Grade +
                                         #Medical_Degree,
                                       data = all_data, x=TRUE, y=TRUE)


#lrm model with relaxed splines for the numerical values
test <- rms::lrm(Match_Status ~ 
                                         white_non_white + 
                                         rcs(Age, 5) + 
                                         Gender + 
                                         Couples_Match + 
                                         #Expected_Visa_Status_Dichotomized + 
                                         US_or_Canadian_Applicant + 
                                         #Medical_School_Type + 
                                         Medical_Education_or_Training_Interrupted + 
                                         Misdemeanor_Conviction + 
                                         Alpha_Omega_Alpha + 
                                         Gold_Humanism_Honor_Society + 
                                         Military_Service_Obligation + 
                                         rcs(USMLE_Step_1_Score,5) + 
                                         Military_Service_Obligation + 
                                         Count_of_Poster_Presentation + 
                                         Count_of_Oral_Presentation + 
                                         Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                                         Count_of_Peer_Reviewed_Book_Chapter + 
                                         Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + 
                                         Count_of_Peer_Reviewed_Online_Publication + 
                                         Misdemeanor_Conviction  + 
                                         Visa_Sponsorship_Needed +
                                         #OBGYN_Grade +
                                         Medical_Degree,
                                       data = all_data, x=TRUE, y=TRUE)
plot(anova(test))

all_data<- as.data.frame(all_data)

dd <- datadist(all_data); options(datadist='dd')
ggplot(Predict(model.binomial.significant), sepdiscrete = 'vertical', vnames = 'names',
       rdata = all_data, 
       histSpike.opts = list(frac = function(f) 0.1*f/max(f)))



plot(summary(model.binomial.significant), log = TRUE)
