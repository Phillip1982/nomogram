#https://lengyueyang.github.io/Research/Nomogram-rms.html
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#https://campus.datacamp.com/courses/multiple-and-logistic-regression/logistic-regression-4?ex=1

#Install and Load packages
#rm(list=ls())
if(!require(pacman))install.packages("pacman")
pacman::p_load('Hmisc', 'readxl', 'XML', 'reshape2', 'devtools', 'plyr', 'packrat', 'highcharter', 'purrr', 'readr', 'htmlwidgets', 'RColorBrewer', 'leaflet', 'rgdal', 'dygraphs', 'quantmod', 'DT', 'formattable', 'ggplot2',  'idbr', 'genderizeR', 'animation', 'dplyr', 'magick', 'tidycensus', 'ggthemes', 'stringr', 'geosphere', 'ggmap', 'grid', 'gmapsdistance', 'zipcode', 'janitor', 'lubridate', 'hms', 'tidyr', 'stringr', 'readr', 'openxlsx', 'forcats', 'RcppRoll', 'tibble', 'bit64', 'munsell', 'scales', 'leaflet', 'rgdal', 'htmltools', 'mapview', 'htmlwidgets', 'sf', 'sp', 'tidyverse', 'viridis', 'fansi', 'webshot', 'geosphere', 'zipcode', 'leaflet.extras', 'raster',  'spData','spDataLarge', 'stplanr', 'tmap', 'osmdata', 'arsenal', 'doMC', "wesanderson", "fasterize", "USAboundaries", "RANN", "tidycensus", "geofacet", "extrafont", "shiny", "ParallelLogger", "parallel", "RSelenium", "humaniformat", "visdat", "skimr", "assertr", "tidylog", "doParallel", "DiagrammeR", "DiagrammeRsvg", "rsvg", "iterators", "parallel", "foreach", "PASWR", "rms", "pROC", "ROCR", "nnet", "janitor", "packrat", "DynNom", "rsconnect", "caret")
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
#Import Data
data <- read_rds("~/Dropbox/Nomogram/nomogram/data/CU_Obgyn_Residency_Applicants_mutate_39.rds")
 colnames(data)
 str(data)
 data$Match_Status_Dichot #The response variable is coded 0 for not matching and 1 for successfully matching
 
 ################################################################
 #Data cleaning, Place nicer labels for the data
 label(data$Self_Identify)    <- 'Race/Ethnicity'
 label(data$Alpha_Omega_Alpha) <- 'AOA Member'
 label(data$USMLE_Step_1_Score) <- 'USMLE Step 1 Score'
 label(data$Gender) <- 'Gender'
 label(data$Couples_Match) <- 'Couples Matching'
 label(data$Expected_Visa_Status_Dichotomized) <- 'Expected Visa Status'
 label(data$Medical_School_Type) <- 'Medical School Type'
 label(data$Medical_Education_or_Training_Interrupted) <- 'Medical School Interrupted'
 label(data$Misdemeanor_Conviction) <- 'Misdemeanor Conviction'
 label(data$USMLE_Step_2_CK_Score) <- 'USMLE Step 2 CK Score'
 label(data$USMLE_Step_2_CS_Score) <- 'USMLE Step 2 CS Score'
 label(data$USMLE_Step_3_Score) <- 'USMLE Step 3 Score'
 label(data$US_or_Canadian_Applicant) <- 'US or Canadian Applicant'
 label(data$Gold_Humanism_Honor_Society) <- 'Gold Humanism Honors Society'
 label(data$Military_Service_Obligation) <- 'Military Service Obligation'
 label(data$Count_of_Oral_Presentation) <- 'Count of Oral Presentations'
 label(data$Count_of_Peer_Reviewed_Book_Chapter) <- 'Count of Peer-Reviewed Book Chapters'
 label(data$Count_of_Poster_Presentation) <- 'Count of Poster Presentations'
 label(data$Other_Service_Obligation) <- 'Other Service Obligation'
 label(data$Med_school_condensed) <- 'Medical School Condensed' 
 label(data$white_non_white) <- 'Race' 
 label(data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts) <- 'Count of Peer-Reviewed Journal Articles' 
 label(data) #Check labels for the data set
 
 ####
 #Visualize the Data in Graphs
 # Draw histogram for continuous data
 ggplot(data) +
   geom_histogram(aes(x = data$USMLE_Step_1_Score))
 
 # Draw out data for the categorical response variable of matching. 
 ggplot(data = data, aes(x = data$USMLE_Step_1_Score, y = data$Match_Status_Dichot)) + 
   geom_jitter(width = 0, height = 0.05, alpha = 0.5) 
 
 ################################################################
 #### Building Table 1 ####
 #Use the arsenal package to create a table one with p-values.  I changed the stats so I get median, IQR.   
 colnames(data)
tab.noby <- tableby(Match_Status ~ white_non_white + Gender + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + USMLE_Step_2_CK_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Poster_Presentation + Military_Service_Obligation + Other_Service_Obligation + Visa_Sponsorship_Needed + Misdemeanor_Conviction, data=data, control = tableby.control(test = TRUE, total = F, digits = 1L, digits.p = 3L, digits.count = 0L, numeric.simplify = F, numeric.stats = c("Nmiss", "median", "q1q3"), cat.stats = c("Nmiss","countpct"), stats.labels = list(Nmiss = "N Missing", Nmiss2 ="N Missing", meansd = "Mean (SD)", medianrange = "Median (Range)", median ="Median", medianq1q3 = "Median (Q1, Q3)", q1q3 = "Q1, Q3", iqr = "IQR",range = "Range", countpct = "Count (Pct)", Nevents = "Events", medSurv ="Median Survival", medTime = "Median Follow-Up")))
 
 #labels
 labels(tab.noby)
 
 #show the table
 summary(tab.noby, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology in 2019', pfootnote=TRUE)
 
 #Adjust for Bonferroni for multiple p-values
 padjust(tab.noby, method = "bonferroni")
 summary(tab.noby, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology in 2019', pfootnote=TRUE)
 
 #Write to HTML
 write2html(tab.noby, paste0("~/Dropbox/Nomogram/nomogram/results/table1.html"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")
 
 #Write to word
 write2word(tab.noby, paste0("~/Dropbox/Nomogram/nomogram/results/table1.doc"))
 
 
 #######################################################################################
 #The first step is to partition the data into training and testing sets.
 #A logistic regression model has been built and the coefficients have been examined. However, some critical questions remain. Is the model any good? How well does the model fit the data? Which predictors are most important? Are the predictions accurate? 
 #https://www.r-bloggers.com/evaluating-logistic-regression-models/
 library(caret)
 data <- as.data.frame(data)
 Train <- caret::createDataPartition(data$Match_Status_Dichot, p=0.6, list=FALSE)  
 #p=0.6 = 60% of data into training
 training <- data[ Train, ]  #60% of the data here 
 dim(training)
 testing <- data[ -Train, ]   #40% of the data here
 dim(testing)
 
 #######################################################################################
#Using the training dataset, which contains 697 observations, we will use logistic regression to model data$Match_Status_Dichot as a function of the predictors.
 #For Logistic regression we use the binomial family.  
 colnames(data)
 mod_fit <- glm(Match_Status_Dichot ~ Self_Identify + Gender + Couples_Match + Expected_Visa_Status_Dichotomized + Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Poster_Presentation + Military_Service_Obligation + Other_Service_Obligation + Visa_Sponsorship_Needed + Count_of_Non_Peer_Reviewed_Online_Publication + USMLE_Step_2_CS_Score, data=data, family="binomial")
 print(mod_fit)
 
 #######################################################################################
 #Run stats to see what is <0.1 and should be included into model
 chisq.test(data$Match_Status, data$Self_Identify)
 chisq.test(data$Match_Status, data$Gender)
 chisq.test(data$Match_Status, data$Couples_Match)
 chisq.test(data$Match_Status, data$Expected_Visa_Status_Dichotomized)
 chisq.test(data$Match_Status, data$Med_school_condensed)
 chisq.test(data$Match_Status, data$Medical_Education_or_Training_Interrupted)
 chisq.test(data$Match_Status, data$Misdemeanor_Conviction)
 chisq.test(data$Match_Status, data$Alpha_Omega_Alpha)
 chisq.test(data$Match_Status, data$US_or_Canadian_Applicant)
 chisq.test(data$Match_Status, data$Gold_Humanism_Honor_Society)
 chisq.test(data$Match_Status, data$Military_Service_Obligation)  #Not significant at all
 chisq.test(data$Match_Status, data$Visa_Sponsorship_Needed)
 chisq.test(data$Match_Status, data$white_non_white)
 chisq.test(data$Match_Status, data$Expected_Visa_Status) #Not significant at all
 chisq.test(data$Match_Status, data$Partner_Match) #Zero significance

#######################################################################################
#Now we want to use the model parameters to predict the value of the target variable in a completely new set of observations. That can be done with the predict function.
predict(mod_fit, newdata=testing)
predict(mod_fit, newdata=testing, type="response")

## Test out two different models with different variables  
#  A logistic regression is said to provide a better fit to the data if it demonstrates an improvement over a model with fewer predictors.
mod_fit_one <- glm(Match_Status_Dichot ~ Self_Identify + Gender + Couples_Match + Expected_Visa_Status_Dichotomized + Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Poster_Presentation + Military_Service_Obligation + Other_Service_Obligation + Visa_Sponsorship_Needed + Count_of_Non_Peer_Reviewed_Online_Publication + USMLE_Step_2_CS_Score, data=data, family="binomial")

mod_fit_two <- glm(Match_Status_Dichot ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation, data=training, family="binomial")

####Pseudo R^2
library(pscl)
pR2(mod_fit_one)  # look for 'McFadden', values closer to zero indicating that the model has no predictive power.
pR2(mod_fit_two) 

####Wald Test - Individual Variable Importance
#  The idea is to test the hypothesis that the coefficient of an independent variable in the model is significantly different from zero. If the test fails to reject the null hypothesis, this suggests that removing the variable from the model will not substantially harm the fit of that model.
library(survey)
regTermTest(mod_fit_one, "Self_Identify")
regTermTest(mod_fit_one, "USMLE_Step_1_Score")
regTermTest(mod_fit_one, "Misdemeanor_Conviction")  #Drop it because Wald test show p=0.641
regTermTest(mod_fit_one, "white_non_white") # p=0.07
regTermTest(mod_fit_one, "Couples_Match") # p=0.08
regTermTest(mod_fit_one, "Alpha_Omega_Alpha")
regTermTest(mod_fit_one, "Count_of_Oral_Presentation") #Drop it with p=0.59
regTermTest(mod_fit_one, "Gold_Humanism_Honor_Society")
regTermTest(mod_fit_one, "Count_of_Peer_Reviewed_Journal_Articles_Abstracts") #p=0.5
regTermTest(mod_fit_one, "Count_of_Peer_Reviewed_Book_Chapter") #Drop it p=0.7
regTermTest(mod_fit_one, "Military_Service_Obligation") #Drop it, p=0.98
regTermTest(mod_fit_one, "Other_Service_Obligation") #Drop
regTermTest(mod_fit_one, "Visa_Sponsorship_Needed") #Drop
regTermTest(mod_fit_one, "Misdemeanor_Conviction") #Drop

#To assess the relative importance of individual predictors in the model, we can also look at the absolute value of the t-statistic for each model parameter. 
caret::varImp(mod_fit_one)
caret::varImp(mod_fit_two)

#### Validation of Predicted Values
#The process involves using the model estimates to predict values on the training set. Afterwards, we will compared the predicted target variable versus the observed values for each observation. 
pred = predict(mod_fit_one, newdata=testing)
accuracy <- table(pred, testing[,"Match_Status_Dichot"])
sum(diag(accuracy))/sum(accuracy)


####Test Area Under the Curve
library(ROCR)
# Compute AUC for predicting Match_Status_Dichot with the model
prob <- predict(mod_fit_two, newdata=testing, type="response")
pred <- prediction(prob, testing$Match_Status_Dichot)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc  #As suspected the second model is the better with AUC of 0.82


##Nomogram for a binary outcome (matching into residency), https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5451623/
#fun.at - Demarcations on the function axis: "Matching into obgyn"
#lp=FALSE so we don't have the logistic progression
colnames(data)
ddist <- datadist(data)
ddist
options (datadist = 'ddist')

######Now that we have picked the variables and the best model time to get a nomogram.  
#Creation of model using format of:  log(odds)=β0+β1∗x1+...+βn∗xn
#These variables need work Medical School Type, Med_school_condensed
#Removed Step 2 CK score because most applicants will not have it and I don't have data on those who did not take the test at the time of applying.  
mod.bi <- rms::lrm(Match_Status_Dichot ~ white_non_white + Gender + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Poster_Presentation + Military_Service_Obligation + Other_Service_Obligation + Visa_Sponsorship_Needed + Misdemeanor_Conviction, data = data)
print(mod.bi)

#Keep predictors in the binary logistic regression model that have a p<0.10 a priori to create nomogram
mod.bi.significant <- rms::lrm(Match_Status_Dichot ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation, data = data)
print(mod.bi.significant)  #Check the C-statistic which is the same as ROC area for binary logistic regression

nom.bi <- rms::nomogram(mod.bi.significant, 
                        #lp.at = seq(-3,4,by=0.5),
                        fun = plogis, 
                        fun.at = c(0.001, 0.01, 0.05, seq(0.2, 0.8, by = 0.2), 0.95, 0.99, 0.999), 
                        funlabel = "Chance of Matching OBGYN", 
                        lp =FALSE,
                        #conf.int = c(0.1,0.7), 
                        abbrev = F,
                        minlength = 9)

plot(nom.bi, lplabel="Linear Predictor",
     cex.sub = 0.8, cex.axis=0.8, cex.main=1, cex.lab=1, ps=10, xfrac=.7,
     #fun.side=c(3,3,1,1,3,1,3,1,1,1,1,1,3),
     #col.conf=c('red','green'),
     #conf.space=c(0.1,0.5),
     label.every=1,
     col.grid = gray(c(0.8, 0.95)),
     which="Match_Status_Dichot")
print(nom.bi)
#legend.nomabbrev(nom.bi, which='Alpha_Omega_Alpha', x=.5, y=5)


#Data partitioning to 80/20 for testing and training
set.seed(88) # set seed for replication
alpha=0.05

#making the sample data in an 80/20 split
ind <- sample(2, nrow(nomo), replace = T, prob = c(0.8, 0.2))
#All rows in the data training set where the value is 1 get assigned to training data set
train <- nomo[ind==1, ]
test <- nomo[ind==2, ]
dim(train)
dim(test)

#######################################################################################
#Sign up for shinyapp.io
#DynNom
  nomo_fit2 <- rms::lrm(Match_Status_Dichot ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation, data = data)
  #fit2 <- stats::glm(survived ~ (age + pclass + sex) ^ 3, titanic3, family = "binomial")
  DynNom::DynNom.lrm(nomo_fit2, data, clevel = 0.95, m.summary = "formatted")
  #rsconnect::deployApp(appDir = getwd())
  
  
  
  #construction of nomograms
  setwd("D:/")
  library(MASS)
  library(foreign)
  library(splines)
  library(rms)
  lc<-read.spss("NPC_training.sav",use.value.labels=T,to.data.frame=T)
  attach(lc) 
  coxm<-cph(Surv(OS,OSstatus)~age+gender+LDH+CRP+Tstage+Nstage+EBV,x=T,y =T,data=lc,surv=T)
  scoxm<-step(coxm)
  dd<-datadist(lc)
  options(datadist="dd")
  surv<-Survival(scoxm)
  surv1<-function(x) surv(1*365,lp=x)
  surv2<-function(x) surv(3*365,lp=x)
  surv3<-function(x) surv(5*365,lp=x)
  nom <-nomogram(scoxm,fun=list(surv1,surv2,surv3),lp=F,funlabel=c('1-year survival','3-year survival','5-year survival'),maxscale=100,fun.at=c (1.00,0.95,0.9,0.85,0.8,0.75,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0))
  plot(nom, xfrac=.45) 
  print(nom)
  

  