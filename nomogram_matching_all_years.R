#Muffly, Liss, Alston, Raffaelli, Janet Corral, PhD , Jelovsek  ###DRAFT###
# JC - Affiliation Associate Professor of Medicine at CU SOM

##################################################################
#Objective:  We sought to construct and validate a model that predict a medical students chances of matching into an obstetrics and gynecology residency.  The prediction target is matching.  

#Install and Load packages
if(!require(pacman))install.packages("pacman")
pacman::p_load('caret', 'readxl', 'XML', 'reshape2', 'devtools', 'purrr', 'readr', 'ggplot2', 'dplyr', 'magick', 'janitor', 'lubridate', 'hms', 'tidyr', 'stringr', 'readr', 'openxlsx', 'forcats', 'RcppRoll', 'tibble', 'bit64', 'munsell', 'scales', 'rgdal', 'tidyverse', "foreach", "PASWR", "rms", "pROC", "ROCR", "nnet", "janitor", "packrat", "DynNom", "export", "caTools", "mlbench", "randomForest", "ipred", "xgboost", "Metrics", "RANN", "AppliedPredictiveModeling", "nomogramEx", "shiny", "earth", "fastAdaboost", "Boruta", "glmnet", "ggforce", "tidylog", "InformationValue", "pscl", "scoring", "DescTools", "gbm", "Hmisc", "arsenal", "pander", "moments", "leaps", "MatchIt", "car", "mice", "rpart", "beepr", "fansi", "utf8", "zoom", "lmtest", "ResourceSelection", "Deducer", "rpart", "rmarkdown", "rattle", "rmda", "funModeling", "DynNom", "tinytex", "caretEnsemble", "rJava")
#.libPaths("/Users/tylermuffly/.exploratory/R/3.5")  # Set libPaths.
#packrat::init(infer.dependencies = TRUE)
packrat_mode(on = TRUE)
set.seed(123456)

##################################################################
#### Set data file locations ####
setwd("~/Dropbox/Nomogram/nomogram")  #Set working directory

################################################################
#Data cleaned in exploratory.io then brought here.  

#Load Data
download.file("https://www.dropbox.com/s/hxkxdmmbd5927j3/all_years_reorder_cols_84.rds?raw=1",destfile=paste0("all_years_mutate_83.rds"), method = "auto", cacheOK = TRUE)
all_data <- read_rds("~/Dropbox/Nomogram/nomogram/data/all_years_reorder_cols_84.rds")  #Bring in years 2015, 2016, 2017, and 2018 data
dplyr::glimpse(all_data)
dim(all_data)
colnames(all_data)
all_data$Match_Status
all_data <- all_data %>%
  select(-"Gold_Humanism_Honor_Society", -"Sigma_Sigma_Phi", -"Misdemeanor_Conviction", -"Malpractice_Cases_Pending", -"Match_Status_Dichot", -"Citizenship", -"BLS", -"Positions_offered")

all_data <- all_data[c('white_non_white', 'Age',  'Year', 'Gender', 'Couples_Match', 'US_or_Canadian_Applicant', "Medical_Education_or_Training_Interrupted", "Alpha_Omega_Alpha",  "Military_Service_Obligation", "USMLE_Step_1_Score", "Count_of_Poster_Presentation",  "Count_of_Oral_Presentation", "Count_of_Peer_Reviewed_Journal_Articles_Abstracts", "Count_of_Peer_Reviewed_Book_Chapter", "Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published", "Count_of_Peer_Reviewed_Online_Publication", "Visa_Sponsorship_Needed", "Medical_Degree", 'Match_Status')]

################################################################
# Place nicer labels for the data
Hmisc::label(all_data$Medical_Degree) <- "Medical Degree"
Hmisc::label(all_data$Visa_Sponsorship_Needed) <- "Visa Sponsorship Needed"
Hmisc::label(all_data$Age)    <- 'Age'
units(all_data$Age) <- 'years'
Hmisc::label(all_data$Alpha_Omega_Alpha) <- 'AOA Member'
Hmisc::label(all_data$USMLE_Step_1_Score) <- 'USMLE Step 1 Score'
Hmisc::label(all_data$Gender) <- 'Gender'
Hmisc::label(all_data$Couples_Match) <- 'Couples Matching'
#Hmisc::label(all_data$Medical_School_Type) <- 'Medical School Type'
Hmisc::label(all_data$Medical_Education_or_Training_Interrupted) <- 'Medical School Interrupted'
#Hmisc::label(all_data$Misdemeanor_Conviction) <- 'Misdemeanor Conviction'
Hmisc::label(all_data$US_or_Canadian_Applicant) <- 'US or Canadian Applicant'
Hmisc::label(all_data$Military_Service_Obligation) <- 'Military Service Obligation'
Hmisc::label(all_data$Count_of_Oral_Presentation) <- 'Count of Oral Presentations'
Hmisc::label(all_data$Count_of_Peer_Reviewed_Book_Chapter) <- 'Count of Peer-Reviewed Book Chapters'
Hmisc::label(all_data$Count_of_Poster_Presentation) <- 'Count of Poster Presentations'
Hmisc::label(all_data$white_non_white) <- 'Race' 
Hmisc::label(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts) <- 'Count of Peer-Reviewed Journal Articles'
Hmisc::label(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published) <-'Count of Peer-Reviewed Research Not Published'
Hmisc::label(all_data$Match_Status) <- 'Matching Status'
Hmisc::label(all_data) #Check labels for the data set

############################################################################################
####Univariate using the Hmisc::describe function
Hmisc::describe(all_data) # A little better than summary.  Gives proportions for categorical variables. Amen!

####
#Look at the data in one graph.  Nice.  Page 292 in Harrell's book
#dev.off()
par("mar")
par(mar=c(1,1,1,1))  #Make borders thinner

colnames(all_data)
all_data$Match_Status <- as.numeric(all_data$Match_Status)
all_data$Match_Status
all_data$Match_Status <- (all_data$Match_Status - 1)
all_data$Match_Status #Outcome must be numeric

############################################################################################
####Univariate using the Hmisc::summary graph of data
dd <- rms::datadist(all_data)
options(datadist='dd')

s <- summary(Match_Status ~ cut2(Age, 30:30) + Gender + Alpha_Omega_Alpha + cut2(USMLE_Step_1_Score, 245:245) + Couples_Match + Medical_Education_or_Training_Interrupted + US_or_Canadian_Applicant + Military_Service_Obligation + Count_of_Oral_Presentation + cut2(Count_of_Peer_Reviewed_Book_Chapter, 0:3) + cut2(Count_of_Poster_Presentation, 0:3) + white_non_white + cut2(Count_of_Peer_Reviewed_Journal_Articles_Abstracts, 0:3) + cut2(Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, 0:3), data = all_data)
s

#dev.off()  #How to save plots as images like PDFs or TIFFs
#tiff("~/Dropbox/Nomogram/nomogram/results/Univariate_Analysis.tiff") 
plot(s, main= "Univariate Analysis", cex.sub = 0.5, cex.axis=0.5, cex.main=0.6, cex.lab=0.6, subtitles = FALSE, xlim=c(0,0.99), xlab = "Chance of Matching into OBGYN Residency")
#dev.off()

# Best Univariate graphs from blog.datascienceheroes.com
# install.packages("funModeling")
funModeling::df_status(all_data)
nrow(all_data)

#Distributions for numerical data
#dev.off()
funModeling::plot_num(all_data, path_out = "~/Dropbox/Nomogram/nomogram/results") #Export results

#Summary stats of the numerical data showing means, medians, skew
funModeling::profiling_num(all_data)

#Shows the variable frequency charted by matching status
#dev.off ()
funModeling::cross_plot(data=all_data, input=(colnames(all_data)), target="Match_Status", path_out = "~/Dropbox/Nomogram/nomogram/results") #, auto_binning = FALSE, #Export results

################################################################
#Look for Missing Data #Page 302 of Harrell book
na.patterns <- Hmisc::naclus(all_data)
na.patterns

################################################################
### Should we use means or medians in table 1?  
#Examination of skewness and kurtosis for numeric values, Zhang book page 65
colnames(all_data)
hist(all_data$Age)  #There is skew in age
hist(all_data$USMLE_Step_1_Score) #No skew with USMLE
moments::agostino.test(all_data$Age) #D'Agostino skewness test is positive for skewness
moments::anscombe.test(all_data$USMLE_Step_1_Score)  #There is kurtosis for the Step 1 score data.  
#Therefore only use medians.  

################################################################
#### Building Table 1: Descriptive Variables by Outcome ####
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
                                      #Misdemeanor_Conviction + 
                                      Alpha_Omega_Alpha + 
                                      #Gold_Humanism_Honor_Society + 
                                      Military_Service_Obligation + 
                                      USMLE_Step_1_Score + 
                                      Military_Service_Obligation + 
                                      Count_of_Poster_Presentation + 
                                      Count_of_Oral_Presentation + 
                                      Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                                      Count_of_Peer_Reviewed_Book_Chapter + 
                                      Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + 
                                      Count_of_Peer_Reviewed_Online_Publication + 
                                      Visa_Sponsorship_Needed +
                                      Medical_Degree,
                                    data=all_data, control = tableby.control(test = TRUE, total = F, digits = 1L, digits.p = 2L, digits.count = 0L, numeric.simplify = F, numeric.stats = c("median", "q1q3"), cat.stats = c("Nmiss","countpct"), stats.labels = list(Nmiss = "N Missing", Nmiss2 ="N Missing", meansd = "Mean (SD)", medianrange = "Median (Range)", median ="Median", medianq1q3 = "Median (Q1, Q3)", q1q3 = "Q1, Q3", iqr = "IQR",range = "Range", countpct = "Count (Pct)", Nevents = "Events", medSurv ="Median Survival", medTime = "Median Follow-Up")))

# labels(table1_all_data)  #labels

#padjust(table1, method = "bonferroni")   #Adjust for Bonferroni for multiple p-values
summary(table1_all_data, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology from 2015 to 2018', pfootnote=TRUE)
# 
# arsenal::write2html(table1_all_data, ("~/Dropbox/Nomogram/nomogram/results/all_data_table1.html"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")   #Write to HTML
# pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.html")

arsenal::write2pdf(table1_all_data, ("~/Dropbox/Nomogram/nomogram/results/all_data_table1.pdf"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")   #Write to PDF
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.pdf")

# arsenal::write2word(table1_all_data, paste0("~/Dropbox/Nomogram/nomogram/results/all_data_table1.doc", title = "Table 1", quiet = FALSE))  #Write to Word 
# pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.doc")
#Need to add a title to the word version

##############################################################################################
###IDentifying NAs and Imputing
nrow(all_data)
ncol(all_data)
sum(is.na(all_data))
all_data <- na.omit(all_data)
sum(is.na(all_data))
str(all_data)
nrow(all_data)

#Plotting NAs in the data, Page 302 of Harrell book
na.patterns <- Hmisc::naclus(all_data)
na.patterns

Hmisc::naplot(na.patterns, 'na per var')  #Graphs the variables with missing data  
#dev.off()

plot(na.patterns) #Cool!! this shows who has the most missing data.  

###################################################################################
#Split the data so that we can run a model and find best factors.  
train <- filter(all_data, Year < 2018)  #Train on years 2015, 2016, 2017
nrow(train)
train <- train %>% select(-"Year")
train
test <- filter(all_data, Year == c(2018)) #Test on 2018 data
nrow(test)
test <- test %>% select(-"Year")
test

# Examine the proportions of the Match_Status class lable across the datasets.
prop.table(table(all_data$Match_Status))       #Original data set proportion 
prop.table(table(train$Match_Status)) #Train data set proportion
prop.table(table(test$Match_Status))  #Test data set proportion
nrow(all_data)

#=================================================================
#  Factor Selection
#=================================================================
#Page 71 of Zhang book
#Step one:  Univariable analysis - "A p-value less than 0.25 and other variables of known clinical relevance can be included for further evaluation."

wilcox.test(all_data$Age~all_data$Match_Status) #SS 
chisq.test(all_data$Match_Status, all_data$white_non_white) #SS
chisq.test(all_data$Match_Status, all_data$Gender) #SS
chisq.test(all_data$Match_Status, all_data$Couples_Match) #SS
t.test(all_data$USMLE_Step_1_Score~all_data$Match_Status) #SS
chisq.test(all_data$Match_Status, all_data$Visa_Sponsorship_Needed) #SS
#chisq.test(all_data$Match_Status, all_data$Med_school_condensed)
chisq.test(all_data$Match_Status, all_data$Medical_Education_or_Training_Interrupted) #SS
chisq.test(all_data$Match_Status, all_data$Alpha_Omega_Alpha) #SS
chisq.test(all_data$Match_Status, all_data$US_or_Canadian_Applicant) #SS
chisq.test(all_data$Match_Status, all_data$Military_Service_Obligation)  #Not significant at all
chisq.test(all_data$Match_Status, all_data$ACLS) #NS
chisq.test(all_data$Match_Status, all_data$Medical_Degree) #SS
chisq.test(all_data$Match_Status, all_data$Year) #SS

#Use Wilcox test as data is assumed not to be normally distributed
wilcox.test(all_data$Count_of_Poster_Presentation ~ all_data$Match_Status) #SS
wilcox.test(all_data$Count_of_Oral_Presentation ~ all_data$Match_Status)  #NS
wilcox.test(all_data$Count_of_Peer_Reviewed_Book_Chapter ~ all_data$Match_Status) #SS
wilcox.test(all_data$Count_of_Peer_Reviewed_Online_Publication ~ all_data$Match_Status) #NS
wilcox.test(all_data$USMLE_Step_1_Score ~ all_data$Match_Status) #SS
wilcox.test(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published ~ all_data$Match_Status) #NS
wilcox.test(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts ~ all_data$Match_Status) #SS

#Factor Selection:  LASSO
#https://rpubs.com/datascientiest/253917, 
#https://campus.datacamp.com/courses/machine-learning-toolbox/tuning-model-parameters-to-improve-performance?ex=10
#http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#log
#https://amunategui.github.io/binary-outcome-modeling/
#https://amunategui.github.io/binary-outcome-modeling/#sourcecode  #Has exact same accent as Falcone
#https://rpubs.com/mbaumer/featureSelection
#http://uc-r.github.io/regularized_regression

################################################
# LASSO model
################################################

# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE)

# na.omit.all_data <- na.omit(all_data)
train$Match_Status <- as.factor(train$Match_Status)
test$Match_Status <- as.factor(test$Match_Status)
names(train)
str(train)

#Levels for glmnet need to be words and not numbers.  Jesus.
levels(train$Match_Status) <- c("NoMatch", "Matched")
levels(test$Match_Status) <- c("NoMatch", "Matched")

# Train glmnet with custom trainControl and tuning: model
lasso.mod <- caret::train(
  Match_Status ~ .,
  data = train,
  family = "binomial",
  tuneGrid = expand.grid(
    alpha = 0:1,
    lambda = seq(0.0001, 1, length = 20)
  ),
  method = "glmnet",
  metric = "ROC",
  trControl = myControl)

# Print model to console
(lasso.mod)
summary(lasso.mod)
lasso.mod[["results"]]
lasso.mod$bestTune #Final model is more of a ridge and less of a LASSO model
best <- lasso.mod$finalModel
coef(best, s=lasso.mod$bestTune$lambda) ###Look for the largest coefficient

#plot results
plot(lasso.mod)  # 0 =1 ridge regression and 1 = LASSO regression, here ridge is better

plot(lasso.mod$finalModel, xvar = 'lambda', label = TRUE)
legend("topright", lwd = 1, col = 1:5, legend = colnames(train), cex = .3)
#https://www.datacamp.com/community/tutorials/tutorial-ridge-lasso-elastic-net

saveRDS(lasso.mod, "best.LASSO.rds")  #save the model

###Making predictions based on the training data
predict(lasso.mod, newx = x[1:5,], type = "prob", s = c(0.05, 0.01))

################################################
# glmnet model
################################################
#https://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection
`%ni%`<-Negate(`%in%`)

# save the outcome for the glmnet model, could use dummyVars with fullRan=FALSE can remove collinearity by removing male.gender so you are either male or female

x <- model.matrix(train$Match_Status~., data=train)
class(x)
x <- x[,-1]  #Removes intercept
x

class(train$Match_Status)
glmnet1<-cv.glmnet(x=x,y=train$Match_Status,type.measure='mse',nfolds=10,alpha=.5, family="binomial")
glmnet1
plot(glmnet1)
#The glmnet1 plot indcates that for high lambda error is very high, and the coefficients are restricted to be too small, and then at some point, it kind of levels off. This indicates that the full model is it is doing a good job.  Also, There’s two vertical lines. The one is at the minimum, and the other vertical line is at one standard error of the minimum, within one standard error. So it’s a slightly more restricted model that does almost as well as the minimum.

c<-coef(glmnet1,s='lambda.min',exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']
variables  ###What variables should be included in the model per LASSO!!!
summary(glmnet1)


################################################
# Use glmnet model on 2018 test data
################################################
#Create predictorsNames variable
outcomeName <- 'Match_Status'
(predictorsNames <- names(test)[names(test) != outcomeName])  #Removes outcome from list of predictrs
class(predictorsNames)

# get predictions on your testing data
b <- model.matrix(test$Match_Status~., data=test) #x <- model.matrix(train$Match_Status~., data=train)
a <- b[,-1]  #Removes intercept from the matrix as we did for model

predictions<-predict(object = glmnet1, newx=a, s="lambda.min", family = "binomial")  #What is the matrix?
predictions

d <- as.data.frame(test[,outcomeName])
levels(d$Match_Status) <- c("NoMatch", "Matched")

test$Match_Status <- as.numeric(test$Match_Status)  #pROC only accepts numeric variables, not a matrix
test$Match_Status <- (test$Match_Status - 1)
predictions <- as.numeric(predictions)

auc <- pROC::roc(test$Match_Status, predictions)
print(auc$auc)

predictions <- as.matrix(predictions)
class(predictions)
class(test)
postResample(pred=predictions, obs=test)  ###NOT WORKING

###########################################
####The Kitchen Sink Model ####  This is essentially a screening model with all variables.  
d <- datadist(train)
options(datadist = "d")

kitchen.sink <- lrm(Match_Status ~ white_non_white +  rcs(Age, 5) + Gender +  Couples_Match + US_or_Canadian_Applicant +  Medical_Education_or_Training_Interrupted + Alpha_Omega_Alpha +  Military_Service_Obligation + rcs(USMLE_Step_1_Score, 4) + rcs(Count_of_Poster_Presentation,3) +  Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + Count_of_Peer_Reviewed_Online_Publication + Visa_Sponsorship_Needed + Medical_Degree, data = train, x = T, y = T)

kitchen.sink
#dev.off()
plot(anova(kitchen.sink), cex=0.5, cex.lab=0.6, cex.axis = 0.7) #According to the ANOVA, USMLE_Step_1_Score, Age, and US_or_Canadian_Applicants are the only statistically significant pieces of the puzzle, and the nonlinear part of the model doesn’t seem to have a real impact.
class(kitchen.sink)
#ggplot(Predict(m.A))

#Step five:  Fast Backwards
rms::fastbw(kitchen.sink, rule = "aic")

##############################################################################################
#Plot Splines

#Age Splines
Hmisc::rcspline.eval(x=all_data$Age, nk=5, type="logistic", inclx = TRUE, knots.only = TRUE, norm = 2, fractied=0.05)  #tells where the knots are located

Hmisc::rcspline.plot(x = all_data$Age, y = all_data$Match_Status, model = "logistic", nk=5, showknots = TRUE, plotcl = TRUE, statloc = 11, main = "Estimated Spline Transformation for Age", xlab = "Age (years)", ylab = "Probability", noprint = TRUE, m = 500) #In the model Age should have rcs(Age, 5)
#Predictions with group size of 500 patients (triangles) and location of knot (arrows).

#USMLE_Step_1_Score Splines
Hmisc::rcspline.eval(x=all_data$USMLE_Step_1_Score, nk=4, type="logistic", inclx = TRUE, knots.only = TRUE, norm = 2, fractied=0.05)  #tells where the knots are located

Hmisc::rcspline.plot(x = all_data$USMLE_Step_1_Score, y = all_data$Match_Status, model = "logistic", nk=5, showknots = TRUE, plotcl = TRUE, statloc = 11, main = "Estimated Spline Transformation for USMLE Step 1 Score", xlab = "USMLE Step 1 Score", ylab = "Probability", noprint = TRUE, m = 500) #In the model USMLE_Step_1 should have rcs(USMLE_Step_1, 6)


#Count of Posters
Hmisc::rcspline.eval(x=all_data$Count_of_Poster_Presentation, nk=5, type="logistic", inclx = TRUE, knots.only = TRUE, norm = 2, fractied=0.05)  #tells where the knots are located

Hmisc::rcspline.plot(x = all_data$Count_of_Poster_Presentation, y = all_data$Match_Status, model = "logistic", nk=5, showknots = TRUE, plotcl = TRUE, statloc = 11, main = "Estimated Spline Transformation for Poster Presentations", xlab = "Count of Poster Presentations", ylab = "Probability", noprint = TRUE, m = 500) #In the model Count of Poster presentations should have rcs(Count of Poster Presentations, 4)


#Count of Oral Presentations
Hmisc::rcspline.eval(x=all_data$Count_of_Oral_Presentation, nk=5, type="logistic", inclx = TRUE, knots.only = TRUE, norm = 2, fractied=0.05)  #tells where the knots are located

Hmisc::rcspline.plot(x = all_data$Count_of_Oral_Presentation, y = all_data$Match_Status, model = "logistic", nk=5, showknots = TRUE, plotcl = TRUE, statloc = 11, main = "Estimated Spline Transformation for Oral Presentations", xlab = "Count of Oral Presentations", ylab = "Probability", noprint = TRUE, m = 1000) #In the model Count of Oral Presentations should have rcs(Count of Oral Presentations, 3)

##############################################################################################
d <- datadist(test)
options(datadist = "d")

paste("These are the variables from LASSO for the nomogram:", variables)

lrm.with.lasso.variables <- lrm(Match_Status ~ rcs(Age, 5) + Alpha_Omega_Alpha + Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Book_Chapter + Couples_Match + Gender + Medical_Degree + Military_Service_Obligation + US_or_Canadian_Applicant +  rcs(USMLE_Step_1_Score, 4) + Visa_Sponsorship_Needed + white_non_white, data = train, x = T, y = T)

lrm.with.lasso.variables
anova(lrm.with.lasso.variables)
#dev.off()

#ANOVA plot after imputation
#dev.off()
plot(anova(lrm.with.lasso.variables))

#Effects plot after imputation
summary(lrm.with.lasso.variables)

#=================================================================
#  Table 2 of Odds Ratios and CIs for Predictors of Matching into OBGYN
#=================================================================
plot(summary(lrm.with.lasso.variables)) #Table 2 of odds ratios in graph form

oddsratios <- as.data.frame(exp(cbind("Odds ratio" = coef(lrm.with.lasso.variables), confint.default(lrm.with.lasso.variables, level = 0.95))))
print(oddsratios)

#Write Table 2 to HTML
arsenal::write2html(oddsratios, ("~/Dropbox/Nomogram/nomogram/results/all_data_oddratios_table2.html"), total=FALSE, title = "Table 2", quiet = FALSE, theme = "yeti")
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_oddratios_table2.html")

#Write to Table 2 word
arsenal::write2word(oddsratios,("~/Dropbox/Nomogram/nomogram/results/all_data_oddsratios_table2.doc"))
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_oddsratios_table2.doc")

#Another way to create odds ratios, page 308 Harrell
#Use Hmisc to plot out odds ratios that are alot clearer than Table 2
dd <- datadist(train); options(datadist='dd')
dd <- datadist
s <- summary(model2)  #########??????????????###################
print(s)
plot(s, log=TRUE)

##############################################################
##  Change Over to Using Test
##############################################################

#First, we need to fit Model 1 in glm, rather than rms to get the AUC.
paste("These are the variables from LASSO for the nomogram:", variables)

test$Match_Status <- as.integer(test$Match_Status+2)

glm.with.lasso.variables  <- glm(Match_Status ~ rcs(Age, 5) + Alpha_Omega_Alpha + Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Book_Chapter + Couples_Match + Gender + Medical_Degree + Military_Service_Obligation + US_or_Canadian_Applicant +  rcs(USMLE_Step_1_Score, 4) + Visa_Sponsorship_Needed + white_non_white,
                   data = test, family = "binomial"(link=logit))  

#ROC Curve type 1 using ggplot with nice control
# requires ROCR package
prob <- predict(glm.with.lasso.variables, data = na.omit(test), type="response")
pred <- prediction(prob, na.omit(test)$Match_Status)
# rest of this doesn't need much adjustment except for titles
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure="auc")
auc <- round(auc@y.values[[1]],3)
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  labs(title = paste0("ROC Curve with area under the curve = ", auc),
       subtitle = "Model A for test data")

#ROC Curve type 2 with nice labels on the x and y
pred <- prediction(prob, test$Match_Status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc  

#ROC Curve Type 3 with nice diagnal line but half of the formula printed
#Page 75 Zhangbook
Deducer::rocplot(glm.with.lasso.variables, diag = TRUE, prob.label.digits = TRUE, AUC = TRUE)


#ROC Curve Type 4, ROC in color, https://rpubs.com/aki2727/cars
perf <- performance(pred, 'tpr','fpr')
plot(perf, colorize = TRUE, text.adj = c(-0.2,1.7), main="Receiver-Operator Curve for Model A")

#Plots of Sensitivity and Specificity
perf1 <- performance(pred, "sens", "spec")
plot(perf1, colorize = TRUE, text.adj = c(-0.2,1.7), main="Sensitivity and Specificity for Model A")

## precision/recall curve (x-axis: recall, y-axis: precision)
perf2 <- performance(pred, "prec", "rec")
plot(perf2, colorize = TRUE, text.adj = c(-0.2,1.7), main="Precision and Recall for Model A")

##################################################################################
#Calibrate Plot for lrm.with.lasso.variables

#dev.off()  #How to save plots as images like PDFs or TIFFs
tiff("~/Dropbox/Nomogram/nomogram/results/calibration curve.tiff") 
calibration.glm.with.lasso.variables <- plot(rms::calibrate(lrm.with.lasso.variables, cmethod=("boot"), B=1000, legend = TRUE, digits = 3, subtitles = T), xlab = "Predicted probability according to model", ylab = "Observation Proportion of Matching") 
#dev.off()


#Plotting the Nomogram for lrm.with.lasso.variables
#######################################################################################
###NOMOGRAM 
#fun.at - Demarcations on the function axis: "Matching into obgyn"
#lp=FALSE so we don't have the logistic progression
d <- datadist(test)
options(datadist = "d")

nomo.from.lrm.with.lasso.variables <- rms::nomogram(lrm.with.lasso.variables, 
         #lp.at = seq(-3,4,by=0.5),
        fun = plogis, 
        fun.at = c(0.001, 0.01, 0.05, seq(0.2, 0.8, by = 0.2), 0.95, 0.99, 0.999), 
        funlabel = "Chance of Matching in OBGYN", 
        lp =FALSE,
        #conf.int = c(0.1,0.7), 
        abbrev = F,
        minlength = 9)
nomogramEx(nomo=nomo.from.lrm.with.lasso.variables ,np=1,digit=2)  #Gives the polynomial formula

#dev.off()  #Run this until null device = 1
nomo_final <- plot(nomo.from.lrm.with.lasso.variables, lplabel="Linear Predictor",
      cex.sub = 0.8, cex.axis=0.8, cex.main=1, cex.lab=1, ps=10, xfrac=.7,
                   #fun.side=c(3,3,1,1,3,1,3,1,1,1,1,1,3),
                   #col.conf=c('red','green'),
                   #conf.space=c(0.1,0.5),
                   label.every=1,
                   col.grid = gray(c(0.8, 0.95)),
                   which="Match_Status")
print(nomo.from.lrm.with.lasso.variables)

#DynNom
model.lrm  <- rms::lrm(Match_Status ~ rcs(Age, 5) + Alpha_Omega_Alpha + Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Book_Chapter + Couples_Match + Gender + Medical_Degree + Military_Service_Obligation + US_or_Canadian_Applicant +  rcs(USMLE_Step_1_Score, 4) + Visa_Sponsorship_Needed + white_non_white,
                   data = all_data, x = TRUE, y= TRUE)  

DynNom::DynNom.lrm(model = model.lrm, data = all_data,  clevel = 0.95)  #Not workings

#Publish to shiny
getwd()
DynNom::DNbuilder(model = model.lrm, data = all_data)

# Check Brier Score, https://rpubs.com/ledongnhatnam/288556
DescTools::BrierScore(glm.with.lasso.variables)  #REQUIRES GLM model

# Calibration
calib <- rms::calibrate(lrm.with.lasso.variables, method = "boot", boot=1000, data = test, rule = "aic", estimates = TRUE)  #Plot test data set
#AUC and calibration matters

plot(calib, legend = TRUE, subtitles = TRUE, cex.subtitles=0.75, xlab = "Predicted probability according to model", ylab = "Observation Proportion of Matching")
calib

###################################################################################
beepr::beep(sound = 4)


sessionInfo()
dev.off()

