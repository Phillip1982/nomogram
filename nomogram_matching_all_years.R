#Muffly, Liss, Alston, Raffaelli, Jelovsek  ###DRAFT###

##################################################################
#Objective:  We sought to construct and validate a model that predict a medical students chances of matching into an obstetrics and gynecology residency.  

#Install and Load packages
if(!require(pacman))install.packages("pacman")
pacman::p_load('caret', 'readxl', 'XML', 'reshape2', 'devtools', 'purrr', 'readr', 'ggplot2', 'dplyr', 'magick', 'janitor', 'lubridate', 'hms', 'tidyr', 'stringr', 'readr', 'openxlsx', 'forcats', 'RcppRoll', 'tibble', 'bit64', 'munsell', 'scales', 'rgdal', 'tidyverse', "foreach", "PASWR", "rms", "pROC", "ROCR", "nnet", "janitor", "packrat", "DynNom", "export", "caTools", "mlbench", "randomForest", "ipred", "xgboost", "Metrics", "RANN", "AppliedPredictiveModeling", "nomogramEx", "shiny", "earth", "fastAdaboost", "Boruta", "glmnet", "ggforce", "tidylog", "InformationValue", "pscl", "scoring", "DescTools", "gbm", "Hmisc", "arsenal", "pander", "moments", "leaps", "MatchIt", "car", "mice", "rpart", "beepr", "fansi", "utf8", "zoom", "lmtest", "ResourceSelection", "Deducer", "rpart")
#.libPaths("/Users/tylermuffly/.exploratory/R/3.5")  # Set libPaths.
#packrat::init(infer.dependencies = TRUE)
packrat_mode(on = TRUE)
set.seed(123456)

##################################################################
#### Set data file locations ####
setwd("~/Dropbox/Nomogram/nomogram")  #Set working directory

################################################################
#Load Data
download.file("https://www.dropbox.com/s/hxkxdmmbd5927j3/all_years_reorder_cols_84.rds?raw=1",destfile=paste0("all_years_mutate_83.rds"), method = "auto", cacheOK = TRUE)
all_data <- read_rds("~/Dropbox/Nomogram/nomogram/data/all_years_reorder_cols_84.rds")  #Bring in years 2015, 2016, 2017, and 2018 data
dplyr::glimpse(all_data)
dim(all_data)
colnames(all_data)
all_data$Match_Status_Dichot

################################################################
# Place nicer labels for the data
Hmisc::label(all_data$BLS) <- "Certification in Basic Life Support"
Hmisc::label(all_data$Positions_offered) <- "OBGYN Intern Positions"
Hmisc::label(all_data$Medical_Degree) <- "Medical Degree"
Hmisc::label(all_data$Visa_Sponsorship_Needed) <- "Visa Sponsorship Needed"
Hmisc::label(all_data$Sigma_Sigma_Phi) <- "Sigma Sigma Phi"
Hmisc::label(all_data$PALS) <- "Certification in Pediatric Life Support"
Hmisc::label(all_data$Age)    <- 'Age'
units(all_data$Age) <- 'years'
Hmisc::label(all_data$Alpha_Omega_Alpha) <- 'AOA Member'
Hmisc::label(all_data$USMLE_Step_1_Score) <- 'USMLE Step 1 Score'
Hmisc::label(all_data$Gender) <- 'Gender'
Hmisc::label(all_data$Couples_Match) <- 'Couples Matching'
#Hmisc::label(all_data$Medical_School_Type) <- 'Medical School Type'
Hmisc::label(all_data$Malpractice_Cases_Pending) <- "Malpractice Cases Pending"
Hmisc::label(all_data$ACLS) <- "Advanced Cardiac Life Support"
Hmisc::label(all_data$Medical_Education_or_Training_Interrupted) <- 'Medical School Interrupted'
Hmisc::label(all_data$Misdemeanor_Conviction) <- 'Misdemeanor Conviction'
Hmisc::label(all_data$US_or_Canadian_Applicant) <- 'US or Canadian Applicant'
Hmisc::label(all_data$Gold_Humanism_Honor_Society) <- 'Gold Humanism Honors Society'
Hmisc::label(all_data$Military_Service_Obligation) <- 'Military Service Obligation'
Hmisc::label(all_data$Count_of_Oral_Presentation) <- 'Count of Oral Presentations'
Hmisc::label(all_data$Count_of_Peer_Reviewed_Book_Chapter) <- 'Count of Peer-Reviewed Book Chapters'
Hmisc::label(all_data$Count_of_Poster_Presentation) <- 'Count of Poster Presentations'
Hmisc::label(all_data$white_non_white) <- 'Race' 
Hmisc::label(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts) <- 'Count of Peer-Reviewed Journal Articles'
Hmisc::label(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published) <-'Count of Peer-Reviewed Research Not Published'
Hmisc::label(all_data$Match_Status_Dichot) <- 'Matching Status'
Hmisc::label(all_data$Match_Status) <- 'Matching Status'
Hmisc::label(all_data) #Check labels for the data set
all_data$Match_Status_Dichot


############################################################################################
####Univariate using the Hmisc::describe function
Hmisc::describe(all_data) # A little better than summary.  Gives proportions for categorical variables. Amen!

####
#Look at the data in one graph.  Nice.  Page 292 in Harrell's book
dev.off()
par("mar")
par(mar=c(1,1,1,1))

colnames(all_data)
all_data$Match_Status_Dichot <- as.numeric(all_data$Match_Status_Dichot)
all_data$Match_Status_Dichot
all_data$Match_Status_Dichot <- (all_data$Match_Status_Dichot - 1)
all_data$Match_Status_Dichot  #Outcome must be numeric
v <- colnames(all_data)
t3 <- all_data[,v]


############################################################################################
####Univariate using the Hmisc::summary graph of data
dd <- rms::datadist(t3)
options(datadist='dd')

s <- summary(Match_Status_Dichot ~ cut2(Age, 30:30) + Gender + Alpha_Omega_Alpha + cut2(USMLE_Step_1_Score, 245:245) + Couples_Match + Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Military_Service_Obligation + Count_of_Oral_Presentation + cut2(Count_of_Peer_Reviewed_Book_Chapter, 0:3) + cut2(Count_of_Poster_Presentation, 0:3) + white_non_white + cut2(Count_of_Peer_Reviewed_Journal_Articles_Abstracts, 0:3) + cut2(Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, 0:3), data = t3)

dev.off()  #How to save plots as images like PDFs or TIFFs
tiff("~/Dropbox/Nomogram/nomogram/results/Univariate_Analysis.tiff") 
plot(s, main= "Univariate Analysis", cex.sub = 0.5, cex.axis=0.5, cex.main=0.6, cex.lab=0.6, subtitles = FALSE, xlab = "Chance of Matching into OBGYN Residency")
#zoom::zm()
dev.off()


# Best Univariate graphs from blog.datascienceheroes.com
# install.packages("funModeling")
library(funModeling)
funModeling::df_status(all_data)
nrow(all_data)

#Distributions for nominal variables  ##CROSSPLOTS IS BETTER
#dev.off()
#funModeling::freq(all_data)
#funModeling::freq(all_data, path_out = "~/Dropbox/Nomogram/nomogram/results") #Export results

#Distributions for numerical data
dev.off()
funModeling::plot_num(all_data, path_out = "~/Dropbox/Nomogram/nomogram/results") #Export results

#Summary stats of the numerical data showing means, medians, skew
funModeling::profiling_num(all_data)

#Shows the variable frequency charted by matching status
dev.off ()
a <- colnames(all_data)
funModeling::cross_plot(data=all_data, input=a, target="Match_Status", path_out = "~/Dropbox/Nomogram/nomogram/results") #, auto_binning = FALSE, #Export results


################################################################
#Look for Missing Data #Page 302 of Harrell book
na.patterns <- Hmisc::naclus(all_data)
na.patterns

who.na <- rpart::rpart(is.na(Gold_Humanism_Honor_Society) ~ Match_Status + Medical_Education_or_Training_Interrupted + USMLE_Step_1_Score + white_non_white + US_or_Canadian_Applicant, data = all_data, minbucket = 15)

#Plots the Fraction of NAs in each Variable.  COOL!
naplot(na.patterns, 'na per var')

#Breakdown of missing data by a variable
#dev.off()
plot(who.na, margin = 0.1); test(who.na)
plot(na.patterns) 
dev.off()

m <- lrm(is.na(Gold_Humanism_Honor_Society) ~ Match_Status + Medical_Education_or_Training_Interrupted + USMLE_Step_1_Score + white_non_white + US_or_Canadian_Applicant, data=all_data) #Wald statistics for is.na)Gold_Humanism. Shows that students not matching are not more likely to have Gold_Humanism.  The higher the step 1 score the less likely that Gold_Humanism to be missing.  
anova(m)

################################################################
### Should we use means or medians in table 1?  
#Examination of skewness and kurtosis for numeric values, Zhang book page 65

colnames(all_data)
par(mfrow=c(1,2))
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
                                      Visa_Sponsorship_Needed +
                                      #OBGYN_Grade +
                                      Medical_Degree,
                                    
                                    data=all_data, control = tableby.control(test = TRUE, total = F, digits = 1L, digits.p = 2L, digits.count = 0L, numeric.simplify = F, numeric.stats = c("median", "q1q3"), cat.stats = c("Nmiss","countpct"), stats.labels = list(Nmiss = "N Missing", Nmiss2 ="N Missing", meansd = "Mean (SD)", medianrange = "Median (Range)", median ="Median", medianq1q3 = "Median (Q1, Q3)", q1q3 = "Q1, Q3", iqr = "IQR",range = "Range", countpct = "Count (Pct)", Nevents = "Events", medSurv ="Median Survival", medTime = "Median Follow-Up")))

labels(table1_all_data)  #labels

#padjust(table1, method = "bonferroni")   #Adjust for Bonferroni for multiple p-values
summary(table1_all_data, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology from 2015 to 2018', pfootnote=TRUE)

arsenal::write2html(table1_all_data, ("~/Dropbox/Nomogram/nomogram/results/all_data_table1.html"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")   #Write to HTML
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.html")

#arsenal::write2pdf(table1_all_data, ("~/Dropbox/Nomogram/nomogram/results/all_data_table1.pdf"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")   #Write to PDF
#pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.pdf")

arsenal::write2word(table1_all_data, paste0("~/Dropbox/Nomogram/nomogram/results/all_data_table1.doc", title = "Table 1", quiet = FALSE))  #Write to Word 
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.doc")
#Need to add a title to the word version

##############################################################################################
#Plot Splines


##############################################################################################
###IDentifying NAs and Imputing
nrow(all_data)
ncol(all_data)
sum(is.na(all_data))
#all_data <- na.omit(all_data)
#sum(is.na(all_data))
str(all_data)
nrow(all_data)

#Plotting NAs in the data, Page 302 of Harrell book
na.patterns <- Hmisc::naclus(all_data)
na.patterns

who.na <- rpart::rpart(is.na(Gold_Humanism_Honor_Society) ~ Match_Status + Medical_Education_or_Training_Interrupted + USMLE_Step_1_Score + white_non_white + US_or_Canadian_Applicant, data = all_data, minbucket = 15)

Hmisc::naplot(na.patterns, 'na per var')  #Graphs the variables with missing data  
dev.off()

plot(who.na, margin = 0.1); test(who.na)
plot(na.patterns) #Cool!! this shows who has the most missing data.  

m <- lrm(is.na(Gold_Humanism_Honor_Society) ~ Match_Status + Medical_Education_or_Training_Interrupted + USMLE_Step_1_Score + white_non_white + US_or_Canadian_Applicant, data=all_data) #Wald statistics for is.na)Gold_Humanism. Shows that students not matching are not more likely to have Gold_Humanism.  The higher the step 1 score the less likely that Gold_Humanism to be missing.  
anova(m)


##############################################################################################
#Split the data so that we can run a model and find best factors.  
train <- filter(all_data, Year < 2018)  #Train on years 2015, 2016, 2017
nrow(train)
test <- filter(all_data, Year == c(2018)) #Test on 2018 data
nrow(test)

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

#Step two:  Variable Clustering, page 166 Harrel book, Heirarchical clustering
vc <- Hmisc::varclus (~white_non_white+  Age+ Gender +  Couples_Match + US_or_Canadian_Applicant +  Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + Alpha_Omega_Alpha + Gold_Humanism_Honor_Society +  Military_Service_Obligation + USMLE_Step_1_Score + Count_of_Poster_Presentation +  Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + Count_of_Peer_Reviewed_Online_Publication + Visa_Sponsorship_Needed + Medical_Degree, sim = 'hoeffding', data=train)  #Variables that are on the same branch are closely related

plot(vc)

#Step three:  Principal Components Analysis
train_pca <- caret::preProcess(dplyr::select(train, - Match_Status), 
                               method = c("center", "scale", "nzv", "pca"))
train_pca
train_pca$method
train_pca$rotation

# Results of PCA, factors selected:
# USMLE_Step_1_Score                                                     -0.20
# Count_of_Oral_Presentation                                              0.43
# Count_of_Other_Articles                                                 0.11
# Count_of_Peer_Reviewed_Journal_Articles_Abstracts                       0.53
# Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published  0.25
# Count_of_Peer_Reviewed_Online_Publication                               0.18
# Count_of_Poster_Presentation                                            0.50
# Age                                                                     0.32


#Step four: Redundancy Analysis
redun <- Hmisc::redun(~ white_non_white + 
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
                        Visa_Sponsorship_Needed +
                        #OBGYN_Grade +
                        Medical_Degree, data = train, type="adjusted", r2 = 0.3, nk = 5, pr=TRUE, digits = 3, allcat = TRUE)    #only predictors
print(redun, digits=3, long=TRUE)
# 
# Rendundant variables:
#   
#   US_or_Canadian_Applicant Count_of_Poster_Presentation Count_of_Peer_Reviewed_Journal_Articles_Abstracts
# 
# Predicted from variables:
#   
#   white_non_white Age Gender Couples_Match Medical_Education_or_Training_Interrupted Misdemeanor_Conviction Alpha_Omega_Alpha Gold_Humanism_Honor_Society Military_Service_Obligation USMLE_Step_1_Score Count_of_Oral_Presentation Count_of_Peer_Reviewed_Book_Chapter Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published Count_of_Peer_Reviewed_Online_Publication Visa_Sponsorship_Needed Medical_Degree 
# 
# Variable Deleted  R^2 R^2 after later deletions
# 1                          US_or_Canadian_Applicant 0.55               0.526 0.514
# 2                      Count_of_Poster_Presentation 0.54                     0.434
# 3 Count_of_Peer_Reviewed_Journal_Articles_Abstracts 0.36      

#Step five:  Dr. Love's Spearman.  https://rpubs.com/TELOVE/project1-demo1_2019-432
#A Spearman ρ2plot suggests that US or Canadian applicant is important, but it’s not clear that Count_of_oral_presentations will be particularly useful. In this example, note that we fit this plot without accounting for the missing values of any of these predictors, so that may have some effect.
dev.off()  
tiff("~/Dropbox/Nomogram/nomogram/results/Spearman.tiff", width=400, height = 350, res = 50) 
#https://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html
plot(spearman2(Match_Status ~ white_non_white+  Age+ Gender +  Couples_Match + US_or_Canadian_Applicant +  Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + Alpha_Omega_Alpha + Gold_Humanism_Honor_Society +  Military_Service_Obligation + USMLE_Step_1_Score + Count_of_Poster_Presentation +  Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + Count_of_Peer_Reviewed_Online_Publication + Visa_Sponsorship_Needed + Medical_Degree, 
               data = train))
#dev.off()

####Model A - The Kitchen Sink Model ####  This is essentially a screening model.  
d <- datadist(train)
options(datadist = "d")

m.A <- lrm(Match_Status ~ white_non_white +  rcs(Age, 5) + Gender +  Couples_Match + US_or_Canadian_Applicant +  Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + Alpha_Omega_Alpha + Gold_Humanism_Honor_Society +  Military_Service_Obligation + rcs(USMLE_Step_1_Score, 5) + rcs(Count_of_Poster_Presentation,3) +  Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + Count_of_Peer_Reviewed_Online_Publication + Visa_Sponsorship_Needed + Medical_Degree, data = train, x = T, y = T)

m.A
anova(m.A)
dev.off()
plot(anova(m.A)) #According to the ANOVA, USMLE_Step_1_Score, Age, and US_or_Canadian_Applicants are the only statistically significant pieces of the puzzle, and the nonlinear part of the model doesn’t seem to have a real impact.

class(m.A)
#ggplot(Predict(m.A))


##Imputation.  
f.A <- aregImpute( ~ as.factor(white_non_white) +  as.numeric(Age) + as.factor(Gender) +  as.factor(Couples_Match) + as.factor(US_or_Canadian_Applicant) +  as.factor(Medical_Education_or_Training_Interrupted) + as.factor(Misdemeanor_Conviction) + as.factor(Alpha_Omega_Alpha) + as.factor(Gold_Humanism_Honor_Society) +  as.factor(Military_Service_Obligation) + as.numeric(USMLE_Step_1_Score) + as.numeric(Count_of_Poster_Presentation) + as.numeric(Count_of_Oral_Presentation) + as.numeric(Count_of_Peer_Reviewed_Journal_Articles_Abstracts) + as.numeric(Count_of_Peer_Reviewed_Book_Chapter) + as.numeric(Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published) + as.numeric(Count_of_Peer_Reviewed_Online_Publication) + as.factor(Visa_Sponsorship_Needed) + as.factor(Medical_Degree), 
                  data = train, 
                  n.impute = 100, nk = 0, #nk=0 so all factors are linear, otherwise would not work
                  pr = TRUE, x = TRUE)
f.A

#Fitting model after imputation
fmi.m.A <- fit.mult.impute(Match_Status ~ white_non_white +  rcs(Age, 5) + Gender +  Couples_Match + US_or_Canadian_Applicant +  Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + Alpha_Omega_Alpha + Gold_Humanism_Honor_Society +  Military_Service_Obligation + rcs(USMLE_Step_1_Score, 5) + rcs(Count_of_Poster_Presentation,3) +  Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + Count_of_Peer_Reviewed_Online_Publication + Visa_Sponsorship_Needed + Medical_Degree, data = train, lrm, f.A)

fmi.m.A

#ANOVA plot after imputation
dev.off()
plot(anova(fmi.m.A))

#Effects plot after imputation
summary(fmi.m.A)

plot(summary(fmi.m.A)) #Table 2 in graph form

#Nomogram after imputation
#plot(nomogram(fmi.m.A)) #must feed the nomogram a lrm model to get it to work.  


##############################################################

#First, we need to fit Model 1 in glm, rather than rms.
modelA.glm  <- glm(Match_Status ~     #Removed predictors suggested by the redundancy analysis
                     white_non_white + 
                     Age + 
                     Gender + 
                     Couples_Match + 
                     #US_or_Canadian_Applicant + 
                     #Medical_School_Type + 
                     Medical_Education_or_Training_Interrupted + 
                     #Misdemeanor_Conviction + 
                     Alpha_Omega_Alpha + 
                     Gold_Humanism_Honor_Society + 
                     Military_Service_Obligation + 
                     USMLE_Step_1_Score + 
                     Military_Service_Obligation + 
                     #Count_of_Poster_Presentation + 
                     Count_of_Oral_Presentation + 
                     #Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                     Count_of_Peer_Reviewed_Book_Chapter + 
                     Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + 
                     Count_of_Peer_Reviewed_Online_Publication + 
                     Visa_Sponsorship_Needed +
                     #OBGYN_Grade +
                     Medical_Degree,
                   data = test, family = "binomial"(link=logit))  

#ROC Curve type 1 using ggplot with nice control
# requires ROCR package
prob <- predict(modelA.glm, data = na.omit(test), type="response")
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
Deducer::rocplot(modelA.glm, diag = TRUE, prob.label.digits = TRUE, AUC = TRUE)


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
#Calibrate Plot for Model A
dev.off() 
calibration.Model.A <- plot(rms::calibrate(m.A, cmethod=("boot"), B=1000, legend = TRUE, digits = 3, subtitles = T))  # The model overpredicts a little at higher values

dev.off()  #How to save plots as images like PDFs or TIFFs
tiff("~/Dropbox/Nomogram/nomogram/results/calibration curve.tiff") 
calibration.Model.A <- plot(rms::calibrate(m.A, cmethod=("boot"), B=1000, legend = TRUE, digits = 3, subtitles = T)) 
#zoom::zm()
dev.off()



#Plotting the Nomogram for fmi.m.A
#######################################################################################
###NOMOGRAM 
#fun.at - Demarcations on the function axis: "Matching into obgyn"
#lp=FALSE so we don't have the logistic progression

nomo_from_fmi.m.A <- rms::nomogram(fmi.m.A, 
         #lp.at = seq(-3,4,by=0.5),
        fun = plogis, 
        fun.at = c(0.001, 0.01, 0.05, seq(0.2, 0.8, by = 0.2), 0.95, 0.99, 0.999), 
        funlabel = "Chance of Matching in OBGYN, 2019", 
        lp =FALSE,
        #conf.int = c(0.1,0.7), 
        abbrev = F,
        minlength = 9)
nomogramEx(nomo=nomo_from_fmi.m.A ,np=1,digit=2)  #Gives the polynomial formula

#dev.off()  #Run this until null device = 1
nomo_final <- plot(nomo_from_fmi.m.A , lplabel="Linear Predictor",
      cex.sub = 0.8, cex.axis=0.8, cex.main=1, cex.lab=1, ps=10, xfrac=.7,
                   #fun.side=c(3,3,1,1,3,1,3,1,1,1,1,1,3),
                   #col.conf=c('red','green'),
                   #conf.space=c(0.1,0.5),
                   label.every=1,
                   col.grid = gray(c(0.8, 0.95)),
                   which="Match_Status")
print(nomo_from_fmi.m.A)

# Check Brier Score
DescTools::BrierScore(nomo_from_fmi.m.A)

# Calibration
calib <- rms::calibrate(fmi.m.A, boot=1000, data = test)  #Plot test data set
#AUC and calibration matters

plot(calib)
calib

##################################################################################
#



#=================================================================
#  Creation of a Model Formula with the Training Data
#=================================================================
model1  <- rms::lrm(Match_Status ~ 
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
                                         Visa_Sponsorship_Needed +
                                         #OBGYN_Grade +
                                         Medical_Degree,
                                       data = train, x=TRUE, y=TRUE)

print(model1)  #Check the C-statistic which is the same as ROC area for binary logistic regression
anova(model1) #Harrell book page 298, The Wald Anova indicates especially strong age, US or Canadian applicants, USMLE score effects.  
validate(model1, B=200) #Not working

model2  <- rms::lrm(Match_Status ~     #Removed predictors suggested by the redundancy analysis
                      white_non_white + 
                      Age + 
                      Gender + 
                      Couples_Match + 
                      #US_or_Canadian_Applicant + 
                      #Medical_School_Type + 
                      Medical_Education_or_Training_Interrupted + 
                      Misdemeanor_Conviction + 
                      Alpha_Omega_Alpha + 
                      Gold_Humanism_Honor_Society + 
                      Military_Service_Obligation + 
                      USMLE_Step_1_Score + 
                      Military_Service_Obligation + 
                      #Count_of_Poster_Presentation + 
                      Count_of_Oral_Presentation + 
                      #Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                      Count_of_Peer_Reviewed_Book_Chapter + 
                      Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + 
                      Count_of_Peer_Reviewed_Online_Publication + 
                      Visa_Sponsorship_Needed +
                      #OBGYN_Grade +
                      Medical_Degree,
                    data = train, x=TRUE, y=TRUE)
model2


model2.glm  <- glm(Match_Status ~     #Removed predictors suggested by the redundancy analysis
                      white_non_white + 
                      Age + 
                      Gender + 
                      Couples_Match + 
                      #US_or_Canadian_Applicant + 
                      #Medical_School_Type + 
                      Medical_Education_or_Training_Interrupted + 
                      #Misdemeanor_Conviction + 
                      Alpha_Omega_Alpha + 
                      Gold_Humanism_Honor_Society + 
                      Military_Service_Obligation + 
                      USMLE_Step_1_Score + 
                      Military_Service_Obligation + 
                      #Count_of_Poster_Presentation + 
                      Count_of_Oral_Presentation + 
                      #Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                      Count_of_Peer_Reviewed_Book_Chapter + 
                      Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + 
                      Count_of_Peer_Reviewed_Online_Publication + 
                      Visa_Sponsorship_Needed +
                      #OBGYN_Grade +
                      Medical_Degree,
                      data = test, family = "binomial")  

#Compare the two models using a likelihood ration test
lmtest::lrtest(model1, model2)  #P-value is not significant so the two models are comparable. 


###Created a model so that when you are in 2nd year of medical school there are some modifiable things you can do.  
modifiable.model  <- rms::lrm(Match_Status ~     #Removed predictors suggested by the redundancy analysis
                      #white_non_white + 
                      #Age + 
                      #Gender + 
                      #Couples_Match + 
                      #US_or_Canadian_Applicant + 
                      #Medical_School_Type + 
                      #Medical_Education_or_Training_Interrupted + 
                      Misdemeanor_Conviction + 
                      Alpha_Omega_Alpha + 
                      Gold_Humanism_Honor_Society + 
                      #Military_Service_Obligation + 
                      USMLE_Step_1_Score + 
                      Military_Service_Obligation + 
                      #Count_of_Poster_Presentation + 
                      Count_of_Oral_Presentation + 
                      #Count_of_Peer_Reviewed_Journal_Articles_Abstracts + 
                      Count_of_Peer_Reviewed_Book_Chapter + 
                      Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published + 
                      Count_of_Peer_Reviewed_Online_Publication + 
                      Misdemeanor_Conviction  + 
                      Visa_Sponsorship_Needed, #+
                      #OBGYN_Grade +
                      #Medical_Degree,
                    data = train, x=TRUE, y=TRUE)
modifiable.model #C-statistic is 0.79

lmtest::lrtest(model2, modifiable.model)  #P-value is significant so the two models are NOT comparable. 

#=================================================================
#Look for Co-linearity with Variance Inflation Factors
#=================================================================
rms::vif(model2) #Should be <4


#Step five: assessing fit of the model, page 75 in Zhang Book
# Compute AUC for predicting Match_Status_Dichot with the model
prob <- predict(model2.glm, newdata=test, type="response",progress="window")  #Must use GLM model
str(prob)


pred <- prediction(prob, test$Match_Status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc  

#Page 75 Zhangbook
Deducer::rocplot(model2.glm, diag = TRUE, prob.label.digits = TRUE, AUC = TRUE)

#ROC Curve in color, https://rpubs.com/aki2727/cars
perf <- performance(pred, 'tpr','fpr')
plot(perf, colorize = TRUE, text.adj = c(-0.2,1.7), main="Receiver-Operator Curve")

perf1 <- performance(pred, "sens", "spec")
plot(perf1)

## precision/recall curve (x-axis: recall, y-axis: precision)
perf2 <- performance(pred, "prec", "rec")
plot(perf2)

#=================================================================
#  Table 2 of Odds Ratios and CIs for Predictors of Matching into OBGYN
#=================================================================
oddsratios <- as.data.frame(exp(cbind("Odds ratio" = coef(model2), confint.default(model2, level = 0.95))))
print(oddsratios)

#Write Table 2 to HTML
arsenal::write2html(oddsratios, ("~/Dropbox/Nomogram/nomogram/results/all_data_oddratios_table2.html"), total=FALSE, title = "Table 2", quiet = FALSE, theme = "yeti")
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_oddratios_table2.html")

#Write to Table 2 word
arsenal::write2word(oddsratios, ("~/Dropbox/Nomogram/nomogram/results/all_data_oddsratios_table2.doc"))

#Another way to create odds ratios, page 308 Harrell
#Use Hmisc to plot out odds ratios that are alot clearer than Table 2
dd <- datadist(train); options(datadist='dd')
dd <- datadist
s <- summary(model2)
print(s)
plot(s, log=TRUE)



###################################################################################
beepr::beep(sound = 4)

#To do's:
#1)  Plot numerical variables showing splines.  Like Jelovsek page 13 transfusion paper
#2)  Decision analysis
#3)  Add CI to concordance index.  
#4)  How do you know when there are non-linear assumptions in the data?  When to add splines?



#https://rpubs.com/kaz_yos/epi288-validation3
#https://rpubs.com/aki2727/cars

sessionInfo()
dev.off()
