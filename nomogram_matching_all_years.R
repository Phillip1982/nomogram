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
dplyr::glimpse(all_data)
dim(all_data)
colnames(all_data)
all_data$Match_Status_Dichot

################################################################
# Place nicer labels for the data
#label(data$Self_Identify)    <- 'Race/Ethnicity'
labels(all_data$Age)    <- 'Age'
units(all_data$Age) <- 'years'
label(all_data$Alpha_Omega_Alpha) <- 'AOA Member'
labels(all_data$USMLE_Step_1_Score) <- 'USMLE Step 1 Score'
labels(all_data$Gender) <- 'Gender'
labels(all_data$Couples_Match) <- 'Couples Matching'
labels(all_data$Visa_Status_Expected) <- 'Expected Visa Status'
labels(all_data$Medical_School_Type) <- 'Medical School Type'
labels(all_data$Medical_Education_or_Training_Interrupted) <- 'Medical School Interrupted'
labels(all_data$Misdemeanor_Conviction) <- 'Misdemeanor Conviction'
#label(all_data$USMLE_Step_2_CK_Score) <- 'USMLE Step 2 CK Score'
#label(all_data$USMLE_Step_2_CS_Score) <- 'USMLE Step 2 CS Score'
#label(all_data$USMLE_Step_3_Score) <- 'USMLE Step 3 Score'
labels(all_data$US_or_Canadian_Applicant) <- 'US or Canadian Applicant'
labels(all_data$Gold_Humanism_Honor_Society) <- 'Gold Humanism Honors Society'
labels(all_data$Military_Service_Obligation) <- 'Military Service Obligation'
labels(all_data$Count_of_Oral_Presentation) <- 'Count of Oral Presentations'
labels(all_data$Count_of_Peer_Reviewed_Book_Chapter) <- 'Count of Peer-Reviewed Book Chapters'
labels(all_data$Count_of_Poster_Presentation) <- 'Count of Poster Presentations'
#labels(all_data$Other_Service_Obligation) <- 'Other Service Obligation'
#label(all_data$Med_school_condensed) <- 'Medical School Condensed' 
labels(all_data$white_non_white) <- 'Race' 
labels(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts) <- 'Count of Peer-Reviewed Journal Articles'
labels(all_data$Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published) <-'Count of Peer-Reviewed Research Not Published'
labels(all_data$Match_Status_Dichot) <- 'Matching Status'
labels(all_data$Match_Status) <- 'Matching Status'
labels(all_data) #Check labels for the data set
all_data$Match_Status_Dichot
####

############################################################################################
####Univariate using the Hmisc::describe function
Hmisc::describe(all_data) # A little better than summary.  Gives proportions for categorical variables. Amen!

####
#Look at the data in one graph.  Nice.  Page 292 in Harrell's book
dev.off()
par("mar")
par(mar=c(1,1,1,1))

all_data$Match_Status_Dichot <- as.numeric(all_data$Match_Status_Dichot)
all_data$Match_Status_Dichot <- (all_data$Match_Status_Dichot - 1)
all_data$Match_Status_Dichot  #Outcome must be numeric
v <- c('Match_Status_Dichot', 'Age', 'Gender', 'Alpha_Omega_Alpha','USMLE_Step_1_Score', 'Couples_Match', 'Medical_Education_or_Training_Interrupted', 'Misdemeanor_Conviction', 'US_or_Canadian_Applicant', 'Gold_Humanism_Honor_Society',  'Military_Service_Obligation', 'Count_of_Oral_Presentation', 'Count_of_Peer_Reviewed_Book_Chapter', 'Count_of_Poster_Presentation', 'white_non_white','Count_of_Peer_Reviewed_Journal_Articles_Abstracts', 'Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published')
t3 <- all_data[,v]


############################################################################################
####Univariate using the Hmisc::summary graph of data
#install.packages("zoom")
library(zoom)
dd <- rms::datadist(t3)
options(datadist='dd')
s <- summary(Match_Status_Dichot ~ cut2(Age, 30:30) + Gender + Alpha_Omega_Alpha + cut2(USMLE_Step_1_Score, 245:245) + Couples_Match + Medical_Education_or_Training_Interrupted + Misdemeanor_Conviction + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Military_Service_Obligation + Count_of_Oral_Presentation + cut2(Count_of_Peer_Reviewed_Book_Chapter, 0:3) + cut2(Count_of_Poster_Presentation, 0:3) + white_non_white + cut2(Count_of_Peer_Reviewed_Journal_Articles_Abstracts, 0:3) + cut2(Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, 0:3), data = t3)
dev.off()  #How to save plots as images like PDFs or TIFFs
tiff("~/Dropbox/Nomogram/nomogram/results/Univariate_Analysis.tiff") 
plot(s, main= "Univariate Analysis", cex.sub = 0.5, cex.axis=0.5, cex.main=0.6, cex.lab=0.6, subtitles = FALSE, xlab = "Chance of Matching into OBGYN Residency")
zoom::zm()
dev.off()



# Best Univariate graphs from blog.datascienceheroes.com
# install.packages("funModeling")
library(funModeling)
funModeling::df_status(all_data)
nrow(all_data)

#Distributions for nominal variables
dev.off()
funModeling::freq(all_data)
funModeling::freq(all_data, path_out = "~/Dropbox/Nomogram/nomogram/results") #Export results

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
#Look for Missing Data
#Page 302 of Harrell book
na.patterns <- Hmisc::naclus(all_data)
na.patterns
require(rpart)

who.na <- rpart::rpart(is.na(Gold_Humanism_Honor_Society) ~ Match_Status + Medical_Education_or_Training_Interrupted + USMLE_Step_1_Score + white_non_white + US_or_Canadian_Applicant, data = all_data, minbucket = 15)

#Plots the Fraction of NAs in each Variable.  COOL!
naplot(na.patterns, 'na per var')
dev.off()

#Breakdown of missing data by a variable
plot(who.na, margin = 0.1); test(who.na)
plot(na.patterns) 

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

padjust(table1, method = "bonferroni")   #Adjust for Bonferroni for multiple p-values
summary(table1_all_data, text=T, title='Table 1:  Demographics of Applicants to Obstetrics and Gynecology from 2015 to 2018', pfootnote=TRUE)

arsenal::write2html(table1_all_data, ("~/Dropbox/Nomogram/nomogram/results/all_data_table1.html"), total=FALSE, title = "Table 1", quiet = FALSE, theme = "yeti")   #Write to HTML
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.html")

arsenal::write2word(table1_all_data, paste0("~/Dropbox/Nomogram/nomogram/results/all_data_table1.doc", title = "Table 1", quiet = FALSE))  #Write to Word 
pander::openFileInOS("~/Dropbox/Nomogram/nomogram/results/all_data_table1.doc")
#Need to add a title to the word version





#######################################################################################
#  Bivariate Analysis.  Explore data for signficance.  
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




##############################################################################################
###IDentifying NAs and Imputing

nrow(all_data)
ncol(all_data)
sum(is.na(all_data))
all_data <- na.omit(all_data)
sum(is.na(all_data))
str(all_data)
nrow(all_data)

