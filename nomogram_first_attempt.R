#https://lengyueyang.github.io/Research/Nomogram-rms.html

#Install and Load packages
#rm(list=ls())
if(!require(pacman))install.packages("pacman")
pacman::p_load('Hmisc', 'readxl', 'XML', 'reshape2', 'devtools', 'plyr', 'packrat', 'highcharter', 'purrr', 'readr', 'htmlwidgets', 'RColorBrewer', 'leaflet', 'rgdal', 'dygraphs', 'quantmod', 'DT', 'formattable', 'ggplot2',  'idbr', 'genderizeR', 'animation', 'dplyr', 'magick', 'tidycensus', 'ggthemes', 'stringr', 'geosphere', 'ggmap', 'grid', 'gmapsdistance', 'zipcode', 'janitor', 'lubridate', 'hms', 'tidyr', 'stringr', 'readr', 'openxlsx', 'forcats', 'RcppRoll', 'tibble', 'bit64', 'munsell', 'scales', 'leaflet', 'rgdal', 'htmltools', 'mapview', 'htmlwidgets', 'sf', 'sp', 'tidyverse', 'viridis', 'fansi', 'webshot', 'geosphere', 'zipcode', 'leaflet.extras', 'raster',  'spData','spDataLarge', 'stplanr', 'tmap', 'osmdata', 'arsenal', 'doMC', "wesanderson", "fasterize", "USAboundaries", "RANN", "tidycensus", "geofacet", "extrafont", "shiny", "ParallelLogger", "parallel", "RSelenium", "humaniformat", "visdat", "skimr", "assertr", "tidylog", "doParallel", "DiagrammeR", "DiagrammeRsvg", "rsvg", "iterators", "parallel", "foreach", "PASWR", "rms", "pROC", "nnet", "janitor", "packrat", "DynNom", "rsconnect")
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
 
 ################################################################
 #Place nicer labels for the data
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
 #Run stats to see what is <0.1 and should be included into model
 TAB <- table (data$Match_Status, data$Gender)
 barplot(TAB, beside= T, legend=T)
 CHI <- chisq.test(TAB, correct=T)
 attributes(CHI)
 CHI$p.value
 fisher.test(TAB, conf.int=T, conf.level=0.99)
 
 #chisq.test(data$Match_Status, data$Self_Identify)
 chisq.test(data$Match_Status, data$Gender)
 chisq.test(data$Match_Status, data$Couples_Match)
 chisq.test(data$Match_Status, data$Expected_Visa_Status_Dichotomized)
 chisq.test(data$Match_Status, data$Med_school_condensed)
 chisq.test(data$Match_Status, data$Medical_Education_or_Training_Interrupted)
 chisq.test(data$Match_Status, data$Misdemeanor_Conviction)
 chisq.test(data$Match_Status, data$Alpha_Omega_Alpha)
 chisq.test(data$Match_Status, data$US_or_Canadian_Applicant)
 chisq.test(data$Match_Status, data$Gold_Humanism_Honor_Society)
 chisq.test(data$Match_Status, data$Military_Service_Obligation)
 chisq.test(data$Match_Status, data$Visa_Sponsorship_Needed)
 chisq.test(data$Match_Status, data$white_non_white)
 chisq.test(data$Match_Status, data$Expected_Visa_Status)
 chisq.test(data$Match_Status, data$Partner_Match)

 #See video on YouTube by Bharatendra Rai
 #Two-way table of factor variables to make sure we do not have any data values equal to zero
 xtabs(~Match_Status + USMLE_Step_1_Score, data = data)
 
 ##Nomogram for a binary outcome (matching into residency), https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5451623/
 #fun.at - Demarcations on the function axis: "Matching into obgyn"
 #lp=FALSE so we don't have the logistic progression
 colnames(data)
 ddist <- datadist(data)
 ddist
 options (datadist = 'ddist')
 
 #These variables need work Medical School Type, Med_school_condensed
 #Removed Step 2 CK score because most applicants will not have it and I don't have data on those who did not take the test at the time of applying.  
 mod.bi <- rms::lrm(Match_Status_Dichot ~ white_non_white + Gender + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Oral_Presentation + Count_of_Peer_Reviewed_Journal_Articles_Abstracts + Count_of_Peer_Reviewed_Book_Chapter + Count_of_Poster_Presentation + Military_Service_Obligation + Other_Service_Obligation + Visa_Sponsorship_Needed + Misdemeanor_Conviction, data = data)
   mod.bi 
 
 #Keep predictors in the binary logistic regression model that have a p<0.10 a priori to create nomogram
 mod.bi.significant <- rms::lrm(Match_Status_Dichot ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation, data = data)
 mod.bi.significant  #Check the C-statistic which is the same as ROC area for binary logistic regression
 
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
  
  ##https://github.com/harrelfe/rms/blob/master/inst/tests/nomogram.r
  # From Andy Bush <andy@kb4lsn.net>
  require(rms)
  set.seed(20)
  x1<-10*runif(20,0,1)
  y1<-c(rep(0,10),rep(1,10))
  y2<-5*rnorm(20,0,1)
  
  d<-data.frame(cbind(y1,y2,x1))
  dd<-datadist(d)
  options(datadist='dd')
  flrm<-lrm(y1~x1,x=T,y=T,model=T)
  nomlrm<-nomogram(flrm)
  plot(nomlrm,xfac=.45)
  fols<-ols(y2~x1,x=T,y=T,model=T)
  nomols<-nomogram(fols)
  plot(nomols,xfac=.45)
  
  
  ## From Zongheng Zhang zh_zhang1984@hotmail.com
  n <- 1000    # sample size
  
  age<- rnorm(n, 65, 11)
  lac<- round(abs(rnorm(n, 3, 1)),1)
  sex<- factor(sample(1:2,n,prob=c(0.6,0.4),TRUE),
               labels=c('male','female'))
  shock<-factor(sample(1:4,n,prob=c(0.3,0.3,0.25,0.15),TRUE),
                labels=c('no','mild','moderate','severe'))
  z<- 0.2*age + 3*lac* as.numeric(sex)+ 5*as.numeric(shock) -rnorm(n,36,15)
  ## linear combination with a bias
  
  y <- ifelse(runif(n) <= plogis(z), 1, 0)
  library(rms)
  ddist <- datadist(age, lac, shock, sex)
  options(datadist='ddist')
  mod <- lrm(y ~ shock+lac*sex+age)
  nom <- nomogram(mod,
                  lp.at=seq(-3,4,by=0.5),
                  fun=plogis,
                  fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                  funlabel="Risk of Death",
                  conf.int=c(0.1, 0.7),
                  abbrev=TRUE, #had not been working for shock
                  minlength=1)
  
  plot(nom, lplabel="Linear Predictor",
       fun.side=c(3,3,1,1,3,1,3,1,1,1,1,1,3),
       col.conf=c('red','green'),
       conf.space=c(0.1,0.5),
       label.every=3,
       col.grid = gray(c(0.8, 0.95)),
       which="shock")
  legend.nomabbrev(nom, which='shock', x=.5, y=.5)

#######################################################################################
  #Sign up for shinyapp.io
  rsconnect::setAccountInfo(name='mufflyt', token='D8846CA8B32E6A5EAEA94BFD02EEEA39', secret='dIXWOv+ud/z6dTPN2xOF9M4BKJtWKROc2cOsZS4U')
#DynNom
  nomo_fit2 <- rms::lrm(Match_Status_Dichot ~ white_non_white + Couples_Match + Alpha_Omega_Alpha + USMLE_Step_1_Score + US_or_Canadian_Applicant + Gold_Humanism_Honor_Society + Count_of_Poster_Presentation, data = data)
  #fit2 <- stats::glm(survived ~ (age + pclass + sex) ^ 3, titanic3, family = "binomial")
  DynNom::DynNom.lrm(nomo_fit2, data, clevel = 0.95, m.summary = "formatted")
  rsconnect::deployApp('path/to/your/app')
  