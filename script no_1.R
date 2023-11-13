
###################################################################################################

#load starting packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(Hmisc)
library(stringr)
library(ggplot2)
library(markdown)
library(gtsummary)
library(survey)
library(prettyR)
library(labelled)
library(haven)
library(survey)
library(jtools)
library(remotes)
library(broom)
library(modelsummary)
library(kableExtra)
library(gt)
library(qwraps2)
library(dataMaid)
library(gmodels)


###################################################################################################

WHI.TotalPA <- read_sas("/Users/Connor/Downloads/ms3662dat2.sas7bdat")
WHI.Walking <- read_sas("/Users/Connor/Downloads/ms3662dat.sas7bdat")

glimpse(WHI.TotalPA)
glimpse(WHI.Walking)

###################################################################################################

#################################################################################
### FIGURING OUT WHY THE DATASET(s) COMTAINS MORE PARTICIPANTS THAN IT SHOULD ###
#################################################################################

CrossTable(WHI.TotalPA$F33PILLSHYP)
CrossTable(WHI.TotalPA$HxCHD)
CrossTable(WHI.TotalPA$prevCanc)

CrossTable(WHI.Walking$HxCHD, WHI.Walking$stroke_f2)

#ANSWER: need to exclude on the following variables: HxCHD & stroke_f2
#ALSO: need to merge walking and total PA analysis dataframes b/c total PA dataframe does not have stroke_f2 variable

###############################
### SETTING ANALYTIC SAMPLE ###
###############################

WHI.Walking.stroke_var <- WHI.Walking %>% select(ID, stroke_f2)
WHI.TotalPA <- WHI.TotalPA %>% left_join(WHI.Walking.stroke_var, by="ID")
WHI.TotalPA <- WHI.TotalPA %>% select(-stroke_f2.x)
WHI.TotalPA <- WHI.TotalPA %>% rename(stroke_f2 = stroke_f2.y)

CrossTable(WHI.TotalPA$HxCHD, WHI.TotalPA$stroke_f2)

WHI.TotalPA.an_samp <- WHI.TotalPA %>% filter(HxCHD==0 & stroke_f2==0)

###################################################################################################
###################################################################################################
###################################################################################################

#############################################################
### INITIAL EXPLORATORY ANALYSES / UNDERSTANDING THE DATA ###
#############################################################

# Create dataMaid::makeDataReport report
makeDataReport(WHI.TotalPA.an_samp, output = "html", replace = TRUE)

##############################################
### data cleaning & formatting for table 1 ###
##############################################

#select requisite variables for analyses
WHI.TotalPA.table1_df <- WHI.TotalPA.an_samp %>% 
  select(ID, F33PILLSHYP, TEXPWK, MILDEXP, MODEXP, HARDEXP, WALKEXP, 
         AGE, ETHNIC, educcat, nses, SMOKING, ALCOHOL, TOTHSTAT, 
         DIABTRT, HICHOLRP, prevCanc, ACTDLY, PHYSFUN, SYST, DIAS, 
         BMI, WAIST, HEI2005, F60SODUM, OSFLAG, HRTARM, DMARM)

#######################################
### transform numeric-coded factors ###
### into appropriate factor labels  ###
#######################################

#hypertension status
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(F33PILLSHYP = structure(case_when(F33PILLSHYP==0 ~ "No",
                                           F33PILLSHYP==1 ~ "Yes"),
                                 label = "Diagnosed-Treated Hypertension"))

#age categories         
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(AGE_cats = structure(case_when(AGE<60 ~ "50-59 years",
                                        AGE<70 ~ "60-69 years",
                                        AGE>69 ~ "70-79 years"),
                              label = "Age at screening (categorical)"))

#race-ethnicity
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(ETHNIC = structure(case_when(ETHNIC==5 ~ "White",
                                      ETHNIC==3 ~ "Black",
                                      ETHNIC==4 ~ "Hispanic",
                                      ETHNIC==1 ~ "American Indian",
                                      ETHNIC==2 ~ "Asian/Pacific Islander",
                                      ETHNIC==8 ~ "Unknown"),
                            label = "Race-Ethnicity"))

#education
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(educcat = structure(case_when(educcat==0 ~ "High school or less",
                                       educcat==1 ~ "College/some college",
                                       educcat==2 ~ "Post-graduate"),
                             label = "Education"))

#smoking
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(SMOKING = structure(case_when(SMOKING==0 ~ "Never Smoked",
                                       SMOKING==1 ~ "Past Smoker",
                                       SMOKING==2 ~ "Current Smoker"),
                             label = "History of Cigarette Smoking"))

#Alcohol use
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(ALCOHOL = structure(case_when(ALCOHOL==1 ~ "Non drinker",
                                       ALCOHOL==2 ~ "Past drinker",
                                       ALCOHOL==3 ~ "<1 drink per month",
                                       ALCOHOL==4 ~ "<1 drink per week",
                                       ALCOHOL==5 ~ "1 to <7 drinks per week",
                                       ALCOHOL==6 ~ "7+ drinks per week"),
                             label = "Alcohol use"))

#Hormone therapy use
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(TOTHSTAT = structure(case_when(TOTHSTAT==0 ~ "Never used",
                                        TOTHSTAT==1 ~ "Past user",
                                        TOTHSTAT==2 ~ "Current user"),
                              label = "Hormone therapy use"))

#Diagnosed-treated diabetes mellitus
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(DIABTRT = structure(case_when(DIABTRT==1 ~ "Yes",
                                       DIABTRT==0 ~ "No"),
                             label = "History of diabetes"))

#Hyperlipidemia
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(HICHOLRP = structure(case_when(HICHOLRP==1 ~ "Yes",
                                        HICHOLRP==0 ~ "No"),
                              label = "Hyperlipidemia"))

#History of cancer
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(prevCanc = structure(case_when(prevCanc==1 ~ "Yes",
                                        prevCanc==0 ~ "No"),
                              label = "History of cancer"))

#ADL score [NUMERIC]
#Physical function score [NUMERIC]
#Systolic BP, mm Hg [NUMERIC]
#Diastolic BP, mm Hg [NUMERIC]
#BMI, kg/m2 [NUMERIC]
#Waist, cm [NUMERIC]
#Healthy eating index diet score [NUMERIC]
#Dietary sodium intake, mg [NUMERIC]

#Study component
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(OSFLAG = structure(case_when(OSFLAG==0 ~ "Clinical trial",
                                      OSFLAG==1 ~ "Observational study"),
                            label = "Study component"))

#Hormone Therapy intervention
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(HRT.int = structure(case_when(HRTARM==0 ~ "No",
                                       HRTARM==1 ~ "Yes",
                                       HRTARM==2 ~ "No",
                                       HRTARM==3 ~ "Yes",
                                       HRTARM==4 ~ "No"),
                             label = "HT intervention"))

#Dietary Modification intervention
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(DM.int = structure(case_when(DMARM==0 ~ "No",
                                      DMARM==2 ~ "No",
                                      DMARM==1 ~ "Yes"),
                            label = "DM intervention"))


###################################################################################################

#############################################
### FINDING TOTAL PA TERTILES / QUANTILES ###
#############################################
WHI.TotalPA.no0METs <- WHI.TotalPA.table1_df %>% filter(TEXPWK>0)

quantile(WHI.TotalPA.no0METs$TEXPWK, c(0, 1/3, 2/3, 1))
#cutpoints from console output: >0–7.5, 7.5-17.5, 17.5+


TotalPA.quantile.cutpoints <- cut(WHI.TotalPA.no0METs$TEXPWK, 
                                  quantile(WHI.TotalPA.no0METs$TEXPWK, 
                                           c(0, 1/3, 2/3, 1)), 
                                  include.lowest = TRUE)

table(TotalPA.quantile.cutpoints)
#cutpoints from console output: >0–<7.5, 7.5-<17.5, 17.5+

#DERIVE CATEGORICAL VARIABLE
WHI.TotalPA.table1_df <- WHI.TotalPA.table1_df %>% 
  mutate(TotalPA.cats = case_when( TEXPWK==0 ~ "0 MET-h/wk",
                                   TEXPWK<=7.5 ~ "0.5 – <7.5 MET-h/wk",
                                   TEXPWK<=17.5 ~ "7.5 - <17.5 MET-h/wk",
                                   TEXPWK>17.5 ~ "17.5+ MET-h/wk"))

CrossTable(WHI.TotalPA.table1_df$TotalPA.cats)




####################################################################################################
####################################################################################################
####################################################################################################
WHI.TotalPA.htntbl1.df <- WHI.TotalPA.table1_df %>% select(ID, F33PILLSHYP, TotalPA.cats, TEXPWK, MILDEXP, MODEXP, HARDEXP, WALKEXP, 
                                                           AGE, AGE_cats, ETHNIC, educcat, nses, SMOKING, ALCOHOL, TOTHSTAT, 
                                                           DIABTRT, HICHOLRP, prevCanc, ACTDLY, PHYSFUN, SYST, DIAS, 
                                                           BMI, WAIST, HEI2005, F60SODUM, OSFLAG, HRT.int, DM.int)


##################################################
### summary table (using "arsenal" package),   ###
### stratified by incident hypertension status ###
##################################################
supp_table_one <- tableby(F33PILLSHYP ~ ., data = WHI.TotalPA.htntbl1.df) 

#------------------------#------------------------#------------------------#------------------------#------------------------
#IN R MARKDOWN
```{r, results = "asis"}
summary(supp_table_one, title = "Table 1. Baseline Characteristics for the Overall Cohort and According to Incident HyDiagnosed-Treated Hypertension Status (N=83,410)")
```
#------------------------#------------------------#------------------------#------------------------#------------------------


##########################################################
###       summary table (using "arsenal" package),     ###
### stratified by Total Recreational Physical Activity ###
##########################################################
table_one <- tableby(TotalPA.cats ~ ., data = WHI.TotalPA.htntbl1.df)

#------------------------#------------------------#------------------------#------------------------#------------------------
#IN R MARKDOWN
```{r, results = "asis"}
summary(table_one, 
        title = "Table 1. Baseline Characteristics for the Overall Cohort and According to Categories of Total Recreational Physical Activity (N=83,410)")
```
#------------------------#------------------------#------------------------#------------------------#------------------------




###################################
### PRACTICE KAPLAN MEIER CURVE ###
###################################

pacman::p_load(tidyverse, survival,survminer,gtsummary,scales,tidycmprsk)

WHI.TotalPA.survtime <- WHI.TotalPA %>% select(ID, timeyr, timedy)

WHI.TotalPA.KM <- WHI.TotalPA.table1_df %>% left_join(WHI.TotalPA.survtime, by = "ID")

WHI.TotalPA.KM <- WHI.TotalPA.KM %>% select(ID, F33PILLSHYP, TotalPA.cats, timedy, timeyr)

WHI.TotalPA.KM <- WHI.TotalPA.KM %>% mutate(htn.num = case_when(F33PILLSHYP=="Yes" ~ 1,
                                                                F33PILLSHYP=="No" ~ 0),
                                            
                                            pa_cats.num = case_when(TotalPA.cats=="0 MET-h/wk" ~ 1,
                                                                    TotalPA.cats=="0.5 – <7.5 MET-h/wk" ~ 2,
                                                                    TotalPA.cats=="7.5 - <17.5 MET-h/wk" ~ 3,
                                                                    TotalPA.cats=="17.5+ MET-h/wk" ~ 4))


library("survminer")
require("survival")
fit <- survfit(Surv(timedy, htn.num) ~ pa_cats.num, data = WHI.TotalPA.KM)
# Drawing curves
km.curve <- ggsurvplot(fit, 
                       conf.int = FALSE,
                       risk.table = T,
                       legend.title = "MET-hr/week:",
                       #risk.table.col = "strata",
                       xlim = c(0,6000),
                       ylim = c(0,1.0),
                       break.time.by = 1000,
                       censor.size=0.25, 
                       size = .5,
                       pval.size = 3.5,
                       pval = TRUE,
                       ggtheme = theme_minimal(),
                       risk.table.y.text.col = F, # colour risk table text annotations.
                       risk.table.y.text = T,
                       xlab = "Time in days",
                       ylab = "Cumulative Incidence",
                       fun = "cumhaz",
                       risk.table.fontsize = 3,
                       palette = "Set1",
                       legend = c(0.8,0.2),
                       legend.labs = c("0", ">0-7.5", ">7.5-17.5", ">17.5"))

km.curve


km.curve$plot <- km.curve$plot + theme(legend.box.background = element_rect(color="grey55", size=0.25),
                                       legend.box.margin = margin(12, 2, 2, 2))


km.curve$plot
















