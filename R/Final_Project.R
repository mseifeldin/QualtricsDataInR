###############################################################################
################################## Set Up #####################################
###############################################################################

#######################################
### Set Working Directory:
#######################################
setwd()

#######################################
### Load Packages:
#######################################

library(dplyr)
library(ggplot2)
library(ltm)
library(car)
library(gmodels)

#######################################
### Load Data:
#######################################

## numeric data
numeric_data <- read.csv(".csv", header=TRUE)
## choice data
choice_data <- read.csv(".csv", header=TRUE)

###############################################################################
################################ Clean Data ###################################
###############################################################################

################################################
### Get rid of Superfluous Qualtrics Columns:
################################################
##in numeric data:
numeric_data <- numeric_data[, -c(1:17)]
##in choice data:
choice_data <- choice_data[, -c(1:17)]

################################################
### Get rid of Metadata rows & Nonconsenters:
################################################
##in numeric data:
numeric_data <- subset(numeric_data, Consent==1)
##in choice data:
choice_data <- subset(choice_data, Consent=="Yes")

###############################################################################
############################### Process Data ##################################
###############################################################################

#############################################
### Reverse Code Likert Scale Variables
#############################################
### Recode appropriate variable scale items:
numeric_data$newreversedcolumnname <- recode(numeric_data$oldcolumnname, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1;")
#repeat as necessary

### Collect appropriate variable scale items into a single dataframe:
variablename <- data.frame()

######################################################
### Perform Cronbach's Alpha on Likert Scale Variables
######################################################
alpha <- cronbach.alpha(variablename, na.rm = TRUE)

######################################################
### Calculate Mean Score Columns if necessary
######################################################
variablename$variablenamemeans <- rowMeans(variablename, na.rm = TRUE)

######################################################
### Calculate Accuracy Score Columns if necessary
######################################################

##Parse drag-n-drop columsn in necessary
numeric_data$word1 <- ifelse(grepl("word1",choice_data$dragndropcolumn),1,0)
#repeat as necessary for all words

##Collect variable raw score columns into a single dataframe
variablename <- data.frame()

##Generate proportion correct answers in new column
variablename$newaccuracyscorecolumn <- rowSums(variablename) / ncol(variablename)

##################################
### Analysis Data Frame Creation
##################################

### Collect relevant colmns into one dataframe
analysis_data <- data.frame()

### Give columns intuitive names (must be in the same order as above)
names(analysis_data) <- c("", "", "")

###############################################################################
########################## Descriptive Statistics #############################
###############################################################################

##################################
### Demographics
##################################

###For categorical demographics:
variable_percentages <- prop.table(ftable(analysis_data$demographicvariable)) * 100

###For continuous demographics:
variable_mean <- mean(analysis_data$demographicvariable)
variable_sd <- sd(analysis_data$demographicvariable)
variable_range <- range(analysis_data$demographicvariable)

##################################
### DV Descriptives Grouped by IV
##################################

##mean:
variable_mean_by_condition <- analysis_data %>%
  group_by(Condition) %>%
  summarise(mean(variablecolumn, na.rm=TRUE))

##sd:
variable_sd_by_condition <- analysis_data %>%
  group_by(Condition) %>%
  summarise(sd(variablecolumn, na.rm=TRUE))

###############################################################################
########################## Inferential Statistics #############################
###############################################################################

##########################################################
### For a Chi - Square (categorical x categorical)
##########################################################
variable1_x_variable2 <- CrossTable(analysis_data$variable1, analysis_data$variable2, chisq = TRUE)

#####################################################################
### For a Pearson Correlation (continuous x continuous)
#####################################################################
variable1_x_variable2 <- cor.test(analysis_data$variable1, analysis_data$variable2, 
                       method = "pearson")

#####################################################################
### For a t-test (1 two-level categorical IV x 1 continuous DV)
#####################################################################
##factorize the independent variable
analysis_data$IVcolumn <- factor(analysis_data$IVcolumn)

IVname_x_DVname <- t.test(analysis_data$DVcolumn ~ analysis_data$IVcolumn, var.equal = TRUE)

#####################################################################
### For an ANOVA (1 Mutilevel categorical IV x 1 continuous DV)
#####################################################################
##factorize the independent variable
analysis_data$IVcolumn <- factor(analysis_data$IVcolumn)
##run analysis:
IVname_x_DVname <- aov(DVcolumn ~ IVcolumn, na.action=na.exclude, data= analysis_data)
##get reportable results:
summary(IVname_x_DVname)
##if results are statistically significant, run Tukey post-hoc test
TukeyHSD(IVname_x_DVname)

#####################################################################
### For a Multiple Regression (>1 IVs x 1 continuous DV)
#####################################################################
##factorize the independent variables
analysis_data$IV1 <- factor(analysis_data$IV1)
analysis_data$IV2 <- factor(analysis_data$IV2)
##run analysis:
IV1_IV2_x_DV <- lm(DV ~ IV1 + IV2, na.action=na.exclude, data= analysis_data)
##get reportable results:
summary(IV1_IV2_x_DV)



