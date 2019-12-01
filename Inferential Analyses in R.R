# R Codes
# LOADING DATA ----

library(haven)
STIData <- read_stata("C:\\Users\\User\\OneDrive\\CORY\\DATA\\STIData.dta")
STIData$CaseStatus = ifelse(STIData$CaseStatus == 2, 0, STIData$CaseStatus)
STIData$CaseStatus = ifelse(STIData$CaseStatus == 3, 1, STIData$CaseStatus)
table(STIData$CaseStatus)

# A) CORRELATION ANALYSES--------
# 1.) Parametric correlations ----
# 1.i) Pearson's r
with(STIData, cor(Weight, Height, method = "pearson"))
with(STIData, cor.test(Height, Weight, method = "pearson")) #Gives the p-value 
# 2.) Non-parametric correlations ----
# 2.i) Spearman's rho (Works for ordinal and non-nornamlly distributed) ----
with(STIData, cor(Weight, Height, method = "spearman"))
with(STIData, cor.test(Weight, Height, method = "spearman")) # Based on ranks, cannot compute exact p-value with ties

# 2.ii) Kendall's tau ----
with(STIData, cor(A1Age, Weight, method = "kendall"))
with(STIData, cor.test(Weight, Height, method = "spearman")) # Based on ranks, cannot compute exact p-value with ties

# 2.iii) Cronbach's alpha ----
psych::alpha(STIData, check.keys=TRUE) # check.keys reverses negative correlation

# B) ASSOCIATION ANALYSIS--------------------------
# 1.) Pearson's Chis-quare test of independence ----
library(gmodels)
CrossTable(STIData$CaseStatus, STIData$AlcoholUse, 
           expected = TRUE,
           chisq = TRUE,
           dnn = c("Case Status", "Alcohol")) 
chisq.test(STIData$CaseStatus, STIData$AlcoholUse,
           correct = FALSE)

# 2.) Pearson's Chis-quare with Yate's Continuity Correction ----
# Yates is used to correct for upward error when some expeced counts are below 5
CrossTable(STIData$CaseStatus, STIData$AlcoholUse, 
           expected = TRUE,
           chisq = TRUE,
           dnn = c("Case Status", "Alcohol")) # For 2 by 2 tables, the function returns Yates as well by default

chisq.test(STIData$CaseStatus, STIData$AlcoholUse,
           correct = TRUE)

# 3.) Fisher's exact ----
# Fisher's is used to correct for upward error when some expeced counts are below 5
# Works only for 2by2 tables
# The alternative to Fisher's for tables higher than 2by2, use Freeman-Halton

gmodels::CrossTable(STIData$CaseStatus, STIData$AlcoholUse, 
           expected = TRUE,
           fisher = TRUE,
           dnn = c("Case Status", "Alcohol")) 

fisher.test(STIData$CaseStatus, STIData$AlcoholUse)

# 4.) McNemar's test ----
# This is a before-and-after test, works like a paired-sample t-test
# Therefore, repeated measures version of chisquare
# The observations are correlated and thus chisquare cannot work,
# Chisquare requires that observations are independent/i.e not paired
# It is therefore not a test of independence, but of consistency across 2 variables
# Like Fisher's, it is restricted to 2by2
McNemarData <- matrix(c(101, 121, 59, 33),
                             nrow = 2,
                             dimnames = list("Before" = c("Negative", "Positive"),
                                             "After" = c("Negative", "Positive")))
mcnemar.test(McNemarData)
library(gmodels)
CrossTable(McNemarData, 
           expected = TRUE,
           mcnemar = TRUE,
           dnn = c("Case Status", "Alcohol")) # For 2 by 2 tables, the function returns Yates as well by default

# 5.Stuart-Maxuell
# This is an extension of McNemanr for 3by3 tables(matrix)
install.packages("nonpar")
library(nonpar)
StrtMx <- matrix(c(12, 30, 13, 7, 70, 34, 3, 20, 32), 3,3)
stuart.maxwell(StrtMx, alpha = 0.5)

# 5.) Cochrane's Q ----
# Test for identical treatment effects in a two-way randomized block design with k treatments
# Test whether treatments are identicall across blocks
library(nonpar)
CochrQ <- matrix(c(1,1,1,1,1,1,
                  1,1,0,1,1,1,
                  0,0,0,1,0,0,
                  0,1,0,0,1,1), 6, 4)
cochrans.q(CochrQ, alpha = 0.05)

# 6.) Cochrane Arimitage test of trend ----
# Tests for presens of significant monotonic trend
install.packages("CATTexact")
library(CATTexact)
# Calculates the Cochran-Armitage trend test statistic (Cochran (1954), Armitage (1955))
# and the one-sided p-value for the corresponding conditional exact test. The conditional exact test
# has been established by Williams (1988). The computation of its p-value is performed using an
# algorithm following an idea by Mehta, et al. (1992)}
d <- c(1,2,3,4)
n <- rep(20,4)
r <- c(1,4,3,8)
catt_asy(d, n, r)

# 6.) Cochrane Arimitage test of trend ----
# Tests for presens of significant monotonic trend
install.packages("CATTexact")
library(CATTexact)
# Calculates the Cochran-Armitage trend test statistic (Cochran (1954), Armitage (1955))
# and the one-sided p-value for the corresponding conditional exact test. The conditional exact test
# has been established by Williams (1988). The computation of its p-value is performed using an
# algorithm following an idea by Mehta, et al. (1992)}
d <- c(1,2,3,4)
n <- rep(20,4)
r <- c(1,4,3,8)
catt_asy(d, n, r)

# 7.) Mantel-Haenszel test ----
# Works for contingency tables with three variables
# The third variable is the stratification variable
Xtab <- xtabs(~STIData$CaseStatus+STIData$A5MaritalStatus+STIData$Sex) #Generate 3D table
Xtab1 <- ftable(Xtab) # Convert to flat table
Xtab1
mantelhaen.test(Xtab,
                correct = TRUE,
                exact = TRUE,
                alternative = "two.sided")

# h) Cramer's V ----
# This is a post-hoc test after Chisquare
# It measures the effect size of the groups
# Contingency tables 2by2 or above
library(vcd)
data("Arthritis")
tab <- xtabs(~Improved + Treatment, data = Arthritis)
summary(assocstats(tab))
assocstats(UCBAdmissions)

# i) Phi ----
# This is a post-hoc test after Chisquare
# It measures the effect size of the groups
# Contingency tables 2by2 only
library(vcd)
data("Arthritis")
tab <- xtabs(~Improved + Treatment, data = Arthritis)
summary(assocstats(tab))
assocstats(UCBAdmissions)

# C) REGRESSION ANALYSIS--------------------------
# 1) Linear Regression (OLS) ----
LR <- lm(Weight ~ A1Age + Height, data = STIData)       # y is continuous outcome, x's are continuous or categorical dependents
LR_Intercation <- lm(Weight ~ A1Age*Height, data = STIData)         # With interaction effects
summary(LR)
summary(LR_Intercation)
anova(LR)
anova(LR_Intercation)
broom::tidy(LR)
broom::tidy(LR_Intercation)

# Robust estiamtors
install.packages("robust")
library(robust)
LR_Robust <- lmRob(Weight ~ A1Age + Height, data = STIData)   
summary(LR_Robust)
anova(LR_Robust)
broom::tidy(LR_Robust)
plot(LR_Robust); hist(LR_Robust$residuals)

# 2) Logistic regression ----
# i) Binary logistic
# Develop training and validation data-sets
# TRAINING SET
set.seed(3000)
Index=sample(2,nrow(STIData),replace = TRUE, prob = c(0.7, 0.3)) #Split the data to 70% and 30%
TrainingData = STIData[Index==1,] #Separate the traiing set
ValiData =STIData[Index==2,] #Separate the testing set

# Training
BLR <- glm(CaseStatus ~ A1Age + Weight + AgeFirstSex + Sex, 
           data = TrainingData, 
           family = binomial(link = "logit"))
summary(BLR)
broom::tidy(BLR) 
BLR$coefficients <- exp(BLR$coefficients) # Exponentiate log odds to get odds ratios
broom::tidy(BLR) #Clean the output
exp(confint(BLR, level = 0.95)) # Obtain 95%CI for ORs
anova(BLR) # Check model iteration process

# Validation
Predict <- predict(BLR, newdata = ValiData, type = "response") # Predict probability scores
library(InformationValue)
optCutOff <- optimalCutoff(TrainingData$CaseStatus, Predict)[1] # Determining optimal cut-off
optCutOff # Gives 0.5099997
Predict <- ifelse(Predict < 0.5099997, 0, 1)
table(Predict)

# ROC Curve
plotROC(TrainingData$CaseStatus, Predict) # Gives AUC = 0.1821, very low

# Reliability tests
length(Predict) #Gives 62
length(TrainingData$CaseStatus) #Gives 164
CaseStatus <- TrainingData$CaseStatus[1:62] #Take the first 62
library(caret)
confusionMatrix(table(CaseStatus, Predict))

# Calculating McFaden's R^2
BLR_Null <- glm(CaseStatus ~ 1, 
           data = STIData,
           family = binomial(link = "logit"))

R_2 <- 1-logLik(BLR_Null)/logLik(BLR)
R_2

# ii) Ordinal logistic
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

STIData$A4LevelOfEducation[STIData$A4LevelOfEducation == "1 none"] <- 1
STIData$A4LevelOfEducation[STIData$A4LevelOfEducation == "2 primary"] <- 2
STIData$A4LevelOfEducation[STIData$A4LevelOfEducation == "3 secondary"] <- 3
STIData$A4LevelOfEducation[STIData$A4LevelOfEducation == "4 tertiary"] <- 4

STIData <- within(STIData, {
  A4LevelOfEducation <- ordered(A4LevelOfEducation,
                                levels = 1:4,
                                labels = c("None", "Primary", "Secondary", "Tertiary"))
})

class(STIData$A4LevelOfEducation)

OLR <- polr(A4LevelOfEducation ~ CaseStatus + A1Age + Weight + AgeFirstSex + Sex,
            data = STIData,
            method = "logistic",
            Hess = TRUE)
summary(OLR)
broom::tidy(OLR)

# GENERATING OR, p-values, and CIs
ctable <- coef(summary(OLR))
ctable[,1] <- exp(ctable[,1]) # Exponentaite coefficients(values) to get ORs
ci <- confint(OLR, level = 0.95) #Generating 95% ci
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2 # Generating p-values
ctable <- cbind(ctable, "p value" = p)

# iii) Multinomial logistic
STIData$A5MaritalStatus <- as.factor(STIData$A5MaritalStatus)
library(nnet)
MLR <- multinom(STIData$A5MaritalStatus ~ STIData$A1Age + 
                  STIData$A3Church + 
                  STIData$A4LevelOfEducation)
Reslt <- broom::tidy(MLR)
Reslt$estimate <- exp(Reslt$estimate)
View(Reslt)

# 3) Probit regression ----
# i) Binary probit
BPR <- glm(CaseStatus ~ A1Age + Weight + AgeFirstSex + Sex, 
           data = STIData,
           family = binomial(link = "probit"))
summary(BPR)
broom::tidy(BPR) 
anova(BPR)

# ii) Ordinal probit
STIData$A4LevelOfEducation[STIData$A4LevelOfEducation == "1 none"] <- 1
STIData$A4LevelOfEducation[STIData$A4LevelOfEducation == "2 primary"] <- 2
STIData$A4LevelOfEducation[STIData$A4LevelOfEducation == "3 secondary"] <- 3
STIData$A4LevelOfEducation[STIData$A4LevelOfEducation == "4 tertiary"] <- 4

STIData <- within(STIData, {
  A4LevelOfEducation <- ordered(A4LevelOfEducation,
                                levels = 1:4,
                                labels = c("None", "Primary", "Secondary", "Tertiary"))
})

library(MASS)
OPR <- polr(A4LevelOfEducation ~ CaseStatus + A1Age + Weight + AgeFirstSex + Sex,
            data = STIData,
            method = "probit",
            Hess = TRUE)
summary(OPR)
broom::tidy(OPR)

# iii) Multinomial probit
install.packages("mlogit")
library(mlogit)
STIData$A5MaritalStatus <- as.factor(STIData$A5MaritalStatus)
MPR <- mlogit(A5MaritalStatus ~ A1Age + A3Church + A4LevelOfEducation, STIData,
              shape = "wide", choice = "A5MaritalStatus",
              seed = 20,
              R = 100,
              probit = TRUE)
MPR
broom::tidy(MPR)
confint(summary(MPR))

# 4) Tobit regression ----
# The tobit model, also called a censored regression model, is designed to estimate linear relationships between variables when there is either left- or right-censoring 
# in the dependent variable (also known as censoring from below and above, respectively). Censoring from above takes place when cases with a value at or above some threshold, 
# all take on the value of that threshold, so that the true value might be equal to the threshold, but it might also be higher. In the case of censoring from below, 
# values those that fall at or below some threshold are censored.
library(ggplot2)
install.packages(c("GGally", "VGAM"))
library(GGally)
library(VGAM)
Tobit <- vglm(Weight ~ A1Age + Height, 
              tobit(Lower = 20, Upper = 60), 
              data = STIData) # We are limiting age at 20 and 60 years
Tobit
summary(Tobit)
anova(Tobit)

# 5) Count outcomes ----
# i) Poisson regression
PR <- glm(STIData$DurationOfillness ~ STIData$A5MaritalStatus + 
            STIData$A1Age + STIData$A3Church + STIData$A4LevelOfEducation, 
          family = poisson(link = "log"))
summary(PR)
ctable <- coef(summary(PR))    #Extract coefficients
ctable[,1] <- exp(ctable[,1])  #Exponentiate to get risk ratios
ci <- confint(PR)              #Obtain 95% CIs
ctable <- cbind(ctable, ci)    #Bind coefficints with 95% CIs

# ii) Negative binomial regression
NBR <- glm.nb(STIData$DurationOfillness ~ STIData$A5MaritalStatus + 
                STIData$A1Age + STIData$A3Church + STIData$A4LevelOfEducation, 
              link = "log")
summary(NBR)
ctable <- coef(summary(NBR))
ctable[,1] <- exp(ctable[,1])
ci <- confint(NBR)
ctable <- cbind(ctable, ci)

# 6) GEE regressions ----
# Applied for longitudinal datasets where ID of the participant 
# is repeadted vertically (long format) and therfore has time component
# Family can be binomial, poisson, gaussiona, etc
# Link can be logit, log, identity, etc
# Reports Relative Risks
# Correlation structure can be '"independence"', '"exchangeable"', '"ar1"', '"unstructured"' and '"userdefined"'
# "unstructured" corstr can crash the R software if not used carefully
write.csv(dietox, file = "LongData.csv")

library(geepack)
data("dietox")
GEE_Model <- formula(Weight~Cu*Time+I(Time^2)+I(Time^3))
GEE <- geeglm(GEE_Model, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
summary(GEE)
broom::tidy(GEE)
anova(GEE)

# D) DIFFERENCE ANALYSIS----
# 1) Parametric analyses ----
# 1) Analysis of Variance ----
# 1.i) One way ANOVA ----
OW_ANOVA <- aov(A1Age ~ Sex, data = STIData) 
summary(OW_ANOVA)
coefficients(OW_ANOVA)
TukeyHSD(OW_ANOVA)
plot(TukeyHSD(OW_ANOVA))

# 1.ii) Two way ANOVA ----
TW_ANOVANoInteraction <- aov(A1Age ~ Sex+A5MaritalStatus, data = STIData) #Without interaction
summary(TW_ANOVA)
coefficients(TW_ANOVA)
TW_ANOVAInteraction <- aov(A1Age ~ Sex+A5MaritalStatus:Sex*A5MaritalStatus, data = STIData) #With interaction
summary(TW_ANOVAInteraction)
coefficients(TW_ANOVAInteraction)


TW_ANOVAInteraction <- aov(A1Age ~ Sex*A5MaritalStatus, data = STIData) #With interaction
summary(TW_ANOVAInteraction)
coefficients(TW_ANOVAInteraction)
TukeyHSD(TW_ANOVAInteraction)
plot(TukeyHSD(TW_ANOVAInteraction))

# 1.iii) Repeated measures ANOVA ----


# 2) T-tests ---- 
# 2.i) Dependent (paired) sample t-tests ----
data("sleep") 
# Data which show the effect of two soporific drugs 
# (increase in hours of sleep compared to control) on 10 patients 
t.test(extra ~ group, 
       paired = TRUE, 
       alternative = "two.sided", 
       data = sleep)

# 2.ii) Independent (Unpaired) sample t-tests ----
# 2.ii.i) >> Equal variances (Student's t-test/pooled variance) ----
t.test(A1Age ~ CaseStatus,paired = FALSE,data = STIData,var.equal = TRUE) #Student's t

# 2.ii.ii)>> Unequal variances (Welch's t-test/Statterthwaite) ----
t.test(A1Age ~ CaseStatus, paired = FALSE, data = STIData, var.equal = FALSE) #Welch's t

t.test(y,                        # y is continuous,
       mu = k,                   # k is a numeric constant representing the population mean
       paired = FALSE,
       alternative = "two.sided",# Two-tailed equal variances
       var.equal = TRUE)

# 2) Non-parametric analyses ----
# 2.i) Kruskal-Wallis test ----
# (non-parametric one way ANOVA)
kruskal.test(A1Age ~ Sex, data = STIData)           

# 2.ii) Friedman rank sum test ----
# (non-parametric two way ANOVA)
friedman.test(A1Age ~ Sex|A5MaritalStatus, data = STIData)# y are data values, A is grouping factor, B is blocking factor.

# 2.iii) Mann Whitney U ---- 
# (non-parametric independent (Unpaired) sample t-tests)
wilcox.test(A1Age ~ CaseStatus, paired = FALSE, data = STIData) # y is numeric, A is binary
wilcox.test(x, y)                # Both x and y are numeric 

# 4.iv) Wilcoxon signed rank test ----
# (non-parametric dependent (paired) sample t-tests)
wilcox.test(extra ~ group, data = sleep, paired = TRUE) 

# E) FACTOR ANALYSIS---------------------------------
# 1) PCA ----
# The variables must be binary
# Use as many variables as posible, above 150
library(statar) # For xtile function
library(dplyr)
library(magrittr)

# Convert NA to 0
STIData[is.na(STIData)] <- 0

# Obtain only binary variables
PCA_Data <- STIData %>% select(CaseStatus, C3StiYesno, D3FuneralAssistance, SexPartner1year:SexPartnerLife3)
class(PCA_Data)
PCA_Data <- sapply(PCA_Data, as.numeric) %>% as.data.frame()
PCA <- prcomp(PCA_Data, scale = FALSE)
PCA

# Predict Components
Prediction <- predict(PCA, newdata = PCA_Data)
Prediction[, 1:4]

# Save components into a data frame
Components<-data.frame(Prediction)
View(Components)
Comp1 <- Components$PC1

# Convert the first component to index
PCA_Data <- PCA_Data %>% mutate(Index = xtile(Comp1,n=5))
table(PCA_Data$Index)

# Generating proportions
library(table1)
table1(~as.factor(PCA_Data$Index) | PCA_Data$CaseStatus)

# 2) MCA ----
library(statar) # For xtile function
library(dplyr)
library(magrittr)
library(MASS)

# Convert NA to 0
STIData[is.na(STIData)] <- 0

MCA_Data <- STIData %>% select(CaseStatus, C3StiYesno, D3FuneralAssistance, SexPartner1year:SexPartnerLife3)
class(MCA_Data)
MCA_Data <- sapply(MCA_Data, as.factor) %>% as.data.frame()
MCA <- mca(MCA_Data, nf = 3)
MCA

# Predict Components
Prediction <- predict(MCA, newdata = MCA_Data)
Prediction

# Save components into a data frame
Dimentions <- data.frame(Prediction)
View(Dimentions)
Dim1 <- Dimentions$X1

# Convert the first component to index
MCA_Data <- MCA_Data %>% mutate(Index = xtile(Dim1,n=5))
table(MCA_Data$Index)

# Generating proportions
library(table1)
table1(~as.factor(MCA_Data$Index) | MCA_Data$CaseStatus)
# F) SURVIVAL ANALYSIS--------------------------
library(survival)
Data <- lung
summary(Data)

# The three main components of survival analysis are:
# a) Time to event, in this data set it is "time"
# b) The grouping variable(s)(any categorical IVs, e.g. sex, religion, etc.)
# c) Event, this is binary with 1 = success (outcome observed), 0 = failure
# Hence change "status" to 0|1

Data$status <- ifelse(Data$status == 2, 1, 0)

# 1) DEFINITION OF TERMS----

# In survival package the function "surv" takes agruments:
# "time" = Start time for interval data
# "time2" = End time for interval data
# "event" = the event of interest
# "type" = the type of censoring, i.e. "right", "left", "interval", "counting", "mstate"
# "origin" = origin of hazard function, (s(x)) for counting process data.

# "right censored" means subjects leave the study before time to event
# "left censored" means subjects join the study after it already started.
# "interval" means "time" is recodred in ranges, e.g. (1-20-hours, where t1 = 1 hr, t2 = 20 hrs) etc.
# "counting" means
# "mstate" means the event is a "multi-state factor", i.e several possible outcomes/levels, where the firt level represents censoring.

# 2) DECLARING SURVIVAL DATA----
survive <- with(Data, Surv(time = time, event = status))
summary(survive)
survive # The plus (+) in the results stand for censored cases (no outcome)

# 3) NON-PARAMETRIC ESTIMATION OF S(X)----
# These models have no assumptions on the distribution of factors.
# Do not take in continuous predictors.
# Do not take in more than one predictors.
# They are not regression models.

# 3.a) Kaplan-Maier Estimation----

# Single S(X) with no comparison (no predictor) (use ~1)
KM_1 <- survfit(survive~1)
KM_1
summary(KM_1)

plot(KM_1, main = "Overall S(X)", ylab = "Time to event", xlab = "S(X)", conf.int = "none") # Without 95%CI
plot(KM_1, main = "Overall S(X)", ylab = "Time to event", xlab = "S(X)") 

# NB: Median Survival Time occurs when s(x) ==0.5

abline(h = 0.5, v = 310, col = "red") # Plotting median time and mean s(x)

# S(X) with comparison/Predictor(use ~x)
Data <- within(Data, {
  sex <- factor(sex,
                levels = 1:2,
                labels = c("Male","Female"))})

KM_2<- survfit(survive~sex, data = Data)
KM_2
summary(KM_2)

plot(KM_2, 
     main = "S(X) for Sex",   # Title
     ylab = "Time to event",  # Y-axis label
     xlab = "S(X)",           # X-axis label
     conf.int = "both",       # To plot 95%CI for both groups, default is "none".
     col = c("red","blue"),   # Color to distinguish groups
     lwd = 2,                 # Plot a line thicker than default
     mark = 3)                # To put marks on censored regions, "3" means "vertical" lines.
legend("topright", 
       legend = c("Male", "Female"), 
       col = c("red", "blue"), 
       lwd = 2)
abline(h = 0.5,              # Mean s(x)
       v = c(270, 426),      # Median times
       col = c("black", "red", "blue"), 
       lty = 2)              # Broken lines
#____________________________________________________________
# Inverse Plot of S(x)
plot(KM_2, 
     fun = "event",         # To flip Y-axis
     main = "S(X) for Sex",   
     ylab = "Time to event",  
     xlab = "S(X)",           
     conf.int = "both",       
     col = c("red","blue"),   
     lwd = 2,                 
     mark = 3)                
legend("topleft", 
       legend = c("Male", "Female"), 
       col = c("red", "blue"), 
       lwd = 2)
#______________________________________________________________
#______________________________________________________________
broom::tidy(KM_2) # Clean the output
library(ggfortify)

ggplot2::autoplot(KM_2) # Requires "ggfortify" package to work.
#_____________________________________________________________

# TESTING WHETHER THE DIFFERENCE IN SURVIVAL TIME BTWN THE TWO GROUPS IS SIGNIFICANT OR DUE TO CHANCE
# A) Log-Rank Test, also called Mantel-Haenszel Test (rho = 0) (More weight on higher values of time)
Diff_1 <- survdiff(survive~sex, data = Data, rho = 0)
Diff_1

# B) Generalised Wilcoxon (rho = 1) (More weight on lower values of time)
Diff_2 <- survdiff(survive~sex, data = Data, rho = 1)
Diff_2

# C) Tarobe-Ware Test (rho = 1.5) (In between Log-rank and wilcoxon)
Diff_3 <- survdiff(survive~sex, data = Data, rho = 1.5)
Diff_3

# NB: p-value < 0.05 implies there is enough evidence that the groups differ significantly. 

# 3.b) Nelson-Aalen Estimation----
N_A <- survfit(coxph(survive~sex, data = Data), type = "aalen")
N_A
summary(N_A)

# 4) SEMI-PARAMETRIC ESTIMATION OF S(X)----
# 4.a) Cox Proportional Hazards----
# Allows inclusion of multiple predictors, both categorical and continuous.
# Propotional Hazard Assumption:
# i) Constant Hazard Ratio over time
# ii) Groups compared therefore diverge at the same rate (or are parallel), but never converge nor cross.
# (Bj) < 0 means increase in the respective X is associated with lower log(HR) and longer survival time.
# (Bj) > 0 means increase in the respective X is associated with higher log(HR) and shorter survival time.

# exp(Bj) == HR;

# HR < 1 means increase in the respective X is associated with lower HR and longer survival time.
# HR > 1 means increase in the respective X is associated with higher HR and shorter survival time.

# To test Ho:B = 0, we can use Wald test,  or Likelihood  Ratio Test(LRT) if events are fewer. 

Cox <- coxph(survive~sex, data = Data)                          # Handles ties by deafult by "Efron" method
Cox_Bres <- coxph(survive~sex, data = Data, method = "breslow") # Handles ties by "Breslow" method.

Cox
summary(Cox)    # Reports LRT and Wald.
anova(Cox)      # Reports logLTR.

# 5) PARAMETRIC ESTIMATION OF S(X)----
# 5.a) Exponential Estimation----
Expo <- survreg(survive~sex, data = Data, dist =  "exponential")
Expo
summary(Expo)

# 5.b) Weibull Estimation----
Weibull <- survreg(survive~sex, data = Data, dist =  "weibull")
Weibull
summary(Weibull)

# 5.c) Loglogistic Estimation----
loglogistic <- survreg(survive~sex, data = Data, dist =  "loglogistic")
loglogistic
summary(loglogistic)

# 5.d) Gaussian Estimation----
gaussian <- survreg(survive~sex, data = Data, dist =  "gaussian")
gaussian
summary(gaussian)

# 5.e) Lognormal Estimation----
lognormal <- survreg(survive~sex, data = Data, dist =  "lognormal")
lognormal
summary(lognormal)

#____________________________END_____________________________________
