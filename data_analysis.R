# Initialization
## Packages. To install, run `install.packages("<package_name>")`.
library(stargazer)
library(car)

## Input data
data <- read.csv("/path/to/data.csv")

## Dummy variables
### Gender dummy variable
data$maledum[data$Gender == "m"] <- 1
data$maledum[data$Gender == "f"] <- 0

### CS dummy variable
data$csdum[data$Major == "cs"] <- 1
data$csdum[data$Major != "cs"] <- 0

## Dataset without IS
data.mis <- data[data$Major != "is",]
data.mis$maledum[data.mis$Gender == "m"] <- 1
data.mis$maledum[data.mis$Gender == "f"] <- 0

### CS dummy variable
data.mis$csdum[data.mis$Major == "cs"] <- 1
data.mis$csdum[data.mis$Major != "cs"] <- 0

# Research questions
## Question 1: How strong is the correlation between CE-AC and RO-AE, and college GPA in CS, IS, and IT?
# Pearson's correlation coefficient
cor.test(data$AC.CE, data$AE.RO)

### Relationship to GPA
#### Combined - not divided between CS and IT
cor.test(data.mis$Major.GPA, data.mis$AE.RO)
cor.test(data.mis$Major.GPA, data.mis$AC.CE)

## Question 2: What is the best multiple regression model to fit these correlations?
# NOTE: these tables are not formatted for the paper.

### Model 1: with AC-CE and AE-RO and a CS dummy variable
fit1 <- lm(Major.GPA ~ csdum + AC.CE + AE.RO, data = data.mis)

### Model 2: plus covariates
fit2 <- lm(Major.GPA ~ csdum + AC.CE + AE.RO + Age + Parents.education, data = data.mis)
stargazer(fit1, fit2)

### Models 3 and 4
#### Divided into just AE and RO
fit3 <- lm(Major.GPA ~ csdum + RO.total + AE.total, data = data.mis)
#### With age and parents' education as covariates
fit4 <- lm(Major.GPA ~ csdum + RO.total + AE.total + Age + Parents.education, data = data.mis)
stargazer(fit3, fit4)

#### Test of joint significance (lht = test linear hypothesis)
xx <- lm(Major.GPA ~ csdum + RO.total + AE.total + Age + Parents.education, data = data.mis)
lht(xx, c("AE.total = 0","Parents.educationgraduate degree = 0", "Parents.educationpost-graduate degree = 0", "Parents.educationsome college = 0", "Parents.educationundergraduate degree = 0", "Age21-24 = 0", "Age25-29 = 0", "Age30-34 = 0"), white.adjust = "hc1")

### Models 5, 6, and 7
fit5 <- lm(Major.GPA ~ csdum + RO.total + csdum*RO.total + Age + Parents.education, data =data.mis)
fit6 <- lm(Major.GPA ~ RO.total, data = data.mis, subset = Major == "cs")
fit7 <- lm(Major.GPA ~ RO.total, data = data.mis, subset = Major == "it")
stargazer(fit5, fit6, fit7)

#### Substantive interpretation
yy <- sd(data.mis$RO.total[data.mis$csdum == 0], na.rm = T)
##### IT standard deviation * 2 * Coeff
yy * 2 * -0.04334

## Question 3: How strong is the correlation between AC-CE and AE-RO, and student satisfaction in CS, IS, and IT?
### Data cleaning: AMSS values index minus questions 3 and 6.
data.mis$amss.ind <- (6- data.mis$amss1) + (6 - data.mis$amss2) + data.mis$amss4 + data.mis$amss5

### Pearson's correlation coefficient for AE-RO and AC-CE and satisfaction
cor.test(data.mis$amss.ind, data.mis$AE.RO)
cor.test(data.mis$amss.ind, data.mis$AC.CE)
cor.test(data.mis$amss.ind, data.mis$RO.total)
cor.test(data.mis$amss.ind, data.mis$AE.total)
cor.test(data.mis$amss.ind, data.mis$AC.total)
cor.test(data.mis$amss.ind, data.mis$CE.total)

### Again for IT majors
cor.test(data.mis$amss.ind[data.mis$Major == "it"], data.mis$RO.total[data.mis$Major == "it"])

### And again for CS majors
cor.test(data.mis$amss.ind[data.mis$Major == "cs"], data.mis$RO.total[data.mis$Major == "cs"])

## Question 4: Is there a correlation between college GPA and student satisfaction?
### Pearson's correlation coefficient between GPA and satisfaction
cor(data.mis$Major.GPA, data.mis$amss.ind)

# Demographics
summary(lm(amss.ind ~ Major.GPA + csdum + Age + Gender, data = data.mis))
