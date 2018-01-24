# Initialization
## Packages.
## To install, run `install.packages("<package_name>")`.
library(stargazer)
library(ggplot2)

## Input data
data <- read.csv("data.csv")

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
## Question 1: How strong is the correlation between AC-CE and
## AE-RO, and college GPA in CS, IS, and IT?
# Set up some variables
cs_ac_ce<-data.mis$AC.CE[data.mis$Major=="cs"]
cs_ae_ro<-data.mis$AE.RO[data.mis$Major=="cs"]
it_ac_ce<-data.mis$AC.CE[data.mis$Major=="it"]
it_ae_ro<-data.mis$AE.RO[data.mis$Major=="it"]
cs_major_gpa<-data.mis$Major.GPA[data.mis$Major=="cs"]
it_major_gpa<-data.mis$Major.GPA[data.mis$Major=="it"]

# Let's take a look at the data
# Plot cs vs it so we can visualize it
df<-data.frame(cs_ac_ce,cs_ae_ro)
df2<-data.frame(it_ac_ce,it_ae_ro)
cs_v_it_plot<-ggplot(df, aes(cs_ac_ce,cs_ae_ro))+geom_point(color="black")
cs_v_it_plot<-cs_v_it_plot+geom_point(data=df2, x=it_ac_ce, y=it_ae_ro,
  color="red")
cs_v_it_plot<-labs(x="AC-CE", y="AE-RO")
cs_v_it_plot<-scale_fill_manual(name="Majors", values =
  c("CS" = "black", "IT" = "red"))
jpeg('cs-v-it-plot.jpg')
cs_v_it_plot
dev.off()

# Pearson's correlation coefficient for both majors
# This doesn't make sense to look at. Do we really care if there's a correlation
# between AC-CE and AE-RO?
cor.test(data$AC.CE, data$AE.RO)

# Do the CS AC-CE and IT AC-CE have any correlation?
# These don't work because they're of varying lengths
t.test(cs_ac_ce,it_ac_ce)
# What about AE-RO?
t.test(cs_ae_ro,it_ae_ro)
# Both are p>0.05.

### Relationship to GPA
#### Combined - not divided between CS and IT
cor.test(data.mis$Major.GPA, data.mis$AE.RO)
cor.test(data.mis$Major.GPA, data.mis$AC.CE)

#### Split by major for each axis per
cor.test(cs_major_gpa, cs_ac_ce)
cor.test(cs_major_gpa, cs_ae_ro)
cor.test(it_major_gpa, it_ac_ce)
# This last one is the only significant one with p < 0.05.
cor.test(it_major_gpa, it_ae_ro)

# Let's see what that last one looks like
df<-data.frame(it_major_gpa, it_ae_ro)
it_major_ae_ro_plot<-ggplot(df, aes(x=it_major_gpa, y=it_ae_ro)) + geom_point()
it_major_ae_ro_plot<-it_major_ae_ro_plot + geom_smooth(method=lm)
jpeg('it_major_ae_ro_plot.jpg')
it_major_ae_ro_plot
dev.off()

## Question 2: What is the best multiple regression model to fit
## these correlations?

### Model 1: with AC-CE and AE-RO and a CS dummy variable
fit1 <- lm(Major.GPA ~ csdum + AC.CE + AE.RO, data = data.mis)

### Model 2: plus covariates
fit2 <- lm(Major.GPA ~ csdum + AC.CE + AE.RO + Age +
 Parents.education, data = data.mis)
stargazer(fit1, fit2)

### Models 3 and 4
#### Divided into just AE and RO
fit3 <- lm(Major.GPA ~ csdum + RO.total + AE.total, data =
 data.mis)
#### With age and parents' education as covariates
fit4 <- lm(Major.GPA ~ csdum + RO.total + AE.total + Age +
 Parents.education, data = data.mis)
stargazer(fit3, fit4)

#### Test of joint significance (lht = test linear hypothesis)
xx <- lm(Major.GPA ~ csdum + RO.total + AE.total + Age +
 Parents.education, data = data.mis)
lht(xx, c("AE.total = 0","Parents.educationgraduate degree = 0",
 "Parents.educationpost-graduate degree = 0",
 "Parents.educationsome college = 0",
 "Parents.educationundergraduate degree = 0", "Age21-24 = 0",
 "Age25-29 = 0", "Age30-34 = 0"), white.adjust = "hc1")

### Models 5, 6, and 7
fit5 <- lm(Major.GPA ~ csdum + RO.total + csdum*RO.total + Age +
 Parents.education, data =data.mis)
fit6 <- lm(Major.GPA ~ RO.total, data = data.mis, subset =
 Major == "cs")
fit7 <- lm(Major.GPA ~ RO.total, data = data.mis, subset =
 Major == "it")
stargazer(fit5, fit6, fit7)

#### Substantive interpretation
yy <- sd(data.mis$RO.total[data.mis$csdum == 0], na.rm = T)
##### IT standard deviation * 2 * Coeff
yy * 2 * -0.04334

## Question 3: How strong is the correlation between AC-CE and
## AE-RO, and student satisfaction in CS, IS, and IT?
### Data cleaning: AMSS values index minus questions 3 and 6.
data.mis$amss.ind <- (6- data.mis$amss1) +
 (6 - data.mis$amss2) + data.mis$amss4 + data.mis$amss5
summary(data.mis$amss.ind)

### Pearson's correlation coefficient for AE-RO and AC-CE and
### satisfaction
cor.test(data.mis$amss.ind, data.mis$AE.RO)
cor.test(data.mis$amss.ind, data.mis$AC.CE)
cor.test(data.mis$amss.ind, data.mis$RO.total)
cor.test(data.mis$amss.ind, data.mis$AE.total)
cor.test(data.mis$amss.ind, data.mis$AC.total)
cor.test(data.mis$amss.ind, data.mis$CE.total)

### Again for IT majors
cor.test(data.mis$amss.ind[data.mis$Major == "it"],
 data.mis$RO.total[data.mis$Major == "it"])

### And again for CS majors
cor.test(data.mis$amss.ind[data.mis$Major == "cs"],
 data.mis$RO.total[data.mis$Major == "cs"])

## Question 4: Is there a correlation between college GPA and
## student satisfaction?
### Pearson's correlation coefficient between GPA and
## satisfaction
cor(data.mis$Major.GPA, data.mis$amss.ind)

# Demographics
summary(lm(amss.ind ~ Major.GPA + csdum + Age + Gender, data =
 data.mis))
