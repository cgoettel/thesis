# Initialization
## Packages.
## To install, run `install.packages("<package_name>")`.
library(ggplot2)
library(grid)
library(Kendall)
library(stargazer)

## Input data
data <- read.csv("data.csv")

## Dataset without IS
data.mis <- data[data$Major != "is"]

## Dummy variables
data.mis$maledum[data.mis$Gender == "m"] <- 1
data.mis$maledum[data.mis$Gender == "f"] <- 0
data.mis$csdum[data.mis$Major == "cs"] <- 1
data.mis$csdum[data.mis$Major != "cs"] <- 0

## CS and IT specific variables
cs_ac_ce<-data.mis$AC.CE[data.mis$Major=="cs"]
cs_ae_ro<-data.mis$AE.RO[data.mis$Major=="cs"]
it_ac_ce<-data.mis$AC.CE[data.mis$Major=="it"]
it_ae_ro<-data.mis$AE.RO[data.mis$Major=="it"]
cs_major_gpa<-data.mis$Major.GPA[data.mis$Major=="cs"]
it_major_gpa<-data.mis$Major.GPA[data.mis$Major=="it"]

# First, let's see if there's a correlation between CS and IT AC-CE and AE-RO.
# "The t-test is used to test whether there is a difference between two groups
# on a continuous dependent variable." That is appropriate for checking if
# there's a difference between CS and IT majors with their AC-CE and AE-RO
# values, but not for checking the significance between those values and the
# relationship to GPA.
t.test(cs_ac_ce, it_ac_ce) # p = 0.5411
t.test(cs_ae_ro, it_ae_ro) # p = 0.3501
# I don't know if I'm doing that right, so just to be safe:
t.test(cs_ac_ce,cs_ae_ro) # p = 0.3756
t.test(it_ac_ce,it_ae_ro) # p = 0.5674
# So there's no relationship between them. And we can see that in the plot:
df<-data.frame(cs_ac_ce,cs_ae_ro)
df2<-data.frame(it_ac_ce,it_ae_ro)
cs_v_it_plot<-ggplot(df, aes(cs_ac_ce,cs_ae_ro)) +
  geom_point(color="black")
cs_v_it_plot<-cs_v_it_plot + geom_point(data=df2, x=it_ac_ce,
  y=it_ae_ro, color="red")
cs_v_it_plot<-cs_v_it_plot + labs(x="AC-CE", y="AE-RO")
cs_v_it_plot<-cs_v_it_plot + scale_fill_manual(name="Majors",
  values = c("CS" = "black", "IT" = "red"))
jpeg('cs-v-it-plot.jpg', width = 1000, height = 1000)
cs_v_it_plot
dev.off()

# Research questions
## Question 1: How strong is the correlation between AC-CE and
## AE-RO, and college GPA in CS, IS, and IT?
# Before we know whether to use Pearson's or Spearman's, we have to know if the
# data is normally distributed. If it is, we use Pearson's; if not, we use
# Spearman's.
shapiro.test(cs_major_gpa) # p = 0.148
shapiro.test(it_major_gpa) # p = 0.8639
shapiro.test(cs_ac_ce) # p = 0.02512
shapiro.test(cs_ae_ro) # p = 0.7826
shapiro.test(it_ac_ce) # p = 0.3601
shapiro.test(it_ae_ro) # p = 0.4583

cor.test(cs_major_gpa, cs_ac_ce) # p = 0.8177
cor.test(cs_major_gpa, cs_ae_ro) # p = 0.6704
cor.test(it_major_gpa, it_ac_ce) # p = 0.9727
cor.test(it_major_gpa, it_ae_ro) # p = 0.02017, cor = 0.4914967
# This means that as IT AE-RO increases, there's a 0.4914967 increase in GPA.
# Let's get a summary of it_major_gpa so we can see just what that means.
summary(it_major_gpa)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  2.340   2.868   3.200   3.204   3.480   3.970
sd(it_major_gpa) # sd = 0.4439921
# CHECK: If I'm reading this right, it's showing an increase of 0.44 GPA per sd.
# This is huge when it comes to IT students being more AE-RO (meaning they're
# stronger in the AE bit).

cor.test(cs_major_gpa, cs_ac_ce, method = "spearman") # p = 0.8232
cor.test(cs_major_gpa, cs_ae_ro, method = "spearman") # p = 0.8342
cor.test(it_major_gpa, it_ac_ce, method = "spearman") # p = 0.9262
cor.test(it_major_gpa, it_ae_ro, method = "spearman") # p = 0.0161, rho = 0.5066
# Spearman's and Pearson's are showing the same thing which is great news. But,
# Spearman's freaks out when there's ties, so let's use Kendall's tau-b to
# check.
Kendall(cs_major_gpa,cs_ac_ce) # p = 0.71763
Kendall(cs_major_gpa,cs_ae_ro) # p = 0.82436
Kendall(it_major_gpa,it_ac_ce) # p = 0.95484
Kendall(it_major_gpa,it_ae_ro) # p = 0.02154, tau = 0.365
# And again the same p-value being significant holds. I'm convinced. Well,
# except for t-tests. Let's actually learn about those.
# The average of the three correlations:
#   ( 0.365 + 0.5066691 + 0.4439921 ) / 3
# is 0.4385537 which is right around what I was writing about earlier on the
# effect that an IT student's AE-RO has on their GPA.

# Let's see what they look like
# First plot
df<-data.frame(cs_major_gpa, cs_ac_ce)
cs_major_ac_ce_plot<-ggplot(df, aes(x=cs_major_gpa, y=cs_ac_ce))+
  geom_point()
cs_major_ac_ce_plot<-cs_major_ac_ce_plot + geom_smooth(method=lm)
cs_major_ac_ce_plot<-cs_major_ac_ce_plot + labs(x="CS Major GPA",
  y="CS AC-CE")
# Second plot
df<-data.frame(cs_major_gpa, cs_ae_ro)
cs_major_ae_ro_plot<-ggplot(df, aes(x=cs_major_gpa, y=cs_ae_ro))+
  geom_point()
cs_major_ae_ro_plot<-cs_major_ae_ro_plot + geom_smooth(method=lm)
cs_major_ae_ro_plot<-cs_major_ae_ro_plot + labs(x="CS Major GPA",
  y="CS AE-RO")
# Third plot
df<-data.frame(it_major_gpa, it_ac_ce)
it_major_ac_ce_plot<-ggplot(df, aes(x=it_major_gpa, y=it_ac_ce))+
  geom_point()
it_major_ac_ce_plot<-it_major_ac_ce_plot + geom_smooth(method=lm)
it_major_ac_ce_plot<-it_major_ac_ce_plot + labs(x="IT Major GPA",
  y="IT AC-CE")
# Fourth plot
df<-data.frame(it_major_gpa, it_ae_ro)
it_major_ae_ro_plot<-ggplot(df, aes(x=it_major_gpa, y=it_ae_ro))+
  geom_point()
it_major_ae_ro_plot<-it_major_ae_ro_plot + geom_smooth(method=lm)
it_major_ae_ro_plot<-it_major_ae_ro_plot + labs(x="IT Major GPA",
  y="IT AE-RO")

# Print them side-by-side
jpeg('major_gpa_lm_plots.jpg', width = 1000, height = 1000)
pushViewport(viewport(layout = grid.layout(2,2)))
print(cs_major_ac_ce_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 1))
print(cs_major_ae_ro_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 2))
print(it_major_ac_ce_plot, vp = viewport(layout.pos.row = 2,
  layout.pos.col = 1))
print(it_major_ae_ro_plot, vp = viewport(layout.pos.row = 2,
  layout.pos.col = 2))
dev.off()

## Question 2: What is the best multiple regression model to fit
## these correlations?
### Model 1: with AC-CE and AE-RO and a CS dummy variable
fit1 <- lm(Major.GPA ~ csdum + AC.CE + AE.RO, data = data.mis)

### Model 2: plus covariates
fit2 <- lm(Major.GPA ~ csdum + AC.CE + AE.RO + Age +
 Parents.education, data = data.mis)
stargazer(fit1, fit2)

#### Test of joint significance (lht = linear hypothesis test)
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
amss_index <- (6 - data.mis$amss1) + (6 - data.mis$amss2) +
  data.mis$amss4 + data.mis$amss5
summary(amss_index)

### Pearson's correlation coefficient for AE-RO and AC-CE and
### satisfaction
cor.test(amss_index, data.mis$AE.RO)
cor.test(amss_index, data.mis$AC.CE)

### For IT majors
cor.test(amss_index[data.mis$Major == "it"],
 data.mis$RO.total[data.mis$Major == "it"])

### And again for CS majors
cor.test(amss_index[data.mis$Major == "cs"],
 data.mis$RO.total[data.mis$Major == "cs"])

## Question 4: Is there a correlation between college GPA and
## student satisfaction?
### Pearson's correlation coefficient between GPA and
### satisfaction
cs_amss <- amss_index[data.mis$Major == "cs"]
it_amss <- amss_index[data.mis$Major == "it"]

cor.test(cs_major_gpa, cs_amss)
cor.test(it_major_gpa, it_amss)

# Let's visualize that
## CS AMSS plot
df<-data.frame(cs_major_gpa, cs_amss)
cs_major_amss_plot<-ggplot(df, aes(x=cs_major_gpa, y=cs_amss))+
  geom_point()
cs_major_amss_plot<-cs_major_amss_plot + geom_smooth(method=lm)
cs_major_amss_plot<-cs_major_amss_plot + labs(x="CS Major GPA",
  y="CS AMSS")
## IT AMSS plot
df<-data.frame(it_major_gpa, it_amss)
it_major_amss_plot<-ggplot(df, aes(x=it_major_gpa, y=it_amss))+
  geom_point()
it_major_amss_plot<-it_major_amss_plot + geom_smooth(method=lm)
it_major_amss_plot<-it_major_amss_plot + labs(x="IT Major GPA",
  y="IT AMSS")

# Print them side-by-side
jpeg('major_gpa_amss_plots.jpg', width = 1000, height = 500)
pushViewport(viewport(layout = grid.layout(1,2)))
print(cs_major_amss_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 1))
print(it_major_amss_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 2))
dev.off()

# Demographics
summary(lm(amss.ind ~ Major.GPA + csdum + Age + Gender, data =
 data.mis))
