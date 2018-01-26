# Initialization
## Packages.
## To install, run `install.packages("<package_name>")`.
library(car)
library(ggplot2)
library(grid)
library(Kendall)
library(stargazer)

## Input data
data <- read.csv("data.csv")

## Dataset without IS
data.mis <- data[data$Major != "is",]

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
fit1 <- lm(data = data.mis, Major.GPA ~ csdum + AC.CE + AE.RO)

### Model 2: plus covariates
fit2 <- lm(Major.GPA ~ csdum + AC.CE + AE.RO + Age +
 Parents.education, data = data.mis)

# Let's visualize that
stargazer(fit1, fit2)

# Plot fit1
df$fit1 <- stats::predict(fit1, newdata=data.mis)
err <- stats::predict(fit1, newdata=data.mis, se = TRUE)
fit1_plot <- ggplot(df)
fit1_plot <- fit1_plot + geom_point(aes(x=major_gpa, y = fit1),
  size = 2)
fit1_plot <- fit1_plot + geom_smooth(data=df, aes(x=major_gpa,
  y=fit1), size = 1.5, colour = "blue", se = TRUE,
  stat = "smooth", method = lm)
fit1_plot <- fit1_plot + labs(x = "Major GPA", y =
  "CS dummy variable + AC-CE + AE-RO")

# Plot fit2
df$fit2 <- stats::predict(fit2, newdata=data.mis)
err <- stats::predict(fit2, newdata=data.mis, se = TRUE)
fit2_plot <- ggplot(df)
fit2_plot <- fit2_plot + geom_point(aes(x=major_gpa, y = fit2),
  size = 2)
fit2_plot <- fit2_plot + geom_smooth(data=df, aes(x=major_gpa,
  y=fit2), size = 1.5, colour = "blue", se = TRUE,
  stat = "smooth", method = lm)
fit2_plot <- fit2_plot + labs(x = "Major GPA", y =
  "CS dummy variable + AC-CE + AE-RO + Age + Parents' education")

jpeg('mr_models_1_2.jpg', width = 1000, height = 500)
pushViewport(viewport(layout = grid.layout(1,2)))
print(fit1_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 1))
print(fit2_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 2))
dev.off()

#### Test of joint significance (linear hypothesis test)
cs_it_lht <- lm(Major.GPA ~ csdum + AE.RO + Age +
  Parents.education, data = data.mis)
lht(cs_it_lht, c("AE.RO = 0",
  "Parents.educationgraduate degree = 0",
  "Parents.educationpost-graduate degree = 0",
  "Parents.educationsome college = 0",
  "Parents.educationundergraduate degree = 0"),
  white.adjust = "hc1")

## Question 3: How strong is the correlation between AC-CE and
## AE-RO, and student satisfaction in CS, IS, and IT?
### Data cleaning: AMSS values index minus questions 3 and 6.
amss_index <- (6 - data.mis$amss1) + (6 - data.mis$amss2) +
  data.mis$amss4 + data.mis$amss5
cs_amss_index <- amss_index[data.mis$Major == "cs"]
it_amss_index <- amss_index[data.mis$Major == "it"]

summary(amss_index)
sd(amss_index) # 2.259477
summary(cs_amss_index)
sd(cs_amss_index) # 2.362094
summary(it_amss_index)
sd(it_amss_index) # 2.113654

### Pearson's correlation coefficient for AC-CE/AE-RO and
### satisfaction (not by major)
cor.test(amss_index, data.mis$AE.RO) # p = 0.1059
cor.test(amss_index, data.mis$AC.CE) # p = 0.8563

### For CS majors
cor.test(cs_amss_index, cs_ac_ce) # p = 0.8134
cor.test(cs_amss_index, cs_ae_ro) # p = 0.1237

### And again for IT majors
cor.test(it_amss_index, it_ac_ce) # p = 0.9566
cor.test(it_amss_index, it_ae_ro) # p = 0.5147

## Question 4: Is there a correlation between college GPA and
## student satisfaction?
### Pearson's correlation coefficient between GPA and
### satisfaction
cor.test(cs_major_gpa, cs_amss_index)
cor.test(it_major_gpa, it_amss_index)

# Let's visualize that
## CS AMSS plot
df <- data.frame(cs_major_gpa, cs_amss_index)
cs_major_amss_plot <- ggplot(df, aes(x = cs_major_gpa,
  y = cs_amss_index)) + geom_point()
cs_major_amss_plot <- cs_major_amss_plot + geom_smooth(method=lm)
cs_major_amss_plot <- cs_major_amss_plot + labs(x =
  "CS Major GPA", y = "CS AMSS")
cs_major_amss_plot <- cs_major_amss_plot + coord_cartesian(ylim =
  c(12,20))
## IT AMSS plot
df <- data.frame(it_major_gpa, it_amss_index)
it_major_amss_plot <- ggplot(df, aes(x = it_major_gpa,
  y = it_amss_index)) + geom_point()
it_major_amss_plot <- it_major_amss_plot + geom_smooth(method=lm)
it_major_amss_plot <- it_major_amss_plot + labs(x="IT Major GPA",
  y="IT AMSS")
it_major_amss_plot <- it_major_amss_plot + coord_cartesian(ylim =
    c(12,20))

# Print them side-by-side
jpeg('major_gpa_amss_plots.jpg', width = 1000, height = 500)
pushViewport(viewport(layout = grid.layout(1,2)))
print(cs_major_amss_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 1))
print(it_major_amss_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 2))
dev.off()

# Demographics
demogs <- lm(amss_index ~ major_gpa + csdum + Age + Gender,
  data = data.mis)
stargazer(demogs)
