# Initialization
## Packages.
## To install, run `install.packages("<package_name>")`.
library(car)
library(ggplot2)
library(grid)
library(Kendall)
library(lmtest)
library(sandwich)
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

# First, let's see if there's a correlation between CS and IT
# AC-CE and AE-RO. "The t-test is used to test whether there is
# a difference between two groups on a continuous dependent
# variable." That is appropriate for checking if there's a
# difference between CS and IT majors with their AC-CE and AE-RO
# values, but not for checking the significance between those
# values and the relationship to GPA.
t.test(cs_ac_ce, it_ac_ce) # p = 0.5411
t.test(cs_ae_ro, it_ae_ro) # p = 0.3501
# So there's no relationship between them. And we can see that
# in the plot:
df<-data.frame(cs_ac_ce,cs_ae_ro)
df2<-data.frame(it_ac_ce,it_ae_ro)
cs_v_it_plot<-ggplot(df, aes(cs_ac_ce,cs_ae_ro)) +
  geom_point(color="black")
cs_v_it_plot<-cs_v_it_plot + geom_point(data=df2, x=it_ac_ce,
  y=it_ae_ro, color="red")
cs_v_it_plot<-cs_v_it_plot + labs(x="AC-CE", y="AE-RO")
cs_v_it_plot<-cs_v_it_plot + scale_fill_manual(name="Majors",
  values = c("CS" = "black", "IT" = "red"))
jpeg('figures/chapter4/cs-v-it-plot.jpg', width = 500,
  height = 500)
cs_v_it_plot
dev.off()

# Research questions
## Question 1: How strong is the correlation between AC-CE and
## AE-RO, and college GPA in CS, IS, and IT?
# Before we know whether to use Pearson's or Spearman's, we have
# to know if the data is normally distributed. If it is, we use
# Pearson's; if not, we use Spearman's.
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
# This means that as IT AE-RO increases, there's a positively
# related change in GPA which we can see in the Figure.

# Let's get a summary of it_major_gpa so we can see just what
# that means.
summary(it_major_gpa)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  2.340   2.868   3.200   3.204   3.480   3.970
sd(it_major_gpa) # sd = 0.4439921

df <- data.frame(it_major_gpa)
hist <- ggplot(data = df, aes(it_major_gpa)) +
  geom_histogram(color="black", fill="white",
  breaks=seq(2.34, 4, by=0.1))
hist <- hist + labs(x = "IT major GPA", y = "Count")
hist <- hist + geom_density(alpha=0.2, fill="red")
hist <- hist + geom_vline(aes(xintercept=mean(it_major_gpa)),
  color="blue", linetype="dashed")
jpeg('figures/chapter4/it-major-gpa-hist.jpg')
hist
dev.off()

# Should only be used on this first one.
cor.test(cs_major_gpa, cs_ac_ce, method = "spearman") # p=0.8232
cor.test(cs_major_gpa, cs_ae_ro, method = "spearman") # p=0.8342
cor.test(it_major_gpa, it_ac_ce, method = "spearman") # p=0.9262
cor.test(it_major_gpa, it_ae_ro, method = "spearman") # p=0.0161,
# rho = 0.5066
# Spearman's and Pearson's are showing the same thing which is
# great news. But, Spearman's freaks out when there's ties, so
# let's use Kendall's tau-b (which handles ties) to check.
Kendall(cs_major_gpa,cs_ac_ce) # p = 0.71763
Kendall(cs_major_gpa,cs_ae_ro) # p = 0.82436
Kendall(it_major_gpa,it_ac_ce) # p = 0.95484
Kendall(it_major_gpa,it_ae_ro) # p = 0.02154, tau = 0.365
# And again the same p-value being significant holds. I'm
# convinced.

# Let's see what they look like
# First plot
df<-data.frame(cs_major_gpa, cs_ac_ce)
cs_major_ac_ce_plot<-ggplot(df, aes(x=cs_major_gpa, y=cs_ac_ce))+
  geom_point()
cs_major_ac_ce_plot<-cs_major_ac_ce_plot + geom_smooth(method=lm)
cs_major_ac_ce_plot<-cs_major_ac_ce_plot + labs(x="CS Major GPA",
  y="CS AC-CE")
cs_major_ac_ce_plot<-cs_major_ac_ce_plot +
  coord_cartesian(xlim = c(2.2,4.1), ylim = c(-32,32),
  expand = FALSE)
# Second plot
df<-data.frame(cs_major_gpa, cs_ae_ro)
cs_major_ae_ro_plot<-ggplot(df, aes(x=cs_major_gpa, y=cs_ae_ro))+
  geom_point()
cs_major_ae_ro_plot<-cs_major_ae_ro_plot + geom_smooth(method=lm)
cs_major_ae_ro_plot<-cs_major_ae_ro_plot + labs(x="CS Major GPA",
  y="CS AE-RO")
cs_major_ae_ro_plot<-cs_major_ae_ro_plot +
  coord_cartesian(xlim = c(2.2,4.1), ylim = c(-32,32),
  expand = FALSE)
# Third plot
df<-data.frame(it_major_gpa, it_ac_ce)
it_major_ac_ce_plot<-ggplot(df, aes(x=it_major_gpa, y=it_ac_ce))+
  geom_point()
it_major_ac_ce_plot<-it_major_ac_ce_plot + geom_smooth(method=lm)
it_major_ac_ce_plot<-it_major_ac_ce_plot + labs(x="IT Major GPA",
  y="IT AC-CE")
it_major_ac_ce_plot<-it_major_ac_ce_plot +
  coord_cartesian(xlim = c(2.2,4.1), ylim = c(-32,32),
  expand = FALSE)
# Fourth plot
df<-data.frame(it_major_gpa, it_ae_ro)
it_major_ae_ro_plot<-ggplot(df, aes(x=it_major_gpa, y=it_ae_ro))+
  geom_point()
it_major_ae_ro_plot<-it_major_ae_ro_plot + geom_smooth(method=lm)
it_major_ae_ro_plot<-it_major_ae_ro_plot + labs(x="IT Major GPA",
  y="IT AE-RO")
it_major_ae_ro_plot<-it_major_ae_ro_plot +
  coord_cartesian(xlim = c(2.2,4.1), ylim = c(-32,32),
  expand = FALSE)

# Print them side-by-side
jpeg('figures/chapter4/major_gpa_lm_plots.jpg', width = 1000,
  height = 1000)
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
# Let's calculate the robust standard of error and use that
# throughout these calculations. The assumption is that you will
# use robust standard of error; if you choose not to, you have
# to prove why you don't have to.
# Robust standard error: normal standard error does not handle
# heteroskedasticity so we need to have the robust standard
# error. So, "I ran this model with standard errors computed with
# Huber-White (HC1) robust standard error to account for the
# heteroskedasticity of the data."

### Model 1: with AC-CE and AE-RO and a CS dummy variable
fit1 <- lm(data = data.mis, Major.GPA ~ csdum + AC.CE + AE.RO)

### Model 2: plus covariates
fit2 <- lm(data = data.mis, Major.GPA ~ csdum + AC.CE + AE.RO +
  Age + Parents.education)

# Here's where we calculate the robust standard errors so we can
# use them in the tables.
cov1 <- vcovHC(fit1, type = "HC1")
robust_se_1 <- sqrt(diag(cov1))
cov2 <- vcovHC(fit1, type = "HC1")
robust_se_2 <- sqrt(diag(cov2))

# Let's visualize that
stargazer(fit1, fit2, se = list(robust_se_1, robust_se_2))

# Plot fit1
df$fit1 <- stats::predict(fit1, newdata=data.mis)
err <- stats::predict(fit1, newdata=data.mis, se = TRUE)
fit1_plot <- ggplot(df)
fit1_plot <- fit1_plot + geom_point(aes(x=major_gpa, y = fit1),
  size = 2)
fit1_plot <- fit1_plot + geom_smooth(data=df, aes(x = major_gpa,
  y = fit1), size = 1.5, colour = "blue", se = TRUE,
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

jpeg('figures/chapter4/mr_models_1_2.jpg', width = 1000, height = 500)
pushViewport(viewport(layout = grid.layout(1,2)))
print(fit1_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 1))
print(fit2_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 2))
dev.off()

#### Test of joint significance (linear hypothesis test)
# We haven't been looking at the individual variables before, so
# let's look at them individually and see if there's a joint
# significance. The null hypothesis is that all of these are 0
# so let's see if at least one of them isn't.
# This is checking the residual (error) sum of squares to see
# if we can explain the deviance in the model (by running two
# models and comparing those error sum of squares values). So not
# including these variables (if the F statistic is high)
# increases the amount of unexplained variance in the data.
# So, including these variables explains more of the variance
# than when you don't include them.
# In reality, having one that's significant at the single level
# will generally make you fail the null hypothesis (meaning there
# is a joint significance so they should be included in the
# regression). If they fail the null hypothesis,
# then you should keep them in the regression because they
# somehow help contribute to the model because they help
# explain the std error (meaning, they help explain the deviance
# in the model). Additionally, if they're all significant then we'll
# reject the joint significance null hypothesis even though none of
# them have a p < 0.05.
# Removing unnecessary variables will give you more power with
# this small of a dataset.
cs_it_lht <- lm(data = data.mis, Major.GPA ~ csdum + AE.RO +
  Age + Parents.education)
lht(cs_it_lht, c("AE.RO = 0",
  "Parents.educationgraduate degree = 0",
  "Parents.educationpost-graduate degree = 0",
  "Parents.educationsome college = 0",
  "Parents.educationundergraduate degree = 0"),
  white.adjust = "hc1")
# With the p-value being so low, we confidently reject the null
# hypothesis that these variables are not significant in
# explaining the variance in the data.
# I feel comfortable not including age here because it has
# already been shown to be significant in the multiple regression
# model.

## Question 3: How strong is the correlation between AC-CE and
## AE-RO, and student satisfaction in CS, IS, and IT?
### Data cleaning: AMSS values index minus questions 3 and 6.
amss_index <- (6 - data.mis$amss1) + (6 - data.mis$amss2) +
  data.mis$amss4 + data.mis$amss5
cs_amss_index <- amss_index[data.mis$Major == "cs"]
it_amss_index <- amss_index[data.mis$Major == "it"]

summary(amss_index)
sd(amss_index) # 2.259477
df <- data.frame(amss_index)
amss_plot <- ggplot(data = df, aes(amss_index)) +
  geom_histogram()
amss_plot <- amss_plot + labs(x = "AMSS Index", y = "Count")
jpeg('figures/chapter4/amss_index_plot.jpg', width = 500,
  height = 500)
amss_plot
dev.off()

# Just to be sure, let's look at each major
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
cor.test(cs_major_gpa, cs_amss_index) # p = 0.3094
cor.test(it_major_gpa, it_amss_index) # p = 0.4532

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
jpeg('figures/chapter4/major_gpa_amss_plots.jpg', width = 1000,
  height = 500)
pushViewport(viewport(layout = grid.layout(1,2)))
print(cs_major_amss_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 1))
print(it_major_amss_plot, vp = viewport(layout.pos.row = 1,
  layout.pos.col = 2))
dev.off()

# Demographics
demogs <- lm(amss_index ~ major_gpa + csdum + Age + Gender,
  data = data.mis)
cov1 <- vcovHC(demogs, type = "HC1")
robust_se_d <- sqrt(diag(cov1))
stargazer(demogs, se = robust_se_d)
