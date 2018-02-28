library(dplyr)
library(psych)
library(plotly)

data <- read.csv("data.csv", header = TRUE)

data.mis <- dplyr::filter(data, Major != 'is')

data.mis$csdum <- 0
data.mis$csdum[data.mis$Major=="cs"] <- 1
data.mis$csdum <- as.factor(data.mis$csdum)

data.mis$new_age <- 0
data.mis$new_age[data.mis$Age=="21-24"] <- 1
data.mis$new_age[data.mis$Age=="25-29"] <- 2
data.mis$new_age[data.mis$Age=="30-34"] <- 3
data.mis$new_age[data.mis$Age=="35+"] <- 4

data.mis$new_par_ed <- 0
data.mis$new_par_ed[data.mis$Parents.education=="elementary school"] <- 1
data.mis$new_par_ed[data.mis$Parents.education=="some college"] <- 2
data.mis$new_par_ed[data.mis$Parents.education=="undergraduate degree"] <- 3
data.mis$new_par_ed[data.mis$Parents.education=="graduate degree"] <- 4
data.mis$new_par_ed[data.mis$Parents.education=="post-graduate degree"] <- 5

data.mis$amss1r <- 6 - data.mis$amss1
data.mis$amss2r <- 6 - data.mis$amss2
data.mis$amss3r <- 6 - data.mis$amss3
data.mis$amss6r <- 6 - data.mis$amss6

amss.items <- dplyr::select(data.mis, amss1r, amss2r, amss3r, amss4, amss5, amss6r)
amss.alpha <- psych::alpha(amss.items)
amss.alpha

# Remove amss6r as it is not loading well
amss.items <- dplyr::select(data.mis, amss1r, amss2r, amss3r, amss4, amss5)
amss.alpha <- psych::alpha(amss.items)
amss.alpha

# Remove amss2r as it is not loading well
amss.items <- dplyr::select(data.mis, amss1r, amss3r, amss4, amss5)
amss.alpha <- psych::alpha(amss.items)
amss.alpha

data.mis$amss_mean_index <- rowMeans(data.mis[,c(13,14,22,24)])

hist(data.mis$AE.RO)
hist(data.mis$AC.CE)
hist(data.mis$amss_mean_index)

amss.model <- glm(formula = amss_mean_index ~ new_age + new_par_ed + Major.GPA + csdum * AE.RO * AC.CE, data = data.mis)
summary(amss.model)

gpa_lm <- glm(Major.GPA ~ new_age + new_par_ed + csdum*AE.RO*AC.CE, data = data.mis)
cov1 <- vcovHC(gpa_lm, type = "HC1")
robust_se_1 <- sqrt(diag(cov1))
stargazer(gpa_lm, se = robust_se_1, type = "text")

p <- plot_ly(data.mis, x = ~Major.GPA, y = ~AE.RO, z = ~AC.CE, color = ~csdum, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Major GPA'),
                      yaxis = list(title = 'AE.RO'),
                      zaxis = list(title = 'AC.CE')))
p
