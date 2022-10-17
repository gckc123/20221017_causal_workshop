################################################################################
#
# Matching
#
################################################################################

#Installing key packages

install.packages("tidyverse", dependencies = TRUE)
install.packages("lmtest", dependencies = TRUE)
install.packages("sandwich", dependencies = TRUE)
install.packages("MatchIt", dependencies = TRUE)

#Loading packgages

library("tidyverse")
library("lmtest")
library("sandwich")
library("MatchIt")

#Loading the dataset

smk_data <- read_csv("https://raw.githubusercontent.com/gckc123/Causal_Analysis_Addiction_Examples/main/smoking_psyc_distress.csv")

#Converting remoteness into a factor variable
smk_data$remoteness <- as.factor(smk_data$remoteness)

#optimal matching using logistic regression
smk_matching <- matchit(smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age, data = smk_data, method = "optimal", distance = "glm")
summary(smk_matching)

#graphical examine of the balance
plot(summary(smk_matching), abs = FALSE)

#extracting the matched data
matched_data <- match.data(smk_matching)

#comparing treatment and control group
smk_model1 <- lm(psyc_distress ~ smoker, data = matched_data, weights = weights)
summary(smk_model1)

coeftest(smk_model1, vcov. = vcovCL, cluster = ~subclass)
coefci(smk_model1, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

#doubly robust estimation
smk_model2 <- lm(psyc_distress ~ smoker + sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age, data = matched_data, weights = weights)
summary(smk_model2)
coeftest(smk_model2, vcov. = vcovCL, cluster = ~subclass)
coefci(smk_model2, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

################################################################################

#using nearest matching with a matching ratio of 1:3 with generalized boosted model

smk_matching2 <- matchit(smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age, data = smk_data, method = "nearest", distance = "gbm", ratio = 3)
summary(smk_matching2)

smk_matching_summary2 <- summary(smk_matching2, addlvariables = ~ I(age^2))
plot(smk_matching_summary2, abs = FALSE)

plot(smk_matching2, type = "density")





#extracting the matched data
matched_data2 <- match.data(smk_matching2)

#comparing treatment and control group
smk_model3 <- lm(psyc_distress ~ smoker, data = matched_data2, weights = weights)
summary(smk_model3)

coeftest(smk_model3, vcov. = vcovCL, cluster = ~subclass)
coefci(smk_model3, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

#doubly robust estimation
smk_model4 <- lm(psyc_distress ~ smoker + sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age, data = matched_data2, weights = weights)
summary(smk_model4)
coeftest(smk_model4, vcov. = vcovCL, cluster = ~subclass)
coefci(smk_model4, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

################################################################################


