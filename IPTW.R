################################################################################
#
# IPTW
#
################################################################################

#installing the packages

install.packages("twang", dependencies = TRUE)
install.packages("survey", dependencies = TRUE)

#loading the packages

library("twang")
library("survey")
library("tidyverse")

#loading the data

smk_data <- read_csv("https://raw.githubusercontent.com/gckc123/Causal_Analysis_Addiction_Examples/main/smoking_psyc_distress.csv")

smk_iptw <- ps(smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age, interaction.depth = 3, data = as.data.frame(smk_data), n.tree = 10000, estimand = "ATE", verbose = FALSE)

plot(smk_iptw)
bal.table(smk_iptw)

smk_data$weight <- get.weights(smk_iptw, stop.method = "es.mean")

design_iptw <- svydesign(ids = ~1, weights = ~weight, data = smk_data)
smk_model5 <- svyglm(psyc_distress ~ smoker, design = design_iptw)
summary(smk_model5)
confint(smk_model5)

################################################################################

smk_data %>%  ggplot(aes(x = weight)) + geom_histogram(color = "white", alpha = 0.60, position = "identity", na.rm = TRUE)


summary(smk_data$weight)

head(sort(smk_data$weight),10)
tail(sort(smk_data$weight),10)

quantile(smk_data$weight, c(.99))

truncated_smk_data <- smk_data[smk_data$weight < 16.82, ]
design_iptw2 <- svydesign(ids = ~1, weights = ~weight, data = truncated_smk_data)

smk_model6 <- svyglm(psyc_distress ~ smoker, design = design_iptw2)
summary(smk_model6)
confint(smk_model6)

################################################################################


smk_iptw2 <- ps(smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age, interaction.depth = 3, data = as.data.frame(smk_data), n.tree = 10000, estimand = "ATT", verbose = FALSE)

plot(smk_iptw2)
bal.table(smk_iptw2)

smk_data$weight2 <- get.weights(smk_iptw2, stop.method = "es.mean")

design_iptw3 <- svydesign(ids = ~1, weights = ~weight2, data = smk_data)
smk_model7 <- svyglm(psyc_distress ~ smoker, design = design_iptw3)
summary(smk_model7)
confint(smk_model7)

################################################################################
