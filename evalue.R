install.packages("EValue", dependencies = T)

library("EValue")

###For the first matching example

#smk_data <- read_csv("https://raw.githubusercontent.com/gckc123/Causal_Analysis_Addiction_Examples/main/smoking_psyc_distress.csv")

###Converting remoteness into a factor variable
#smk_data$remoteness <- as.factor(smk_data$remoteness)

###optimal matching using logistic regression
#smk_matching <- matchit(smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age, data = smk_data, method = "optimal", distance = "glm")
#summary(smk_matching)

### graphical examine of the balance
#plot(summary(smk_matching), abs = FALSE)

###extracting the matched data
#matched_data <- match.data(smk_matching)

###comparing treatment and control group
#smk_model1 <- lm(psyc_distress ~ smoker, data = matched_data, weights = weights)
#summary(smk_model1)

#coeftest(smk_model1, vcov. = vcovCL, cluster = ~subclass)
#coefci(smk_model1, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

#After running the above codes, use sd to calculate the standard deviation of the outcome

sd(matched_data$psyc_distress)

est = OLS(1.72, sd = 6.9261)
evalue(est = est, se = 0.315)

################################################################################

