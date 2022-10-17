################################################################################
#
# MSM using IPTW
#
################################################################################

library("tidyverse")
library("twang")
library("survey")

alc_data <- read_csv("https://raw.githubusercontent.com/gckc123/Causal_Analysis_Addiction_Examples/main/home_alc.csv")

alc_iptw <- iptw(list(home_alc_2 ~ home_alc_1 + alc_use1 + alc_peer1 + smk1,
                      home_alc_3 ~ alc_use2 + alc_peer2 + smk2,
                      home_alc_4 ~ alc_use3 + alc_peer3 + smk3
                ),
                timeInvariant ~ sex,
                data = as.data.frame(alc_data),
                cumulative = TRUE,
                priorTreatment = TRUE,
                stop.method = "es.max",
                n.trees = 10000)

plot(alc_iptw, plots = 1)

bal.table(alc_iptw)

alc_data$unstab_weight <- get.weights.unstab(alc_iptw, stop.method = "es.mean")[,1]

num_fm <- list(glm(home_alc_2 ~ 1, family = binomial, data = alc_data),
               glm(home_alc_3 ~ home_alc_2, family = binomial, data = alc_data),
               glm(home_alc_4 ~ home_alc_2 + home_alc_3, family = binomial, data = alc_data))

num_weights <- get.weights.num(alc_iptw, num_fm)

alc_data$stab_weight <- num_weights* alc_data$unstab_weight

alc_data$total_home_alc = alc_data$home_alc_2 + alc_data$home_alc_3 + alc_data$home_alc_4 
alc_data$total_home_alc = as.factor(alc_data$total_home_alc)


design_iptw <- svydesign(ids = ~1, weights = ~stab_weight, data = alc_data)
alc_model <- svyglm(adult_alc_risky ~ total_home_alc, design = design_iptw, family = binomial)
summary(alc_model)
exp(coef(alc_model))
exp(confint(alc_model))
