library(tidyverse)
library(MASS)
library(stargazer)
data <- read_csv("Data/govt_culture.csv")

# regressions with government response + other indexes as response variable
model_govt_resp <- lm(GovernmentResponseIndex ~ pdi + idv + mas + uai + ltowvs + ivr, data=data)
model_govt_resp <- stepAIC(model_govt_resp, scope = ~., trace=0)
summary(model_govt_resp)

model_stringency <- lm(StringencyIndex ~ pdi + idv + mas + uai + ltowvs + ivr, data=data)
model_stringency <- stepAIC(model_stringency, scope = ~., trace=0)
summary(model_stringency)

model_cont_health <- lm(ContainmentHealthIndex ~ pdi + idv + mas + uai + ltowvs + ivr, data=data)
model_cont_health <- stepAIC(model_cont_health, scope = ~., trace=0)
summary(model_cont_health)

# regressions with death rate and recovered rate as reponse varibales
data2 <- read_csv("Data/govt_culture_cases.csv")
model_death_rate <- lm(death_rate ~ pdi + idv + mas + uai + ltowvs + ivr, data=data2)
model_death_rate <- stepAIC(model_death_rate, scope = ~., trace=0)
summary(model_death_rate)

model_recovered_rate <- lm(recovered_rate ~ pdi + idv + mas + uai + ltowvs + ivr, data=data2)
model_recovered_rate <- stepAIC(model_recovered_rate, scope = ~., trace=0)
summary(model_recovered_rate)

