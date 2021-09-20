library(tidyverse)
library(sf)
library(tmap)
library(survival)
library(ggplot2)
library(dplyr)
library(ranger)
library(ggfortify)
library(survminer)
library(janitor)

setwd("C:/Users/CHRISTINA/OneDrive - University College London/UCL/Vagrant/notebooks/Dissertation/Camden High Streets")

camden_survival <-  read_csv("camden-final-survival-full-with-dimensions.csv")

camden_survival <- camden_survival %>% 
  mutate(vacant_null = replace(vacant, vacant==0, NA)) %>% 
  mutate(sub_cat_clean = str_replace_all(sub_category, "[[:punct:]]", " "))

camden_survival %>%
    clean_names()

names(camden_survival) <- gsub(" ", "_", names(camden_survival))

colnames(camden_survival)

camden_survival$`Car_Parks_(Multi-Storey)`

# Kaplan Meier Survival Curve
km <- with(camden_survival, Surv(survival, vacant_null))
head(km,80)


km_fit <- survfit(Surv(survival, vacant) ~ 1, data=camden_survival)
summary(km_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_fit)

km_trt_fit <- survfit(Surv(survival, vacant) ~ category, data=camden_survival)
ggsurvplot(km_trt_fit, data = camden_survival, conf.int = TRUE, risk.table = T, risk.table.height = 0.4, pval = T)
autoplot(km_trt_fit)

fit <- survfit(Surv(survival, vacant) ~ sub_category, data=camden_survival)
summary(fit, times = c(1,30,60,90*(1:10)))
ggsurv <- ggsurvplot(fit, data = camden_survival, conf.int = TRUE, risk.table = F, risk.table.height = 0.4, pval = T, legend = c(0.7, 0.2))
ggsurv
ggsave("ggsurv.png")

cox0 <- coxph(Surv(survival, vacant) ~ category, data = camden_survival)
summary(cox0)
cox.zph(cox0)
ggforest(cox0, data = camden_survival)
ggplot(Predict(cox0, category))

cox <- coxph(Surv(survival, vacant) ~ sub_category, data = camden_survival)
summary(cox)
model_sub_cat <- summary(cox)
model_sub_cat$coefficients
write.csv(model_sub_cat$coefficients, "surv.csv")
cox.zph(cox)
ggforest(cox, data = camden_survival)

cox1 <- coxph(Surv(survival, vacant) ~ use_category, data = camden_survival)
summary(cox1)
cox.zph(cox1)
ggforest(cox1, data = camden_survival)


anova(cox1, cox, test="LRT")

cox_transport <- coxph(Surv(survival, vacant) ~ bus_stops + train + Car_Spaces + `Car_Parks_(Multi-Storey)` + `Car_Parks_(Surfaced_Open)`,  data = camden_survival)
summary(cox_transport)
test.ph <- cox.zph(cox_transport)

write.csv(test.ph, "transport steinfield.csv")
ggcoxzph(test.ph)
ggforest(cox_transport, data = camden_survival)
anova(cox)


cox_crime<- coxph(Surv(survival, vacant) ~ crime + Over_65s + `Average_data_usage_(GB)` + `Density_(number_of_persons_per_hectare)`,  data = camden_survival)
summary(cox_crime)
ggforest(cox_crime, data = camden_survival)

cox_econ <- coxph(Surv(survival, vacant) ~ median_18Q4 + count_18Q4 + household_estimate_12_13 + mean_rates_paid + mean_rental_valuation + mean_rates_expected + Economically_inactive_total,  data = camden_survival)
summary(cox_econ)
ggforest(cox_econ, data = camden_survival)

cox_rates <- coxph(Surv(survival, vacant) ~ mean_rates_paid + mean_rental_valuation + mean_rates_expected,  data = camden_survival)
summary(cox_rates)
cox.zph(cox_rates)
ggforest(cox_rates, data = camden_survival)

cox_environ <- coxph(Surv(survival, vacant) ~ trees + noise_pollution + NOxTotal10 + PM10Tot10,  data = camden_survival)
summary(cox_environ)
cox.zph(cox_environ)
ggforest(cox_environ, data = camden_survival)


cox_physical <- coxph(Surv(survival, vacant) ~ bench + fountain + camera_surveillance + post_box + telephone + recycling + public_toilets,  data = camden_survival)
summary(cox_physical)
cox.zph(cox_physical)
ggforest(cox_physical, data = camden_survival)

