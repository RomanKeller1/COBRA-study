library(tidyverse)
library(arrow)
library(nlme)
library(readr)
library(car); library(ggplot2); library(nlme); library(reshape)
library(lme4)
library(sjstats)
library(effectsize)
library(gap)
library(DHARMa)
library(arrow)
library(lmerTest)
library(effects)
library(MuMIn)
library(dsem)
library(moments)
library(readr)
library(sjPlot)
options(scipen = 999)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Singapore/01_PhD_CDHI/10_PhD_Thesis/17_COBRA_study/04_Data_Analysis/01_Preprocessed/new/new_respot_neprocomp/")

####################### Next-prompt-straight-lining model #####################
df_nepro_careless <- arrow::read_parquet("./df_nepro_careless.parquet")
colnames(df_nepro_careless)[colnames(df_nepro_careless) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_nepro_careless
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])

res_nepro_longstring_multi <-glmer(next_prompt_longstring ~ sleep_quality + stress + affect + hunger + fatigue+ obscomp_steps  + age + gender + incentive_category + factor(day_of_monitoring) + factor(time_of_the_day_categories) + day_of_the_week_binary + ipi + (1 | subject_id) + (1 | subject_id:day_of_monitoring) + (1 | subject_id:time_of_the_day_categories_dummy), family=binomial, data=df_standardized, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e9)))
summary(res_nepro_longstring_multi)
est_nepro_longstring_multi <- data.frame(coef(summary(res_nepro_longstring_multi)))
est_nepro_longstring_multi$or <- exp(est_nepro_longstring_multi[,1])
est_nepro_longstring_multi$lb <- exp(est_nepro_longstring_multi[,1] - 1.96 * est_nepro_longstring_multi[,2])
est_nepro_longstring_multi$ub <- exp(est_nepro_longstring_multi[,1] + 1.96 * est_nepro_longstring_multi[,2])
round(est_nepro_longstring_multi, 3)

write.csv(round(est_nepro_longstring_multi, 3), file = "next_prompt_straightlining_cofficients_with_odds_ratios_incentive.csv")

#### Correlations cognitive and motivational predictors: 
correlation_matrix <- cor(df_standardized[c("affect", "stress", "fatigue", "hunger")])
print(correlation_matrix)




####################### Next-prompt compliance model #######################
df_nepro <- arrow::read_parquet("./df_full_no_last_prompt_full_surveys.parquet")
colnames(df_nepro)[colnames(df_nepro) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_nepro
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])

res_neprom_multi <-glmer(next_prompt_compliance ~ sleep_quality + stress + affect + hunger + fatigue+ obscomp_steps + age + gender + incentive_category + factor(day_of_monitoring) + factor(time_of_the_day_categories_dummy) + day_of_the_week_binary + ipi +(1 | subject_id) + (1 | subject_id:day_of_monitoring) + (1 | subject_id:time_of_the_day_categories_dummy), family=binomial, data=df_standardized, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e9)))
summary(res_neprom_multi)
est_neprom_multi <- data.frame(coef(summary(res_neprom_multi)))
est_neprom_multi$or <- exp(est_neprom_multi[,1])
est_neprom_multi$lb <- exp(est_neprom_multi[,1] - 1.96 * est_neprom_multi[,2])
est_neprom_multi$ub <- exp(est_neprom_multi[,1] + 1.96 * est_neprom_multi[,2])
round(est_neprom_multi, 3)
write.csv(round(est_neprom_multi, 3), file = "next_prompt_compliance_cofficients_with_odds_ratios_incentive.csv")


####################### TPI model ###########################

df_tpi <- arrow::read_parquet("./df_tpi.parquet")
colnames(df_tpi)[colnames(df_tpi) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_tpi
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])
df_standardized$ct_per_survey_item[df_standardized$ct_per_survey_item <= 1.0] <- NaN
df_standardized$ct_per_survey_item[df_standardized$ct_per_survey_item > 12.16071] <- NaN
res_ct_per_item <- lmer(ct_per_survey_item ~ sleep_quality + stress + affect + hunger + fatigue+ obscomp_steps+ age  +  gender + incentive_category + factor(day_of_monitoring) + factor(time_of_the_day_categories) + day_of_the_week_binary + (1 | subject_id) + (1 | subject_id:day_of_monitoring) + (1 | subject_id:time_of_the_day_categories), data=df_standardized)
summary(res_ct_per_item)
ci <- confint(res_ct_per_item)
ci <- round(ci, 3)
ci
write.csv(ci, file = "tpi_one_upper_fence_95%CI_coefficients_incentive.csv")
write.csv(summary(res_ct_per_item)$coefficients, file = "tpi_one_upper_fence_coefficients_incentive.csv")

################### TPI model - sensitivity analysis #########################


df_tpi <- arrow::read_parquet("./df_tpi.parquet")
colnames(df_tpi)[colnames(df_tpi) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_tpi
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])
df_standardized$ct_per_survey_item[df_standardized$ct_per_survey_item <= 1.5] <- NaN
df_standardized$ct_per_survey_item[df_standardized$ct_per_survey_item > 12.16071] <- NaN
res_ct_per_item <- lmer(ct_per_survey_item ~ sleep_quality + stress + affect + hunger + fatigue+ obscomp_steps+ age  +  gender + incentive_category + factor(day_of_monitoring) + factor(time_of_the_day_categories) + day_of_the_week_binary + (1 | subject_id) + (1 | subject_id:day_of_monitoring) + (1 | subject_id:time_of_the_day_categories), data=df_standardized)
summary(res_ct_per_item)
ci <- confint(res_ct_per_item)
ci <- round(ci, 3)
ci
write.csv(ci, file = "tpi_one_upper_fence_95%CI_coefficients_sensitivity_incentive.csv")
write.csv(summary(res_ct_per_item)$coefficients, file = "tpi_one_upper_fence_coefficients_sensitivity_incentive.csv")


####################### Response time model ################################
df_resp_ti <- arrow::read_parquet("./df_response_delay_10mins.parquet")
colnames(df_resp_ti)[colnames(df_resp_ti) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_resp_ti
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])
res_rt_multi <- lmer(log(response_delay_10min) ~ sleep_quality + stress + affect + hunger + fatigue+ obscomp_steps_ten + age  +  gender + incentive_category + factor(day_of_monitoring) + factor(time_of_the_day_categories) + day_of_the_week_binary + (1 | subject_id) + (1 | subject_id:day_of_monitoring) + (1 | subject_id:time_of_the_day_categories), data=df_standardized)
summary(res_rt_multi)
# confidence intervals (95%)
ti <- confint(res_rt_multi)
round(ti, 3)
ti 
write.csv(ti, file = "response_delay_95%CI_coefficients_incentive.csv")
write.csv(summary(res_rt_multi)$coefficients, file = "response_delay_coefficients_incentive.csv")



