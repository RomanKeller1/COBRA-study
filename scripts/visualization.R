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
library(arrow)
library(dplyr)
library(lme4)
library(ggplot2)
library(gridExtra)


#options(pillar.sigfig=3)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Singapore/01_PhD_CDHI/10_PhD_Thesis/17_COBRA_study/04_Data_Analysis/01_Preprocessed/new/new_respot_neprocomp/")



library(arrow)
library(dplyr)
library(lme4)
library(ggplot2)
library(gridExtra)

# Function to create forest plot
create_forest_plot <- function(data, title) {
  ggplot(data, aes(x = Predictor, y = or)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dotted", color = "red") +
    xlim(0.5, 1.5) +
    coord_flip() +
    xlab("") +
    ylab("Odds Ratio") +
    ggtitle(title) +
    theme_minimal()
}

# Plot 1
df_nepro_careless <- arrow::read_parquet("./df_nepro_careless.parquet")
colnames(df_nepro_careless)[colnames(df_nepro_careless) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_nepro_careless
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])
rename_map <- c(
  sleep_quality = "Sleep_Quality",
  stress = "Stress",
  affect = "Positive_Affect",
  hunger = "Hunger",
  fatigue = "Fatigue",
  age = "Age",
  gender = "Gender",
  incentive_category = "Incentive_Category",
  day_of_monitoring = "Chronological_Study_Days",
  time_of_the_day_categories_dummy = "Time_Of_The_Day",
  day_of_the_week_binary = "Day_Of_Week",
  ipi = "IPI",
  obscomp_steps = "Number_Of_Missed_Prior_Prompts"
)
df_standardized <- df_standardized %>%
  rename_with(~ rename_map[.x], .cols = names(rename_map))
res_nepro_longstring_multi <- glmer(next_prompt_longstring ~ Sleep_Quality + Stress + Positive_Affect + Hunger + Fatigue + Number_Of_Missed_Prior_Prompts + Age + Gender + Incentive_Category + factor(Chronological_Study_Days) + factor(Time_Of_The_Day) + Day_Of_Week + IPI + (1 | subject_id) + (1 | subject_id:Chronological_Study_Days) + (1 | subject_id:Time_Of_The_Day), family = binomial, data = df_standardized, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e9)))
est_nepro_longstring_multi <- data.frame(coef(summary(res_nepro_longstring_multi)))
est_nepro_longstring_multi$or <- exp(est_nepro_longstring_multi[, 1])
est_nepro_longstring_multi$lb <- exp(est_nepro_longstring_multi[, 1] - 1.96 * est_nepro_longstring_multi[, 2])
est_nepro_longstring_multi$ub <- exp(est_nepro_longstring_multi[, 1] + 1.96 * est_nepro_longstring_multi[, 2])
est_nepro_longstring_multi$Predictor <- rownames(est_nepro_longstring_multi)
est_nepro_longstring_multi <- est_nepro_longstring_multi[!grepl("(Intercept)", est_nepro_longstring_multi$Predictor), ]
desired_order <- c(
  "Sleep_Quality", "Stress", "Positive_Affect", "Hunger", "Fatigue", "Age", "Gender", "Incentive Category",
  "Time_Of_The_Day 2", "Time_Of_The_Day 3", "Time_Of_The_Day 4", "Time_Of_The_Day 5",
  "Chronological_Study_Days 2", "Chronological_Study_Days 3", "Chronological_Study_Days 4", "Chronological_Study_Days 5", "Chronological_Study_Days 6", "Chronological_Study_Days 7", "Chronological_Study_Days 8", "Chronological_Study_Days 9",
  "Day_Of_Week", "IPI", "Number_Of_Missed_Prior_Prompts"
)
est_nepro_longstring_multi$Predictor <- gsub("factor\\(([^)]+)\\)", "\\1", est_nepro_longstring_multi$Predictor)
est_nepro_longstring_multi$Predictor <- gsub("_", " ", est_nepro_longstring_multi$Predictor)
est_nepro_longstring_multi$Predictor <- gsub("([a-zA-Z])([0-9])", "\\1 \\2", est_nepro_longstring_multi$Predictor)
est_nepro_longstring_multi$Predictor <- factor(est_nepro_longstring_multi$Predictor, levels = rev(gsub("_", " ", desired_order)))
selected_vars <- c("Sleep Quality", "Stress", "Positive Affect", "Hunger", "Fatigue")
filtered_effect_sizes <- est_nepro_longstring_multi[est_nepro_longstring_multi$Predictor %in% selected_vars, ]
filtered_effect_sizes$Predictor <- factor(filtered_effect_sizes$Predictor, levels = rev(selected_vars))
forest_nepro_longstring_five_pred <- create_forest_plot(filtered_effect_sizes, "Next-prompt Straight-lining")

# Plot 2
df_nepro <- arrow::read_parquet("./df_full_no_last_prompt_full_surveys.parquet")
colnames(df_nepro)[colnames(df_nepro) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_nepro
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])
rename_map <- c(
  sleep_quality = "Sleep_Quality",
  stress = "Stress",
  affect = "Positive_Affect",
  hunger = "Hunger",
  fatigue = "Fatigue",
  age = "Age",
  gender = "Gender",
  incentive_category = "Incentive_Category",
  day_of_monitoring = "Chronological_Study_Days",
  time_of_the_day_categories_dummy = "Time_Of_The_Day",
  day_of_the_week_binary = "Day_Of_Week",
  ipi = "IPI",
  obscomp_steps = "Number_Of_Missed_Prior_Prompts"
)
df_standardized <- df_standardized %>%
  rename_with(~ rename_map[.x], .cols = names(rename_map))
res_neprom_multi <- glmer(next_prompt_compliance ~ Sleep_Quality + Stress + Positive_Affect + Hunger + Fatigue + Number_Of_Missed_Prior_Prompts + Age + Gender + Incentive_Category + factor(Chronological_Study_Days) + factor(Time_Of_The_Day) + Day_Of_Week + IPI + (1 | subject_id) + (1 | subject_id:Chronological_Study_Days) + (1 | subject_id:Time_Of_The_Day), family = binomial, data = df_standardized, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e9)))
est_neprom_multi <- data.frame(coef(summary(res_neprom_multi)))
est_neprom_multi$or <- exp(est_neprom_multi[, 1])
est_neprom_multi$lb <- exp(est_neprom_multi[, 1] - 1.96 * est_neprom_multi[, 2])
est_neprom_multi$ub <- exp(est_neprom_multi[, 1] + 1.96 * est_neprom_multi[, 2])
est_neprom_multi$Predictor <- rownames(est_neprom_multi)
est_neprom_multi <- est_neprom_multi[!grepl("(Intercept)", est_neprom_multi$Predictor), ]
desired_order <- c(
  "Sleep_Quality", "Stress", "Positive_Affect", "Hunger", "Fatigue", "Age", "Gender", "Incentive Category",
  "Time_Of_The_Day 2", "Time_Of_The_Day 3", "Time_Of_The_Day 4", "Time_Of_The_Day 5",
  "Chronological_Study_Days 2", "Chronological_Study_Days 3", "Chronological_Study_Days 4", "Chronological_Study_Days 5", "Chronological_Study_Days 6", "Chronological_Study_Days 7", "Chronological_Study_Days 8", "Chronological_Study_Days 9",
  "Day_Of_Week", "IPI", "Number_Of_Missed_Prior_Prompts"
)
                  

# Plot 2 (continued)
est_neprom_multi$Predictor <- gsub("factor\\(([^)]+)\\)", "\\1", est_neprom_multi$Predictor)
est_neprom_multi$Predictor <- gsub("_", " ", est_neprom_multi$Predictor)
est_neprom_multi$Predictor <- gsub("([a-zA-Z])([0-9])", "\\1 \\2", est_neprom_multi$Predictor)
est_neprom_multi$Predictor <- factor(est_neprom_multi$Predictor, levels = rev(gsub("_", " ", desired_order)))
selected_vars <- c("Sleep Quality", "Stress", "Positive Affect", "Hunger", "Fatigue")
filtered_effect_sizes <- est_neprom_multi[est_neprom_multi$Predictor %in% selected_vars, ]
filtered_effect_sizes$Predictor <- factor(filtered_effect_sizes$Predictor, levels = rev(selected_vars))
forest_nepro_five_pred <- create_forest_plot(filtered_effect_sizes, "Next-prompt Compliance")

# Plot 3
df_resp_ti <- arrow::read_parquet("./df_response_delay_10mins.parquet")
colnames(df_resp_ti)[colnames(df_resp_ti) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_resp_ti
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])
rename_map <- c(
  sleep_quality = "Sleep_Quality",
  stress = "Stress",
  affect = "Positive_Affect",
  hunger = "Hunger",
  fatigue = "Fatigue",
  age = "Age",
  gender = "Gender",
  incentive_category = "Incentive_Category",
  day_of_monitoring = "Chronological_Study_Days",
  time_of_the_day_categories = "Time_Of_The_Day",
  day_of_the_week_binary = "Day_Of_Week",
  obscomp_steps_ten = "Unresponded_Prompts_In_10_Minutes"
)
df_standardized <- df_standardized %>%
  rename_with(~ rename_map[.x], .cols = names(rename_map))
res_rt_multi <- lmer(log(response_delay_10min) ~ Sleep_Quality + Stress + Positive_Affect + Hunger + Fatigue + Unresponded_Prompts_In_10_Minutes + Age + Gender + Incentive_Category + factor(Chronological_Study_Days) + factor(Time_Of_The_Day) + Day_Of_Week + (1 | subject_id) + (1 | subject_id:Chronological_Study_Days) + (1 | subject_id:Time_Of_The_Day), data = df_standardized)
fixed_effects <- fixef(res_rt_multi)
effect_sizes <- data.frame(
  Predictor = names(fixed_effects)[-1],
  Estimate = fixed_effects[-1]
)
vcov_matrix <- vcov(res_rt_multi)
se_values <- sqrt(diag(vcov_matrix))
effect_sizes$SE <- se_values[-1]
effect_sizes$CI_lower <- effect_sizes$Estimate - 1.96 * effect_sizes$SE
effect_sizes$CI_upper <- effect_sizes$Estimate + 1.96 * effect_sizes$SE
desired_predictors <- c("Sleep_Quality", "Stress", "Positive_Affect", "Hunger", "Fatigue")
effect_sizes <- effect_sizes %>% filter(Predictor %in% desired_predictors)
effect_sizes$Predictor <- gsub("factor\\(([^)]+)\\)", "\\1", effect_sizes$Predictor)
effect_sizes$Predictor <- gsub("_", " ", effect_sizes$Predictor)
effect_sizes$Predictor <- gsub("([a-zA-Z])([0-9])", "\\1 \\2", effect_sizes$Predictor)
effect_sizes$Predictor <- factor(effect_sizes$Predictor, levels = rev(gsub("_", " ", desired_predictors)))
forest_rt_multi <- ggplot(effect_sizes, aes(x = Predictor, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  xlim(-0.2, 0.2) +
  coord_flip() +
  xlab("") +
  ylab("Estimate") +
  ggtitle("log(Response Delay)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 4
df_tpi <- arrow::read_parquet("./df_tpi.parquet")
colnames(df_tpi)[colnames(df_tpi) == "subject_id_roman_x"] <- "subject_id"
df_standardized <- df_tpi
variables_to_scale <- c("sleep_quality", "stress", "affect", "hunger", "fatigue", "age", "obscomp_steps", "obscomp_steps_ten", "ipi")
df_standardized[, variables_to_scale] <- scale(df_standardized[, variables_to_scale])
df_standardized$ct_per_survey_item[df_standardized$ct_per_survey_item <= 1.0] <- NaN
df_standardized$ct_per_survey_item[df_standardized$ct_per_survey_item > 12.16071] <- NaN
rename_map <- c(
  sleep_quality = "Sleep_Quality",
  stress = "Stress",
  affect = "Positive_Affect",
  hunger = "Hunger",
  fatigue = "Fatigue",
  age = "Age",
  gender = "Gender",
  incentive_category = "Incentive_Category",
  day_of_monitoring = "Chronological_Study_Days",
  time_of_the_day_categories = "Time_Of_The_Day",
  day_of_the_week_binary = "Day_Of_Week",
  obscomp_steps = "Number_Of_Missed_Prior_Prompts"
)
df_standardized <- df_standardized %>%
  rename_with(~ rename_map[.x], .cols = names(rename_map))
res_ct_per_item <- lmer(ct_per_survey_item ~ Sleep_Quality + Stress + Positive_Affect + Hunger + Fatigue + Number_Of_Missed_Prior_Prompts + Age + Gender + Incentive_Category + factor(Chronological_Study_Days) + factor(Time_Of_The_Day) + Day_Of_Week + (1 | subject_id) + (1 | subject_id:Chronological_Study_Days) + (1 | subject_id:Time_Of_The_Day), data = df_standardized)
fixed_effects <- fixef(res_ct_per_item)
effect_sizes <- data.frame(
  Predictor = names(fixed_effects)[-1],
  Estimate = fixed_effects[-1]
)
vcov_matrix <- vcov(res_ct_per_item)
se_values <- sqrt(diag(vcov_matrix))
effect_sizes$SE <- se_values[-1]
effect_sizes$CI_lower <- effect_sizes$Estimate - 1.96 * effect_sizes$SE
effect_sizes$CI_upper <- effect_sizes$Estimate + 1.96 * effect_sizes$SE
desired_predictors <- c("Sleep_Quality", "Stress", "Positive_Affect", "Hunger", "Fatigue")
effect_sizes <- effect_sizes %>% filter(Predictor %in% desired_predictors)
effect_sizes$Predictor <- gsub("factor\\(([^)]+)\\)", "\\1", effect_sizes$Predictor)
effect_sizes$Predictor <- gsub("_", " ", effect_sizes$Predictor)
effect_sizes$Predictor <- gsub("([a-zA-Z])([0-9])", "\\1 \\2", effect_sizes$Predictor)
effect_sizes$Predictor <- factor(effect_sizes$Predictor, levels = rev(gsub("_", " ", desired_predictors)))
forest_ct_per_item <- ggplot(effect_sizes, aes(x = Predictor, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  xlim(-0.2, 0.2) +
  coord_flip() +
  xlab("") +
  ylab("Estimate") +
  ggtitle("Time per Item") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine the four plots into a 2x2 panel plot
grid.arrange(
  forest_nepro_longstring_five_pred,
  forest_nepro_five_pred,
  forest_rt_multi,
  forest_ct_per_item,
  ncol = 2,
  nrow = 2
)


# Define the layout matrix
layout_matrix <- rbind(c(2, 1),
                       c(3, 4))

# Combine the four plots into a 2x2 panel plot using the layout matrix
grid.arrange(
  forest_nepro_longstring_five_pred,
  forest_nepro_five_pred,
  forest_rt_multi,
  forest_ct_per_item,
  layout_matrix = layout_matrix
)




summary(res_nepro_longstring_multi)
summary(res_neprom_multi)
summary(res_ct_per_item)
summary(res_rt_multi)



