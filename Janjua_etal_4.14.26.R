#### Risk and Resilience (F31) FPS-EDA Analyses for Ujala SoBP ####
## Authored by Lana Ruvolo Grasser PhD, lgrasser@med.wayne.edu, and Ujala Janjua, hw9356@wayne.edu 
## Last updated on 26 November 2025
## Written in R version 4.3.1

#### Import Data and Libraries ####

#Clear existing data and graphics
rm(list=ls())
graphics.off()

#Set working directory
setwd("~/Documents/Research/Stress Risk and Resilience in Syrian and Iraqi Refugees and Survivors of Torture/Data/F31") # Edit this line to match the folder path for where your data is housed
getwd()

#Load in libraries
library(Hmisc)
library(tidyverse)
library(dplyr)
library(psych)
library(psy)
library(eeptools)
library(datarium)
library(car)
library(emmeans)
library(psycho)
library(lme4)
library(huxtable)
library(sjPlot)
library(mice)
library(VIM)
library(finalfit)
library(norm)
library(data.table)
library(ggpubr)
library(rstatix)
library(nlme)
library(multcompView)
library(lsmeans)
library(rcompanion)
library(RColorBrewer)
library(dlookr)
library(ggplot2)
library(apaTables)
library(broom)
library(ggstatsplot)
library(corrplot)
library(lavaan)
library(lavaanPlot)
library(lcmm)
library(reshape2)
library(stats)
library(backports)
library(effects)
library(interactions)
library(lmerTest)
library(plyr)
library(lattice)
library(jtools)
library(reghelper)
library(merTools)
library(psy)
library(pwr)
library(RcmdrMisc)
library(datarium)
library(onewaytests)
library(epiDisplay)
library(cocor)
library(jtools)
library(stargazer)
library(ggstance)
library(interactions)
library(jtools)
library(stargazer)
library(rockchalk)
library(huxtable)
library(ggeffects)
library(car)
library(moments)
library(semPlot)
library(MBESS)
library(sjstats)
# If you are missing any of these libraries, use install.packages("library name")

# # Read in data file
# df <- read.csv("F31_UJ_04.14.25.csv", na.strings=c("NA","NA")) # na.strings recodes all blank cells to NA
# str(df)
# 
# ## Recode sex as a factor variable with labels
# df$Sex <- as.factor(df$Sex)
# 
# #### Descriptive Statistics ####
# 
# #Non-continuous variables
# 
# Sex.Distribution <- lapply(df%>%select_if(is.factor), function(x){table(x)})
# Sex.Distribution
# # 35 F, 36 M
# 
# colnames(df)
# 
# # Continuous variables
# colnames(df)
# numSummary(df[,c("Age_at_Visit","Cumulative_Trauma","Total_PTSD","Reexperiencing","Avoidance","Hyperaousal",
#                  "Dissociative"),
#               drop=FALSE], statistics=c("mean", "sd", "se(mean)", "IQR", "quantiles","cv","skewness","kurtosis",
#                                         "range"),quantiles=c(0,.25,.5,.75,1))
# 
# demo <- df %>% dplyr::select(Age_at_Visit,Cumulative_Trauma,Total_PTSD,Reexperiencing,Avoidance,Hyperaousal,
#                               Dissociative)
# describeBy(demo, df$Sex, mat = TRUE)
# 
# #### Sex Differences ####
# for (i in demo) {
#   print(t.test(i ~ df$Sex))
# }
# 
# #### Impute Missing Data ####
# 
# library(naniar)
# library(VIM)
# library(mice)
# library(GGally)
# library(finalfit)
# 
# colnames(df)
# 
# # df_bind = all of the variables we are NOT going to impute
# df_bind <- df %>% dplyr::select(SID,DOB,Date_of_Visit,Age_Dichotomous,Sex,Family_Number,Religion,Height,Weight,Subjective_Physical_Health,
#                                 Subjective_Mental_Health,Education,Race_Ethnicity,Medical_Condition,Medication)
# # df_impute = all of the variables we ARE going to impute
# df_impute <- df %>% dplyr::select(Age_at_Visit,Baseline_30,SCR,Average_SCL,Cumulative_Trauma,Total_PTSD,Reexperiencing,Avoidance,Negative,
#                                   Hyperaousal,Dissociative,
#                                   EDA_ACQ_A1_SQRT, EDA_ACQ_A2_SQRT, EDA_ACQ_A3_SQRT,
#                                   EDA_ACQ_B1_SQRT, EDA_ACQ_B2_SQRT, EDA_ACQ_B3_SQRT,
#                                   EDA_EXT_A1_SQRT, EDA_EXT_A2_SQRT, EDA_EXT_A3_SQRT, EDA_EXT_A4_SQRT,
#                                   EDA_EXT_B1_SQRT, EDA_EXT_B2_SQRT, EDA_EXT_B3_SQRT, EDA_EXT_B4_SQRT)
# 
# # Now we are going to check for whether data is missing completely at random or not
# mcar(df_impute)
# naniar::mcar_test(df_impute)
# 
# ## Separating variables to screen for differences in missingness based on demographic factors
# explanatory <- c("Age_Dichotomous", "Sex")
# dependent <- c("EDA_ACQ_A1_SQRT", "EDA_ACQ_A2_SQRT", "EDA_ACQ_A3_SQRT",
#                                   "EDA_ACQ_B1_SQRT", "EDA_ACQ_B2_SQRT", "EDA_ACQ_B3_SQRT",
#                                   "EDA_EXT_A1_SQRT", "EDA_EXT_A2_SQRT", "EDA_EXT_A3_SQRT", "EDA_EXT_A4_SQRT",
#                                   "EDA_EXT_B1_SQRT", "EDA_EXT_B2_SQRT", "EDA_EXT_B3_SQRT", "EDA_EXT_B4_SQRT")
# 
# ## Example of screening missingness based on three demographic variables
# df %>% missing_pairs(dependent, explanatory)
# 
# dependent <- ("irr.0")
# for (i in dependent) {
#   print(missing_mcar <- df %>%
#           missing_compare(i, explanatory))
#         missing_mcar
# }
# 
# ## Check the amount of missing data
# sapply(df_impute, function(x) sum(is.na(x)))
# 
# ## Give the proportion of cases with missing data for each variable
# pMiss <- function(x){sum(is.na(x))/length(x)*100}
# sapply(df_impute, pMiss)
# 
# ## Replace missing data using multiple imputation.
# tempData8 <- mice(df_impute, m=8, maxit=50, meth='pmm',seed=500)
# 
# ## Get all the data together from each imputation and create the new dataset to work with
# df_imputed <- complete(tempData8,1)
# 
# ## Ensure that we do not have any more missing data
# sapply(df_imputed, function(x) sum(is.na(x)))
# 
# ## Bind the missing data to the original dataset
# df_imputed <- cbind(df_bind, df_imputed)
# 
# ## Write out imputed dataset
# write.csv(df_imputed, "F31_UJ_Imputed_12.02.2025.csv")

#### START HERE ####
df_imputed <- read.csv("F31_UJ_Imputed_12.02.2025.csv", na.strings=c("NA","NA")) # na.strings recodes all blank cells to NA
str(df)

#### Sex Differences ####
EDAvars <- df_imputed %>% dplyr::select("EDA_ACQ_A1_SQRT", "EDA_ACQ_A2_SQRT", "EDA_ACQ_A3_SQRT","EDA_ACQ_B1_SQRT", "EDA_ACQ_B2_SQRT", "EDA_ACQ_B3_SQRT",
               "EDA_EXT_A1_SQRT", "EDA_EXT_A2_SQRT", "EDA_EXT_A3_SQRT", "EDA_EXT_A4_SQRT",
               "EDA_EXT_B1_SQRT", "EDA_EXT_B2_SQRT", "EDA_EXT_B3_SQRT", "EDA_EXT_B4_SQRT")
for (i in EDAvars) {
  print(t.test(i ~ df_imputed$Sex))
}

#### Correlations ####
print(corr.test(EDAvars,EDAvars,use = "pairwise",method = "spearman",adjust = "none",ci=TRUE),short=FALSE)
print(corr.test(EDAvars,df_imputed$Age_at_Visit,use = "pairwise",method = "spearman",adjust = "holm",ci=TRUE),short=FALSE)


#### ACQUISITION RMANOVAs ####

library(psych)
library(tidyr)
library(dplyr)
library(rstatix)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(xts)

# ACQ <- df_imputed %>%
#   gather(key = "Stimulus", value = "EDA", EDA_ACQ_A1_SQRT, EDA_ACQ_A2_SQRT, EDA_ACQ_A3_SQRT, 
#          EDA_ACQ_B1_SQRT, EDA_ACQ_B2_SQRT, EDA_ACQ_B3_SQRT) %>%
#   convert_as_factor(SID, Stimulus)
# 
# write.csv(ACQ, "ACQ_RMANOVA_12.02.25.csv") # Labeling stimulus as 1 = CS+, 2 = CS- and adding Block column
ACQ <- read.csv("ACQ_RMANOVA_12.02.25.csv")

ACQ$Stimulus <- 
  factor(ACQ$Stimulus, levels=c('1','2'),
         labels=c("CS+",
                  "CS-"))
ACQ$Block <- 
  factor(ACQ$Block, levels=c('1','2','3'),
         labels=c("Block 1",
                  "Block 2",
                  "Block 3"))

ACQ %>%
  group_by(Block, Stimulus) %>%
  get_summary_stats(EDA, type = "mean_sd")

ACQ %>%
  group_by(Sex) %>%
  get_summary_stats(EDA, type = "mean_sd")

bxp <- ggboxplot(
  ACQ, x = "Block", y = "EDA",
  color = "Stimulus", palette = c("#E29FB7","#05443A") # Change to lab color palette for manuscript 
)
bxp
ggsave(filename = "Acquisition Box Plot.jpg", plot = last_plot(), width = 6, height = 4)

# Computation
res.aov0 <- anova_test(
  data = ACQ, dv = EDA, wid = SID, 
  within = c(Block,Stimulus), formula = EDA ~ Block + Stimulus
)
get_anova_table(res.aov0)

res.aov1 <- anova_test(
  data = ACQ, dv = EDA, wid = SID, 
  within = c(Block,Stimulus), formula = EDA ~ Block * Stimulus
)
get_anova_table(res.aov1)

res.aov1.1 <- anova_test(
  data = ACQ, dv = EDA, wid = SID, 
  between = Sex, within = c(Block,Stimulus), formula = EDA ~ Sex + Block + Stimulus
)
get_anova_table(res.aov1.1)

res.aov1.2 <- anova_test(
  data = ACQ, dv = EDA, wid = SID, 
  between = Sex, within = c(Block,Stimulus)
)
get_anova_table(res.aov1.2)

# Posthoc tests for non-significant effects
ACQ %>%
  pairwise_t_test(
    EDA ~ Block, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
 
ACQ %>%
  pairwise_t_test(
    EDA ~ Stimulus, paired = TRUE,
    p.adjust.method = 
  )

# Visualization with p-values
bxp +
  labs(
    subtitle = get_test_label(res.aov1, detailed = TRUE)
  )
ggsave(filename = "Acquisition Box Plot w Summary Stat.jpg", plot = last_plot(), width = 6, height = 4)

#### ANCOVA Model with Cumulative_Trauma ACQ ####

res.aov2 <- ACQ %>%
  anova_test(EDA ~ Block * Stimulus * Cumulative_Trauma)
get_anova_table(res.aov2)

#### Acquisition ANCOVA with Total PTSD ####

res.aov3 <- ACQ %>%
  anova_test(EDA ~ Block * Stimulus * Total_PTSD)
get_anova_table(res.aov3)

res.aov3.1 <- ACQ %>%
  anova_test(EDA ~ Block * Stimulus * Total_PTSD * Sex)
get_anova_table(res.aov3.1)
eta_squared(res.aov3.1)

library(lme4)
model <- lmer(
  EDA ~ Block * Stimulus * Total_PTSD * Sex + (1 | SID),
  data = ACQ
)
summary(model)
ptsd_mean <- mean(ACQ$Total_PTSD, na.rm = TRUE)
ptsd_sd <- sd(ACQ$Total_PTSD, na.rm = TRUE)
ptsd_vals <- c(
  "Mean - 1 SD" = ptsd_mean - ptsd_sd,
  "Mean" = ptsd_mean,
  "Mean + 1 SD" = ptsd_mean + ptsd_sd
)
library(emmeans)
emm <- emmeans(
  model,
  ~ Block * Stimulus * Sex | Total_PTSD,
  at = list(Total_PTSD = ptsd_vals)
)
emm_table <- as.data.frame(emm)
emm_table

library(dplyr)
library(tidyr)

final_table <- emm_table |>
  mutate(
    PTSD_level = factor(
      Total_PTSD,
      levels = ptsd_vals,
      labels = names(ptsd_vals)
    ),
    emmean = round(emmean, 3),
    SE = round(SE, 3)
  ) |>
  dplyr::select(Block, Stimulus, Sex, PTSD_level, emmean, SE) |>
  pivot_wider(
    names_from = PTSD_level,
    values_from = c(emmean, SE),
    names_glue = "{PTSD_level}_{.value}"
  ) |>
  arrange(Block, Stimulus, Sex)

final_table

library(flextable)

final_table |>
  flextable() |>
  autofit()

library(ggplot2)

plot_data <- emm_table |>
  mutate(
    PTSD_level = factor(
      Total_PTSD,
      levels = ptsd_vals,
      labels = c("-1 SD", "Mean", "+1 SD")
    )
  )

ggplot(plot_data, aes(x = Block, y = emmean, color = Stimulus, group = Stimulus)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1) +
  facet_grid(Sex ~ PTSD_level) +
  labs(
    y = "Estimated Skin Conductance Response",
    x = "Block",
    color = "Stimulus"
  ) +
  scale_color_manual(values = c("#E29FB7","#05443A")) +
  theme_minimal(base_size = 12)

stimulus_effects <- contrast(
  emm,
  method = "pairwise",
  by = c("Block", "Sex", "Total_PTSD"),
  adjust = "bonferroni"
)

as.data.frame(stimulus_effects)

block_effects <- contrast(
  emm,
  method = "pairwise",
  by = c("Stimulus", "Sex", "Total_PTSD"),
  adjust = "bonferroni"
)

as.data.frame(block_effects)

library(emmeans)

ptsd_trends <- emtrends(
  model,
  ~ Block * Stimulus * Sex,
  var = "Total_PTSD"
)

summary(ptsd_trends)

# Posthoc by Sex
aov3.2 <- ACQ %>%
  group_by(Sex) %>%
  anova_test(EDA ~ Block * Stimulus * Total_PTSD)
aov3.2

bxp <- ggboxplot(
  ACQd, x = "Block", y = "EDA",
  color = "Stimulus", palette = c("#E29FB7","#05443A"),
  facet.by = c("PTSD","Sex"), short.panel.labs = FALSE
)
bxp
ggsave(filename = "Acquisition Box Plot by PTSD and Sex.jpg", plot = last_plot(), width = 6, height = 4)

#### Repeated Measures ANOVA, Extinction ####

# EXT <- df_imputed %>%
#   gather(key = "Stimulus", value = "EDA", EDA_EXT_A1_SQRT, EDA_EXT_A2_SQRT, EDA_EXT_A3_SQRT, EDA_EXT_A4_SQRT, 
#          EDA_EXT_B1_SQRT, EDA_EXT_B2_SQRT, EDA_EXT_B3_SQRT, EDA_EXT_B4_SQRT) %>%
#   convert_as_factor(SID, Stimulus)
# 
# write.csv(EXT, "EXT_RMANOVA_12.02.25.csv") # Labeling stimulus as 1 = CS+, 2 = CS- and adding Block column
EXT <- read.csv("EXT_RMANOVA_12.02.25.csv")

EXT$Stimulus <- 
  factor(EXT$Stimulus, levels=c('1','2'),
         labels=c("CS+",
                  "CS-"))
EXT$Block <- 
  factor(EXT$Block, levels=c('1','2','3','4'),
         labels=c("Block 1",
                  "Block 2",
                  "Block 3",
                  "Block 4"))

EXT %>%
  group_by(Block, Stimulus) %>%
  get_summary_stats(EDA, type = "mean_sd")

bxp <- ggboxplot(
  EXT, x = "Block", y = "EDA",
  color = "Stimulus" # Change to lab color palette for manuscript 
)
bxp

# Computation
res.aov5 <- anova_test(
  data = EXT, dv = EDA, wid = SID, 
  within = c(Block,Stimulus), formula = EDA ~ Block + Stimulus
)
get_anova_table(res.aov5)

res.aov6 <- anova_test(
  data = EXT, dv = EDA, wid = SID, 
  within = c(Block,Stimulus), formula = EDA ~ Block * Stimulus
)
get_anova_table(res.aov6)

# Posthoc tests for signficant effects
one.way1 <- EXT %>%
  group_by(Stimulus) %>%
  anova_test(dv = EDA, wid = SID, within = Block) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1

one.way2 <- EXT %>%
  group_by(Block) %>%
  anova_test(dv = EDA, wid = SID, within = Stimulus) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

pwc1 <- EXT %>%
  group_by(Stimulus) %>%
  pairwise_t_test(
    EDA ~ Block, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

pwc2 <- EXT %>%
  group_by(Block) %>%
  pairwise_t_test(
    EDA ~ Stimulus, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2

# Visualization with p-values
pwc1 <- pwc1 %>% add_xy_position(x = "Block")
bxp +
  stat_pvalue_manual(pwc1, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov5, detailed = TRUE),
    caption = get_pwc_label(pwc1)
  )

#### ANCOVA Model with Cumulative_Trauma EXT ####

res.aov6 <- EXT %>%
  anova_test(EDA ~ Block * Stimulus * Cumulative_Trauma)
get_anova_table(res.aov6)

# Simple Main Effect of Block
EXT %>%
  group_by(Stimulus) %>%
  anova_test(EDA ~ Block)

# Simple Main Effect of Stimulus
EXT %>%
  group_by(Block) %>%
  anova_test(EDA ~ Stimulus)

# Simple Pairwise Comparisons
pwc3 <- EXT %>%
  emmeans_test(
    EDA ~ Block, covariate = Cumulative_Trauma,
    p.adjust.method = "bonferroni"
  )
pwc3

pwc4 <- EXT %>%
  group_by(Block) %>%
  emmeans_test(
    EDA ~ Stimulus, 
    p.adjust.method = "bonferroni"
  )
pwc4

pwc5 <- EXT %>%
  emmeans_test(
    EDA ~ Block,
    p.adjust.method = "bonferroni"
  )
pwc5

pwc6 <- EXT %>%
  emmeans_test(
    EDA ~ Stimulus,
    p.adjust.method = "bonferroni"
  )
pwc6

EXT %>%
  group_by(Block, Stimulus) %>%
  get_summary_stats(EDA, type = "mean_sd")

EXTs <- EXT

EXTs <- EXTs %>%
  mutate(Trauma = case_when(Cumulative_Trauma > 4.01 ~ '1',
                            Cumulative_Trauma < 4.0001 ~ '0'))

EXTs$Trauma <- 
  factor(EXTs$Trauma, levels=c('0','1'),
         labels=c("Low",
                  "High"))

EXTs %>%
  group_by(Trauma, Block) %>%
  get_summary_stats(EDA, type = "mean_sd")

#### ANCOVA EXT with Total PTSD ####

res.aov7 <- EXT %>%
  anova_test(EDA ~ Block * Stimulus * Total_PTSD)
get_anova_table(res.aov7)




