library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(BayesFactor)

# Experiment 2 ----
exp2 = read.csv("./hilgard_cleaned_data/exp2.csv")

# Bayes factors via model comparison ----
# Code and discard those who dropped out before post
exp2.bf = exp2 %>% 
  filter(n_s == "s") %>%  # Drop post-only
  select(survey_number, gw_mean, gw_plus_mean, pre_post, conservative) %>% 
  filter(complete.cases(.)) %>% # Drop casewise missing
  mutate(survey_number = as.factor(survey_number)) 
# See who doesn't have both timepoints
tab = table(exp2.bf$survey_number) 
tab[tab == 1]
tab[tab == 1] %>% names
# drop them
exp2.bf = exp2.bf %>% 
  filter(!survey_number %in% names(tab[tab==1])) %>% 
  as.data.frame()

# Could plot distribution of change scores given T1 score
# or distribution of change scores given conservative

# run BayesFactor models
exp2.m0 = lmBF(gw_mean ~ survey_number, data = exp2.bf)
exp2.m1 = lmBF(gw_mean ~ pre_post + survey_number, data = exp2.bf)
exp2.m2 = lmBF(gw_mean ~ conservative + survey_number, data = exp2.bf,
               iterations = 5e3)
exp2.m3 = lmBF(gw_mean ~ conservative + pre_post + survey_number, data = exp2.bf,
               iterations = 1e5)
exp2.m4 = lmBF(gw_mean ~ conservative * pre_post + survey_number, data = exp2.bf, 
               iterations = 1e5)

exp2.bayesResult = c(exp2.m0, exp2.m1, exp2.m2, exp2.m3, exp2.m4)
exp2.bayesResult
plot(exp2.bayesResult)

exp2.m1/exp2.m0 # 58:1 evidence for efficacy of intervention
exp2.m3/exp2.m2 # 49:1 evidence for efficacy of intervention, ctrl for conserv
exp2.m4/exp2.m3 # evidence against interaction -- perhaps they are right

# TODO: run again with gw_plus_mean
# TODO: run BayesFactor models with party code?

# Experiment 