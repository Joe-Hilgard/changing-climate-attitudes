library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(psych)
library(BayesFactor)
library(lme4)

# Experiment 2 ----
# I would like to check out this no-pretest control group, but not sure which col that is
exp2.1 = read.csv("UCp_mech_core_intervention_notext.csv")
exp2.1$sample = "UC-Berkeley"
exp2.2 = read.csv("UT_mech_core_intervention_notext.csv")
exp2.2$sample = "UT-Brownsville"
exp2.2 = separate(exp2.2, survey_number, c('f11', 'survey_number'), sep = "-")
exp2.2$survey_number = as.numeric(as.character(exp2.2$survey_number))
# We're gonna combine these data frames so let's make sure survey_number is distinct
intersect(exp2.1$survey_number, exp2.2$survey_number) # not distinct
exp2.2$survey_number = exp2.2$survey_number + 1000
intersect(exp2.1$survey_number, exp2.2$survey_number) # distinct

# Looks like only n_s == s has pre/post (altho sometimes pre-only?)
# n_s == n looks like the post-only control
table(exp2.1$n_s, exp2.1$survey_number)
table(exp2.2$n_s, exp2.2$survey_number)

# remove knowledge, ghg, light, energy, columns for convenience
exp2.1 = exp2.1 %>% select(X:reread, total.score, sample)
exp2.2 = exp2.2 %>% select(X:reread, total.score, sample)

# Combine datasets into one master. Add composite columns.
# NOTE: Should n_s == "n" be excluded at this point??
exp2 = bind_rows(exp2.1, exp2.2) %>% 
  mutate(gw_mean = (gw1_2 + gw2_1 + gw2_2 + gw2_3 + gw2_4)/5,
         gw_plus_mean = (gw_mean*5 + lifestyle)/6)
# Make pre/post into a factor, set levels for plotting convenience  
exp2$time = factor(exp2$pre_post, levels = c("pre", "post"))

# Check combined dataset for number of conservatives
table(exp2$conservative, useNA = 'always')
barplot(table(exp2$conservative, useNA = 'always'))
table(exp2$party, useNA = 'always') # 8 republicans out of 175

# Flatten dataset for ggplot2's facet_grid
exp2.flat = exp2 %>%
  gather(key = "item", value = "value", starts_with("gw"))
 
# Plot pre/post changes among democrats & republicans for each item
exp2.flat %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  filter(!(item %in% c("gw_mean", "gw_plus_mean"))) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = survey_number)) +
  facet_grid(party~item)

# Plot pre/post changes for two aggregate strategies
exp2.flat %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  #separate(item, into = c("item", "time"), sep = -4) %>% 
  filter(item %in% c("gw_mean", "gw_plus_mean")) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = survey_number)) +
  facet_grid(party~item)

# Can I reproduce their analyses?
# Paired-samples t-tests and other within-subjects pre-post analyses
# Full-sample analysis
exp2.m1 = lmer(gw_mean ~ pre_post + (1|survey_number), data = exp2) 
exp2.m2 = lmer(gw_mean ~ pre_post*conservative + (1|survey_number), data = exp2)
exp2.m3 = lmer(gw_plus_mean ~ pre_post + (1|survey_number), data = exp2) 
exp2.m4 = lmer(gw_plus_mean ~ pre_post*conservative + (1|survey_number), data = exp2)

summary(exp2.m1); Anova(exp2.m1, type = 3)
summary(exp2.m2); Anova(exp2.m2, type = 3)
summary(exp2.m3); Anova(exp2.m3, type = 3)
summary(exp2.m4); Anova(exp2.m4, type = 3)

# Per manuscript, split by school
# Berkeley -- Doesn't match yet, not sig. Sample size wrong?
# One composite
exp2.m1.ucb = lmer(gw_mean ~ pre_post + (1|survey_number), 
     data = exp2[exp2$sample == "UC-Berkeley",]) 
exp2.m2.ucb = lmer(gw_mean ~ pre_post*conservative + (1|survey_number), 
     data = exp2[exp2$sample == "UC-Berkeley",])
# Other composite
exp2.m3.ucb = lmer(gw_plus_mean ~ pre_post + (1|survey_number), 
     data = exp2[exp2$sample == "UC-Berkeley",]) 
exp2.m4.ucb = lmer(gw_plus_mean ~ pre_post*conservative + (1|survey_number), 
     data = exp2[exp2$sample == "UC-Berkeley",]) 

summary(exp2.m1.ucb); Anova(exp2.m1.ucb, type = 3)
summary(exp2.m2.ucb); Anova(exp2.m2.ucb, type = 3)
summary(exp2.m3.ucb); Anova(exp2.m3.ucb, type = 3)
summary(exp2.m4.ucb); Anova(exp2.m4.ucb, type = 3)

# Brownville -- p-value seems about right
# One composite
exp2.m1.utb = lmer(gw_mean ~ pre_post + (1|survey_number), 
     data = exp2[exp2$sample == "UT-Brownsville",])
exp2.m2.utb = lmer(gw_mean ~ pre_post*conservative + (1|survey_number), 
     data = exp2[exp2$sample == "UT-Brownsville",])
# Other composite
exp2.m3.utb = lmer(gw_plus_mean ~ pre_post + (1|survey_number), 
     data = exp2[exp2$sample == "UT-Brownsville",])
exp2.m4.utb = lmer(gw_plus_mean ~ pre_post*conservative + (1|survey_number), 
     data = exp2[exp2$sample == "UT-Brownsville",])

summary(exp2.m1.utb); Anova(exp2.m1.utb, type = 3)
summary(exp2.m2.utb); Anova(exp2.m2.utb, type = 3)
summary(exp2.m3.utb); Anova(exp2.m3.utb, type = 3)
summary(exp2.m4.utb); Anova(exp2.m4.utb, type = 3)

# How curious that adding "conservative" as a predictor has such an effect on the main effect.
# Perhaps more conservative people are more likely to drop out?
# TODO: Count up number of obs per subject, see if conservatives drop out?

# Make pre/post difference scores and run t-test
makeDiff = function(x) return(x[2] - x[1]) # function subtracts "pre" from "post"
# Demonstration: 
# demo = data.frame("ID" = c(1, 1, 2, 2), 
#                   "time" = c("pre", "post", "pre", "post"), 
#                   "value" = c(4, 5, 6, 7))
# demo %>% group_by(ID) %>% summarize_each(funs(makeDiff), value)
diffs = exp2 %>% 
  group_by(survey_number) %>% 
  summarize_each(funs(makeDiff), starts_with("gw"))
diffs$pre_post = "diff"

# names(diffs)[-1] = paste(names(diffs)[-1], "diff", sep = "_")
# exp2.diff = right_join(diffs, exp2, by = "survey_number") %>% 
#   filter(pre_post == "post", n_s == "s") %>% 
#   mutate(gw_composite_diff = gw1_2_diff + gw2_1_diff + gw2_2_diff + gw2_3_diff + gw2_4_diff)
# exp2 %>% filter(survey_number == 1) %>% select(starts_with("gw"),)
# exp2.diff %>% filter(survey_number == 1) %>% select(starts_with("gw"))

# Add difference scores as further rows to original dataset.
# TODO: double-check the direction of difference.
exp2 = bind_rows(exp2, diffs) %>% 
  arrange(survey_number) %>% 
  select(-X)
exp2 %>% select(survey_number, pre_post, starts_with("gw"))

# Make dataset of difference scores ----
# TODO: get the subject-level stuff back on here (e.g. political views)
exp2.diff = exp2 %>% 
  select(survey_number, party, conservative, sample, pre_post, starts_with("gw")) %>% 
  filter(pre_post == "diff") %>% 
  mutate(gw_composite = gw1_2 + gw2_1 + gw2_2 + gw2_3 + gw2_4)

# one-sample t-tests of difference scores
#  something's not quite right
t.test(exp2.diff$gw1_2)
t.test(exp2.diff$gw2_1)
t.test(exp2.diff$gw2_2)
t.test(exp2.diff$gw2_3)
t.test(exp2.diff$gw2_4)
t.test(exp2.diff$gw_composite)
# t.test(exp2.diff$gw_composite[exp2.diff$sample == "UC-Berkeley"]) # does not match
# t.test(exp2.diff$gw_composite[exp2.diff$sample == "UT-Brownsville"]) # approx match


# Bayes factors via model comparison ----
# Code and discard those who dropped out before post
exp2.bf = exp2 %>% 
  filter(n_s == "s") %>%  # Drop post-only
  select(survey_number, gw_composite, pre_post, conservative) %>% 
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

exp2.m4/exp2.m3 # evidence against interaction -- perhaps they are right

# TODO: run again with gw_plus_mean
# TODO: run BayesFactor models with party code?



# Experiment 3 ----
exp3.1 = read.csv("UCo_mech_core_intervention_notext.csv") 
exp3.2 = read.csv("UCo_mech_full_intervention_notext.csv") 
# Prune columns I don't need
exp3.1 = exp3.1 %>% 
  select(-starts_with("k1time"), -starts_with("k2time"), -starts_with("k3time"),
         -starts_with("codes."), -starts_with("coder."))
exp3.2 = exp3.2 %>% 
  select(-starts_with("k1time"), -starts_with("k2time"), -starts_with("k3time"),
         -starts_with("codes."), -starts_with("coder."),
         -starts_with("Q"))
exp3.1$sample = "core"
exp3.2$sample = "full"

# Combine datasets and make composite outcome.
# Appears that no suffix implies pretest so we'll make the names conform
# Also we'll make composites for conservatism and gw outcomes
# TODO: use alpha() to assess wisdom of composite outcome
exp3 = bind_rows(exp3.1, exp3.2) %>% 
  rename(gw1_2_pre = gw1_2,
         gw2_1_pre = gw2_1,
         gw2_2_pre = gw2_2,
         gw2_3_pre = gw2_3,
         gw2_4_pre = gw2_4,
         engage_pre = engage,
         lifsty_pre = lifesty) %>% 
  mutate(gw_mean_pre = (gw1_2_pre + gw2_1_pre + gw2_2_pre + gw2_3_pre + gw2_4_pre)/5,
         gw_plus_mean_pre = (gw_mean_pre*5 + engage_pre + lifsty_pre)/7,
         gw_mean_pst = (gw1_2_pst + gw2_1_pst + gw2_2_pst + gw2_3_pst + gw2_4_pst)/5,
         gw_plus_mean_pst = (gw_mean_pst*5 + engage_pst + lifsty_pst)/7,
         gw_mean_fol = (gw1_2_fol + gw2_1_fol + gw2_2_fol + gw2_3_fol + gw2_4_fol)/5,
         gw_plus_mean_fol = (gw_mean_fol*5 + engage_fol + lifsty_fol)/7,
         conservative = social.cons + econ.cons) 
# Also appears to be no subject identifier so we'll add one
exp3$ID = factor(1:nrow(exp3))

# See how many conservatives we've got.
# Conservatism items: social and economic. Most data is missing.
table(exp3$social.cons, useNA='always')
barplot(table(exp3$social.cons, useNA = 'always')) 
table(exp3$econ.cons, useNA='always')
barplot(table(exp3$econ.cons, useNA = 'always'))
# Political party. Most are "none", "NA", or "democrat"
table(exp3$party, useNA = 'always')
table(exp3$party.pre, useNA = 'always')

# Flatten dataset and separate() columns by timepoint
exp3.flat = exp3 %>% 
  # drop cols for ease of View()
  select(-starts_with("pni"), -starts_with("evo"), -starts_with("dty"), 
         -starts_with("natmil"), -ends_with(".study"), -ends_with(".delay")) %>% 
  gather(key = "item", value = "value", 
         starts_with("gw"), starts_with("engage"), starts_with("lifsty")) %>% 
  separate(col = item, into = c("item", "time"), sep = -4)  %>% 
  mutate(item = substr(item, 1, nchar(item) - 1)) %>% # drop the excess underscore for tidiness
  mutate(time = factor(time, levels = c("pre", "pst", "fol"))) # rearrange levels for plotting 

# Spread items back out so that I can refer to gw_mean or gw_plus_mean
exp3.wide = exp3.flat %>% 
  filter(item %in% c("gw_mean", "gw_plus_mean")) %>% 
  spread(key = item, value = value)

# Debugging ----
View(exp3.flat[exp3.flat$ID == 1,] )

# Analyses ----
# NOTE: May be necessary yet to filter according to completeness
# Pre/post LMER
exp3.m1 = lmer(gw_mean ~ time + (1|ID), data = exp3.wide)
exp3.m2 = lmer(gw_mean ~ time * conservative + (1|ID), data = exp3.wide)
exp3.m3 = lmer(gw_plus_mean ~ time + (1|ID), data = exp3.wide)
exp3.m4 = lmer(gw_plus_mean ~ time * conservative + (1|ID), data = exp3.wide)

summary(exp3.m1); Anova(exp3.m1, type = 3)
summary(exp3.m2); Anova(exp3.m2, type = 3)
summary(exp3.m3); Anova(exp3.m3, type = 3)
summary(exp3.m4); Anova(exp3.m4, type = 3)

# Plots ----
# NOTE: Something's wrong here!!! Where's my time == "pre" data?
## Is it possible that nobody with "pre" data indicated their politics??

# Plot pre/post changes among democrats & republicans for each item
exp3.flat %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  filter(!(item %in% c("gw_mean", "gw_plus_mean"))) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = ID)) +
  facet_grid(party~item)

# Plot pre/post changes for two aggregate strategies
exp3.flat %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  filter(item %in% c("gw_mean", "gw_plus_mean")) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = ID)) +
  facet_grid(party~item)

# Plot pre/post changes, collapsing across parties
# Individual items
exp3.flat %>% 
  filter(!(item %in% c("gw_mean", "gw_plus_mean"))) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = ID)) +
  facet_grid(~item)

# Aggregates
exp3.flat %>% 
  filter(item %in% c("gw_mean", "gw_plus_mean")) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = ID)) +
  facet_grid(~item)

# Experiment 4 ----
exp4.1 = read.csv("cco_mech_core_intervention_notext.csv")
exp4.2 = read.csv("cco_mech_delayed_test_notext.csv")

table(exp4.1$party) # you guys only have 8 republicans in the whole sample
barplot(table(exp4.1$conserv), xlab = "conservatism") # sample leans left
table(exp4.1$party, exp4.1$conserv)

# are all the delayed people in the pre-post dataset?
intersect(exp4.1$subj_id, exp4.2$subj_id)
setdiff(exp4.1$subj_id, exp4.2$subj_id)
setdiff(exp4.2$subj_id, exp4.1$subj_id) # yes

# Make composite of gw items, gw + engage + lifesty items
# Maybe other studies should aggregate into "democrat", "other/NA", "republican" for plotting
exp4 = left_join(exp4.1, exp4.2, by = "subj_id") %>% 
  mutate(party_3lvl = mapvalues(party, 
                                c("democrat", "independent", "none", "other", "republican"),
                                c("democrat", "other", "other", "other", "republican")),
         gw_mean_pre = (gw1_2_pre + gw2_1_pre + gw2_2_pre + gw2_3_pre + gw2_4_pre)/5,
         gw_plus_mean_pre = (gw_mean_pre*5 + engage_pre + lifsty_pre)/7,
         gw_mean_pst = (gw1_2_pst + gw2_1_pst + gw2_2_pst + gw2_3_pst + gw2_4_pst)/5,
         gw_plus_mean_pst = (gw_mean_pst*5 + engage_pst + lifsty_pst)/7,
         gw_mean_fol = (gw1_2_fol + gw2_1_fol + gw2_2_fol + gw2_3_fol + gw2_4_fol)/5,
         gw_plus_mean_fol = (gw_mean_fol*5 + engage_fol + lifsty_fol)/7) %>% 
  mutate(ID = factor(subj_id))

# Flatten dataset and separate() columns by timepoint
# drop cols for ease of View()
exp4.flat = exp4 %>% 
  select(-starts_with("k1time"), -starts_with("k2time"), -starts_with("k3time"),
         -starts_with("evo"), -starts_with("dty"), 
         -starts_with("natmil"), -ends_with(".study"), -ends_with(".delay")) %>%   
  gather(key = "item", value = "value", 
         starts_with("gw"), starts_with("engage"), starts_with("lifsty")) %>% 
  separate(col = item, into = c("item", "time"), sep = -4)  %>% 
  mutate(item = substr(item, 1, nchar(item) - 1)) %>% # drop the excess underscore for tidiness
  mutate(time = factor(time, levels = c("pre", "pst", "fol"))) # rearrange levels for plotting 

# Spread items back out so that I can refer to gw_mean or gw_plus_mean
exp4.wide = exp4.flat %>% 
  filter(item %in% c("gw_mean", "gw_plus_mean")) %>% 
  spread(key = item, value = value)

# Analysis ----
exp4.m1 = lmer(gw_mean ~ time + (1|ID), data = exp4.wide)
exp4.m2 = lmer(gw_mean ~ time * conserv + (1|ID), data = exp4.wide)
exp4.m3 = lmer(gw_plus_mean ~ time + (1|ID), data = exp4.wide)
exp4.m4 = lmer(gw_plus_mean ~ time * conserv + (1|ID), data = exp4.wide)

summary(exp4.m1); Anova(exp4.m1, type = 3)
summary(exp4.m2); Anova(exp4.m2, type = 3)
summary(exp4.m3); Anova(exp4.m3, type = 3)
summary(exp4.m4); Anova(exp4.m4, type = 3)

# Plotting ----
# Plot pre/post changes among democrats & republicans for each item
exp4.flat %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  filter(!(item %in% c("gw_mean", "gw_plus_mean"))) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = ID)) +
  facet_grid(party~item)

# Plot pre/post changes for two aggregate strategies
exp4.flat %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  filter(item %in% c("gw_mean", "gw_plus_mean")) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = ID)) +
  facet_grid(party~item)

# Plot pre/post changes, collapsing across parties
# Individual items
exp4.flat %>% 
  filter(!(item %in% c("gw_mean", "gw_plus_mean"))) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = ID)) +
  facet_grid(~item)

# Aggregates
exp4.flat %>% 
  filter(item %in% c("gw_mean", "gw_plus_mean")) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = ID)) +
  facet_grid(~item)

# Bayesian models ----
temp = exp4.flat %>% 
  group_by(subj_id, party, party_3lvl, conserv, time) %>% 
  summarise(value = sum(value)) 

exp4.bf = exp4.flat %>% 
  left_join(exp4.bf)
  group_by() %>% 
  #filter(complete.cases(.)) %>% 
  mutate(subj_id = as.factor(subj_id)) %>% 
  as.data.frame


# Experiment 5 ----
# No political information
exp5 = read.csv("HS_mech+stats_full_intervention.csv")
exp5$time = factor(exp5$time, levels = c("pre", "post", "followup"))

exp5 %>% 
  gather(item, value, GW1:GW4) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point() +
  geom_line(aes(group = Code)) +
  facet_wrap(~item)
