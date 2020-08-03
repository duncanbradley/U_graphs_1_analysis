library(tidyverse)
library(zoo)
library(lme4)
library(lmerTest)
library(fitdistrplus)
library(naniar)
library(ggeffects)
library(parameters)
library(effectsize)
library(performance)

# DATA WRANGLING ####

# specifying conflict preference
select <- dplyr::select

# read in csv
df <- read_csv("U_graphs_1_data.csv",
                     col_types = cols(slider_2.rt = col_number()))

# select required columns
df <- df %>% select(participant,
                         graph_image,
                         slider_1.response,
                         slider_2.response,
                         slider_3.response,
                         key_resp_done.rt,
                         slider_1.rt,
                         slider_2.rt,
                         slider_3.rt,
                         block_name.thisTrialN,
                         choose_blocks.thisN,
                         key_resp_debrief.keys,
                         `Age *`,
                         `Gender *`,
                         `Highest level of education completed so far (e.g~ High school/grade (US); GCSE/Secondary School (UK); A-Levels; Bachelor's degree etc~) *`
)

# filter to remove cases where there is no entry in
# graph_image OR choose_blocks.thisN OR key_resp_debrief.rt
# THEN
# where there is a value in key_resp_debrief.rt, 
# change choose_blocks.thisN on that row to 7
# otherwise, leave choose_blocks.thisN as it is
# THEN
# remove key_resp_debrief.keys as it is no longer needed
df <- df %>% 
  filter(!is.na(graph_image) |
           !is.na(choose_blocks.thisN) |
           !is.na(key_resp_debrief.keys)) %>% 
  mutate(choose_blocks.thisN = 
           ifelse(!is.na(key_resp_debrief.keys), 7, 
                  choose_blocks.thisN)) %>%
  select(- key_resp_debrief.keys)

# fill in NAs in the choose_blocks.thisN column based on the next non-NA value
# so observations are carried backwards
df$choose_blocks.thisN <- na.locf(df$choose_blocks.thisN, fromLast = TRUE)

# remove rows without an observation in graph_image as they are no longer needed
df <- df %>% filter(!is.na(graph_image))

# add a new column, presentation
# this shows the presentation order across the whole experiment
df <- df %>%
  mutate(presentation = block_name.thisTrialN + (choose_blocks.thisN * 9))

# add a new column, RT
# this is the time from blank graph appearing to last slider response
# pmax is the parallel (rowwise) maximum
df <- df %>%
  mutate(total_RT = pmax(slider_1.rt, slider_2.rt, slider_3.rt))

# renaming columns
df <- df %>% rename(
    Gender = `Gender *`,
    Age = `Age *`,
    Education = starts_with("Highest level of education"))

# paste the name of the folders that contain the data
path <- "U_graphs_1/summary_stats"

# read the csvs
stats_summary <- read_csv(file.path(path, pattern = "all_summary.csv"))
axis_summary <- read_csv(file.path(path, pattern = "axis_summary.csv"))
blank_axis_summary <- read_csv(file.path(path, pattern = "blank_axis_summary.csv"))

# check that the axis summary and blank axis summary data are identical
all(axis_summary$graph_id == blank_axis_summary$graph_id)
all(axis_summary$`1` == blank_axis_summary$`1`)
all(axis_summary$`2` == blank_axis_summary$`2`)

# remove column with the x axis labels
# THEN
# recode the data in the 'label' column to refer to slider numbers
# this will ultimately match the column headings in df
# THEN
# pivot to wide format, with separate columns for each cluster and statistic
# names_glue() controls the ordering of column headers
# THEN
# graph_id is renamed graph_image so the column name matches df
stats_summary <- stats_summary %>%
  select(- x) %>%
  mutate(label = recode(label,
                        "gp1_label" = "slider_1",
                        "gp2_label" = "slider_2",
                        "gp3_label" = "slider_3")) %>%
  pivot_wider(names_from = label, values_from = c(mean, sd),
              names_glue = "{label}.{.value}") %>%
  rename(graph_image = graph_id)

# renaming columns
axis_summary <- axis_summary %>% rename(lower_lim = `1`,
                                        upper_lim = `2`,
                                        graph_image = graph_id)

# adding prefix and suffix to match graph_image in df
axis_summary$graph_image <- paste0("graphs/graph", axis_summary$graph_image, ".png")
stats_summary$graph_image <- paste0("graphs/graph", stats_summary$graph_image, ".png")

# joining df and axis summary by 'graph_image' column
df <- inner_join(df, axis_summary, by = "graph_image") 

# joining df and stats summary by 'graph_image' column
df <- inner_join(df, stats_summary, by = "graph_image")

# adding estimate columns, which translate participant response in numerical estimate
# the response is multiplied by the range and added on to the lower limit
df <- df %>% 
  mutate(slider_1.estimate = (lower_lim + (slider_1.response * (upper_lim - lower_lim))),
         slider_2.estimate = (lower_lim + (slider_2.response * (upper_lim - lower_lim))),
         slider_3.estimate = (lower_lim + (slider_3.response * (upper_lim - lower_lim))) 
         )

# reading in graphs_book1, which provided the stats used to build the graphs
graphs_book1 <- read_csv(file.path(pattern = "U_graphs_1/graphs_book1.csv"))

# adding prefix and suffix to match graph_image in df
graphs_book1$graph_image <- paste0("graphs/graph", graphs_book1$graph_id, ".png") 

# removing the contests of brackets from 'y_label' column
graphs_book1$y_label <- str_remove(graphs_book1$y_label, " \\(.*\\)")

# replace spaces with underscores
graphs_book1$y_label <- str_replace_all(graphs_book1$y_label, " ", "_")

# select the necessary columns from graphs_book1
# then join with df by 'graph_image'
df <- graphs_book1 %>%
  select(y_label,
         graph_image,
         ooo_pos,
         y_min,
         y_max
         ) %>%
  inner_join(df, ., by = "graph_image")

# adding z score columns:
# (estimate - mean)/sd
# 0 is perfect score
# positive z_score = over-estimation
# negative z_score = under-estimation
df <- df %>%
  mutate(slider_1.z_score = (slider_1.estimate - slider_1.mean)/slider_1.sd,
         slider_2.z_score = (slider_2.estimate - slider_2.mean)/slider_2.sd,
         slider_3.z_score = (slider_3.estimate - slider_3.mean)/slider_3.sd
  ) 

# First, I calculate the difference between the standard (same pop. mean)
# and the odd-one-out (different pop. mean)
# I take the average of the difference in z scores.
# This generates a standardised difference measure.
# I include rowwise() before this so that this is calculated for each row
# THEN
# New column 'difference sign' - whether the difference is positive or negative
# Positive = odd-one-out is higher than the standard
# Negative = odd-one-out is lower than the standard
# THEN
# change values in the 'difference' column to absolute (remove sign)
df <- df %>% 
  rowwise() %>%
  mutate(difference = case_when(ooo_pos == 1 ~ 
                                 (slider_1.mean - 
                                     mean(slider_2.mean, slider_3.mean)
                                  )/
                                   (y_max - y_min),
                                ooo_pos == 2 ~ 
                                  (slider_1.mean - 
                                     mean(slider_2.mean, slider_3.mean)
                                  )/
                                  (y_max - y_min),
                                ooo_pos == 3 ~ 
                                  (slider_1.mean - 
                                     mean(slider_2.mean, slider_3.mean)
                                  )/
                                  (y_max - y_min)
  )
  ) %>%
  mutate(difference_sign = case_when(difference > 0 ~ "Positive",
                                     difference < 0 ~ "Negative")) %>% 
  mutate(difference = abs(difference))

# pivoting to longer format
# 'cols' selects all the columns that have observations split by slider/cluster
# the new columns are 'slider', which contains the slider number
# and 'measure', which contains the measure term
# names_pattern defines how the new columns are populated
# the previous column names are separated by '.'
# everything in the first part goes to the 'slider' column, 
# everything in the second part goes to the 'measure' column
# THEN
# pivot_wider so that each measure gets a separate column, 
# containing corresponding values
df <- df %>% 
  pivot_longer(cols = c(slider_1.response:slider_3.response,
                        slider_1.rt:slider_3.rt,
                        slider_1.mean:slider_3.estimate,
                        slider_1.z_score:slider_3.z_score),
               names_to = c("slider", "measure"),
               names_pattern = "(.*)\\.(.*)") %>%
  pivot_wider(names_from = measure, values_from = value)

# removing "slider_" from values in the slider column
df <- df %>% 
  mutate(slider = str_replace(slider, ".*_", "")) 

# changing ooo_pos to character so that it can be compared to slider (which also character type)
df$ooo_pos <- as.character(df$ooo_pos)

# adding new column, 'is_ooo'
# TRUE if the row refers to observations on an 'odd-one-out' cluster
# FALSE if the row refers to observations that aren't odd-ones-out
df <- df %>%
  rowwise() %>%
  mutate(is_ooo = case_when(ooo_pos == slider ~ TRUE,
                            ooo_pos != slider ~ FALSE))

df <- df %>%
  rowwise() %>%
  mutate(height = ((mean - lower_lim)/(upper_lim - lower_lim)))

df %>% ggplot(aes(x = z_score,
                  y = (estimate-mean),
                  colour = y_label)) +
  geom_jitter(alpha= 0.1) +
  facet_wrap(~ y_label, scales = "free_y")

df %>% ggplot(aes(x = ,
                  y = difference,
                  colour = y_label)) +
  geom_jitter(alpha= 0.1) +
  facet_wrap(~ y_label, scales = "free_y")
  

# VISUALISATION ####

# visualising z_scores individually for each participant
# participant 71 produced the unusually high estimates
df %>% 
  ggplot(aes(x = participant,
             y = z_score)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun = mean, size = 0.1, colour = "red")

# visualisation of mean z-score against difference 
# for each slider on each trial
# suggests over-estimation,generally, varying at different levels of difference
df %>%
  ggplot(aes(x = difference,
             y = z_score)) +
  #geom_smooth(method = lm) +
  geom_smooth() +
  stat_summary(fun = mean, size = 0.1, 
               mapping = aes(colour = slider))

# there doesn't seem to be an interaction between difference and is_ooo
df %>% 
  ggplot(aes(x = difference,
             y = z_score,
             colour = is_ooo)) +
  geom_smooth(method = lm) +
  stat_summary(fun = mean, size = 0.1)

# there might be an interaction between difference and difference_sign
# as difference increases, accuracy gets worse on graphs with negative ooo
# and gets better on graphs with postive ooo
df %>%
  ggplot(aes(x = difference,
             y = z_score,
             colour = difference_sign)) +
  geom_smooth(method = lm) +
  stat_summary(fun = mean, size = 0.1)

# plotting z_score separately for positive and negative difference
# and separately for instances where
# cluster is the standard (FALSE)
# and cluster is the odd-one-out (TRUE)
# looks like there might be a slight tendency 
# to over-estimate the physically higher clusters
# (the standard in negative cases and the odd-one-out in positive cases)
df %>%
  ggplot(aes(x = difference_sign,
             y = z_score,
             colour = is_ooo)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1.8, 1.8))

# visualisation of mean z-score against z_mean (standardised actual height)
# suggests that over-estimation increases when clusters are higher on the y axis
df %>%
  ggplot(aes(x = height,
             y = z_score)) +
  #geom_jitter(alpha = 0.1) +
  geom_smooth(method = lm ) 
  #geom_smooth() +
  #stat_summary(fun = mean, size = 0.1)

# but maybe this is only true for the standard, not the ooo
df %>%
  ggplot(aes(x = height,
             y = z_score,
             colour = is_ooo)) +
  geom_smooth(method = lm)

# suggests the increase in error as height increases is bigger
# where there is a negative ooo than a positive ooo
df %>%
  ggplot(aes(x = height,
             y = z_score,
             colour = difference_sign)) +
  geom_smooth(method = lm) +
  stat_summary(fun = mean, size = 0.1)

# ANALYSIS ####

# checking for missing values
vis_miss(df)

# coding 'is_ooo' and 'difference_sign' as factors
df$is_ooo <- as.factor(df$is_ooo)
df$difference_sign <- as.factor(df$difference_sign)

# checking the distribution of the DV (z_score)
descdist(df$z_score)
# there is almost no skew but very high kurtosis - check effects on LMMs
hist(df$z_score)

# summary stats
mean(df$z_score)
# on average, there is slight over-estimation
sd(df$z_score)
lCI <- mean(df$z_score) - ((sd(df$z_score))*1.96)
uCI <- mean(df$z_score) + ((sd(df$z_score))*1.96)
lCI
uCI
uCI - lCI
# 95% of responses are within 3 cluster widths of the cluster centroid

# Is there evidence for over-estimation changing as a function of y_ axis position?
model1 <- lmer(z_score ~ height +
                 (1 | participant) + 
                 (1 | graph_image), 
               data = df)
model1_null <- lmer(z_score ~ 
                      (1 | participant) + 
                      (1 | graph_image), 
                    data = df)
anova(model1, model1_null)
summary(model1)
# yes

# Does accuracy differ as a function of difference magnitude (and sign)?
model2 <- lmer(z_score ~ difference:difference_sign +
                 (1 | participant) +
                 (1 | graph_image), 
               data = df)
model2_null <- lmer(z_score ~ 
                  (1 | participant) +
                  (1 | graph_image), 
                data = df)
anova(model2, model2_null)
# no evidence for full model

# is there an interaction between difference_sign and is_ooo?
model3 <- lmer(z_score ~ difference_sign*is_ooo +
                 (1 | participant) +
                 (1 | graph_image),
               data = df)
model3_null <- lmer(z_score ~ 
                 (1 | participant) +
                 (1 | graph_image),
               data = df)
anova(model3_null, model3)
summary(model3)# no evidence for full model

# is there an interaction between height and is_ooo?
model4 <- lmer(z_score ~ height*is_ooo +
                 (1 | participant) +
                 (1 | graph_image),
               data = df)
model4_null <- lmer(z_score ~ 
                      (1 | participant) +
                      (1 | graph_image),
                    data = df)
anova(model4_null, model4)
summary(model4)
# yes

# is there an interaction between height and difference sign?
model5 <- lmer(z_score ~ height*difference_sign +
                 (1 | participant) +
                 (1 | graph_image),
               data = df)
model5_null <- lmer(z_score ~ 
                      (1 | participant) +
                      (1 | graph_image),
                    data = df)
anova(model5_null, model5)
# no evidence for full model

# including an interaction and two additive fixed effects
model6 <- lmer(z_score ~ height*is_ooo + difference_sign + difference +
                 (1 | participant) +
                 (1 | graph_image),
               data = df)
model6_null <- lmer(z_score ~
                      (1 | participant) +
                      (1 | graph_image),
                    data = df)
anova(model6_null, model6)
summary(model6)

# removing the difference fixed effect 
# in order to make way for random participant slopes
model7 <- lmer(z_score ~ height*is_ooo + difference_sign +
                 (1 + height | participant) +
                 (1 | graph_image),
               data = df)
model7_null <- lmer(z_score ~ 
                      (1 + height | participant) +
                      (1 | graph_image),
                    data = df)
anova(model7_null, model7)
summary(model7)
anova(model7)

check_model(model7)
model_performance(model7)
plot(compare_performance(model7, 
                         model6, 
                         model5, 
                         model4, 
                         model3, 
                         model2, 
                         model1,
                         rank = TRUE))
plot(compare_performance(model7, model7_null))
plot(compare_performance(model7, model7_null, model4))
model_parameters(model7)
plot_model(model7,
           show.values = TRUE,
           value.offset = .4)
plot_model(model7, type = "pred", terms = c("height", "is_ooo"))
standardize_parameters(model6)
eta_squared(model6)
anova(model7)
F_to_eta2(
  f = c(16.2328),
  df = c(1),
  df_error = c(4733)
)

# investigating effect of learning:
df %>%
  ggplot(aes(x = presentation,
             y = z_score)) +
  stat_summary(fun = mean, size = 0.1, 
               mapping = aes(colour = slider)) +
  geom_smooth()

# no effect of learning
model8 <- lmer(z_score ~ presentation + 
                 (1 | participant) +
                 (1 | graph_image), 
               data = df)
model8_null <- lmer(z_score ~ 
                      (1 | participant) +
                      (1 | graph_image),
                    data = df)
anova(model8_null, model8)



# Overlapping/non-overlapping clusters?
# Bayes?

