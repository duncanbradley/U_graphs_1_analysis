library(tidyverse)
library(zoo)
library(lme4)
library(lmerTest)
library(fitdistrplus)
library(naniar)

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
         ooo_pos
         ) %>%
  inner_join(df, ., by = "graph_image")

# adding z score columns:
# (estimate - mean)/sd
# 0 is perfect score
# positive z_score = over-estimation
# negative z_score = under-estimation
df <- df %>% 
  mutate(slider_1.z_estimate = ((slider_1.estimate)/slider_1.sd),
         slider_2.z_estimate = ((slider_2.estimate)/slider_2.sd),
         slider_3.z_estimate = ((slider_3.estimate)/slider_3.sd),
         slider_1.z_mean = ((slider_1.mean)/slider_1.sd),
         slider_2.z_mean = ((slider_2.mean)/slider_2.sd),
         slider_3.z_mean = ((slider_3.mean)/slider_3.sd)
  ) %>%
  mutate(slider_1.z_score = (slider_1.z_estimate - slider_1.z_mean),
         slider_2.z_score = (slider_2.z_estimate - slider_2.z_mean),
         slider_3.z_score = (slider_3.z_estimate - slider_3.z_mean)
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
                                    mean(
                                      (slider_1.z_mean - slider_2.z_mean),
                                      (slider_1.z_mean - slider_3.z_mean)
                                    ),
                                ooo_pos == 2 ~ 
                                    mean(
                                      (slider_2.z_mean - slider_1.z_mean),
                                      (slider_2.z_mean - slider_3.z_mean)
                                    ),
                                ooo_pos == 3 ~ 
                                    mean(
                                      (slider_3.z_mean - slider_1.z_mean),
                                      (slider_3.z_mean - slider_2.z_mean)
                                    )
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
                        slider_1.z_estimate:slider_3.z_score),
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

# VISUALISATION ####

# visualisation of all data: shows slight overestimation at a uniform rate
df %>%
  ggplot(aes(x = difference,
             y = z_score)) +
  geom_point(alpha = 0.05
             ) +
  geom_smooth()

# visualising z_scores individually for each participant
df %>% 
  ggplot(aes(x = participant,
             y = z_score)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun = mean, size = 0.1, color = "red")

# coding 'is_ooo' as a factor
df$is_ooo <- as.factor(df$is_ooo)

# plotting z_score separately for positive and negative difference
# and separately for instances where
# cluster is the standard (FALSE)
# and cluster is the odd-one-out (TRUE)
df %>%
  ggplot(aes(x = difference_sign,
             y = z_score,
             colour = is_ooo,
            fill = is_ooo)) +
  geom_violin() +
  stat_summary(fun = mean, size = 0.5, color = "black", 
               position = position_dodge(width = 0.9))

# ANALYSIS ####

# checking for missing values
vis_miss(df)

# checking the distribution of the DV (z_score)
descdist(df$z_score)
# there is almost no skew but very high kurtosis - check effects on LMMs
hist(df$z_score)

# summary stats
mean(df$z_score)
# on average, there is slight over-estimation

# Does accuracy differ as a function of difference magnitude (and sign)?
#
# coding difference_sign as a factor
df$difference_sign <- as.factor(df$difference_sign)
#
# random effects for participant and y_label
# not adding random slopes for difference to random effects:
# 1. results in singular fit error when added to participant
# 2. would not expect any relationship between difference and y_label (block)
# (distribution of difference magnitude is similar across blocks)
#
# not adding random slopes for difference_sign to random effect
# 1. model doesn't converge when added to participant
# 2. would not expect any relationship between difference_sign and y_label
# (positive/negative difference numbers similar across blocks )
model1 <- lmer(z_score ~ difference + difference_sign + 
                 (1 | participant) + 
                 (1 | y_label), 
               data = df)
model1
# both fixed effects appear to make a significant contribution

# but no significant difference in z_scores based on difference or difference sign
# the intercept is significantly different from 0
# This suggests over-estimation in general, but not as a function of difference
summary(model1)


# what do the summary stats look like when grouped by 'is_ooo'?
df %>% 
  group_by(is_ooo) %>%
  summarise(mean = mean(z_score),
            sd = sd(z_score))

# Did accuracy differ for the standard vs the odd-one out?
# 
# coding 'is_ooo' as a factor
df$is_ooo <- as.factor(df$is_ooo)
#
# as previously mentioned, 
# there is no justification for added random slopes based on graph features
# to the random effects term 'y_label' (block)
#
# adding any fixed effects term as a random slope on participants
# results in singular fit or failure to converge
model2 <- lmer(z_score ~ is_ooo + difference + difference_sign + 
                 (1 | participant) +
                 (1 | y_label), 
               data = df)
model2
# all fixed effects are significant contributors
summary(model2)
# no fixed effects are significant predictors of z_score


