library(tidyverse)
library(lme4)
library(lmerTest)
library(zoo)
library(fitdistrplus)
library(performance)
library(naniar)
library(broom.mixed)
library(interactions)
library(patchwork)
library(ggforce)

# DATA WRANGLING ####

# specifying conflict preference
select <- dplyr::select
rename <- dplyr::rename


# read in csv
df <- read_csv("U_graphs_1_data.csv")

# select required columns
df <- df %>% select(participant,
                    graph_image,
                    slider_1.response,
                    slider_2.response,
                    slider_3.response,
                    slider_1.rt, 
                    slider_2.rt,
                    slider_3.rt,
                    block_name.thisTrialN,
                    choose_blocks.thisN,
                    key_resp_debrief.keys,
                    time_taken,
                    `Age *`,
                    `Gender *`,
                    `Highest level of education completed so far (e.g~ High school/grade (US); GCSE/Secondary School (UK); A-Levels; Bachelor's degree etc~) *`
)

# in this dataframe, there are rows that contain no useful data
# this is due to the recording of consent prior to beginning the experiment
# and because `choose_blocks.thisN`, which shows the presentation ordering of blocks,
# is only recorded at the end of each block
# except for the final block, so `key_resp_debrief.rt` is used as a temporary placeholder

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

# add a new column, graph_order
# this shows the presentation order of individual graphs across the whole experiment
df <- df %>%
  mutate(graph_order = block_name.thisTrialN + (choose_blocks.thisN * 9))

# add a new column, RT
# this is the time from blank graph appearing to last slider response
# last slider response is the maximum value (this accounts for revisiting sliders)
# pmax is the parallel (rowwise) maximum
# THEN 
# remove individual reaction time columns
df <- df %>%
  mutate(total_RT = pmax(slider_1.rt, slider_2.rt, slider_3.rt)) %>%
  select(- slider_1.rt,
         - slider_2.rt,
         - slider_3.rt)

# renaming columns
df <- df %>% dplyr::rename(
  within_block_order = block_name.thisTrialN,
  block_order = choose_blocks.thisN,
  Gender = `Gender *`,
  Age = `Age *`,
  Education = starts_with("Highest level of education"))

# Adding data pertaining to the graphs: 

# paste the name of the folder that contain the graph data
path <- "graph_generation/summary_stats"

# read the csvs
stats_summary <- read_csv(file.path(path, pattern = "all_summary.csv"))
axis_summary <- read_csv(file.path(path, pattern = "axis_summary.csv"))
blank_axis_summary <- read_csv(file.path(path, pattern = "blank_axis_summary.csv"))

# demonstrate that the axis summary and blank axis summary data are identical
all(axis_summary$graph_id == blank_axis_summary$graph_id)
all(axis_summary$`1` == blank_axis_summary$`1`)
all(axis_summary$`2` == blank_axis_summary$`2`)

# in  stats_summary 
# remove column with the x axis labels (not required)
# THEN
# recode the data in the 'label' column to refer to slider numbers
# this will ultimately match the column headings in `df`
# THEN
# pivot to wide format, with separate columns for each cluster and statistic
# names_glue() controls the ordering of column headers
# THEN
# graph_id is renamed graph_image so the column name matches `df`
stats_summary <- stats_summary %>%
  select(- x) %>%
  mutate(label = recode(label,
                        "gp1_label" = "slider_1",
                        "gp2_label" = "slider_2",
                        "gp3_label" = "slider_3")) %>%
  pivot_wider(names_from = label, values_from = c(mean, sd, min_value, max_value),
              names_glue = "{label}.{.value}") %>%
  dplyr::rename(graph_image = graph_id)

# renaming columns in axis_summary
axis_summary <- axis_summary %>% rename(lower_lim = `1`,
                                        upper_lim = `2`,
                                        graph_image = graph_id)

# adding prefix and suffix to match graph_image in `df`
axis_summary$graph_image <- paste0("graphs/graph", axis_summary$graph_image, ".png")
stats_summary$graph_image <- paste0("graphs/graph", stats_summary$graph_image, ".png")

# joining df and axis summary by 'graph_image' column
df <- inner_join(df, axis_summary, by = "graph_image") 

# joining df and stats summary by 'graph_image' column
df <- inner_join(df, stats_summary, by = "graph_image")

# adding estimate columns, which translate participant response into a  numerical estimate
# the response is multiplied by the range and added on to the lower y-axis limit
df <- df %>% 
  mutate(slider_1.estimate = (lower_lim + (slider_1.response * (upper_lim - lower_lim))),
         slider_2.estimate = (lower_lim + (slider_2.response * (upper_lim - lower_lim))),
         slider_3.estimate = (lower_lim + (slider_3.response * (upper_lim - lower_lim))) 
         )

# reading in graphs_book1, which provides the statistics used to build the graphs
graphs_book1 <- read_csv(file.path(pattern = "graph_generation/graphs_book1.csv"))

# adding prefix and suffix to match graph_image in `df`
graphs_book1$graph_image <- paste0("graphs/graph", graphs_book1$graph_id, ".png") 

# removing the contents of brackets from 'y_label' column
graphs_book1$y_label <- str_remove(graphs_book1$y_label, " \\(.*\\)")

# replace spaces with underscores
graphs_book1$y_label <- str_replace_all(graphs_book1$y_label, " ", "_")
# y_label will act as a block name column

# rename ooo_pos as unique_xpos
# (x-axis position of unique (odd-one-out) cluster)
graphs_book1$unique_xpos <- graphs_book1$ooo_pos

# select the necessary columns from graphs_book1
# then join with df by 'graph_image'
df <- graphs_book1 %>%
  select(y_label,
         graph_image,
         unique_xpos
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

# First, I calculate the separation
# between the unique clusters (different pop. mean)
# and the non-unique clusters (same pop. mean)
# This is divided by the corresponding y-axis range 
# to generate a standardised  measure.
# THEN
# New column 'unique_ypos':
# whether the unique value in the graph is positioned above or below
# above = unique cluster is higher than the non-unique clusters
# below = unique cluster is lower than the non-unique clusters
# THEN
# change values in the 'separation' column to absolute (remove sign)
df <- df %>% 
  rowwise() %>%
  mutate(separation = case_when(unique_xpos == 1 ~ 
                                 ((slider_1.mean - 
                                     mean(slider_2.mean, slider_3.mean)
                                  )/(upper_lim - lower_lim)),
                                unique_xpos == 2 ~ 
                                  ((slider_2.mean - 
                                     mean(slider_1.mean, slider_3.mean)
                                  )/(upper_lim - lower_lim)),
                                unique_xpos == 3 ~ 
                                  ((slider_3.mean - 
                                     mean(slider_1.mean, slider_2.mean)
                                  )/(upper_lim - lower_lim))
  )
  ) %>%
  mutate(unique_ypos = case_when(separation > 0 ~ "above",
                                     separation < 0 ~ "below")) %>% 
  mutate(separation = abs(separation))

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
                        slider_1.mean:slider_3.estimate,
                        slider_1.z_score:slider_3.z_score),
               names_to = c("slider", "measure"),
               names_pattern = "(.*)\\.(.*)") %>%
  pivot_wider(names_from = measure, values_from = value)

# removing "slider_" from values in the slider column
df <- df %>% 
  mutate(slider = str_replace(slider, ".*_", "")) 

# changing slider to double so that it can be compared to unique_xpos (which is also double)
typeof(df$unique_xpos)
typeof(df$slider)
df$slider <- as.double(df$slider)

# adding new column, 'is_unique'
# TRUE if the row refers to observations on an 'odd-one-out' cluster
# FALSE if the row refers to observations that aren't odd-ones-out
df <- df %>%
  rowwise() %>%
  mutate(is_unique = case_when(unique_xpos == slider ~ TRUE,
                               unique_xpos != slider ~ FALSE))

# adding new column, 'height': standardised measure of vertical (y-axis) position
df <- df %>%
  rowwise() %>%
  mutate(height = ((mean - lower_lim)/(upper_lim - lower_lim))) 
  


# VISUALISATION AND ANALYSIS####

# checking for missing values
vis_miss(df)

# PARTICIPANT INFO

# changing participant to character (it is not numerical)
df$participant <- as.character(df$participant)

# creating a new data frame, with only required columns
# and only one row for each participant
participant_info <- df %>%
  select(participant, Age, Gender, time_taken) %>%
  distinct(distinct_values = participant, .keep_all = TRUE)

# looking at the unique values of 'Gender'
unique(participant_info$Gender)

# changing every value of Gender to lowercase
participant_info <- participant_info %>% 
  mutate(Gender = tolower(Gender))
unique(participant_info$Gender)

# checking whether this participant has mixed up input of gender and age
participant_info %>%
  filter(Gender == "19") %>%
  select(participant, Age, Gender) 

# changing 'm' to 'male
# and changing '19' to NA
participant_info <- participant_info %>%
  mutate(
    Gender = recode(Gender, "m" = "male"),
    Gender = recode(Gender, "19" = "NA"))  
unique(participant_info$Gender)

# calculating percentages of each gender
participant_info %>% 
  count(Gender) %>% 
  as.data.frame() %>%
  mutate(percentage = n/sum(n))

# calculating mean age
mean(participant_info$Age)

# counting how many participants
length(unique(participant_info$participant))

# average time taken
mean(participant_info$time_taken)


# coding 'is_unique' and 'unique_ypos' as factors
df$is_unique <- as.factor(df$is_unique)
df$unique_ypos <- as.factor(df$unique_ypos)
# setting contrasts for these factors
contrasts(df$is_unique) <- matrix(c(.5, -.5))
contrasts(df$unique_ypos) <- matrix(c(.5, -.5))


# checking the distribution of the DV (z_score)
# Cullen and Frey plot:
descdist(df$z_score)
# histogram: there is almost no skew but very high kurtosis
hist(df$z_score)

# summary stats
mean(df$z_score)
sd(df$z_score)
# on average, there is slight over-estimation

# because kurtosis is high, the 95% CI for the mean does not include 95% of responses
# here, I visualise 95% of responses against the histogram
qts <- quantile(df$z_score, probs = c(.025,.975))
hist(df$z_score)
abline(v=qts[1],col="red")
abline(v=qts[2],col="red")
abline(v=mean(df$z_score),col="blue")
qts[1]
qts[2]

# visualising z_scores individually for each participant
# participant 71 produced the unusually high estimates
df %>% 
  #filter(participant == 1) %>%
  ggplot(aes(x = participant,
             y = z_score)) +
  geom_point(alpha = 0.2, size = 2) +
  stat_summary(fun = mean, size = 0.5, colour = "coral") +
  theme_minimal(base_size=18) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "Estimation Error\n(in Cluster Standard Deviations)",
       x = "Participant") +
  geom_segment(aes(x = -1, xend = -1, y= 0, yend= 2),
               arrow=arrow(length=unit(0.2,"cm")), colour = "darkgrey") +
  geom_segment(aes(x = -1, xend = -1, y= 0, yend= -2),
               arrow=arrow(length=unit(0.2,"cm")), colour = "darkgrey") +
  geom_text(aes(x = -1, y= 5, angle = 90),
            label = "Over-estimation", colour = "darkgrey", size = 4) +
  geom_text(aes(x = -1, y= -5, angle = 90),
            label = "Under-estimation", colour = "darkgrey", size = 4) +
  coord_cartesian(xlim = c(-3, 82))


# MODEL 1 - VISUALISATIONS:
# are unique or non-unique clusters more accurate?
df %>%
  ggplot(aes(y = z_score,
             x = is_unique)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1.8, 1.8))

# how does separation between unique and non-unique clusters affect accuracy?
df %>%
  ggplot(aes(x = separation,
             y = z_score)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm)

# how does relative position of unique clusters affect accuracy?
df %>%
  ggplot(aes(y = z_score,
             x = unique_ypos)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1.8, 1.8))

# Interaction between separation and uniqueness
df %>% 
  ggplot(aes(x = separation,
             y = z_score,
             colour = is_unique)) +
  geom_smooth(method = lm) 

# Interaction between relative position of unique clusters and uniqueness
df %>%
  ggplot(aes(x = unique_ypos,
             y = z_score,
             colour = is_unique)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1.7, 1.7)) +
  theme_minimal(base_size=18) +
  labs(x = "Position Of Unique Cluster (Relative to Other Clusters)",
       y = "Estimation Error\n(in Cluster Standard Deviations)",
       colour = "Cluster Uniqueness")  +
  scale_x_discrete(labels = c('Above','Below')) +
  theme(legend.position="right") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_brewer(palette="Dark2", labels = c('Duplicated','Unique')) +
  geom_segment(aes(x = 0.5, xend = 0.5, y= 0, yend= 0.3),
               arrow=arrow(length=unit(0.2,"cm")), colour = "darkgrey") +
  geom_segment(aes(x = 0.5, xend = 0.5, y= 0, yend= -0.3),
               arrow=arrow(length=unit(0.2,"cm")), colour = "darkgrey") +
  geom_text(aes(x = 0.5, y= 1.1, angle = 90),
               label = "Over-estimation", colour = "darkgrey", size = 5) +
  geom_text(aes(x = 0.5, y= -1.1, angle = 90),
            label = "Under-estimation", colour = "darkgrey", size = 5)

# Interaction between separation and relative position of unique clusters
df %>% 
  ggplot(aes(x = separation,
             y = z_score,
             colour = unique_ypos)) +
  geom_point(alpha = 0.05, colour = "black") +
  geom_smooth(method = lm, se = FALSE, size = 1.5) +
  facet_zoom(ylim = c(0, 0.3)) +
  labs(x = "Amount of Separation Between Unique and Non-Unique Clusters",
       colour = "Position of Unique Cluster\n(Relative to Non-Unique Clusters)",
       y = "Estimation Error\n(in Cluster Standard Deviations)") +
  theme_grey(base_size = 18) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_brewer(palette="Dark2", labels = c('Above','Below'))

# MODEL 1 - MODEL:
model1 <- lmer(z_score ~
                 is_unique +
                 separation +
                 unique_ypos +
                 is_unique:separation +
                 is_unique:unique_ypos +
                 separation:unique_ypos + 
               (1 | participant) + 
               (1 | graph_image),
             data = df)
summary(model1)
  

table1 <- tidy(model1) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model1")) %>%
  arrange(Model) %>%
  select(Model, 
         term:p.value,
         - effect,
         - group)

table1$estimate <- round(table1$estimate, 2)
table1$std.error <- round(table1$std.error, 2)
table1$statistic <- round(table1$statistic, 2)
table1$df <- round(table1$df, 2)
table1$p.value <- round(table1$p.value, 3)

write_csv(table1, file.path("model1_summary"))

# MODEL 2 - VISUALISATIONS:
# Effect of height (vertical y axis position) on estimates
df %>%
  ggplot(aes(x = height,
             y = z_score)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = lm )

df %>%
  ggplot(aes(x = height,
             y = z_score)) +
  geom_smooth(method = lm )

# MODEL 2 - MODEL:
model2 <- lmer(z_score ~ height +
                 (1 | participant) + 
                 (1 | graph_image),
               data = df)
summary(model2)

table2 <- tidy(model2) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model 2")) %>%
  arrange(Model) %>%
  select(Model, 
         term:p.value,
         - effect,
         - group)

table2$estimate <- round(table2$estimate, 2)
table2$std.error <- round(table2$std.error, 2)
table2$statistic <- round(table2$statistic, 2)
table2$df <- round(table2$df, 2)
table2$p.value <- round(table2$p.value, 3)

write_csv(table2, file.path("model2_summary"))

# MODEL 3 - VISUALISATIONS:
# Interaction between height and relative position of unique clusters
df %>% 
  ggplot(aes(x = height,
             y = z_score,
             colour = unique_ypos)) +
  #geom_point(alpha = 0.1, colour = "black") +
  geom_smooth(method = lm)

df %>%
  ggplot(aes(x = height,
             y = z_score,
             colour = unique_ypos)) +
  geom_point(alpha = 0.05, colour = "black") +
  geom_smooth(method = lm, se = FALSE, size = 1.5) +
  facet_zoom(ylim = c(0, 0.3)) +
  labs(x = "Cluster Vertical Position",
       colour = "Position of Unique Cluster\n(Relative to Non-Unique Clusters)",
       y = "Estimation Error\n(in Cluster Standard Deviations)") +
  theme_grey(base_size = 18) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_brewer(palette="Dark2", labels = c('Above','Below'))

# Interaction between height and magnitude of separation
df %>% 
  ggplot(aes(x = height,
             y = z_score,
             colour = separation)) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_smooth(method = lm)

# Interaction between height and uniqueness
df %>%
  ggplot(aes(x = height,
             y = z_score,
             colour = is_unique)) +
  geom_point(alpha = 0.05, colour = "black") +
  geom_smooth(method = lm, se = FALSE, size = 1.5) +
  facet_zoom(ylim = c(0, 0.3)) +
  labs(x = "Cluster Vertical Position",
       colour = "Cluster Uniqueness",
       y = "Estimation Error\n(in Cluster Standard Deviations)") +
  theme_grey(base_size = 18) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_brewer(palette="Dark2", labels = c('Duplicated','Unique'))

# MODEL 3 - MODEL:
model3 <- lmer(z_score ~ 
                 separation*height +
                 unique_ypos*height +
                 is_unique*height +
                 (1 | participant) + 
                 (1 | graph_image),
               data = df)
summary(model3)

table3 <- tidy(model3) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model 3")) %>%
  arrange(Model) %>%
  select(Model, 
         term:p.value,
         - effect,
         - group)

table3$estimate <- round(table3$estimate, 2)
table3$std.error <- round(table3$std.error, 2)
table3$statistic <- round(table3$statistic, 2)
table3$df <- round(table3$df, 2)
table3$p.value <- round(table3$p.value, 3)

write_csv(table3, file.path("model3_summary"))

# MODEL 4 - VISUALISATION:
# extension_difference
df <- df %>%
  rowwise() %>%
  mutate(upper_extension = (max_value - mean)/(upper_lim - lower_lim),
         lower_extension = (mean - min_value)/(upper_lim - lower_lim),
         extension_difference = upper_extension - lower_extension) 

hist(df$extension_difference)


df %>%
  ggplot(aes(x = extension_difference,
             y = z_score)) +
  geom_point(alpha = 0.05, colour = "black") +
  geom_smooth(method = lm, se = FALSE, size = 1.5, colour = "orange1") +
  facet_zoom(ylim = c(-0.15, 0.4)) +
  labs(x = "Extension Difference",
       y = "Estimation Error\n(in Cluster Standard Deviations)") +
  theme_grey(base_size = 18) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# MODEL 4 - MODEL:
# previous significant interactions included in order to test whether extension difference
# accounts for previous data
model4 <- lmer(z_score ~ 
               separation*unique_ypos +
               height*unique_ypos + 
               height*is_unique + 
               extension_difference +
               (1 | participant) + 
               (1 | graph_image),
             data = df)
summary(model4)

table4 <- tidy(model4) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model 4")) %>%
  arrange(Model) %>%
  select(Model, 
         term:p.value,
         - effect,
         - group)

table4$estimate <- round(table4$estimate, 2)
table4$std.error <- round(table4$std.error, 2)
table4$statistic <- round(table4$statistic, 2)
table4$df <- round(table4$df, 2)
table4$p.value <- round(table4$p.value, 3)

write_csv(table4, file.path("model4_summary"))



# SUPPLEMENTARY VISUALISATIONS ####

# theme used for the stimuli visualisations
theme_mres <- function() {
  theme_minimal(base_size=2.7, base_family="Helvetica") +
    theme(plot.title = element_text(hjust = 0.5, size = 25),
          axis.text.x = element_text(size = rel(3.5)),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = rel(1),
                                      margin = unit(c(0, 1, 0, 0), "mm")),
          axis.text.y = element_text(size = rel(3)),
          axis.title = element_text(size = rel(3)),
          panel.grid.major.x = element_blank(), # removes vertical gridlines
          panel.grid.major.y = element_blank(), # removes major horizontal gridlines
          panel.grid.minor.y = element_blank(), # removes minor horizontal gridlines
          axis.text.y.right = element_text(colour = "white"),
          axis.title.y.right = element_text(colour = "white",
                                            margin = unit(c(0, 1, 0, 0), "mm")),
          panel.border = element_rect(colour = "black", fill = NA),
          aspect.ratio = 0.60
    )
}

# example 1: proposed mechanism
index <- 36
# the following function takes the parameters of a dataset and uses them to 
# build a dataframe. 
create_df <- function(my_df){
  set.seed(1234 + index)
  y <- c(rnorm(24, my_df$gp1_pop_mean, my_df$gp1_pop_sd), 
         rnorm(24, my_df$gp2_pop_mean, my_df$gp2_pop_sd),
         rnorm(24, my_df$gp3_pop_mean, my_df$gp3_pop_sd))
  x <- c(rep(my_df$gp1_label, 24), rep(my_df$gp2_label, 24),
         rep(my_df$gp3_label, 24))
  df <- as_tibble(cbind(y, x)) %>%
    mutate(y = as.double(y)) %>%
    mutate(x = as.factor(x))
}

my_graphs <- graphs_book1

build_this_one <- my_graphs %>%
  filter(graph_id == index) %>%
  create_df() 

ylab <- my_graphs[my_graphs$graph_id == index,]$y_label
y_min <- my_graphs[my_graphs$graph_id == index,]$y_min
y_max <- my_graphs[my_graphs$graph_id == index,]$y_max

avg_est <- mean(df$z_score)

build_this_one <- build_this_one %>%
  group_by(x) %>%
  mutate(est = mean(y) + (sd(y)*avg_est),
         mean = mean(y),
         val = (est - mean)/sd(y))

set.seed(1234 +index)
eg1 <- build_this_one %>% 
  ggplot(aes(x = x, y = y)) +
  geom_jitter(width = .1, alpha = .75, size = 1.5, height = 0) +
  #stat_summary(fun = mean, size = 0.05, colour = "red") +
  #stat_summary(aes(x = x, y = est), fun = mean, size = 0.05, colour = "blue") +
  theme_mres() +
  theme(plot.title = element_blank(),
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_y_continuous(sec.axis = dup_axis(),
                     expand = c(0, 0), limits = c(60, 260)) +
  geom_segment(aes(x=1, xend=1, y=70, yend=155), 
                 arrow = arrow(length = unit(0.5, "cm")), colour = "blue", size = 1)  +
  geom_segment(aes(x=2, xend=2, y=70, yend=155), 
               arrow = arrow(length = unit(0.5, "cm")), colour = "blue",  size = 1) +
  geom_segment(aes(x = 0.8, y = 193, xend = 3.2, yend = 193), colour = "coral2",  size = 1) +
  geom_segment(aes(x=3, xend=3, y=160, yend=190), 
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "coral2",  size = 1) +
  guides(color = FALSE,  
         fill = guide_legend(order = 1)) +
  geom_text(aes(x = 1.5, y= 90),
            label = "Over-compensation \nfor blank space", 
            colour = "blue", size = 3) +
  geom_text(aes(x = 3, y= 215),
            label = "Landmark effect\n(attraction/perceptual pull)", 
            colour = "coral2", size = 3)

# example 2: proposed mechanism
index <- 19
# the following function takes the parameters of a dataset and uses them to 
# build a dataframe. 
create_df <- function(my_df){
  set.seed(1234 + index)
  y <- c(rnorm(24, my_df$gp1_pop_mean, my_df$gp1_pop_sd), 
         rnorm(24, my_df$gp2_pop_mean, my_df$gp2_pop_sd),
         rnorm(24, my_df$gp3_pop_mean, my_df$gp3_pop_sd))
  x <- c(rep(my_df$gp1_label, 24), rep(my_df$gp2_label, 24),
         rep(my_df$gp3_label, 24))
  df <- as_tibble(cbind(y, x)) %>%
    mutate(y = as.double(y)) %>%
    mutate(x = as.factor(x))
}

my_graphs <- graphs_book1

build_this_one <- my_graphs %>%
  filter(graph_id == index) %>%
  create_df() 

ylab <- my_graphs[my_graphs$graph_id == index,]$y_label
y_min <- my_graphs[my_graphs$graph_id == index,]$y_min
y_max <- my_graphs[my_graphs$graph_id == index,]$y_max

avg_est <- mean(df$z_score)

build_this_one <- build_this_one %>%
  group_by(x) %>%
  mutate(est = mean(y) + (sd(y)*avg_est),
         mean = mean(y),
         val = (est - mean)/sd(y))

set.seed(1234 +index)
eg2 <- build_this_one %>% 
  ggplot(aes(x = x, y = y)) +
  geom_jitter(width = .1, alpha = .75, size = 1.5, height = 0) +
  theme_mres() +
  theme(plot.title = element_blank(),
        axis.text.x=element_blank(), 
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_y_continuous(sec.axis = dup_axis(),
                     expand = c(0, 0), limits = c(3500, 6000)) +
  geom_segment(aes(x=2, xend=2, y=3500, yend=3700), 
               arrow = arrow(length = unit(0.3, "cm")), colour = "blue", size = 1)  +
  geom_segment(aes(x=3, xend=3, y=3500, yend=3700), 
               arrow = arrow(length = unit(0.3, "cm")), colour = "blue",  size = 1) +
  geom_segment(aes(x = 0.8, y = 4368, xend = 3.2, yend = 4368), colour = "coral2",  size = 1) +
  geom_segment(aes(x=1, xend=1, y=4800, yend=4400), 
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "coral2",  size = 1) +
  guides(color = FALSE,  
         fill = guide_legend(order = 1)) +
  geom_text(aes(x = 1, y= 4000),
            label = "Landmark effect\n(attraction/perceptual pull)", 
            colour = "coral2", size = 3)

eg1 + eg2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 18))

# Visualising Extension Difference in Individual Clusters
# negative extension difference
index <- 45
build_this_one <- my_graphs %>%
  filter(graph_id == index) %>%
  create_df() 

ylab <- my_graphs[my_graphs$graph_id == index,]$y_label
y_min <- my_graphs[my_graphs$graph_id == index,]$y_min
y_max <- my_graphs[my_graphs$graph_id == index,]$y_max

avg_est <- mean(df$z_score)

build_this_one <- build_this_one %>%
  group_by(x) %>%
  mutate(est = mean(y) + (sd(y)*avg_est),
         mean = mean(y),
         val = (est - mean)/sd(y))
set.seed(1234 +index)
build_this_one %>% 
  ggplot(aes(x = x, y = y)) +
  geom_jitter(width = .05, alpha = .75, size = 1.5, height = 0) +
  stat_summary(fun = mean, size = 0.6, colour = "darkgreen") +
  theme_mres() +
  coord_cartesian(ylim = c(45,80),
                  xlim = c(1,1)) +
  geom_segment(aes(x=1.07, xend=1.07, y=59.5, yend=49), 
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "coral2",  size = 1)  +
  geom_segment(aes(x=1.07, xend=1.07, y=59.5, yend=66.5), 
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "royalblue3",  size = 1) +
  geom_text(aes(x = 1, y= 47),
            label = "Extension Difference = -0.06", 
            colour = "black", size = 4) +
  geom_segment(aes(x = 1, xend = 1.12, y = 59.5, yend = 59.5),
               colour = "darkgreen") +
  geom_text(aes(x = 1.16, y= 59.5),
            label = "mean", 
            colour = "darkgreen", size = 3) +
  geom_text(aes(x = 1.21, y= 54),
            label = "Larger extension\nbelow centroid", 
            colour = "coral2", size = 4) +
  geom_text(aes(x = 1.21, y= 62.5),
            label = "Smaller extension\nabove centroid", 
            colour = "royalblue3", size = 4) +
  theme_void()

# positive extension difference
index <- 38
build_this_one <- my_graphs %>%
  filter(graph_id == index) %>%
  create_df() 

ylab <- my_graphs[my_graphs$graph_id == index,]$y_label
y_min <- my_graphs[my_graphs$graph_id == index,]$y_min
y_max <- my_graphs[my_graphs$graph_id == index,]$y_max

avg_est <- mean(df$z_score)

build_this_one <- build_this_one %>%
  group_by(x) %>%
  mutate(est = mean(y) + (sd(y)*avg_est),
         mean = mean(y),
         val = (est - mean)/sd(y))
set.seed(1234 +index)
build_this_one %>% 
  ggplot(aes(x = x, y = y)) +
  geom_jitter(width = .05, alpha = .75, size = 1.5, height = 0) +
  stat_summary(fun = mean, size = 0.6, colour = "darkgreen") +
  theme_mres() +
  coord_cartesian(ylim = c(60,95),
                  xlim = c(3,3)) +
  geom_segment(aes(x=3.07, xend=3.07, y=82.35, yend=77), 
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "coral2",  size = 1)  +
  geom_segment(aes(x=3.07, xend=3.07, y=82.35, yend=92.5), 
               arrow = arrow(length = unit(0.5, "cm")),
               colour = "royalblue3",  size = 1) +
  geom_text(aes(x = 3, y= 75),
            label = "Extension Difference = 0.09", 
            colour = "black", size = 4) +
  geom_segment(aes(x = 3, xend = 3.12, y = 82.35, yend = 82.35),
               colour = "darkgreen") +
  geom_text(aes(x = 3.16, y= 82.4),
            label = "mean", 
            colour = "darkgreen", size = 3) +
  geom_text(aes(x = 3.21, y= 80),
            label = "Smaller extension\nbelow centroid", 
            colour = "coral2", size = 4) +
  geom_text(aes(x = 3.21, y= 87),
            label = "Larger extension\nabove centroid", 
            colour = "royalblue3", size = 4) +
  theme_void()

# bar graph vs. univariate scatterplot
gp1 <- rnorm(30, 70, 11)
gp2 <- rnorm(30, 75, 7)
gp3 <- rnorm(30, 60, 4)

df1 <- as.data.frame(cbind(gp1, gp2, gp3))

df1 <- df1 %>%
  rename(`Mr. Smith` = gp1,
         `Miss Robertson` = gp2,
         `Mrs. Jenkins` = gp3)

df1 <- df1 %>%
  pivot_longer(names_to = "Teacher",
               values_to = "Rating (%)",
               cols = c(`Mr. Smith`,
                        `Miss Robertson`,
                        `Mrs. Jenkins`)) 

p1 <- df1 %>%
  ggplot(aes(x = Teacher,
             y = `Rating (%)`)) +
  geom_bar(stat = "summary", fun.y = "mean", width = .3) +
  theme_minimal() +
  coord_cartesian(ylim = c(40, 100)) +
  theme_minimal(base_size = 18) +
  theme_mres()

set.seed(80)
p2 <- df1 %>%
  ggplot(aes(x = Teacher,
             y = `Rating (%)`)) +
  geom_jitter(width = 0.1, alpha = 0.1) +
  theme_minimal() +
  coord_cartesian(ylim = c(40, 100)) +
  theme_minimal(base_size = 18) +
  theme_mres(base_size = 20)

p1 + p2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 18))

# anscombe's quartet
anscombe_df <- anscombe %>%
  as.data.frame() %>%
  pivot_longer(names_to = "Teacher",
               values_to = "Rating (%)",
               cols = x1:y4)

aq1 <- ggplot(anscombe, aes(x=x1, y=y1)) + 
  geom_point(size= 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_mres() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(3, 13)) 

aq2 <- ggplot(anscombe, aes(x=x2, y=y2)) + 
  geom_point(size= 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_mres() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(3, 13)) 


aq3 <- ggplot(anscombe, aes(x=x3, y=y3)) + 
  geom_point(size= 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_mres() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(3, 13)) 


aq4 <- ggplot(anscombe, aes(x=x4, y=y4)) + 
  geom_point(size= 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_mres() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(3, 13)) 

aq1 + aq2 + aq3 + aq4

# potential issues with centroids
c_shape_y <- c(12, 11.5, 12, 12.5, 13, 14, 15, 16, 16.5, 17, 17.5, 17)
c_shape_x <- c(16, 15, 14, 13, 12, 12, 12, 12, 13, 14, 15, 16)
c_shape <- cbind(c_shape_x, c_shape_y) %>%
  as.data.frame()

c_shape_mean <- c_shape %>% 
  summarise(c_shape_y = mean(c_shape_y),
            c_shape_x  = mean(c_shape_x))

ex1 <- c_shape %>% ggplot(aes(x = c_shape_x,
                              y = c_shape_y)) +
  geom_point(size = 2) +
  geom_point(data = c_shape_mean, size = 4, shape = 4, colour = "blue") +
  theme_mres() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(11, 18),
                  xlim = c(10, 18)) +
  geom_label(
    label="Centroid", 
    x=15.5,
    y=14.5,
    label.size = 0.01,
    color = "black",
  )
ex1


bi_dist <- c(1, 1, 1, 1, 1, 1, 1, 1,
             2, 2, 2, 2, 2,
             3, 3, 
             9, 9,
             10, 10, 10, 10, 10,  
             11, 11, 11, 11, 11, 11, 11, 11)
placeholder <- rnorm(length(bi_dist), 0, 1)
df2 <- data.frame(cbind(bi_dist, placeholder))

df2 <- df2 %>% gather(x_ax, y_ax) 

ex2 <- df2 %>% filter(x_ax == "bi_dist") %>% 
  ggplot(aes(x = x_ax,
             y = y_ax)) +
  geom_jitter(width = 0.03, alpha = 0.7) +
  stat_summary(fun.y = "mean", shape = 4, 
               colour = "blue", size = 1) +
  theme_mres() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  geom_label(
    label="Centroid", 
    y = 6.2,
    x = 1.25,
    label.size = 0.1,
    color = "black",
  )

ex1 + ex2 + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 18))


