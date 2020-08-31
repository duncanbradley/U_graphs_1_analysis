library(tidyverse)
library(lme4)
library(lmerTest)
library(zoo)
library(fitdistrplus)
library(performance)
library(naniar)
library(broom.mixed)
library(interactions)

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
path <- "U_graphs_1/summary_stats"

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
graphs_book1 <- read_csv(file.path(pattern = "U_graphs_1/graphs_book1.csv"))

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
                                 (slider_1.mean - 
                                     mean(slider_2.mean, slider_3.mean)
                                  )/
                                   (upper_lim - lower_lim),
                                unique_xpos == 2 ~ 
                                  (slider_2.mean - 
                                     mean(slider_1.mean, slider_3.mean)
                                  )/
                                  (upper_lim - lower_lim),
                                unique_xpos == 3 ~ 
                                  (slider_3.mean - 
                                     mean(slider_1.mean, slider_2.mean)
                                  )/
                                  (upper_lim - lower_lim)
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


# ANALYSIS

# coding 'is_unique' and 'unique_ypos' as factors
df$is_unique <- as.factor(df$is_unique)
df$unique_ypos <- as.factor(df$unique_ypos)

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

# t-test: the true mean is not equal to 0
t.test(df$z_score)

# visualising z_scores individually for each participant
# participant 71 produced the unusually high estimates
df %>% 
  ggplot(aes(x = participant,
             y = z_score)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun = mean, size = 0.1, colour = "red")

# null model: no fixed effects
null_model <- lmer(z_score ~ 
                     (1 | participant) + 
                     (1 | graph_image), 
                   data = df)

# MODEL 1:
# are unique or non-unique clusters more accurate?
df %>%
  ggplot(aes(y = z_score,
             x = is_unique)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1.8, 1.8))

uniqueness <- lmer(z_score ~ is_unique +
                 (1 | participant) + 
                 (1 | graph_image), 
               data = df)

tests <- tidy(uniqueness) %>% 
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model1"))

# MODEL 2:
# how does separation between unique and non-unique clusters affect accuracy?
df %>%
  ggplot(aes(x = separation,
             y = z_score)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm)

separation <- lmer(z_score ~ separation +
                 (1 | participant) + 
                 (1 | graph_image), 
               data = df)

tests <- tidy(separation) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model2")) %>%
  rbind(tests) 

# MODEL 3:
# how does relative position of unique clusters affect accuracy?
df %>%
  ggplot(aes(y = z_score,
             x = unique_ypos)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1.8, 1.8))

unique_ypos <- lmer(z_score ~ unique_ypos +
                          (1 | participant) + 
                          (1 | graph_image), 
                        data = df)

tests <- tidy(unique_ypos) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model3")) %>%
  rbind(tests) 

# MODEL 4:
# Interaction between separation and uniqueness
df %>% 
  ggplot(aes(x = separation,
             y = z_score,
             colour = is_unique)) +
  geom_smooth(method = lm) 

separation_uniqueness <- lmer(z_score ~ separation*is_unique +
                 (1 | participant) + 
                 (1 | graph_image), 
               data = df)

tests <- tidy(separation_uniqueness) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model4")) %>%
  rbind(tests) 

# MODEL 5:
# Interaction between relative position of unique clusters and uniqueness
df %>%
  mutate(relative_position = case_when(is_unique = TRUE |
                                         unique_ypos = "above" ~ "above"))
  ggplot(aes(x = is_unique,
             y = z_score,
             colour = unique_ypos)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-1.8, 1.8)) +
  theme_minimal(base_size=20, 
                base_family="Helvetica") +
  #labs(title = "Relationship Between Cluster Uniqueness\nand Relative Position",
       #x = "Cluster Uniqueness",
       #y = "Estimate (z-score)",
       #colour = "Position Relative to Other Cluster(s)")  +
  #scale_colour_discrete(labels = c('Above','Below')) +
  #scale_x_discrete(labels = c('Duplicated','Unique')) +
  theme(legend.position="right")



unique_ypos_uniqueness <- lmer(z_score ~ unique_ypos*is_unique +
                                 (1 | participant) + 
                                 (1 | graph_image), 
                               data = df)

tests <- tidy(unique_ypos_uniqueness) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model5")) %>%
  rbind(tests) 

# MODEL 6:
# Interaction between separation and relative position of unique clusters
df %>% 
  ggplot(aes(x = separation,
             y = z_score,
             colour = unique_ypos)) +
  #geom_point(alpha = 0.1, colour = "black") +
  geom_smooth(method = lm) 

separation_unique_ypos <- lmer(z_score ~ separation*unique_ypos +
                                     (1 | participant) + 
                                     (1 | graph_image), 
                                   data = df)

tests <- tidy(separation_unique_ypos) %>%
  filter(effect == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("Model6")) %>%
  rbind(tests) %>%
  arrange(Model) %>%
  select(Model, 
         term:p.value,
         - effect,
         - group)

tests$estimate <- round(tests$estimate, 2)
tests$std.error <- round(tests$std.error, 2)
tests$statistic <- round(tests$statistic, 2)
tests$df <- round(tests$df, 2)
tests$p.value <- round(tests$p.value, 3)

write_csv(tests, file.path("model1-6_summary"))


summary(separation_unique_ypos)
simple_slopes(separation_unique_ypos)
ss <- sim_slopes(separation_unique_ypos, pred = separation, modx = unique_ypos, v.co = extension_difference)
ss
ss$slopes
ss$ints
plot(ss)

# MODEL 7:
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

height <- lmer(z_score ~ height + extension_difference + 
                 (1 | participant) + 
                 (1 | graph_image), 
               data = df)

anova(height, null_model)
summary(height)

# MODEL 8
# Interaction between height and relative position of unique clusters
df %>% 
  ggplot(aes(x = height,
             y = z_score,
             colour = unique_ypos)) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_smooth(method = lm)

height_unique_ypos <- lmer(z_score ~ height*unique_ypos +
                 (1 | participant) + 
                 (1 | graph_image), 
               data = df)

anova(height_unique_ypos, null_model)
summary(height_unique_ypos)


# MODEL 9:
# Interaction between height and magnitude of separation
df %>% 
  ggplot(aes(x = height,
             y = z_score,
             colour = separation)) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_smooth(method = lm)

height_separation <- lmer(z_score ~ height*separation +
                                 (1 | participant) + 
                                 (1 | graph_image), 
                               data = df)
anova(height_separation, null_model)
summary(height_separation)

# MODEL 10:
# Interaction between height and uniqueness
df %>%
  ggplot(aes(x = height,
             y = z_score,
             colour = is_unique)) +
  geom_point(alpha = 0.1, colour = "black") +
  geom_smooth(method = lm)

height_uniqueness <- lmer(z_score ~ height*is_unique + extension_difference +
                 (1 | participant) + 
                 (1 | graph_image), 
               data = df)

anova(height_uniqueness, null_model)
summary(height_uniqueness)

df %>%
  group_by(is_unique, unique_ypos) %>%
  summarise(height = mean(height))

# simple slopes analysis
ss <- sim_slopes(height_uniqueness, pred = height, modx = is_unique)

ss
ss$slopes
ss$ints
plot(ss)

# MODEL 11:
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
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm) +
  theme_minimal()

extension_difference <- lmer(z_score ~ extension_difference +
              (1 | participant) +
              (1 | graph_image),
            df)
summary(extension_difference)

# MODEL 12:
# adding extension_difference as a covariate 
# to see whether it can explain away existing significant interaction
# between separation and separation sign (MODEL 6)

separation_unique_ypos_ed <- lmer(z_score ~ separation*unique_ypos + extension_difference +
                                 (1 | participant) + 
                                 (1 | graph_image), 
                               data = df)
summary(separation_unique_ypos_ed)

interact_plot(separation_unique_ypos_ed, pred = separation, modx = unique_ypos)
sim_slopes(separation_unique_ypos_ed, pred = separation, modx = unique_ypos)
ss <- sim_slopes(separation_unique_ypos_ed, pred = separation, modx = unique_ypos)
ss$slopes
ss$ints
plot(ss)
coef(separation_unique_ypos)

# MODEL 13:
# adding extension_difference as a covariate 
# to see whether it can explain away existing significant interaction
# between height and uniqueness (MODEL 6)

height_uniqueness_ed <- lmer(z_score ~ height*is_unique + extension_difference +
                                    (1 | participant) + 
                                    (1 | graph_image), 
                                  data = df)
summary(height_uniqueness_ed)

interact_plot(height_uniqueness_ed, pred = height, modx = is_unique)
sim_slopes(height_uniqueness_ed, pred = height, modx = is_unique)
ss <- sim_slopes(height_uniqueness_ed, pred = height, modx = is_unique)
ss$slopes
ss$ints
plot(ss)






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

index <- 53

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
build_this_one %>% 
  ggplot(aes(x = x, y = y)) +
  geom_jitter(width = .1, alpha = .75, size = .25, height = 0) +
  stat_summary(fun = mean, size = 0.05, colour = "red") +
  stat_summary(aes(x = x, y = est),
               fun = mean, size = 0.05, colour = "blue") +
  labs(y = ylab) +
  theme_mres() +
  theme(plot.title = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis(),
                     expand = c(0, 0), limits = c(y_min, y_max)) 

# fitted all with REML = FALSE
# this changes values on performance spider diagram
# work out what is happening here
check_model(model8)
model_performance(height_uniqueness)
plot(compare_performance(separation_unique_ypos, 
                         height_uniqueness))
model_parameters(height_uniqueness)
plot_model(height_uniqueness,
           show.values = TRUE,
           value.offset = .4)
plot_model(model7, type = "pred", terms = c("height", "is_ooo"))
