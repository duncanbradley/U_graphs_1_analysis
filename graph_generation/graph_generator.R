library(tidyverse)
library(egg)

create.dir("graphs")

# options explored for setting y axis limits:
# ylim(y_min, y_max) # same as scale_y_continuous
# coord_cartesian(ylim = c(y_min, y_max)) # also expands x axis
# expand_limits(y = c(y_min, y_max)) # adds extra expansion beyond specified limits

# creating a theme
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

# the following function takes a tibble, a label for the x-axis, a label for the y-axis,
# and a title and constructs a jittered graph
# height = 0 to prevent vertical jitter (only horizontal)
# the final two lines of the plot can't go in the theme
# the final two lines in the function return the y axis limits
my_graph <- function(df, labx, laby, title, y_min, y_max) {
  set.seed(1234)
  p <- df %>% 
    ggplot(aes(x = x, y = y)) +
    geom_jitter(width = .1, alpha = .75, size = .25, height = 0) +
    labs(x = labx, y = laby, title = title) +
    theme_mres() +
    theme(plot.title = element_blank()) +
    scale_y_continuous(sec.axis = dup_axis(),
                       expand = c(0, 0), limits = c(y_min, y_max)) 
  
  vec <- ggplot_build(p)$layout$panel_params[[1]]$y.range
  
  p <- set_panel_size(p,
                      width  = unit(6, "cm"),
                      height = unit(4, "cm"))
  p <- arrangeGrob(p)
  
  return(list(p, vec))
  
}

# the following function takes a graph and saves it in the graphs folder, generates 
# summary statistics of the data and saves them as a .csv file in the 
# summary_stats folder. In both cases a unique index is created to allow the
# graph and the summary stats to be paired up later during coding of the 
# responses of participants. 
save_graph <- function(current_graph){
  
  ggsave(paste0("graphs/graph", index, ".png"), 
         current_graph, 
         width = 3.5, height = 1.8, dpi = 300)
  
  build_this_one %>%
    group_by(x) %>%
    summarise(mean = mean(y), sd = sd(y), min_value = min(y), max_value = max(y)) %>%
    cbind(index) %>%
    mutate(label = c("gp1_label", "gp2_label", "gp3_label")) %>%
    write_csv(paste0("summary_stats/stats", index, "s.csv"))
  
}  

# this reads in the .csv file that contains the parameters of the graphs to be
# generated
my_graphs <- read_csv("graphs_book1.csv")

# this loops through the my_graphs which contains the parameters of the 
# graphs to be generated.  It runs once per each unique graph_id.
# save_graph(graph[[1]]) saves the graph
# the following line saves the y axis limits in a csv file
# .x to differentiate from other summary files
for(index in my_graphs$graph_id) {
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_df() 
  
  graph <- my_graph(build_this_one, 
                    my_graphs[my_graphs$graph_id == index,]$x_label, 
                    my_graphs[my_graphs$graph_id == index,]$y_label, 
                    my_graphs[my_graphs$graph_id == index,]$title,
                    my_graphs[my_graphs$graph_id == index,]$y_min,
                    my_graphs[my_graphs$graph_id == index,]$y_max)
  
  save_graph(graph[[1]])
  
  # y axis panel limits
  graph[[2]] %>%
    enframe() %>%
    pivot_wider(names_from = name, values_from = value) %>%
    cbind(index) %>%
    write_csv(., paste0("summary_stats/axis", index, "x.csv"))
  
  # y axis limits
  cbind(index, 
        my_graphs[my_graphs$graph_id == index,]$y_label,
        min = as.character(min(build_this_one$y)),
        max = as.character(max(build_this_one$y))
  ) %>%
    as.data.frame() %>%
    write_csv(paste0("summary_stats/minmax", index, "m.csv"))
}

data_path <- "summary_stats"   # path to the data

# For summary stats files
# .s to differentiate from other summary files
statsfiles <- dir(data_path, pattern = "*s.csv") # get file names

# the following reads in all the s.csv files from the summary_stats folder
# and creates one master df
stats_data <- statsfiles %>%
  map(function(x) read_csv(file.path(data_path, x))) %>%  
  reduce(rbind) %>%
  mutate(graph_id = index) %>%
  select(- index)

write_csv(stats_data, file.path(data_path, "all_summary.csv"))

# For axis limits files
axisfiles <- dir(data_path, pattern = "*x.csv") # get file names

axis_data <- axisfiles %>%
  map(function(x) read_csv(file.path(data_path, x))) %>%  
  reduce(rbind) %>%
  mutate(graph_id = index) %>%
  select(- index)

write_csv(axis_data, file.path(data_path, "axis_summary.csv"))

axis_data$graph_id <- as.numeric(axis_data$graph_id)

axis_data %>%
  arrange(graph_id)

# For min max files
minmaxfiles <- dir(data_path, pattern = "*m.csv") # get file names

minmax_data <- minmaxfiles %>%
  map(function(x) read_csv(file.path(data_path, x))) %>%  
  reduce(rbind) %>%
  mutate(graph_id = index) %>%
  select(- index)

minmax_data$min <- as.character(minmax_data$min)
minmax_data$max <- as.character(minmax_data$max)
minmax_data$min <- as.numeric(minmax_data$min)
minmax_data$max <- as.numeric(minmax_data$max)

write_csv(minmax_data, file.path(data_path, "minmax_summary.csv"))

minmax_summary <- read_csv("summary_stats/minmax_summary.csv")

minmax_summary$min <- as.character(minmax_summary$min)
minmax_summary$max <- as.character(minmax_summary$max)
minmax_summary$min <- as.numeric(minmax_summary$min)
minmax_summary$max <- as.numeric(minmax_summary$max)

minmax_df <- minmax_summary %>% 
  rename(y_label = V2) %>%
  group_by(y_label) %>%
  summarise(min = min(min), max = max(max)) %>%
  as.data.frame()

my_graphs <- merge(my_graphs , minmax_df, by = "y_label") 

check <- my_graphs %>%
  mutate(min_safe = y_min < min,
         max_safe = y_max > max) %>%
  select(y_label, 
         graph_id, 
         y_min,
         y_max,
         min,
         max,
         min_safe,
         max_safe)

# BLANK GRAPHS
blank_graph <- function(df, labx, laby, title, y_min, y_max) {
  set.seed(1234+index)
  b <- df %>% 
    ggplot(aes(x = x, y = y)) +
    geom_jitter(width = .1, alpha = 0, size = .25, height = 0) +
    labs(x = labx, y = laby, title = title) +
    theme_mres() +
    theme(plot.title = element_blank()) +
    scale_y_continuous(sec.axis = dup_axis(),
                       expand = c(0, 0), limits = c(y_min, y_max))
  
  vec <- ggplot_build(b)$layout$panel_params[[1]]$y.range
  
  b <- set_panel_size(b,
                      width  = unit(6, "cm"),
                      height = unit(4, "cm"))
  b <- arrangeGrob(b)
  
  return(list(b, vec))
  
}

save_blank_graph <- function(current_graph){
  
  ggsave(paste0("graphs/graph", index, "b.png"), 
         current_graph, 
         width = 3.5, height = 1.8, dpi = 300)
  
  build_this_one %>%
    group_by(x) %>%
    summarise(mean = mean(y), sd = sd(y)) %>%
    cbind(index) %>%
    write_csv(paste0("summary_stats/summary", index, "b.csv"))
}  

# for loop for blank graphs
for(index in my_graphs$graph_id) {
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_df() 
  
  my_blank_graph <- blank_graph(build_this_one, 
                                my_graphs[my_graphs$graph_id == index,]$x_label, 
                                my_graphs[my_graphs$graph_id == index,]$y_label, 
                                my_graphs[my_graphs$graph_id == index,]$title,
                                my_graphs[my_graphs$graph_id == index,]$y_min,
                                my_graphs[my_graphs$graph_id == index,]$y_max)
  
  save_blank_graph(my_blank_graph[[1]])
  my_blank_graph[[2]] %>%
    enframe() %>%
    pivot_wider(names_from = name, values_from = value) %>%
    cbind(index) %>%
    write_csv(., paste0("summary_stats/summary", index, "b.csv"))
} 


# Axis limits files for blank graphs
# To ensure that the axes are the same as the graphs with data
blankaxisfiles <- dir(data_path, pattern = "*b.csv") # get file names

blankaxis_data <- blankaxisfiles %>%
  map(function(x) read_csv(file.path(data_path, x))) %>%  
  reduce(rbind) %>%
  mutate(graph_id = index) %>%
  select(- index)

write_csv(blankaxis_data, file.path(data_path, "blank_axis_summary.csv"))


# to fit PSYCHOPY, and maintain aspect ratio:
# the width and height specified in ggsave()
# are multiplied by 0.36 
# so in PSYCHOPY, height = 
3.5 * 0.36
# and width = 
1.8 * 0.36
