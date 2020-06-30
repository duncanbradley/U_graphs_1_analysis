library(tidyverse)

# paste the name of the folder that contains the data
folder <- "Understanding_Graphs_1_41319_2020-06-25_10h50"

# locate the (only) csv file
csv_file <- dir(folder, pattern = "*.csv") # get file names

# read the csv as 'all'
all <- read_csv(file.path(folder, csv_file))

# the first in the database is a test
unique(all$participant)

# remove this test participant
all <- all %>% filter(participant != "test1")

# put each Prolific ID in a vector, each occurring once
keys <- unique(all$participant)

# creates a vector of numbers from 1 to the number of participants
vals <- 1:length(keys)

# replaces each Prolific ID with the corresponding number from the `vals` vector
all$participant <- vals[ match(all$participant, keys) ]

# remove all other columns containing Prolific IDs
# and columns that ARE Prolific IDs (start with '5e'...)
all <- all %>% 
  select(- session,
         - `__session`,
         - `__participant`,
         - starts_with("5e"))

# write new csv
write_csv(all, "U_graphs_1_data.csv")

###################################
# VISUALISE DATA TO CHECK RESPONSES
# graphs 53 and 58
all %>% 
  select(slider_1.response, 
         slider_2.response, 
         slider_3.response, 
         graph_image, 
         participant) %>%
  filter(graph_image == "graphs/graph27.png") %>%
  pivot_longer(names_to = "names", 
               values_to = "values",
               cols = c("slider_1.response", "slider_2.response", "slider_3.response")) %>%
  ggplot(aes(x = names,
             y = values)) +
  geom_jitter(width = 0.1) +
  ylim(0,1) +
  geom_text(aes(label= participant), hjust = 0, vjust = 0)




