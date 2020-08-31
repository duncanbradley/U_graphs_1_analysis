library(tidyverse)

# PROLIFIC DATA ####
# paste the name of the folder that contains the data
location <- dir("Downloads") # get file names

# read the csv as 'all'
prolific_data <- read_csv(file.path("prolific_export.csv")) 

# including only those approved and excluding those with NOCODE (explanations below)
# dividing time_taken by 60 to get minutes
# renaming participant_id as participant to match pavlovia data
# then  selecting only participant and time_taken columns
prolific_data <- prolific_data %>%
  filter(status == "APPROVED") %>%
  filter(entered_code != "NOCODE") %>%
  mutate(time_taken = time_taken/60) %>%
  rename(participant = participant_id) %>%
  select(participant,
         time_taken)

# PAVLOVIA DATA ####
# paste the name of the folder that contains the data
folder <- "Understanding_Graphs_1_41319_2020-08-31_16h49"

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

# joining time_taken data from prolific_data dataframe
all <- inner_join(all, prolific_data, 
           by = "participant")

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



# "I finished the experiment, 
# reached the last page where I was told that all the data was fictional 
# and pressed y to understand and then spacebar to finish but then got an error: 
# ExPrag_UoM/understanding_graphs1; 
# when saving data from a previously opened session on the server; 
# the credit reserved for this session has been released: results cannot be saved.   
# Unfortunately I have to go for an appointment and will be unable to 
# try and repeat the experiment so I am submitting the results with NOCODE."

# "Hello, finally I managed to the study and I finiseh all the graphs, 
# but at the end, there appeard an info that I encountered the following error: 
# when uploading participant's results for experiment: 
# ExPrag_UoM/understanging_graphs_1 Has my submission been saved?"
# # # recorded time = 9 mins
# # # also has NOCODE as entered_code

# VISUALISE DATA TO CHECK RESPONSES - FOR APPROVING SUBMISSIONS ####
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




