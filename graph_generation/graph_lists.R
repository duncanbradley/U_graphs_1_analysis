library(tidyverse)

numbers <- c(1:9)
numbers <- (numbers+9)
numbers

marathon <- as_tibble(paste0('graphs/graph', 1:9, '.png')) %>% 
  rename(graph_image = value) %>%
  cbind(paste0('graphs/graph', 1:9, 'b.png')) %>%
  rename(blank_graph_image = 2) %>%
  write_csv(path = "lists/marathon_block.csv")

teachers <- as_tibble(paste0('graphs/graph', 10:18, '.png')) %>% 
  rename(graph_image = value) %>%
  cbind(paste0('graphs/graph', 10:18, 'b.png')) %>%
  rename(blank_graph_image = 2) %>%
  write_csv(path = "lists/teachers_block.csv")

schools <- as_tibble(paste0('graphs/graph', 19:27, '.png')) %>% 
  rename(graph_image = value) %>%
  cbind(paste0('graphs/graph', 19:27, 'b.png')) %>%
  rename(blank_graph_image = 2) %>%
  write_csv(path = "lists/schools_block.csv")

performances <- as_tibble(paste0('graphs/graph', 28:36, '.png')) %>% 
  rename(graph_image = value) %>%
  cbind(paste0('graphs/graph', 28:36, 'b.png')) %>%
  rename(blank_graph_image = 2) %>%
  write_csv(path = "lists/performances_block.csv")

teachers2 <- as_tibble(paste0('graphs/graph', 37:45, '.png')) %>% 
  rename(graph_image = value) %>%
  cbind(paste0('graphs/graph', 37:45, 'b.png')) %>%
  rename(blank_graph_image = 2) %>%
  write_csv(path = "lists/teachers2_block.csv")

trains <- as_tibble(paste0('graphs/graph', 46:54, '.png')) %>% 
  rename(graph_image = value) %>%
  cbind(paste0('graphs/graph', 46:54, 'b.png')) %>%
  rename(blank_graph_image = 2) %>%
  write_csv(path = "lists/trains_block.csv")

taxi <- as_tibble(paste0('graphs/graph', 55:63, '.png')) %>% 
  rename(graph_image = value) %>%
  cbind(paste0('graphs/graph', 55:63, 'b.png')) %>%
  rename(blank_graph_image = 2) %>%
  write_csv(path = "lists/taxi_block.csv")

expenditure <- as_tibble(paste0('graphs/graph', 64:72, '.png')) %>% 
  rename(graph_image = value) %>%
  cbind(paste0('graphs/graph', 64:72, 'b.png')) %>%
  rename(blank_graph_image = 2) %>%
  write_csv(path = "lists/expenditure_block.csv")

choose_blocks <- as_tibble(c('lists/marathon_block.csv',
                             'lists/teachers_block.csv',
                             'lists/schools_block.csv',
                             'lists/performances_block.csv',
                             'lists/teachers2_block.csv',
                             'lists/trains_block.csv',
                             'lists/taxi_block.csv',
                             'lists/expenditure_block.csv')) %>%
  rename(block_file = value) %>%
  cbind(c('The following graphs show marathon times for a sample of 24 runners from each country.',
          "The following graphs show how pupils' exam grades changed in one year. The data are taken from 24 pupils from each teacher's class.",
          'The following graphs show how much money schools spend per pupil (on average). The data are taken from 24 schools in and around each English town or city.',
          'The following graphs show the number of live music performances held per year in different cities. The data are taken from 24 venues in each city.',
          "The following graphs show teachers' performance. 24 pupils in each teacher's class were asked to rate their lesson out of 100.",
          'The following graphs show the proportion of delayed trains arriving into English train stations each month, over a 24 month period.',
          'The following graphs show the average trip length for 24 taxi drivers in various cities.',
          'The following graphs show how much money different companies invest in sustainability. The data are taken from 24 companies from each country.')) %>%
  rename(intro_text = 2) %>%
  write_csv(path = "lists/choose_block.csv")



tutorial <- as_tibble(c("tutorial_and_prac_graphs/tut1.png",
                        "tutorial_and_prac_graphs/tut1.png",
                        "tutorial_and_prac_graphs/tut2.png",
                        "tutorial_and_prac_graphs/tut3.png",
                        "tutorial_and_prac_graphs/tut4.png",
                        "tutorial_and_prac_graphs/tut5.png",
                        "tutorial_and_prac_graphs/tut6.png",
                        "tutorial_and_prac_graphs/tut7.png")) %>%
  rename(tut_graph_image = value) %>%
  cbind(c('This graph shows the average amount of \n rainfall per day in Edinburgh for one month', #tut1
          
          'The average amount of rainfall per day \n was just over 5 mm', #tut1
          
          'That average is calculated using the \n rainfall from all 30 days in that month', #tut2
          
          'That average is calculated using the \n rainfall from all 30 days in that month', #tut3
          'That average is calculated using the \n rainfall from all 30 days in that month', #tut4
          'That average is calculated using the \n rainfall from all 30 days in that month', #tut5
          
          'The red circle shows the average for \n the whole month', #tut6
          
          'This average is exactly the same as \n the bar chart from earlier' #tut7
  )) %>%
  rename(tut_text = 2) %>%
  write_csv(path = "lists/tutorial.csv")


prac_instructions_old <- as_tibble(c(
  'In this experiment, you will be asked to estimate the average value for clusters of raw data, as shown in the example you just saw.',
  'Each graph will be displayed for 4 seconds.',
  'Then, you should use the slider to position the red dot where you think the average value is.',
  'The red dot will appear when you click on the line.', 
  'You can then adjust its position according to where you think the average was.',
  'You will now be given an opportunity to practice this. First, try a graph with one dot-cluster, then try a graph with three dot-clusters.',
  'Your mouse will be temporarily hidden whilst you view the graphs.'
)) %>%
  rename(prac_instructions_text = value)

prac_instructions <- as_tibble(c(
  'In this experiment, you will be asked to estimate the average value for clusters of raw data, as shown in the example you just saw.',
  'Each graph will be displayed for 4 seconds.',
  'Then, you should use the slider to position the red dot where you think the average value is.',
  'You will now be shown a video demonstrating this. Please make sure your computer sound is on so that you can hear the commentary.'
)) %>%
  rename(prac_instructions_text = value) %>%
  write_csv(path = "lists/prac_instructions.csv")

consent_form <- as_tibble(c(
  'I confirm that I have read the study description for this study and have had the opportunity to consider the information.',
  'I understand that my participation in the study is voluntary and that I am free to withdraw at any time without giving a reason and without detriment to myself, but I will not be paid for the study.  I understand that it will not be possible to remove my data from the project once it has been anonymised and forms part of the data set.  
I agree to take part on this basis.',
  'I understand that any data collected may be published in anonymous form in academic books, reports or journals. I understand that it will also be permanently stored in anonymous form in a public online repository, and may be used for future studies.',
  'I understand that data collected during the study may be looked at by individuals from The University of Manchester or regulatory authorities, where it is relevant to my taking part in this research. I give permission for these individuals to have access to my data.',
  'I agree to take part in this study.'
)) %>%
  rename(new_questionText = value) %>% 
  cbind(c("1 of 5", "2 of 5", "3 of 5", "4 of 5", "5 of 5"
  )) %>%
  rename(consent_number = 2) %>%
  write_csv(path = "lists/consent_form.csv")

