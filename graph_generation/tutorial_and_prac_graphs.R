# all saved at 500x500px

library(tidyverse)
library(gganimate)
library(egg)
library(grid)

theme_mres <- function() {
  theme_minimal(base_size=2.7, base_family="Helvetica") +
    theme(plot.title = element_text(hjust = 0.5, size = 25),
          axis.text.x = element_text(size = rel(3.5)),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = rel(1)),
          axis.text.y = element_text(size = rel(3)),
          axis.title = element_text(size = rel(3)),
          panel.grid.major.x = element_blank(), # removes vertical gridlines
          panel.grid.major.y = element_blank(), # removes major horizontal gridlines
          panel.grid.minor.y = element_blank(), # removes minor horizontal gridlines
          axis.text.y.right = element_text(colour = "white"),
          axis.title.y.right = element_text(colour = "white"),
          panel.border = element_rect(colour = "black", fill = NA),
          aspect.ratio = 0.60
    )
}

# building the dataframe
set.seed(12345)
edi <- rnorm(30, 5, 4)
edi[edi<0] <- 0
gla <- rnorm(30, 4.2, 1)
gla[gla<0] <- 0
abe <- rnorm(30, 2, 0.8)
abe[abe<0] <- 0
dun <- rnorm(30, 3.7, 0.5)
dun[dun<0] <- 0
df <- data.frame(cbind(edi, gla, abe, dun))
colnames(df) <- c("Edinburgh", "Glasgow", "Aberdeen", "Dundee")

Day <- rep(1:30, 4)

df<- df %>% gather(City, Rain) %>% cbind(Day)

df %>% filter(City == 'Edinburgh') %>%
  summarise(mean = mean(Rain))

# This graph shows the average amount of rainfall per day in Edinburgh for one month
p <- df %>% filter(City == 'Edinburgh') %>% 
  ggplot(aes(x = City,
             y = Rain)) +
  geom_bar(stat = "summary", fun.y = "mean", width = .11) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  theme(panel.grid.major.y = element_line()) +
  theme(panel.grid.minor.y = element_line()) +
  labs(x = NULL) +
  theme(plot.title = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0, 15))

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/tut1.png"), p, width = 3.15, height = 1.8, dpi = 300)

# The average amount of rainfall per day was just over 5 mm


# That average is calculated using the rainfall from all 30 days in that month
set.seed(100); p <- df %>% filter(City == 'Edinburgh') %>% 
  ggplot(aes(x = City,
             y = Rain)) +
  geom_jitter((aes(group = seq_along(Day))), width =.06, alpha = .7, size = 0.3) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  theme(panel.grid.major.y = element_line()) +
  theme(panel.grid.minor.y = element_line()) +
  labs(x = NULL) +
  theme(plot.title = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0, 15))

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/tut2.png"), p, width = 3.15, height = 1.8, dpi = 300)



# On the first day of the month, there was about 7.5 mm of rainfall
set.seed(100); p <- df %>% filter(City == 'Edinburgh') %>% 
  filter(Day == '1') %>% 
  ggplot(aes(x = City,
             y = Rain)) +
  geom_jitter(width =.06, alpha = .7, size = 0.3) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  theme(panel.grid.major.y = element_line()) +
  theme(panel.grid.minor.y = element_line()) +
  labs(x = NULL) +
  theme(plot.title = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0, 15)) +   
  annotate(geom = "curve", x = 1.1, y = 3.4, xend = 1, yend = 7.1, 
           curvature = .4, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 1, y = 2, size = 3,
           label = "On the first day of the month,\nthere was about 7.5 mm of rainfall")

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/tut3.png"), p, width = 3.15, height = 1.8, dpi = 300)




# On the sixth day of the month, there was no rainfall
set.seed(100); p <- df %>%
  filter(Day == '6') %>% 
  filter(City == 'Edinburgh') %>%
  ggplot(aes(x = City,
             y = Rain)) +
  geom_jitter(width =.06, alpha = .7, size = 0.3) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  theme(panel.grid.major.y = element_line()) +
  theme(panel.grid.minor.y = element_line()) +
  labs(x = NULL) +
  theme(plot.title = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0, 15)) +   
  annotate(geom = "curve", x = 1.2, y = 5.5, xend = 1, yend = 0.3, 
           curvature = -.4, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 1, y = 7.1, size = 3,
           label = "On the sixth day of the month,\n there was no rainfall")

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/tut4.png"), p, width = 3.15, height = 1.8, dpi = 300)



# On the 26th day, there was about 12 mm of rainfall
set.seed(100); p <- df %>%
  filter(Day == '12') %>% 
  filter(City == 'Edinburgh') %>%
  ggplot(aes(x = City,
             y = Rain)) +
  geom_jitter(width =.06, alpha = .7, size = 0.3) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  theme(panel.grid.major.y = element_line()) +
  theme(panel.grid.minor.y = element_line()) +
  labs(x = NULL) +
  theme(plot.title = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0, 15)) +   
  annotate(geom = "curve", x = 1.1, y = 8.3, xend = 1, yend = 12, 
           curvature = .4, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 1, y = 6.1, size = 3,
           label = "On the 26th day,\n there was about\n12 mm of rainfall")

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/tut5.png"), p, width = 3.15, height = 1.8, dpi = 300)


# The red circle shows the average for the whole month
set.seed(100); p <- df %>% filter(City == 'Edinburgh') %>% 
  ggplot(aes(x = City,
             y = Rain)) +
  geom_jitter((aes(group = seq_along(Day))), width =.06, alpha = .7, size = 0.3) +
  stat_summary(fun.y = "mean", geom = "point", 
               colour = "red", size = 2) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  theme(panel.grid.major.y = element_line()) +
  theme(panel.grid.minor.y = element_line()) +
  labs(x = NULL) +
  theme(plot.title = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0, 15))

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/tut6.png"), p, width = 3.15, height = 1.8, dpi = 300)



# This average is exactly the same as the bar chart from earlier
set.seed(100); p <- df %>% filter(City == 'Edinburgh') %>% 
  ggplot(aes(x = City,
             y = Rain)) +
  geom_bar(stat = "summary", fun.y = "mean", width = .11, alpha = .9) +
  geom_jitter((aes(group = seq_along(Day))), width =.06, alpha = .7, size = 0.3) +
  stat_summary(fun.y = "mean", geom = "point",  
               colour = "red", size = 2) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  theme(panel.grid.major.y = element_line()) +
  theme(panel.grid.minor.y = element_line()) +
  labs(x = NULL) +
  theme(plot.title = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0, 15))

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/tut7.png"), p, width = 3.15, height = 1.8, dpi = 300)







# PRACTICE GRAPHS FOR MRES PROJECT ####

# 1 cluster only
set.seed(1234) ; p <- df %>% 
  filter(City == "Glasgow") %>%
  ggplot(aes(x = City, y = Rain)) +
  geom_jitter(width = .1, height = 0, alpha = .75, size = .25) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme(axis.text.y.right = element_text(colour = "white"),
        axis.title.y.right = element_text(colour = "white")) +
  theme(plot.title = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  coord_cartesian(ylim = c(0, 10))

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/prac1.png"), p, width = 3.15, height = 1.8, dpi = 300)


# 1 cluster blank graph
# 1 cluster only
set.seed(1234) ; p <- df %>% 
  filter(City == "Glasgow") %>%
  ggplot(aes(x = City, y = Rain)) +
  #geom_jitter(width = .1, height = 0, alpha = .75, , size = .25) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme(axis.text.y.right = element_text(colour = "white"),
        axis.title.y.right = element_text(colour = "white")) +
  theme(plot.title = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  coord_cartesian(ylim = c(0, 10))

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/prac1_blank.png"), p, width = 3.15, height = 1.8, dpi = 300)



# 3 clusters
set.seed(1234) ; p <- df %>% 
  filter(City != "Edinburgh") %>%
  ggplot(aes(x = City, y = Rain)) +
  geom_jitter(width = .1, height = 0, alpha = .75, size = .25) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme(axis.text.y.right = element_text(colour = "white"),
        axis.title.y.right = element_text(colour = "white")) +
  theme(plot.title = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill=NA))

set_panel_size(p = NULL, g = ggplot2::ggplotGrob(p), file = NULL,
               margin = unit(1, "mm"), width = unit(500, "in"), height = unit(50, "cm"))


p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/pracfull.png"), p, width = 3.15, height = 1.8, dpi = 300)



# 3 clusters blank
set.seed(1234) ; p <- df %>% 
  filter(City != "Edinburgh") %>%
  ggplot(aes(x = City, y = Rain)) +
  #geom_jitter(width = .1, height = 0, alpha = .75, , size = .25) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme(axis.text.y.right = element_text(colour = "white"),
        axis.title.y.right = element_text(colour = "white")) +
  theme(plot.title = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill=NA))

p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/pracfull_blank.png"), p, width = 3.15, height = 1.8, dpi = 300)


#3 clusters blank with gridlines to determine x_position
p <- df %>% 
  filter(City != "Edinburgh") %>%
  ggplot(aes(x = City, y = Rain)) +
  #geom_jitter(width = .1, height = 0, alpha = .75, , size = .25) +
  labs(y = "Rainfall (mm)") +
  theme_mres() +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme(axis.text.y.right = element_text(colour = "white"),
        axis.title.y.right = element_text(colour = "white")) +
  theme(plot.title = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill=NA)) +
  theme(panel.grid.major.x = element_line(size = 0.5))


p <- set_panel_size(p,
                    width  = unit(6, "cm"),
                    height = unit(4, "cm"))
p <- arrangeGrob(p)

ggsave(paste0("tutorial_and_prac_graphs/guides.png"), p, width = 3.15, height = 1.8, dpi = 300)


###############################
# ANIMATED PLOT IF NEEDED
# This shows the data points for each day in the month, in order.
df %>% filter(City == 'Edinburgh') %>% 
  ggplot(aes(x = City,
             y = Rain)) +
  geom_jitter((aes(group = seq_along(Day))), width =.11, alpha = .7) +
  labs(y = "Rainfall (mm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  theme(axis.text.x = element_text(size = rel(2.5))) +
  theme(axis.text.y = element_text(size = rel(2))) +
  theme(axis.title = element_text(size = rel(1.8))) +
  labs(x = NULL) +
  coord_cartesian(ylim = c(0, 15)) + 
  transition_reveal(Day, keep_last = TRUE) -> animated_plot
animated_plot
anim_save("tg_6.png", units = "cm", width = 24, height = 32)




