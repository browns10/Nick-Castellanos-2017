#Install Statcast Data

install.packages("tidyverse")
library(tidyverse)
install.packages("devtools")
library(devtools)
devtools::install_github('BillPetti/baseballr')
install.packages('baseballr')
library(baseballr)
s1 <- scrape_statcast_savant_batter_all("2017-04-02", 
                                        "2017-04-08")
s2 <- scrape_statcast_savant_batter_all("2017-04-09", 
                                        "2017-04-15")
s3 <- scrape_statcast_savant_batter_all("2017-04-16", 
                                        "2017-04-22")
s4 <- scrape_statcast_savant_batter_all("2017-04-23", 
                                        "2017-04-29")
s5 <- scrape_statcast_savant_batter_all("2017-04-30", 
                                        "2017-05-06")
s6 <- scrape_statcast_savant_batter_all("2017-05-07", 
                                        "2017-05-13")
s7 <- scrape_statcast_savant_batter_all("2017-05-14", 
                                        "2017-05-20")
s8 <- scrape_statcast_savant_batter_all("2017-05-21", 
                                        "2017-05-27")
s9 <- scrape_statcast_savant_batter_all("2017-05-28", 
                                        "2017-06-03")
s10 <- scrape_statcast_savant_batter_all("2017-06-04", 
                                         "2017-06-10")
s11 <- scrape_statcast_savant_batter_all("2017-06-11", 
                                         "2017-06-17")
s12 <- scrape_statcast_savant_batter_all("2017-06-18", 
                                         "2017-06-24")
s13 <- scrape_statcast_savant_batter_all("2017-06-25", 
                                         "2017-07-01")
s14 <- scrape_statcast_savant_batter_all("2017-07-02", 
                                         "2017-07-08")
s15 <- scrape_statcast_savant_batter_all("2017-07-09", 
                                         "2017-07-15")
s16 <- scrape_statcast_savant_batter_all("2017-07-16", 
                                         "2017-07-22")
s17 <- scrape_statcast_savant_batter_all("2017-07-23", 
                                         "2017-07-29")
s18 <- scrape_statcast_savant_batter_all("2017-07-30", 
                                         "2017-08-05")
s19 <- scrape_statcast_savant_batter_all("2017-08-06", 
                                         "2017-08-12")
s20 <- scrape_statcast_savant_batter_all("2017-08-13", 
                                         "2017-08-19")
s21 <- scrape_statcast_savant_batter_all("2017-08-20", 
                                         "2017-08-26")
s22 <- scrape_statcast_savant_batter_all("2017-08-27", 
                                         "2017-09-02")
s23 <- scrape_statcast_savant_batter_all("2017-09-03", 
                                         "2017-09-09")
s24 <- scrape_statcast_savant_batter_all("2017-09-10", 
                                         "2017-09-16")
s25 <- scrape_statcast_savant_batter_all("2017-09-17", 
                                         "2017-09-23")
s26 <- scrape_statcast_savant_batter_all("2017-09-24", 
                                         "2017-09-30")
s27 <- scrape_statcast_savant_batter_all("2017-10-01", 
                                         "2017-11-01")
sc1 <- rbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
             s11, s12, s13, s14)
sc2 <- rbind(s15, s16, s17, s18, s19, s20, s21,
             s22, s23, s24, s25, s26, s27)

statcast_2017 <- rbind(sc1, sc2)

#Tigers Hitters

#Nick Castellanos
playerid_lookup(last_name = "Castellanos")

#MLBAM ID: 592206      FangrpahsID: 11737

statcast_2017 %>%
  filter(batter == 592206) %>%
  write_csv("/Users/owner/Documents/BaseballBookRData/BookWork/pitch level data/Tigers_Hitters/Nick_Castellanos_2017.csv")

#load Nick Castellanos data from baseballr
Castellanos_Data <- read_csv("/Users/owner/Documents/BaseballBookRData/BookWork/pitch level data/Tigers_Hitters/Nick_Castellanos_2017.csv")

library(readr)
library(dplyr)
library(ggplot2)
library(mgcv)

#What pitches did Castellanos face in 2017?

ggplot(Castellanos_Data,
       aes(x = pitch_type,
           #bar chart with percent along y axis
           y = (..count..) / sum(..count..))) +
geom_bar() +
labs(title = "Types of Pitches Thrown Against Nick Castellanos in 2017",
       x = "Pitch Type",
       y = "Proportion of Pitches",
       caption = "Data courtesy of MLBAM") +
theme_bw()

#Narrow the most freequent pitches seen by Castellanos ->*filter()*
#how does he perform against them?
castellanos_pitch_summary_stats <- Castellanos_Data %>%
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL")) %>%
  
#Next, using the description, type, and events columns define some useful
#columns that will help us calculate stats for each of the different pitch types:
#Create an indicator for whether or not Votto made an attempt at the pitch:
mutate(swing = ifelse(description %in%
                     c("foul", "foul_bunt",
                       "foul_tip", "hit_into_play",
                       "hit_into_play_no_out",
                       "hit_into_play_score",
                       "missed_bunt", "swinging_strike",
                       "swinging_strike_blocked"), 
                   1, 0),
       miss = ifelse(description %in%
                       c("missed_bunt", "swinging_strike",
                         "swinging_strike_blocked"), 
                     1, 0),
       
#Using the type and events column, create indicators for each hit type:       
       single = ifelse(type == "X" & events == "single", 1, 0),
       double = ifelse(type == "X" & events == "double", 1, 0),
       triple = ifelse(type == "X" & events == "triple", 1, 0),
       home_run = ifelse(type == "X" & events == "home_run", 1, 0)) %>%
    
#Now can calculate various stats at the pitch type level:
group_by(pitch_type) %>%
  
#Summarise function to calculate the frequencies for each of
#these indicators:
summarise(n_pitches = n(),
            n_swings = sum(swing, na.rm = TRUE),
            n_miss = sum(miss, na.rm = TRUE),
            n_singles = sum(single, na.rm = TRUE),
            n_doubles = sum(double, na.rm = TRUE),
            n_triples = sum(triple, na.rm = TRUE),
            n_home_runs = sum(home_run, na.rm = TRUE)) %>%
  
#Calculate commonly seen baseball stats:
mutate(swing_rate = round(n_swings / n_pitches, 3),
         SwStr_rate = round(n_miss / n_pitches, 3),
         miss_rate = round(n_miss / n_swings, 3),
         batting_average = round((n_singles + n_doubles + n_triples + n_home_runs) / n_swings, 3),
         slugging_percentage = round((n_singles + 2 * n_doubles + 3 * n_triples + 4 * n_home_runs) / n_swings, 3),
         ops = round(batting_average + slugging_percentage, 3))  

#Visual summaries of Castellanos' Whiff rate against different pitches
castellanos_pitch_summary_stats %>%
ggplot(aes(x = pitch_type, y = miss_rate, fill = pitch_type)) + 
scale_fill_brewer(palette = "Set1", guide = FALSE) +
geom_bar(stat = "identity") + 
labs(title = "Nick Castellanos Whiff Rate by Pitch Type in 2017",
       caption = "Data courtesy of MLBAM",
       x = "Pitch Type",
       y = "Whiff Rate") +
  theme_bw()

#Visual summmaries of Castellanos' SwStr rate
castellanos_pitch_summary_stats %>%
  ggplot(aes(x = pitch_type, y = SwStr_rate, fill = pitch_type)) + 
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  geom_bar(stat = "identity") + 
  labs(title = "Nick Castellanos SwStr % by Pitch Type in 2017",
       caption = "Data courtesy of MLBAM",
       x = "Pitch Type",
       y = "SwStr %") +
  theme_bw()

#Visualize summaries of Castellanos' OPS against different pitches 
castellanos_pitch_summary_stats %>%
ggplot(aes(x = pitch_type, y = ops, fill = pitch_type)) + 
scale_fill_brewer(palette = "Set1", guide = FALSE) +
geom_bar(stat = "identity") + 
labs(title = "Nick Castellanos OPS by Pitch Type in 2017",
       caption = "Data courtesy of MLBAM",
       x = "Pitch Type",
       y = "OPS") +
  theme_bw()  

#Strize zones technically vary by batter (and umpire...) but we'll use an average
#The `plate_x` and `plate_z` variables are the horizontal and vertical 
#locations of the pitches, in feet from center of strike zone.

#Create a data frame of the strike zone corners:
top_zone <- 3.5
bot_zone <- 1.6
left_zone <- -0.95
right_zone <- 0.95
strike_zone_df <- data.frame(
x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone))

#Visualize all pitches seen against Castellanos last season
Castellanos_Data %>%
#First filter the dataset to be the most common pitches:
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL"),
         type == "X") %>%
  
#Plot all pitches Castellanos saw in 2017 with the strike zone over top:
ggplot(Castellanos_Data, aes(x = plate_x, y = plate_z)) + 
geom_point(alpha = 0.5) + 
geom_path(data = strike_zone_df,aes(x, y), lwd = 1.0, color = "red") + 
#coord_fixed just makes sure the axes are scaled properly in relation to each other
coord_fixed() +
labs(title = "Location of All Pitches Thrown Against Nick Castellanos in 2017 ",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)") +
  theme_bw()  

#Most common types of pitches
Castellanos_Data %>%
#First filter the Castellanos dataset to be the most common pitches:
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL")) %>%
ggplot(aes(x = plate_x, y = plate_z, color = pitch_type)) + 
geom_point(alpha = 0.5) + 
scale_color_brewer(palette = "Set1", "Pitch Type") + 
geom_path(data = strike_zone_df,aes(x, y), lwd = 2.0, color = "black") + 
coord_fixed() +
labs(title = "Location of Pitches Thrown Against Nick Castellanos in 2017 by Pitch Type",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)") +
  theme_bw()
 
#Model Nick Castellanos' swing and whiff tendencies across the strike zone
Castellanos_Data %>%
mutate(swing = ifelse(description %in%
                        c("foul", "foul_bunt",
                          "foul_tip", "hit_into_play",
                          "hit_into_play_no_out",
                          "hit_into_play_score",
                          "missed_bunt", "swinging_strike",
                          "swinging_strike_blocked"), 
                      1, 0),
       miss = ifelse(description %in%
                       c("missed_bunt", "swinging_strike",
                         "swinging_strike_blocked"), 
                     1, 0))


#Using the pitch location, we can predict the probability of Nick swinging
swing_model_fit <- gam(swing ~ s(plate_x, plate_z), family = binomial, data = Castellanos_Data)

#Find predicted probabilities over a 50 x 50 grid
x <- seq(-1.5, 1.5, length.out=50)
z <- seq(0.5, 5, length.out=50)
swing_predict_data <- data.frame(plate_x = c(outer(x, z * 0 + 1)),
                                 plate_z = c(outer(x * 0 + 1, z)))

#Get the predicted values from the model and convert to probability values:
swing_model_preds <- predict(swing_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
mutate(swing_prob = exp(swing_model_preds) / (1 + exp(swing_model_preds)))

#Now plot Castellanos' predicted probabilities of swinging with the strike zone:
ggplot(swing_predict_data) +
geom_tile(aes(x = plate_x, y = plate_z, fill = swing_prob)) +
scale_fill_gradient(low = "darkblue", high = "darkorange1", "Swing Probability",
                    limit = c(0,1)) +
geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
coord_fixed() +
theme_bw() + 
labs(title = "Nick Castellanos' Swing Probability in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 




#Swing and miss probability:
miss_model_fit <- gam(miss ~ s(plate_x, plate_z), family=binomial, 
                      data = filter(Castellanos_Data, swing == 1))

#Get the predicted values from the model and convert to probability values:
miss_model_preds <- predict(miss_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
mutate(miss_prob = exp(miss_model_preds) / (1 + exp(miss_model_preds)))

ggplot(swing_predict_data) +
geom_tile(aes(x = plate_x, y = plate_z, fill = miss_prob)) +
scale_fill_gradient(low = "darkblue", high = "darkorange1", "Whiff Probability",
                      limit = c(0,1)) +
geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
coord_fixed() +
theme_bw() + 
labs(title = "Castellanos' Whiff Probability in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 





#How does the count factor in? (Could also use the unite function in the tidyr package)
Castellanos_Data %>%
  mutate(count = paste(as.character(balls), "-", as.character(strikes)))

#Swing and miss probability with two strikes
miss_model_fit_two_strikes <- gam(miss ~ s(plate_x, plate_z), family=binomial, 
                                  data = filter(castellanos_data, swing == 1, 
                                                count %in% c( "0 - 2", "1 - 2", "2 - 2", "3 - 2")))

#Get the predicted values from the model and convert to probability values:
miss_model_preds_two_strikes <- predict(miss_model_fit_two_strikes, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(miss_prob_two_strikes = exp(miss_model_preds_two_strikes) / (1 + exp(miss_model_preds_two_strikes)))

ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = miss_prob_two_strikes)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Whiff Probability",
                      limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  labs(title = "Nick Castellanos' Whiff Probability in 2017 with Two Strikes",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 


#What about exit velocity?
velo_model_fit <- gam(launch_speed ~ s(plate_x, plate_z), 
                      data = filter(Castellanos_Data, type == "X"))

#Get the predicted values from the model and convert to probability values:
velo_model_preds <- predict(velo_model_fit, swing_predict_data)
swing_predict_data <- swing_predict_data %>%
  mutate(exit_velo = velo_model_preds)

ggplot(swing_predict_data) +
  geom_tile(aes(x = plate_x, y = plate_z, fill = exit_velo)) +
  scale_fill_gradient(low = "darkblue", high = "darkorange1", "Exit Velocity (mph)") +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "white") + 
  coord_fixed() +
  theme_bw() + 
  labs(title = "Nick Castellanos' Exit Velocity in 2017",
       subtitle = "From the Catcher's POV: Against RHP & LHP",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)",
       caption = "Data courtesy of MLBAM") 




#Spray Chart of Castellanos' batted balls?

#Need to adjust the columns provided to us that represent the batted ball coordinates:
Castellanos_Data <- Castellanos_Data %>%
  mutate(hit_x = hc_x - 125.42, 
         hit_y = 198.27 - hc_y)

#Spray chart showing the density of Castellanos' batted balls:
#geom_point reaveals each type batted ball
Castellanos_Data %>%
  filter(type == "X") %>%
  ggplot(aes(x = hit_x, y = hit_y)) + 
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="darkblue", high="darkorange1", "Density") +
  #geom_point(aes(x = hit_x, y = hit_y, color = events), fill = "black", shape = 21) +
  #geom_segment(x=0, xend = 100, y=0, yend = 100, color = "white") +
  geom_segment(x=0, xend = -100, y=0, yend = 100, color = "white") +
  geom_curve(x = -45, xend = 45, y = 53, yend = 53, curvature = -.65, linetype = "dotted", color = "white") +
  theme_bw() + 
  labs(title = "Spray Chart of Nick Castellanos' Batted Balls in 2017",
       caption = "Data courtesy of MLBAM") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())# + 
#facet reveals two different spray charts... vsRHP and vs LHP  
#facet_grid(. ~ p_throws)


#Spray charts of all batted balls with hit types and plotted locations
#Visualized Splits (LHP & RHP)

Castellanos_Data %>%
  filter(type == "X") %>%
  ggplot(aes(x = hit_x, y = hit_y)) + 
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="darkblue", high="darkorange1", "Density") +
  geom_point(aes(x = hit_x, y = hit_y, color = events), fill = "black", shape = 21) +
  geom_segment(x=0, xend = 100, y=0, yend = 100, color = "white") +
  geom_segment(x=0, xend = -100, y=0, yend = 100, color = "white") +
  geom_curve(x = -45, xend = 45, y = 53, yend = 53, curvature = -.65, linetype = "dotted", color = "white") +
  theme_bw() + 
  labs(title = "Spray Chart of Nick Castellanos' Batted Balls in 2017",
       caption = "Data courtesy of MLBAM") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) + 
#facet reveals two different spray charts... vsRHP and vs LHP  
facet_grid(. ~ p_throws)


















