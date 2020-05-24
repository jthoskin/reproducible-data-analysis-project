library(dplyr)
library(magrittr)
library(tidyverse)

players <- read_csv("Data/processed/2018-19_nba_player_stats_salaries_combined.csv")#read in processed players data#

team <- read_csv("Data/processed/2018-19_nba_team_stats_combined.csv")#read in processed team data#

head(team) #check team data format#

head(players) #check player data format#

team %>% ggplot(aes(x = `eFG%`, y = W, colour = Team)) + geom_point()
 #shows relationship between eFG% and positive effect on wins# 

team %>% ggplot(aes(x = `3P%`, y = W, colour = Team)) + geom_point()
#shows relationship between 3p% and wins, again shows positive effect#

team %>% ggplot(aes(x = `3PA`, y = W, colour = Team)) + geom_point()
#shows relationship between 3PA and wins#

team %>% ggplot(aes(x = `2P%`, y = W, colour = Team)) + geom_point()
#shows relationship between 2PA and wins#

team <- mutate(team, PTS_per_game = PTS/G) #adds points per game for further analysis#

team %>% ggplot(aes(x = PTS_per_game, y = W, colour = Team)) + geom_point()
#shows relationship between teams and pts per game and wins#

#overall these three factors show that they are important to the overall number of wins#
#this means these three factors will be further analsed for individual players#


players <- mutate(players, 
                  Pts_per_game = PTS/G, #create points per game variable#
                  PTS_per_min = PTS/MP, #create points per minute variable#
                  FG_z = (FG - mean(FG)) / sd(FG), #create z score for FGs to see variance#
                  FG_category = if_else(condition = FG_z < 0, #create condition to see if players are above below average for fgs made#
                                        true = "below average", false = "above average"))

filter(players, 
       FG_category == "above average") #shows list of 212 players with above average FG made#


Pts_per_game <- select(players, player_name,
                       Pos, Pts_per_game, salary) %>%  #saves Pts_per_game leaders from highest to lowest with salaries#
                       arrange(desc(Pts_per_game)) #note here devin booker stands out already as a target#

head(Pts_per_game, 15) #shows top 15 points per game and salaries#
#going off just points Devin Booker and karl Anthony towns look like potential targets before further analysis#

players <- mutate(players, AST_TOV_ratio = AST/TOV) #added assist to turnover ratio#


players <- rename(players, 'FG_per' = 'FG%',
                  'x3P_per' = '3P%', 'x2P_per' = '2P%',
                  'eFG_per' = 'eFG%', 'FT_per' = 'FT%', 
                  'x3P' = '3P', 'x3PA' = '3PA', 'x2P' = '2P', 
                  'x2PA' = '2PA') #had to remove % and number symbols#

naniar::vis_miss(players) #shows some missing values in new variables#


players[is.na(players)] = 0 #values missing are because of nodata so make them 0#

sum(is.na(players)) #no more NAs#

players$Pos <- as.factor(players$Pos) #change Pos to factor#

write_csv(x = players,  
          path = "Data/processed/2018-19_nba_players_processed.csv") #save as new fully tidy/processed file#


players %>% group_by(Pos) %>%
            summarise(mn = mean(x3P_per,
            na.rm = TRUE), sd = sd(x3P_per, 
            na.rm = TRUE), count = n()) #shows summary of three point % for positions#

ggplot(data = players, aes(x = eFG_per, 
                           y = Pts_per_game, #shows relationship between Pts_per_game and eFG_per#
                           colour = Pos)) + geom_point() 
#not much correlation between variables shows centres have highest eFG but most other positions no difference#

ggplot(data = players, aes(x = Pts_per_game,
                           y = x3P_per, colour = Pos)) + 
                           geom_point() #shows correlation between x3P and Pts_per_game#
#little correlation again shows centres have lower X3p but average more Pts_per game when it goes up#

ggplot(data = players, 
       aes(x = Pts_per_game, #shows correlation of Minutes played to points per game#
           y = MP, colour = Pos)) + geom_point()
#as expected is a correlation between minutes played and points per game#

ggplot(data = players, 
       aes(x = Pts_per_game, 
           y = salary, colour = Pos)) + 
           geom_point() #shows points per game in correlation to salaries#
#shows that there is a correlation but are obvious outliers with players on lower salaries but high points per game#
#shows 3 obvious SG that average between 20-27 ppg but only on low wage#
#shows a SF/SG at 20 PPG minimum wage and a centre at 24PPG on low wage#
#3 PGs and 3 PFs just below 20 PPG on low wages# 


shooting <- players %>% 
  filter(G >= mean(G) ) %>% 
  select(player_name, Pos, G, 
         FG_per, x3P_per, x2P_per, 
         eFG_per, salary) %>%  #creates data frame of all shooting percentages and only puts in players who played above average Gs#
  arrange(desc(eFG_per)) #arranges from highest eFG to lowest#

#shows that centres dominate highest eFG (due to most shots being under the basket and not taking any 3s)# 

filter(shooting, pos != "C") #show shooting table without centres#
#shows high number of PF but shows two potential low salary outliers Davis Bertans and Joe Harris#
#both have high  3 point percentages as well which means they efficent shooters# 

players <- mutate(players, ORB_per_game = ORB/G,
                  DRB_per_game = DRB/G, TRB_per_game = TRB/G,
                  AST_per_game = AST/G, STL_per_game = STL/G,
                  BLK_per_game = BLK/G, TOV_per_game = TOV/G) #create per game stats for players#

players <- mutate(players, x3P_game = x3P/G, x3PA_game = x3PA/G) #add 3 pointers made and attempts per game#

players <- mutate(players, x2P_game = x2P/G) #add 2 pointers per game#
 
ggplot(players, aes(x = x3P_game, y = Pts_per_game)) + 
                geom_point() + 
                geom_smooth(method = "lm") #shows relationship between 3p made per game pts per game for players#
#this this that there is a relationship between the two vaariables that the more 3s made usually means more points#

ggplot(players, aes(x = x3P_game, y = Pts_per_game,
                    colour = Pos)) + geom_point() #same relationship as previous code with positions added#
#this shows that the higher the x3p_game for each position the greater number of Pts_per_game


write_csv(x = players,  
          path = "Data/processed/2018-19_nba_players_processed.csv") #save updated processed file#

players_per_game <- players[c(1:7, 10, 13, 16:17, 45, 31:41, 30)] #create df with just per game stats for easy modelling#

write_csv(x = players_per_game,
          path = "Data/processed/2018-19_nba_per_game_stats_processed.csv") #save as csv for easier future import#
 

ggplot(players, aes(x = x3P_game,
                    y = salary/1000000,
                    colour = Pos)) + 
       geom_point() #shows salary in comparison to 3 pointers made per game for positions#

ggplot(players, aes(x = x3PA_game, 
                    y = salary/1000000, #shows salary in comparison to 3 pointers attempted per game for positions#
                     colour = Pos)) + 
       geom_point()

ggplot(players, aes(x = x2P_game, 
                    y = salary/1000000, #shows salary in comparison to 2 pointers made per game for positions#
                       colour = Pos)) + 
       geom_point()




