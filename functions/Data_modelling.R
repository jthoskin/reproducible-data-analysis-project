library(dplyr)
library(magrittr)
library(tidyverse)
library(broom)
players_per_game <- read_csv("Data/processed/2018-19_nba_per_game_stats_processed.csv") #import player_per_game stats for linear model#

team <- read_csv("Data/processed/2018-19_nba_team_stats_combined.csv")#read in processed team data#

pairs(formula = ~ Pts_per_game + x3P_game + x2P_game + TRB_per_game + AST_per_game data = players_per_game) #this creates a figure to inspect if these variables are linear with pts per game#
#3, 2 pointers per game and TRB have an impact on points but not on each other so it is ok to continue#

lm_players <- lm(Pts_per_game ~  #create a multiple linear model to see how these variables impact PTS per game#
                   x3P_game + x2P_game + TRB_per_game + AST_per_game, data = players_per_game)

tidy(lm_players, confit.int = TRUE) #shows the output of the created model#

teams_per_game <- team %>% mutate(x3P_game = `3P`/G, x2P_game = `2P`/G, TRB_per_game = TRB/G,
                                  STL_per_game = STL/G, BLK_per_game = BLK/G,#creates team statistics per game#
                                  AST_per_game = AST/G, TOV_per_game = TOV/G)


teams_per_game <- mutate(teams_per_game, #creates team expected points using the model created#
                         exp_pts_per_game = predict(lm_players,
                                                    newdata = teams_per_game))


ggplot(teams_per_game, aes(x = W, y = exp_pts_per_game, label = Team)) + #shows expected points in comparison to wins#
       geom_point(colour = "dodgerblue") +  geom_text(nudge_x = 2.5, cex = 3)

players <- read_csv("Data/processed/2018-19_nba_players_processed.csv") #read in processed player data#
#read in processed full player stats for player possesion metric#



poss_per_game <- players %>% group_by(Tm) %>% 
  summarize(poss_per_game = sum(FGA - ORB + TOV + (0.44 * FTA)) / max(G)) %>%
  pull(poss_per_game) %>% mean() #creates possesion per game league average to normalzize player data#
                             
players <- players %>% 
  mutate(player_poss = FGA - ORB + TOV + (0.44 * FTA)) #creates possesions for each player for the season#


players_poss <- players %>% group_by(player_name) %>%
                                      summarise(G = sum(player_poss) / poss_per_game, 
                                      FG_per_game = sum(FG)/G, 
                                      x3P_game = sum(x3P)/G,
                                      x2P_game = sum(x2P)/G, 
                                      FT_per_game = sum(FT)/G, 
                                      ORB_per_game = sum(ORB)/G, 
                                      DRB_per_game = sum(DRB)/G, 
                                      TRB_per_game = sum(TRB)/G,
                                      AST_per_game = sum(AST)/G, 
                                      STL_per_game = sum(STL)/G, 
                                      BLK_per_game = sum(BLK)/G,
                                      TOV_per_game = sum(TOV)/G, 
                                      player_poss = sum(player_poss)) %>% 
  filter(player_poss >= 400) %>% select(-G) 
#creates new standarized player stats adjusted for possesions using league average#

players_poss <- mutate(players_poss,
                       exp_points = predict(lm_players, #predicts points per game for each player if team consisted of just that player#
                                            newdata = players_poss))  

players_poss <- players %>% select(player_name, Pos, salary) %>% 
  right_join(players_poss, by = "player_name") #adds postion and salary to new data#

players_poss %>% ggplot(aes(x = salary/1000000, y = exp_points, color = Pos)) + 
  geom_point() +
  xlab("Salary (Millions)") #creates plot to show player salaries in comparison to expected points#

players_poss %>% filter(Pos == "C") %>% select(player_name, salary,TOV_per_game, x2P_game,
                                               TRB_per_game, ORB_per_game,
                                               BLK_per_game, player_poss ,exp_points) %>%
  arrange(desc(exp_points), salary) %>% top_n(5) 
#used to determine which players were most valuable adjustin position and variables depndent on postion and desired statistics # 
  
write_csv(x = players_poss,
          path = "Data/processed/nba_player_possesion_exp_points.csv") #save new created metric as a csv for easier future analysis#
  
  
