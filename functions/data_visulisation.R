team <- read_csv("Data/processed/2018-19_nba_team_stats_combined.csv")#import team data#

players_poss <- read_csv("Data/processed/nba_player_possesion_exp_points.csv") #import possesion modeled data#

x3P_wins <-team %>% ggplot(aes(x = `3P`, y = W)) + geom_point(aes(colour = W)) + 
scale_colour_gradient(low = "green", high = "red") #create plot showing team wins against three pointers taken#

                                                               
x3P_wins + labs(title = "Three pointers made impact on team wins",
                subtitle = "Teams with a better win record tend to make more threes",
                x = "Three pointers made",
                y = "Wins") + 
          theme_classic() + 
          theme(plot.title = element_text(size = 11, face = "bold")) #add labels and titles and colours to previous figure#


players_poss %>% ggplot(aes(x = salary/1000000, y = exp_points, color = Pos)) + 
  geom_point() +
  xlab("Salary (Millions)") + labs(title = "Expected points in comparison to salary",
                                   subtitle = "Players with high expected points not always highly paid",
                                   y = "Expected points") + 
          theme_classic() + 
          theme(plot.title = element_text(size = 11, face = "bold")) 
#create plot showing players salries in comparison to created expected points metric#
