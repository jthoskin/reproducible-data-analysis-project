#following code used to import the raw data and save into global enviroment#
player_salaries <- read_csv("Data/raw/2018-19_nba_player-salaries.csv")
player_statistics <- read_csv("Data/raw/2018-19_nba_player_statistics.csv")
team_stats_1 <- read_csv("Data/raw/2018-19_nba_team_statistics_1.csv")
team_stats_2 <- read_csv("Data/raw/2018-19_nba_team_statistics_2.csv") 
team_payroll <- read_csv("Data/raw/2019-20_nba_team-payroll.csv")


player_salaries <- select(player_salaries,
                          player_id:salary) #delete the empty columns in player salary dataset#

team_stats_1 <- select(team_stats_1, 1:22) #delete the empty columns in team_stats_1#

team_stats_joined <- full_join(x = team_stats_1, 
                               y = team_stats_2[-c(1)], #dont need this column as ranking is in first dataset# 
                               by = c("Team")) #combine team_stats_1 and team_stats_2 into single Data Frame#

team_stats_joined <-team_stats_joined[c(1:5,23, 24, 6:22, 25:45)] #reorder move MP, G to the start#

sum(is.na(team_stats_joined))
 #no N/As in new datset# 

write_csv(x = team_stats_joined,
          path = "Data/processed/2018-19_nba_team_stats_combined.csv") #save processed team data#

player_stats <- player_statistics %>% 
  arrange(player_name, desc(Tm =="TOT")) %>% 
  distinct(player_name,
           .keep_all = TRUE) #remove players who are repeated due to trades and keep the sum of all stats for all teams played for in the season#

player_stats <- player_stats %>% #remove Latin Ascii from player names to match player_salaries#
  mutate(player_name = stri_trans_general(str = player_name, "Latin-ASCII"))

player_stats <- player_stats %>% #replace .. with nothing to make names match player_salries#
  mutate(player_name =  str_replace_all(player_name, pattern = "\\.", 
                                        replacement = ""))
player_stats <- player_stats %>%
  filter(player_name != "Cameron Reynolds" &
           player_name != "Ish Smith" & player_name != "JJ Barea" &
           player_name != "Naz Mitrou-Long" & player_name != "Nene Hilario" &
           player_name != "Ray Spalding" & player_name != "Timothe Luwawu-Cabarrot" &
           player_name != "Vince Edwards" & player_name != "Walt Lemon" &
           player_name != "Luc Mbah a Moute") #this removes all players who are two-way players or players stil beng paid while retired#


player_stats$player_name <- recode(player_stats$player_name, "DeAndre' Bembry" = "DeAndre Bembry",
                                   "Dennis Schroder" = "Dennis Schroeder", "Devonte' Graham" = "Devonte Graham",
                                   "Jakob Poltl" = "Jakob Poeltl", "Lou Williams" = "Louis Williams",
                                   "Maurice Harkless" = "Moe Harkless", "Patty Mills" = "Patrick Mills",
                                   "Taurean Waller-Prince" = "Taurean Prince" ) #change names to match layer_salries#


player_salaries <- player_salaries[, -c(1)] #remove player ID from player_salaries to combine with player_stats#

players_data_combined <- left_join(x = player_stats, 
                                   y = player_salaries, 
                                   by = c("player_name")) #add salaries to player data#

subset(players_data_combined, is.na(players_data_combined$salary)) %>% select(player_name)
#shows that 18 players still dont have salaries#

player_stats <- player_stats %>%
  filter(player_name != "Cameron Reynolds" &
         player_name != "Ish Smith" & player_name != "JJ Barea" &
         player_name != "Naz Mitrou-Long" & player_name != "Nene Hilario" &
         player_name != "Ray Spalding" & player_name != "Timothe Luwawu-Cabarrot" &
         player_name != "Vince Edwards" & player_name != "Walt Lemon" &
         player_name != "Luc Mbah a Moute") #this removes all players who are two-way players or players stil beng paid while retired#


player_stats$player_name <- recode(player_stats$player_name, "DeAndre' Bembry" = "DeAndre Bembry",
                                            "Dennis Schroder" = "Dennis Schroeder", "Devonte' Graham" = "Devonte Graham",
                                            "Jakob Poltl" = "Jakob Poeltl", "Lou Williams" = "Louis Williams",
                                            "Maurice Harkless" = "Moe Harkless", "Patty Mills" = "Patrick Mills",
                                            "Taurean Waller-Prince" = "Taurean Prince" ) #change names to match Player salries#

players_data_combined[is.na(players_data_combined)] = 0 #all N/As in data need to be 0 as it is usually due to 0 occurences#

write_csv(x = players_data_combined,
          path = "Data/processed/2018-19_nba_player_stats_salaries_combined.csv") #save processed player data#



