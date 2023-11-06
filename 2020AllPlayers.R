library(tidyverse)
library (ggrepel)
library (nflreadr)
library (nflplotR)


getwd()

receiving2020 <- read.csv("receiving_summary2020.csv")
passing2020 <- read.csv("passing_summary2020.csv")
blocking2020 <- read.csv("offense_blocking2020.csv")
defense2020 <- read.csv("defense_summary2020.csv")
osnaps2020 <- read.csv("offense_snaps2020.csv")
dsnaps2020 <- read.csv("defense_snaps2020.csv")
contracts <- read.csv("contracts2018_2020.csv")



filtered_contracts2020 <- contracts %>%
  summarize(name = Player, cap = Cap.Number, year =  Year) %>%
    filter(year == 2020)

print(filtered_contracts2020)

filtered_osnaps <- osnaps2020 %>%
  summarize(name = NAME, team = TEAM, snaps = Total)

filtered_dsnaps <- dsnaps2020 %>%
  summarize(name = NAME, team = TEAM, snaps = Total)

total_snaps <- rbind(filtered_dsnaps, filtered_osnaps) %>%
  arrange(-snaps)

total_snaps

filtered_receiving <- receiving2020 %>%
  summarize(name = player, id = player_id, grade = grades_offense, position = position, games = player_game_count) %>%
    arrange(-grade) 
      

filtered_passing <- passing2020 %>%
  summarize(name = player, id = player_id, grade = grades_offense, position = position, games = player_game_count) %>%
    arrange(-grade) 
      

filtered_defense <- defense2020 %>%
  summarize(name = player, id = player_id, grade = grades_defense, position = position, games = player_game_count) %>%
    arrange(-grade) 
      

filtered_blocking <- blocking2020 %>%
  summarize(name = player, id = player_id, grade = grades_offense, position = position, games = player_game_count) %>%
    arrange(-grade) 
      



total <- rbind(filtered_blocking, filtered_defense, filtered_passing, filtered_receiving) %>%
  merge(total_snaps, by = "name") %>%
    merge(filtered_contracts2020, by = "name") %>%
      distinct(id, .keep_all = TRUE) %>%
        arrange(-grade) %>%
          filter(snaps > 200)
totalbackwards <- rbind(filtered_blocking, filtered_defense, filtered_passing, filtered_receiving) %>%
  merge(total_snaps, by = "name") %>%
    merge(filtered_contracts2020, by = "name") %>%
      distinct(id, .keep_all = TRUE) %>%
        arrange(grade) %>%
          filter(snaps > 200)


print(total)

dupes <- total %>%
  summarize(name = name) %>%
    duplicated()
otherdupes <- total %>%
  arrange(grade) %>%
    summarize(name = name) %>%
      duplicated()

for (x in 1:length(dupes)) {
  if (dupes[x] == TRUE){
    print(x)
  }
}
    
for (x in 1:length(otherdupes)) {
  if (otherdupes[x] == TRUE){
    print(x)
  }
}

dupenames <- slice(total,318,915,1006,1008,1085,1122)
otherdupenames <- slice(totalbackwards, 48, 758, 790, 818, 823)

otherdupenames
dupenames

newtotal <- edit(total)
newtotal

snap_filtered <- filter(newtotal, snaps > 200)

themean <- mean(snap_filtered$grade)
thesd <- sd(snap_filtered$grade)

newtotal$zscore <- (newtotal$grade-themean)/thesd

newtotal


##############################################
data <- load_pbp(2022)
pbp_rp <- data %>%
  filter(pass == 1, !is.na(epa))

pbp_rp %>%
  filter(pass == 1) %>%
  group_by(passer) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), plays = n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 100)
