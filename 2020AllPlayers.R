filtered_contracts2020 <- contracts %>%
  summarize(name = Player, cap = Cap.Number, year =  Year, position = Pos) %>%
  filter(year == 2020)


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

nflreadr_snaps_seasons <- nflreadr_snaps_season %>% filter(season == 2020) %>% rename(name = player)



total <- rbind(filtered_blocking, filtered_defense, filtered_passing, filtered_receiving) %>%
  merge(nflreadr_snaps_seasons, join_by = c("name", "position")) %>%
  merge(filtered_contracts2020, join_by = c("name", "position")) %>%
  distinct(id, .keep_all = TRUE) %>%
  arrange(-grade) %>%
  filter(snaps > 200)


print(total)

#dupes <- total %>%
#  summarize(name = name) %>%
#  duplicated()
#otherdupes <- total %>%
#  arrange(grade) %>%
#  summarize(name = name) %>%
#  duplicated()

#for (x in 1:length(dupes)) {
#  if (dupes[x] == TRUE){
#    print(x)
#  }
#}

#for (x in 1:length(otherdupes)) {
#  if (otherdupes[x] == TRUE){
#    print(x)
#  }
#}

#dupenames <- slice(total,318,915,1006,1008,1085,1122)
#otherdupenames <- slice(totalbackwards, 48, 758, 790, 818, 823)

#otherdupenames
#dupenames

#newtotal <- edit(total)
newtotal <- total







snap_filtered <- filter(newtotal, snaps > 200)

with_mean <- snap_filtered %>% group_by(position) %>% mutate(mean_grade = mean(grade))
with_sd <- with_mean %>% group_by(position) %>% mutate(sd_grade = sd(grade))
with_zscore <- with_sd %>% mutate(zScore = (grade-mean_grade)/sd_grade)

with_zscore$cap <- gsub("\\$","",as.character(with_zscore$cap)) 
with_zscore$cap <- gsub(",","",as.character(with_zscore$cap)) 

with_zscore$cap <- as.numeric(with_zscore$cap)

with_mean2 <- with_zscore %>% group_by(position) %>% mutate(mean_cap = mean(cap))
with_sd2 <- with_mean2 %>% group_by(position) %>% mutate(sd_cap = sd(cap))
with_zscore2 <- with_sd2 %>% mutate(zScore_cap = (cap-mean_cap)/sd_cap)
