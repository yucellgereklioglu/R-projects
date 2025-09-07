file_path <- file.choose()
df <- read.csv(file_path, header = TRUE, encoding = "UTF-8")
head(df)


##Chapter1: cutting front libero
str(df)
library(dplyr)
min_playtime_threshold <- 900
processed_data <- df %>%
  filter(Minutes > min_playtime_threshold) %>%
  mutate(
    blocks_p90 = (Blocks / Minutes) * 90,
    tackles_p90 = (Tackles / Minutes) * 90,
    yellow_cards_p90 = (Yellow.Cards / Minutes) * 90
  )
avg_blocks_p90 <- mean(processed_data$blocks_p90, na.rm = TRUE)
avg_tackles_p90 <- mean(processed_data$tackles_p90, na.rm = TRUE)
avg_yellow_cards_p90 <- mean(processed_data$yellow_cards_p90, na.rm = TRUE)
aggressive_profile_players <- processed_data %>%
  filter(
    blocks_p90 > avg_blocks_p90 &
      tackles_p90 > avg_tackles_p90 &
      yellow_cards_p90 > avg_yellow_cards_p90
  ) %>%
  select(Player.Name, Club, Position, Minutes, blocks_p90, tackles_p90, yellow_cards_p90) %>%
  arrange(desc(tackles_p90))
print(aggressive_profile_players)

##Chapter2: tower centerback
library(readr)
min_playtime_threshold <- 900
center_backs_data <- df %>%
  filter(Position == 'DEF', Minutes > min_playtime_threshold) %>%
  mutate(
    pass_success_rate = parse_number(Passes.),
    aerial_duel_win_rate = parse_number(aDuels..),
    aerial_duels_won_p90 = (aDuels.Won / Minutes) * 90,
    clearances_p90 = (Clearances / Minutes) * 90,
    interceptions_p90 = (Interceptions / Minutes) * 90,
    blocks_p90 = (Blocks / Minutes) * 90
  )
pass_rate_threshold <- quantile(center_backs_data$pass_success_rate, 0.60, na.rm = TRUE)
aerial_rate_threshold <- quantile(center_backs_data$aerial_duel_win_rate, 0.60, na.rm = TRUE)
tower_stoppers_profile <- center_backs_data %>%
  filter(
    pass_success_rate >= pass_rate_threshold &
      aerial_duel_win_rate >= aerial_rate_threshold
  ) %>%
  select(
    Player.Name, Club, Minutes, pass_success_rate, aerial_duel_win_rate, aerial_duels_won_p90, clearances_p90
  ) %>%
  arrange(desc(aerial_duel_win_rate), desc(pass_success_rate))
print(tower_stoppers_profile)

##Chapter 3 offensive back
min_playtime_threshold <- 900
attacking_backs_data <- df %>%
  filter(Position == 'DEF', Minutes > min_playtime_threshold) %>%
  mutate(
    assists_p90 = (Assists / Minutes) * 90,
    successful_crosses_p90 = (Successful.Crosses / Minutes) * 90,
    progressive_carries_p90 = (Progressive.Carries / Minutes) * 90,
    total_carries_p90 = (Carries / Minutes) * 90
  )
assists_threshold <- quantile(attacking_backs_data$assists_p90, 0.75, na.rm = TRUE)
crosses_threshold <- quantile(attacking_backs_data$successful_crosses_p90, 0.75, na.rm = TRUE)
prog_carries_threshold <- quantile(attacking_backs_data$progressive_carries_p90, 0.75, na.rm = TRUE)
final_selection_fullbacks <- attacking_backs_data %>%
  filter(
    assists_p90 >= assists_threshold &
      successful_crosses_p90 >= crosses_threshold &
      progressive_carries_p90 >= prog_carries_threshold
  ) %>%
  select(
    Player.Name, Club, Minutes, assists_p90, successful_crosses_p90, progressive_carries_p90
  ) %>%
  arrange(desc(assists_p90), desc(progressive_carries_p90))
print(final_selection_fullbacks)

library(ggplot2)
df_cleaned <- df %>%
  mutate(
    Passes_Rate_Num = parse_number(Passes.),
    Conversion_Rate_Num = parse_number(Conversion..),
    fThird_Passes_Rate_Num = parse_number(fThird.Passes..),
    Crosses_Rate_Num = parse_number(Crosses..),
    gDuels_Rate_Num = parse_number(gDuels..),
    aDuels_Rate_Num = parse_number(aDuels..)
  )
ggplot(df_cleaned, aes(x = Passes_Rate_Num)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "white", alpha = 0.7) +
  labs(title = "Distrubitions EPL players",
       x = "Pass Succesfull (%)",
       y = "Count EPL Players") +
  theme_minimal()

ggplot(df, aes(x = Position, y = Tackles, fill = Position)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Distribution of Tackles by Position",
       x = "Position",
       y = "Sum Tackles") +
  theme_minimal()



