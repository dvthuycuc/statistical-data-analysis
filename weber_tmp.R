library(duckdb)
library(dplyr)
library(ggplot2)
library(ggdist)
library(stringr)
library(qqplotr)
library(lubridate)
library(ggridges)
source("helper.R")
library(tidyverse)

db_path <- Sys.getenv("STEAM_DB_PATH", "C:/Users/TCC/Documents/git_cuc/teaching-data/steam_data/steam_data.duckdb")
con <- dbConnect(duckdb::duckdb(), db_path)
df_app <- dbReadTable(con, "STEAM_APPDETAILS")
df_players <- dbReadTable(con, "STEAM_NUM_PLAYERS")
df_avg_players <- df_players |> group_by(appid) |>
  summarise(
    avg_player = round(mean(num_players, na.rm = TRUE)),
    .groups = "drop"
  )
df_app <- df_app |>
  left_join(df_avg_players, by = "appid")

df_app <- df_app |> 
  filter(metacritic_score!=0)

df_sum <- df_app |> 
  group_by(
    publishers
  ) |> 
  summarise(
    mean_mc_score = mean(metacritic_score),
    num_games = n(),
    most_pop_game = name[which.max(metacritic_score)]
  ) |> 
  filter(
    num_games>2,
    !is.na(publishers)
  )

df_sum |> 
  ggplot(
    aes(
      x = mean_mc_score,
      y = reorder(publishers,mean_mc_score),
      fill = num_games,
      label = most_pop_game
    )
  )+
  geom_col()+
  geom_text(
    x = 1,
    color = "white",
    hjust = 0
  )+
  scale_x_continuous(
    expand = c(0,0,0.05,0)
  )+
  labs(
    title = "Publisher vs. Metacritic",
    x = "Metacritic Score",
    y = "",
    fill = "Anzahl an Spielen",
    caption = "Anzahl Spiele pro Publisher > 2"
  )+
  theme_minimal()

t.test(is_free ~ metacritic_score,data = df_app)

with(df_app, t.test(metacritic_score[is_free == 0], metacritic_score[is_free == 1]))


df_paid <- df_app |> 
  group_by(
    is_free
  ) |> 
  summarise(
    num = n(),
    mean_mc = mean(metacritic_score)
  )

