# NBA Bradley-Terry Model in R
# Adam Wickwire - 11-11-2023

# This script uses the Bradley-Terry model to predict the winner of a NBA game. 
# It scrapes data from basketball-reference.com and uses the data to create a
# logistic regression model. The model is then used to predict the winner of a
# game between two teams. 

# The Bradley-Terry model is a common method for ranking teams in sports. It is
# a logistic regression model that uses the difference in points between two
# teams to predict the probability that one team will win. The model is
# represented by the following equation:

# log(p(Win)) = log(odds) = log(p(Win)/p(Loss)) = log(odds_ratio) = β0 + β1 + β2

# where p(Win) is the probability that a team will win, p(Loss) is the
# probability that a team will lose, and odds_ratio is the odds that a team will
# win. The odds ratio is calculated by taking the exponent of the coefficient
# for a team. The odds ratio is the ratio of the probability that a team will
# win to the probability that a team will lose. The odds ratio can be
# interpreted as the number of times more likely a team is to win than lose.

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Load Libraries
#-----------------------------------------------------------------------------#
library(tidyverse)
library(rvest)
library(broom)
#-----------------------------------------------------------------------------#



# Two Functions used in the script
# First function is used to scrape data from basketball-reference.com
# Second function is used to predict the winner of a game between two teams

#First Function
#-----------------------------------------------------------------------------#
# Function to scrape game results for each month of a given NBA year
scrape_year_results <- function(year) {
  months <- c("october", "november", "december", "january", "february", "march", "april")
  
  # Function to scrape a single month
  scrape_month_results <- function(year, month) {
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_games-", month, ".html")
    tbl <- url %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table()
    
    # Turn table into dataframe
    df <- data.frame(tbl[[1]])
    
    # Convert 'Pts' and 'Pts.1' columns to numeric
    df$PTS <- as.numeric(as.character(df$PTS))
    df$PTS.1 <- as.numeric(as.character(df$PTS.1))
    
    # Rename the columns
    colnames(df)[colnames(df) %in% c("Visitor.Neutral")] <- "Visitor"
    colnames(df)[colnames(df) %in% c("PTS")] <- "Visitor.PTS"
    colnames(df)[colnames(df) %in% c("Home.Neutral")] <- "Home"
    colnames(df)[colnames(df) %in% c("PTS.1")] <- "Home.PTS"
    
    # Remove rows where points column has NA
    df <- df[!is.na(df$Visitor.PTS), ]
    
    return(df)
  }
  
  # Scrape and combine data for all months
  all_game_results <- do.call(rbind, lapply(months, function(m) scrape_month_results(year, m)))
  
  return(all_game_results)
}
#-----------------------------------------------------------------------------#

# Second Function
#-----------------------------------------------------------------------------#
# Create a function to predict the winner
predict_winner <- function(team1, team2) {
  # Create a data frame with the two teams
  df <- data.frame(Winner = team1, Loser = team2)
  
  # Predict the winner
  pred <- predict(model, df, type = "response")
  
  # Return the prediction
  return(pred)
}
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


# Set the year to scrape and gather data
#-----------------------------------------------------------------------------#
nba_game_results <- scrape_year_results(2024)
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


# Create a Bradley-Terry Model and Ranking
#-----------------------------------------------------------------------------#
# Combined data manipulation for efficiency
bt_model <- nba_game_results %>%
  mutate(Winner = case_when(Visitor.PTS > Home.PTS ~ Visitor, TRUE ~ Home),
         Loser = case_when(Visitor.PTS < Home.PTS ~ Visitor, TRUE ~ Home)) %>%
  select(Winner, Loser)

matchups <- bt_model %>%
  mutate(Win = 1) %>%
  bind_rows(select(bt_model, Winner = Loser, Loser = Winner) %>% mutate(Win = 0))

# Logistic Regression Model
model <- glm(Win ~ Winner + Loser, data = matchups, family = "binomial")

# Extracting and Ranking Teams Based on Odds Ratio
ranked_teams <- tidy(model) %>%
  filter(str_detect(term, "Winner")) %>%
  mutate(team = str_remove(term, "Winner"), 
         odds_ratio = exp(estimate)) %>%
  select(team, odds_ratio) %>%
  arrange(desc(odds_ratio))

# Viewing Ranked Teams
ranked_teams
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


# Predict the winner of the game between two teams
#-----------------------------------------------------------------------------#
predict_winner("Chicago Bulls", "Milwaukee Bucks")
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

