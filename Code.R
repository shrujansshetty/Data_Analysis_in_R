install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages(c("plotrix", "plot3D"))
install.packages(“tidyr”)
install.packages(“lubridate”)
library(plotrix)
library(dplyr)
library(ggplot2)
library(caret)
library(plot3D)
library(lubridate)
library(tidyr)

# Read the CSV file
data <- read.csv("data_with_result.csv")

# 1 -> Analyze the structure of the data

summary(data)
str(data)


# 2 -> Analyze the goal scoring distribution using bar graph
# Add a new column named "Goals" with values from 1 to 705

library(readr)

# Assuming 'data' is your dataset
# Write the dataset to a CSV file
write_csv(data, "F://R Mini//data_with_result_updated.csv")

# Write the updated data to a new CSV file

write_csv(data, "F://R Mini//data_with_result_updated.csv")

ggplot(data, aes(x = Date, y = Goals)) +
  geom_line(color = "blue") +
  labs(title = "Goals Over Time", x = "Date", y = "Goals") +
  theme_minimal()
print(data)





# 3 -> Analyze the playing position


barplot(table(data$Playing_Position), main = "Playing Position Distribution", xlab = "Playing Position", ylab = "Count", col="black")
dev.copy(png, "Playing_Position_Distribution_BarGraph.png")
dev.off()


# 4 -> Analyze the Minute of Goal Distribution using a Line Graph


plot(table(data$Minute), type = "l", main = "Minute of Goal Distribution", xlab = "Minute", ylab = "Count")
dev.copy(png, "Minute_of_Goal_Distribution_LineGraph.png")
dev.off()


# 5 -> Compare Competition-wise Win Percentage using bar graphs


win_percentage <- table(data$Competition, data$Result == "Win")[, 2] / table(data$Competition) * 100
barplot(win_percentage, main = "Competition-wise Win Percentage", xlab = "Competition", ylab = "Win Percentage")
dev.copy(png, "Competitionwise_Win_Percentage_BarGraph.png")
dev.off()










# 6-> Compare Goal Assists using bar graphs




# Assuming assist_counts is a table of assist counts
# Assuming assist_counts is a table of assist counts

# Convert assist_counts to a data frame
assist_df <- as.data.frame(assist_counts)

# Select the top 10 players based on assist counts
top_10_players <- head(assist_df[order(-assist_df$Freq), ], 10)

# Plot a pie chart for the top 10 players
pie(top_10_players$Freq, labels = top_10_players$Var1, main = "Top 10 Goal Assist Leaders")



# 7 -> Plot a Minute-wise Goal Scatter Plot


plot(data$Minute, main = "Minute-wise Goal Scatter Plot", xlab = "Minute", ylab = "Count", pch = 16)
dev.copy(png, "Minute_wise_Goal_ScatterPlot.png")
dev.off()


# 8 -> Analyze the win percentage using pie chart





win_percentage <- mean(data$Result == "Win") * 100
print(win_percentage)
ggplot(data, aes(x = "", fill = Result)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Win Percentage")









# 9 -> Top 10 players with most assist


top_assists <- data %>%
  group_by(Goal_assist) %>%
  summarise(Total_Assists = n()) %>%
  arrange(desc(Total_Assists)) %>%
  filter(Goal_assist != "") %>%  # Filtering out the empty string
  head(10)
print(top_assists)
ggplot(top_assists, aes(x = reorder(Goal_assist, -Total_Assists), y = Total_Assists)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = Goal_assist), position = position_stack(vjust = 0.5), size = 4.5, angle = 90, color = "white") +  # Displaying player names inside bars vertically
  labs(title = "Top Assist Providers", x = "", y = "Total Assists") +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.title.x = element_text(size = 12),  # Set x-axis title size
        axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        axis.title.y = element_text(size = 12),  # Adjust y-axis title size
        plot.title = element_text(size = 16),  # Adjust plot title size
        axis.text = element_text(angle = 0, vjust = 0.5)) +  # Adjust angle and position of text
  xlab("Player Name")  # Set x-axis label as Player Name


# 10 -> Goals with different opponents


library(plotly)
library(dplyr)

# Assuming 'data' is your dataset

# Separating scores and calculating goal difference
data <- data %>%
  separate(At_score, into = c("Home_score", "Away_score"), sep = ":") %>%
  mutate(
    Home_score = as.numeric(Home_score),
    Away_score = as.numeric(Away_score),
    Goal_difference = Home_score - Away_score
  )

# Summarize data to count occurrences of each goal difference
summary_data <- data %>%
  count(Goal_difference) %>%
  arrange(Goal_difference)

# Filter summary data to select top differences
top_summary_data <- head(summary_data, 10)

# Create a pie chart using plotly
pie_chart <- plot_ly(
  labels = top_summary_data$Goal_difference,
  values = top_summary_data$n,
  type = "pie",
  marker = list(colors = rainbow(length(top_summary_data$Goal_difference)), line = list(color = "black", width = 1)),
  textinfo = "label+percent",
  insidetextfont = list(color = "#FFFFFF", size = 14)
)

# Display the pie chart
pie_chart



# 1 1-> Goal variation with respect to opponent and match day
install.packages("tidyr")
library(dplyr)
library(ggplot2)
library(tidyr)


# Assuming 'data' contains your dataset
data <- read.csv("data_with_result.csv")

# Identify top 10 teams by match count
top_teams <- data %>%
  count(Opponent) %>%
  top_n(5, wt = n)

# Filter data to include matches with top 10 teams
filtered_data <- data %>%
  filter(Opponent %in% top_teams$Opponent)

# Downsampling to 5 rows per team
downsampled_data <- filtered_data %>%
  group_by(Opponent) %>%
  sample_n(5, replace = TRUE)

# Separating scores and calculating goal difference
downsampled_data <- downsampled_data %>%
  separate(At_score, into = c("Home_score", "Away_score"), sep = ":") %>%
  mutate(
    Home_score = as.numeric(Home_score),
    Away_score = as.numeric(Away_score),
    Goal_difference = Home_score - Away_score
  )

# Plot 'Goal_difference' variation for each team after downsampling
ggplot(downsampled_data, aes(x = Matchday, y = Goal_difference, group = Opponent, color = Opponent)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(title = "Goal Difference Variation with Top 10 Teams (Downsampled)", x = "Matchday", y = "Goal Difference (in Goals)") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 90, vjust = 0.5))




# Identify top 10 teams by match count
top_teams <- data %>%
  count(Opponent) %>%
  top_n(5, wt = n)
# Filter data to include matches with top 10 teams
filtered_data <- data %>%
  filter(Opponent %in% top_teams$Opponent)
# Downsampling to 5 rows per team
downsampled_data <- filtered_data %>%
  group_by(Opponent) %>%
  sample_n(5, replace = TRUE)
# Separating scores and calculating goal difference
downsampled_data <- downsampled_data %>%
  separate(At_score, into = c("Home_score", "Away_score"), sep = ":") %>%
  mutate(
    Home_score = as.numeric(Home_score),
    Away_score = as.numeric(Away_score),
    Goal_difference = Home_score - Away_score
  )
# Plot 'Goal_difference' variation for each team after downsampling
ggplot(downsampled_data, aes(x = Matchday, y = Goal_difference, group = Opponent, color = Opponent)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(title = "Goal Difference Variation with Top 10 Teams (Downsampled)", x = "Matchday", y = "Goal Difference (in Goals)") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "right")


# 1 2-> Plot a line chart for goals in FC Barcelona and PSG over time
library(readr)
library(dplyr)
library(ggplot2)

# Load the data from your CSV file
data <- read_csv("F://R Mini//data_with_result.csv", show_col_types = FALSE)
data$Goals <- seq(1, nrow(data))

# Save the dataset with the new column as a CSV file
write.csv(data, file = "data_with_result_updated.csv", row.names = FALSE)

# Read the updated CSV file
updated_data <- read.csv("data_with_result_updated.csv")

# Filter the dataset for goals scored at FC Barcelona
messi_goals_fcb <- updated_data %>%
  filter(Club == "FC Barcelona")

# Calculate the total number of goals scored with the left foot and right foot
goals_by_foot <- messi_goals_fcb %>%
  group_by(Type) %>%
  summarise(total_goals = n())

# Display summary statistics for goals scored with each foot
summary_stats <- messi_goals_fcb %>%
  group_by(Type) %>%
  summarise(mean_goals = mean(Goals),
            median_goals = median(Goals),
            max_goals = max(Goals),
            min_goals = min(Goals))

# Perform statistical comparison (e.g., t-test or chi-square test)
# Example: t-test for comparing mean goals between left and right foot
t_test_results <- t.test(Goals ~ Type, data = messi_goals_fcb)

# Visualization: Create a bar plot to show the proportion of goals by foot
# Visualization: Create a bar plot to show the proportion of goals by foot
ggplot(goals_by_foot, aes(x = Type, y = total_goals, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Goal Type Scored at FC Barcelona", x = "Goal Type", y = "Total Goals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Print statistical findings and interpretations
print(summary_stats)
print(t_test_results)

#13
library(readr)
library(dplyr)
library(ggplot2)

# Load the data from your CSV file
data <- read_csv("F://R Mini//data_with_result.csv", show_col_types = FALSE)
data$Goals <- seq(1, nrow(data))

# Save the dataset with the new column as a CSV file
write.csv(data, file = "data_with_result_updated.csv", row.names = FALSE)

# Read the updated CSV file
updated_data <- read.csv("data_with_result_updated.csv")

# Filter the dataset for goals scored at Paris Saint Germain
messi_goals_psg <- updated_data %>%
  filter(Club == "Paris Saint-Germain")

# Calculate the total number of goals scored with the left foot and right foot
goals_by_foot <- messi_goals_psg %>%
  group_by(Type) %>%
  summarise(total_goals = n())

# Display summary statistics for goals scored with each foot
summary_stats <- messi_goals_psg %>%
  group_by(Type) %>%
  summarise(mean_goals = mean(Goals),
            median_goals = median(Goals),
            max_goals = max(Goals),
            min_goals = min(Goals))

# Perform statistical comparison (e.g., t-test or chi-square test)
t_test_results <- t.test(Goals ~ Type, data = messi_goals_psg)

# Visualization: Create a bar plot to show the proportion of goals by foot
ggplot(goals_by_foot, aes(x = Type, y = total_goals, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Goal Type Scored at Paris Saint Germain", x = "Goal Type", y = "Total Goals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Print statistical findings and interpretations
print(summary_stats)
print(t_test_results)

# 14 -> Create a Line Chart Showing the Goals Scored vs Time over the years


# Add a new column named "Goals" with values from 1 to 705

# Add a new column named "Goals" with values from 1 to 705
data$Goals <- 1:nrow(data)

# Write the updated data to a new CSV file

write_csv(data, "F://R Mini//data_with_result_updated.csv")

ggplot(data_updated, aes(x = Date, y = Goals)) +
  geom_line(color = "blue") +
  labs(title = "Goals Over Time", x = "Date", y = "Goals") +
  theme_minimal()



# 15 -> Calculate the win percentage


win_percentage <- mean(data$Result == "Win") * 100
print(win_percentage)
ggplot(data, aes(x = "", fill = Result)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Win Percentage")


# 1 6-> Build a regression model to predict the match outcome


football_data$Result <- ifelse(football_data$Result == "Win", 1, 0)

# Create a logistic regression model
model <- glm(Result ~ Venue + Opponent + Playing_Position, data = football_data, family = binomial)

# Function to predict the match result
predict_result <- function(venue, opponent, playing_position) {
  new_data <- data.frame(Venue = venue, Opponent = opponent, Playing_Position = playing_position)
  result_prediction <- predict(model, newdata = new_data, type = "response")
  
  return(ifelse(result_prediction > 0.5, "Win", "Lose"))
}

# Example usage:
venue_input <- "H"
opponent_input <- "Real Madrid"
position_input <- "CF"

predicted_result <- predict_result(venue_input, opponent_input, position_input)
print(predicted_result)