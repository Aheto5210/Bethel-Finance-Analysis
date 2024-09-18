install.packages("plotly")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("knitr")

library(dplyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(knitr)


Transactions <- read.csv("transactions.csv") # Loading the Transactions dataset
Transactions

# a bit of of exploratory analysis

head(Transactions)
colnames(Transactions) # To display Column names

str(Transactions) # The datatype of all the columns
dim(Transactions) # Checking for number of rows and columns

sum(is.na(Transactions)) # Checking for missing Values

summary(Transactions$amount)

# Splitted the amount values into deposit and withdrawals based on the column "type"

Transd <- Transactions %>% # Assigned Transactions to Transd
  select(amount, type, trans_date ) %>% # Selected the preferred columns
  mutate(
    deposit_amount = ifelse(type == "Deposit", amount, NA),
    withdrawal_amount = ifelse(type == "Withdraw", amount, NA)
  )

unique(Transactions$type)

#Changing date to just months for more analysis
Transd <- Transd %>%
  mutate(trans_date = ymd_hms(trans_date))

Transd <- Transd %>%
  mutate(month = month(trans_date, label = TRUE, abbr = FALSE))

ggplot(Transd, aes(x = month, y = amount, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with dodge for side-by-side bars
  labs(title = "Deposits and Withdrawals by Month",
       x = "Month",
       y = "Amount")+
  theme_bw() +
  scale_fill_manual(values = c("Deposit" = "darkblue", "Withdraw" = "darkred"))

# Sum the transactions
Transum <- Transd %>%
  group_by(month, type) %>%
  summarize(total_amount = sum(amount), .groups = 'drop')

#total amount for each month and percentage of each transaction type
Transtotal <- Transum %>%
  group_by(month) %>%
  mutate(total_month_amount = sum(total_amount)) %>%
  ungroup() %>%
  mutate(percentage = (total_amount / total_month_amount) * 100)

#the total deposits and withdrawals grouped by month
Transtotal <- Transum %>%
  group_by(month) %>%
  mutate(total_month_amount = sum(total_amount)) %>%
  ungroup() %>%
  mutate(percentage = (total_amount / total_month_amount) * 100) %>%
  group_by(month, type) %>%
  summarize(total_amount_by_type = sum(total_amount), .groups = 'drop')  # Summarize by type

#stacked line chart with markers
ggplot(Transtotal, aes(x = month, y = total_amount_by_type, color = type, group = type)) +
  geom_line(size = 1) +  # Line size for deposits and withdrawals
  geom_point(size = 3) +  # Markers for deposits and withdrawals
  labs(title = "Total Deposits and Withdrawals by Month", 
       y = "Total Amount", 
       x = "Month") +
  scale_color_manual(values = c("Deposit" = "blue", "Withdraw" = "red")) +  # Custom colors
  theme_minimal()  # Clean theme

ggplot(Transum, aes(x = "", y = total_amount, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Total Deposits and Withdrawals") +
  theme_void() +
  scale_fill_manual(values = c("Deposit" = "navy", "Withdraw" = "orange"))


transd_depsum <- Transd %>%
  filter(type == "deposit") %>%
  group_by(month) %>%
  summarize(deposit_amount = sum(amount), .groups = 'drop')


# Summarize the data for deposits and withdrawals
Transtotal_summary <- Transtotal %>%
  group_by(type) %>%
  summarize(total_amount = sum(total_amount_by_type, na.rm = TRUE), .groups = 'drop')

# Create the plotly 3D doughnut chart
plot_ly(Transtotal_summary, labels = ~type, values = ~total_amount, type = 'pie',
        hole = 0.6,  # Creates the doughnut hole
        textinfo = 'label+percent',  # Show both the label and percentage
        hoverinfo = 'label+value+percent',  # Show value on hover
        marker = list(colors = c('navy', 'orange'), line = list(color = '#FFFFFF', width = 2))) %>%
  layout(title = "Total Deposits and Withdrawals",
         showlegend = TRUE,
         annotations = list(
           text = "Total Transactions",  # Center label inside doughnut
           showarrow = FALSE,
           font = list(size = 20)
         ),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

Transtotal_summary <- Transtotal %>%
  group_by(month, type) %>%
  summarize(total_amount = sum(total_amount_by_type, na.rm = TRUE), .groups = 'drop')


# Create the heatmap
ggplot(Transtotal_summary, aes(x = month, y = type, fill = total_amount)) +
  geom_tile(color = "white") +  # Create the heatmap tiles
  scale_fill_gradient(low = "white", high = "navy") +  # Color gradient for total amounts
  labs(title = "Heatmap of Total Deposits and Withdrawals by Month",
       x = "Month",
       y = "Transaction Type",
       fill = "Total Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# bubble chart
Transtotal_summary <- Transtotal %>%
  group_by(month, type) %>%
  summarize(total_amount_by_type = sum(total_amount_by_type, na.rm = TRUE), .groups = 'drop')

# Create the bubble chart
ggplot(Transtotal_summary, aes(x = month, y = total_amount_by_type, size = total_amount_by_type, color = type, fill = type)) +
  geom_point(alpha = 0.7, shape = 21, stroke = 1.5) +  # Create the bubbles with fill
  scale_size_continuous(range = c(5, 20)) +  # Adjust bubble sizes based on total_amount_by_type
  scale_color_manual(values = c("Deposit" = "blue", "Withdraw" = "red")) +  # Custom bubble border colors
  scale_fill_manual(values = c("Deposit" = "lightblue", "Withdraw" = "lightcoral")) +  # Custom bubble fill colors
  labs(title = "Bubble Chart of Total Deposits and Withdrawals by Month",
       x = "Month",
       y = "Total Amount",
       size = "Total Amount",
       fill = "Transaction Type",
       color = "Transaction Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



  ggplot(Transtotal_summary, aes(x = month, y = total_amount_by_type, color = type)) +
    geom_point(size = 3) +  # Create scatter plot points
    geom_smooth(method = "lm", se = FALSE, aes(group = type)) +  # Add a line of best fit without confidence interval
    scale_color_manual(values = c("Deposit" = "blue", "Withdraw" = "red")) +  # Custom colors for points and lines
    labs(title = "Scatter Plot with Line of Best Fit for Deposits and Withdrawals",
         x = "Month",
         y = "Total Amount",
         color = "Transaction Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

  # Calculate average, maximum, and minimum for each month and type
  summary_stats <- Transtotal %>%
    group_by(month, type) %>%
    summarize(
      avg_amount = mean(total_amount_by_type, na.rm = TRUE),
      max_amount = max(total_amount_by_type, na.rm = TRUE),
      min_amount = min(total_amount_by_type, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_longer(cols = starts_with("avg_amount"):starts_with("min_amount"),
                 names_to = "statistic",
                 values_to = "value") %>%
    mutate(statistic = factor(statistic, levels = c("avg_amount", "max_amount", "min_amount"),
                              labels = c("Average", "Maximum", "Minimum")))
  
  # Calculate average, maximum, and minimum for each month and type
  summary_stats <- Transtotal %>%
    group_by(month, type) %>%
    summarize(
      avg_amount = mean(total_amount_by_type, na.rm = TRUE),
      max_amount = max(total_amount_by_type, na.rm = TRUE),
      min_amount = min(total_amount_by_type, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Prepare data for table display
  summary_table <- summary_stats %>%
    pivot_longer(cols = starts_with("avg_amount"):starts_with("min_amount"),
                 names_to = "statistic",
                 values_to = "value") %>%
    mutate(statistic = factor(statistic, levels = c("avg_amount", "max_amount", "min_amount"),
                              labels = c("Average", "Maximum", "Minimum"))) %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    arrange(month, type)
  
  # Display the table using kable
  kable(summary_table, caption = "Summary Statistics for Withdrawals and Deposits per Month")
  
  
 
  
  
  
  
  
  
  
  
  