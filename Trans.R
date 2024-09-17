library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

library(ggplot2)


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
  scale_fill_manual(values = c("Deposit" = "navy", "Withdraw" = "orange"))

# Sum the transactions
Transum <- Transd %>%
  group_by(month, type) %>%
  summarize(total_amount = sum(amount), .groups = 'drop')

# Calculate the total amount for each month and percentage of each transaction type
Transum <- Transto %>%
  group_by(month) %>%
  mutate(total_month_amount = sum(total_amount)) %>%
  ungroup() %>%
  mutate(percentage = (total_amount / total_month_amount) * 100)

Transum
Transtotal <- Transum %>% # Assigned Transactions to Transd
  select(total_month_amount, type, month ) %>% # Selected the preferred columns
  mutate(
    Total_Deposit = ifelse(type == "Deposit", total_month_amount, NA),
    Total_withdrawal = ifelse(type == "Withdraw", total_month_amount, NA)
  )


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

Transd

# doughnut plot to show total deposit amount
ggplot(Transtotal, aes(x = 2, y = Total_Deposit, fill = month)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +  # This creates the hole in the middle
  theme_void() +  # Remove all background and axis elements
  labs(title = "Deposit Amounts by Month") +
  scale_fill_brewer(palette = "Set3") # Use a color palette for the months

#  doughnut plot to show the total of both desposit and withdrawal amount
ggplot(Transtotal, aes(x = 2, y = total_month_amount, fill = month)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~type)+
  xlim(0.5, 2.5) +  # This creates the hole in the middle
  theme_void() +  # Remove all background and axis elements
  labs(title = "Deposit Amounts by Month") +
  scale_fill_brewer(palette = "Set3") # Use a color palette for the months

ggplot(Transtotal, aes(x = month(), fill = type))+
  geom_bar()+ theme_bw() + labs(y = "s", title = "s")




