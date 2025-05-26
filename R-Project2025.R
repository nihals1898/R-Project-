load("Enron (1).Rdata")
ls()
str(employeelist)
head(employeelist)
summary(employeelist)
str(message)
head(message)
summary(message)
str(recipientinfo)
head(recipientinfo)
summary(recipientinfo)
library(dplyr)
install.packages("lubridate")
library(lubridate)
glimpse(employeelist)
glimpse(message)
glimpse(recipientinfo)
glimpse(flights)
glimpse(planes)
library(dplyr)
library(ggplot2)

top_senders <- message %>%
  group_by(sender) %>%
  summarise(n_sent = n()) %>%
  arrange(desc(n_sent))
head(top_senders, 10)

top_senders %>%
  slice_max(n_sent, n = 10) %>%
  ggplot(aes(x = reorder(sender, n_sent), y = n_sent))
  geom_bar(stat = "identity", fill = "steelblue")
  coord_flip()
  labs(title = "Top 10 Most Active Email Senders",
     x = "Sender",
     y = "Number of Emails Sent")
  theme_minimal()
  
library(dplyr)
library(ggplot2)  

message_with_status <- message %>%
  left_join(employeelist, by = c("sender" = "Email_id"))
emails_by_status_clean <- message_with_status %>%
  filter(!is.na(status)) %>%
  group_by(status) %>%
  summarize(n_sent = n()) %>%
  arrange(desc(n_sent))
print(emails_by_status_clean)

emails_by_status_clean <- message_with_status %>%
  filter(!is.na(status), status != "N/A") %>%
  group_by(status) %>%
  summarise(n_sent = n()) %>%
  arrange(desc(n_sent))
print(emails_by_status_clean)


ggplot(emails_by_status_clean, aes(x = reorder(status, n_sent), y = n_sent)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Emails Sent by Employee Role ( Known Status Only)",
    x = "Employee Role (Status)",
    y = "Number of Emails Sent")
  theme_minimal()
  

library(dplyr)
library(lubridate)
library(ggplot2)

message <- message %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

emails_over_time <- message %>%
  mutate(month = floor_date(date, unit = "month")) %>%
  group_by(month) %>%
  summarise(n_emails = n(), .groups = "drop")

ggplot(emails_over_time, aes(x = month, y = n_emails)) +
  geom_line(color = "tomato", linewidth = 1) +
  labs(
    title = "Monthly Email Volume at Enron",
    subtitle = "Clear trends visible leading up to and during 2001 Enron scandal",
    x = "Month",
    y = "Number of Emails Sent"
  ) +
  theme_minimal()

str(message$date)
head(message$date, 10)



library(dplyr)
library(lubridate)
library(ggplot2)

message <- message %>%
  mutate(date = as.POSIXct(date, tz = "UTC"))

emails_over_time <- message %>%
  mutate(month_str = format(date, "%Y-%m"),   
         month = ym(month_str)) %>%       
  group_by(month) %>%
  summarise(n_emails = n(), .groups = "drop")


ggplot(emails_over_time, aes(x = month, y = n_emails)) +
  geom_line(color = "tomato", linewidth = 1) +
  labs(
    title = "Monthly Email Volume at Enron",
    subtitle = "Clear trends visible leading up to and during the 2001 Enron scandal",
    x = "Month",
    y = "Number of Emails Sent"
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "3 months"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


message <- message %>% mutate(month_str = format(date, "%Y-%m"))

unique_months <- unique(message$month_str)
print(unique_months)


library(dplyr)
library(lubridate)
library(ggplot2)

message <- message %>%
  mutate(date = as.POSIXct(date, tz = "UTC"))

message_clean <- message %>%
  filter(date >= as.POSIXct("1998-01-01", tz = "UTC") & date < as.POSIXct("2003-01-01", tz = "UTC"))

emails_over_time <- message_clean %>%
  mutate(month_str = format(date, "%Y-%m"),
         month = lubridate::ym(month_str)) %>%
  group_by(month) %>%
  summarise(n_emails = n(), .groups = "drop")

ggplot(emails_over_time, aes(x = month, y = n_emails)) +
  geom_line(color = "tomato", linewidth = 1) +
  labs(
    title = "Monthly Email Volume at Enron",
    subtitle = "Clear trends visible leading up to and during the 2001 Enron scandal",
    x = "Month",
    y = "Number of Emails Sent"
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "3 months"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

install.packages('tidytext')
install.packages("tidytext")
library(dplyr)
library(tidytext)
library(ggplot2)

subject_words <- message %>%
  select(subject) %>%
  filter(!is.na(subject)) %>%
  unnest_tokens(word, subject)

data("stop_words")  
subject_words_clean <- subject_words %>%
  anti_join(stop_words, by = "word")

top_subject_words <- subject_words_clean %>%
  count(word, sort = TRUE)

top_subject_words %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top Words in Enron Email Subjects",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()
