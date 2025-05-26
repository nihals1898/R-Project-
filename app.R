load("Enron (1).Rdata")

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidytext)

ui <- fluidPage(
  titlePanel("Enron Email Explorer"),
  
  tabsetPanel(
    
    tabPanel("Top Senders",
             sidebarLayout(
               sidebarPanel(
                 selectInput("employee", "Filter by Sender :",
                             choices = c("All", unique(top_senders$sender)))
               ),
               mainPanel(
                 plotOutput("topSendersPlot")
               )
             )
    ),
    
    tabPanel("Role by Status", plotOutput("rolePlot")),
    
    tabPanel("Email Volume Over Time", plotOutput("timePlot")),
    
    tabPanel("Subject Word Frequency", plotOutput("wordPlot"))
  )
)

server <- function(input, output) {
  
  filtered_senders <- reactive({
    if (input$employee == "All") {
      top_senders
    } else {
      top_senders %>% filter(sender == input$employee)
    }
  })
  
  output$topSendersPlot <- renderPlot({
    filtered_senders() %>%
      slice_max(n_sent, n = 10) %>%
      ggplot(aes(x = reorder(sender, n_sent), y = n_sent)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Filtered Top Email Senders", x = "Sender", y = "Emails Sent") +
      theme_minimal()
  })
  
  output$rolePlot <- renderPlot({
    ggplot(emails_by_status_clean, aes(x = reorder(status, n_sent), y = n_sent)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(title = "Emails Sent by Role", x = "Role", y = "Emails Sent") +
      theme_minimal()
  })
  
  output$timePlot <- renderPlot({
    ggplot(emails_over_time, aes(x = month, y = n_emails)) +
      geom_line(color = "tomato", linewidth = 1) +
      labs(title = "Monthly Email Volume", x = "Month", y = "Emails Sent") +
      theme_minimal()
  })
  
  output$wordPlot <- renderPlot({
    top_subject_words %>%
      slice_max(n, n = 15) %>%
      ggplot(aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Subject Line Words", x = "Word", y = "Frequency") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
