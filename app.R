#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinythemes)
library(rsconnect)
library(tidyverse)
library(sjlabelled)
library(viridis)
library(haven)
library(shinyWidgets)
library(shinysurveys)
library(readr)

setwd("F:/social survey website project/data")
religion2018 <- read_rds("religion2018.rds")

choicecountry <- religion2018 %>% count(c_alphan)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Social Survey Data explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            varSelectInput("religionquestion",
                        label = p("Question"),
                        data = religion2018[, c(3:372)],
                        multiple = FALSE),
            selectInput("countryreligion",
                        label = p("Country"),
                        choices = choicecountry[, 1],
                        multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barplotreli")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  barplotreli <- reactive({
    religion2018 %>% 
      filter(c_alphan %in% input$countryreligion) %>% 
      ggplot()+
      aes(y = !!input$religionquestion, x = after_stat(count/sum(count)), fill = c_alphan)+
      geom_bar(col="black", alpha=0.8, width = 0.8, na.rm = TRUE, position = "dodge")+
      scale_x_continuous(position = "top",
                         expand = c(0.001,0))+
      xlab("")+
      ylab("")+
      scale_fill_viridis(option = "turbo", discrete = TRUE)+
      ggtitle(input$religionquestion)
      
  })
  
  
  
  
    output$barplotreli <- renderPlot({
      barplotreli()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
