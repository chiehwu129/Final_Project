library(shiny)
library(colourpicker)
library(plotly)
library(ggplot2)
library(datasets)
library(shinythemes)
library(dplyr)
library(tidyverse)

degrees <- read.csv("data/degrees-that-pay-back.csv",header = TRUE, sep = ',')
degrees$Starting.Median.Salary <- as.numeric(gsub('[$,]', '', degrees$Starting.Median.Salary))
degrees$Mid.Career.Median.Salary  <- as.numeric(gsub('[$,]', '', degrees$Mid.Career.Median.Salary))
degrees$Mid.Career.10th.Percentile.Salary  <- as.numeric(gsub('[$,]', '', degrees$Mid.Career.10th.Percentile.Salary))
degrees$Mid.Career.25th.Percentile.Salary <- as.numeric(gsub('[$,]', '', degrees$Mid.Career.25th.Percentile.Salary))
degrees$Mid.Career.75th.Percentile.Salary <- as.numeric(gsub('[$,]', '', degrees$Mid.Career.75th.Percentile.Salary))
degrees$Mid.Career.90th.Percentile.Salary <- as.numeric(gsub('[$,]', '', degrees$Mid.Career.90th.Percentile.Salary))

degrees %>% arrange(Starting.Median.Salary)

ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  # Application title
  titlePanel("Degrees That Pay"),
  
  navlistPanel(
    "Menu",
    tabPanel(title = "About", h3("About Degrees That Pay Data"),
             p(),
             p("This data set will explore which undergradratue majors will generate the highest paying salary.
                                       It samples salaries from a variety of undergraduate majors and illistrates the median salary from start and mid-career. 
                                       There is also data that shows different perentiles ranging from the 10th, 25th, 75th, and 90th percentile.",
               br(),
               br(),
               HTML('<img src="study1.png", height="350px"    
          style="float:left"/>','<p style="color:black"></p>')
             )),
    tabPanel(title = "Degrees and Expected Salary",
             sidebarPanel(
               sliderInput(inputId = "PercentChange", 
                           label = "Percent Change from Starting to Mid Career Salary",
                           width = 10000,
                           min = 10, max = 100, value = c(100)),
               width = 8,
               position = "bottom left"
             ),
             mainPanel(
               plotlyOutput('plot1'),
               br(),
               br(),
               plotlyOutput('plot2'))),
    
    tabPanel( title = "Salary Range by Major",
              sidebarPanel(
                selectizeInput(inputId='majorchoice',
                               label = 'Choose a Major', 
                               choices = degrees$Undergraduate.Major, 
                               multiple = TRUE,
                               selected = "Accounting")),
              mainPanel(
                plotlyOutput('plot3'))),
    tabPanel( title = "Comparative Salary Range",
              
              sidebarPanel(sliderInput(inputId = 'Salary',
                                       label = 'Filter Salary Range by Starting Salary',
                                       width = 1000,
                                       min = min(degrees$Starting.Median.Salary), max = max(degrees$Starting.Median.Salary), value = c(45000), sep = ""),
                           width = 10,
                           position = 'bottom left'),
              
              mainPanel(
                plotlyOutput('plot4')))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlotly(
    plot1 <- degrees %>%
      
      filter(Percent.change.from.Starting.to.Mid.Career.Salary > input$PercentChange) %>%
      
      plot_ly( x = ~Starting.Median.Salary, y = ~Mid.Career.Median.Salary, text = ~Undergraduate.Major, type = 'scatter', mode = 'markers', color = ~Percent.change.from.Starting.to.Mid.Career.Salary, 
               colors = 'Blues',
               width = 1000,
               marker = list(title = 'Percent Change', orientation = 'h',size = ~Percent.change.from.Starting.to.Mid.Career.Salary, opacity = .5)) %>%
      layout(title = "Starting Salary and Mid-Career Salary Percent Change Comparison",
             yaxis = list(title = 'Stating Median Salary'),
             xaxis = list(title = 'Mid-Career Median Salary'),
             marker = list(title = 'Percent Change', orientation = 'h'))) 
  
  
  
  output$plot2 <- renderPlotly(
    
    
    plot_ly(degrees, x = ~Undergraduate.Major , y = ~Starting.Median.Salary, type = 'bar', name = 'Starting Salary', width = 1000, height = 500,  order = "ascending") %>% 
      add_trace(y = ~Mid.Career.Median.Salary, name = 'Mid Career Salary') %>%
      layout(title = "Starting and Mid-Career Salary by Major",
             yaxis = list(title = 'Salary ($)'),
             xaxis = list(title = 'Undergrad Major',
                          categoryorder = 'total ascending'))) 
  
  
  output$plot3 <-renderPlotly(
    
    plot3 <- degrees %>%
      
      filter(Undergraduate.Major %in% input$majorchoice) %>%
      
      plot_ly( x = ~Undergraduate.Major , y = ~Mid.Career.10th.Percentile.Salary , type = 'bar', name = '90th Percentile', width = 500) %>% 
      add_trace(y = ~Mid.Career.25th.Percentile.Salary, name = '75th Percentile') %>% 
      add_trace(y = ~Mid.Career.Median.Salary, name = '50th Percentile') %>%
      add_trace(y = ~Mid.Career.75th.Percentile.Salary, name = ' 25th Percentile') %>%
      add_trace(y = ~Mid.Career.90th.Percentile.Salary, name = '10th Percentile') %>%
      
      layout(title = 'Salary Range by Major',
             plot_bgcolor = "#e5ecf6",
             yaxis = list(title = 'Salary ($)'),
             xaxis = list(title = 'Undergraduate Major'))
  )
  
  output$plot4 <- renderPlotly(
    
    plot4 <- degrees %>%
      
      filter(Starting.Median.Salary > input$Salary) %>%
      
      
      plot_ly(x = ~Undergraduate.Major , y = ~Starting.Median.Salary, type = 'scatter', name = 'Starting Salary', width = 1000, height = 500) %>%
      add_trace(y = ~Mid.Career.Median.Salary, name = 'Mid Career Salary') %>%
      add_trace(y = ~Mid.Career.10th.Percentile.Salary, name = 'Mid Career 10th Percentile Salary', legendrank=1) %>%
      add_trace(y = ~Mid.Career.25th.Percentile.Salary, name = 'Mid Career 25th Percentile Salary', legendrank=2) %>%
      add_trace(y = ~Mid.Career.75th.Percentile.Salary, name = 'Mid Career 75th Percentile Salary', legendrank=3) %>%
      add_trace(y = ~Mid.Career.90th.Percentile.Salary, name = 'Mid Career 90th Percentile Salary', legendrank=4) %>%
      layout(yaxis = list(title = 'Salary ($)'),
             xaxis = list(title = 'Undergrad Major',
                          categoryorder = 'total ascending'), showlegend=TRUE))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
