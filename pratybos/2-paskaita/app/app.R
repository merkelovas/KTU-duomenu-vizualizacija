library(shiny)
# add dependency
library(tidyverse)
library(lubridate)

ui <- fluidPage(
  
  titlePanel("2 Paskaita"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imone", label = "Imones pavadinimas", choices = NULL, selected = NULL)
    ),
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  # changed dir
  
  # data <- read_csv("../../../laboratorinis/data/lab_sodra.csv")
  data <- read_csv("https://raw.githubusercontent.com/kestutisd/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv")
  updateSelectInput(session, "imone", choices = unique(data$name))
  output$distPlot <- renderPlot({
    data %>%
      filter(name == input$imone) %>%
      ggplot(aes(y = numInsured, x = month)) +
      geom_point() +
      geom_line()
  })
  
  output$plot <- renderPlot({
    data %>%
      filter(name == input$imone) %>%
      mutate(month = parse_date_time(month, "ym")) %>%
      ggplot(aes(x = month, y = avgWage, color = municipality)) +
      geom_point(aes(size = numInsured)) +
      geom_smooth(method="glm", se=F) +
      ylab("Vidutinis atlyginimas") +
      xlab("")
  })
}

shinyApp(ui = ui, server = server)