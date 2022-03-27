library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Paskaita 2"),
  dashboardSidebar( selectizeInput(inputId = "imone", label = "Imones pavadinimas", choices = NULL, selected = NULL)),
  dashboardBody(plotOutput("distPlot"),
                plotOutput("plot"),
                tableOutput("table"))
)

server <- function(input, output, session) {
  data <- read_csv("../../../laboratorinis/data/lab_sodra.csv")
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
  
  output$table <- renderTable({
    data %>%
      filter(name == input$imone)
  })
}

shinyApp(ui, server)