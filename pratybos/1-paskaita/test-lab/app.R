library(shiny)
ui <- fluidPage(

    titlePanel("Lab test"),
    sidebarLayout(
        sidebarPanel(
            textInput("test", "test")
        ),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- read_csv("../../laboratorinis/data/lab_sodra.csv")
    output$distPlot <- renderPlot({
      data %>%
        filter(tax < as.numeric(input$test)) %>%
        ggplot(aes(x = tax)) +
        geom_histogram(bins = 200)
    })
}

shinyApp(ui = ui, server = server)
