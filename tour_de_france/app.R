library(shiny, warn.conflicts = FALSE)
library(shinythemes, warn.conflicts = FALSE)

general <- read.csv('data/general.csv')
green <- read.csv('data/green.csv')
peas <- read.csv("data/peas.csv")
white <- read.csv("data/white.csv")
team <- read.csv("data/team.csv")

ui <- shiny::fluidPage(theme = shinythemes::shinytheme('flatly'),
                       shiny::titlePanel(shiny::h1("Tour de France 2022", align = "center"), windowTitle = "Tour de France 2022"),
                       
                       shiny::sidebarLayout(
                         shiny::sidebarPanel(width=2,
                                             shiny::selectInput("first_rider", label = "Выберите первого гонщика",
                                                                choices = sort(unique(general$RIDER)),
                                                                selected = "Jonas Vingegaard"),
                                             shiny::selectInput("second_rider", label = "Выберите второго гонщика",
                                                                choices = sort(unique(general$RIDER))),
                                             shiny::radioButtons("type_plot", label = "Type of plot", choices = c("Every stage", "All tour"))),
                         
                         shiny::mainPanel(width=10,
                                          shiny::plotOutput("plot_diff")
                                          )
                       ))


server <- function(input, output) {
  
  data <- shiny::reactive({
    data <- general %>% 
      filter(RIDER == input$first_rider | RIDER == input$second_rider)
    
    longer_data <- cbind(data[seq(1, nrow(data), 2),], data[seq(2, nrow(data), 2),])
    colnames(longer_data) <- c("POS1", "RIDER1", "COUNTRY1", "TIME1", "STAGE1", "POS2", "RIDER2", "COUNTRY2", "TIME2", "STAGE2")
    longer_data$DIFF_TIME <- (longer_data$TIME1 - longer_data$TIME2)
    
    longer_data <- longer_data %>% 
      arrange(STAGE1) %>% 
      mutate(DIFF_STAGE = DIFF_TIME - lag(DIFF_TIME, default = 0))
    longer_data
  })
  
  output$plot_diff <- shiny::renderPlot({
    gap_on_first_stage <- data()[longer_data$STAGE1 == 1, "DIFF_TIME"]
    
    if(input$type_plot == "Every stage"){
      ggplot(longer_data, aes(STAGE1, DIFF_STAGE)) +
        geom_line() +
        geom_point()
    } else {
      ggplot(longer_data, aes(STAGE1, DIFF_TIME)) +
        geom_line() + 
        geom_point()
    }
  }) 
}

shiny::shinyApp(ui = ui, server = server)