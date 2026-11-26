library(shiny)

# Read the survey data (CSV sits next to app.R)
df <- read.csv("AI_Attendance_Survey_Clean.csv")

ui <- fluidPage(
  titlePanel("STA304 Survey Explorer"),
  textOutput("n_rows")
)

server <- function(input, output) {
  output$n_rows <- renderText({
    paste("Loaded", nrow(df), "students.")
  })
}

shinyApp(ui, server)
