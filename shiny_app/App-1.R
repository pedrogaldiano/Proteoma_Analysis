

# Define UI for app that draws a histogram ----
# ui <- page_sidebar(
#   # App title ----
#   title = "Hello world!",
#   # Sidebar panel for inputs ----
#   sidebar = sidebar(
#     # Input: Slider for the number of bins ----
#     sliderInput(
#       inputId = "bins",
#       label = "Number of bins:",
#       min = 5,
#       max = 50,
#       value = 30
#     )
#   ),
#   # Output: Histogram ----
#   plotOutput(outputId = "distPlot", )
# )
# 
# 
# # Define server logic required to draw a histogram ----
# server <- function(input, output) {
#   
#   # Histogram of the Old Faithful Geyser Data ----
#   # with requested number of bins
#   # This expression that generates a histogram is wrapped in a call
#   # to renderPlot to indicate that:
#   #
#   # 1. It is "reactive" and therefore should be automatically
#   #    re-executed when inputs (input$bins) change
#   # 2. Its output type is a plot
#   output$distPlot <- renderPlot({
#     
#     x    <- faithful$waiting
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     
#     hist(x, breaks = bins, col = "#007bc2", border = "white",
#          xlab = "Waiting time to next eruption (in mins)",
#          main = "Histogram of waiting times")
#     
#   })
#   
# }


s <- mtcars


# Install rhandsontable if you don't have it:
# install.packages("rhandsontable")

library(shiny)
library(rhandsontable)

ui <- fluidPage(
  titlePanel("Editable Table with Run Button"),
  rHandsontableOutput("table"),
  
  actionButton("add_row", "Add Row"),
  actionButton("run_btn", "Run")
)


server <- function(input, output, session) {
  
  # Initial data frame
  df <- data.frame(ID = character(40),
                   SAMPLE = character(40),
                   stringsAsFactors = FALSE)
  

  
  # Render the editable table
  myTable <- renderRHandsontable(
    rhandsontable(df, useTypes = FALSE) %>%
    hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    )
  
  output$table <- myTable
  
  
  # # When user edits the table
  # 
  # # When user clicks "Run" button
  # observeEvent(input$run_btn, {
  #   updated_df <- myData()
  #   
  #   
  #   print(updated_df)
  #   
  #   # Here you can run whatever code you need with updated_df
  #   showModal(modalDialog(
  #     title = "Table Submitted",
  #     "Your table has been processed. Check the console for output."
  #   ))
  # })
}
shinyApp(ui, server)

