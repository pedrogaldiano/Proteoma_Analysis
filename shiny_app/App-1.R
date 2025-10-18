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

# Install rhandsontable if you don't have it:
# install.packages("rhandsontable")

# library(rhandsontable)

library(rhandsontable)
library(dplyr)


ui <- fluidPage(
  titlePanel("Edit Data File"),
  helpText(
    "Changes to the table will be automatically saved to the source file."
  ),

  # uncomment line below to use action button to commit changes
  actionButton("saveBtn", "Save"),
  rHandsontableOutput("myTable")
)


server <- function(input, output, session) {
  observe({
    # remove button and isolate to update file automatically
    # after each table change
    input$saveBtn

    myTable = isolate(input$myTable)
    if (!is.null(myTable)) {
      df <- hot_to_r(myTable)
      df <- df[!apply(df == "", 0, all), ]

      print(df)
    }
  })

  output$myTable = renderRHandsontable({
    if (!is.null(input$myTable)) {
      DF = hot_to_r(input$myTable)
    } else {
      DF = data.frame("ID" = character(10), "SAMLE" = character(10))
    }

    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
}

shinyApp(ui, server)

# Erro: `server` must be a function

# server <- function(input, output, session) {
#
#   # Initial data frame
#   df <- data.frame(ID = character(5),
#                    SAMPLE = character(5),
#                    stringsAsFactors = FALSE)
#
#
#
#   # Render the editable table
#   myTable <- renderRHandsontable(
#     rhandsontable(df, useTypes = FALSE) %>%
#     hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
#     )
#
#   output$table <- myTable
#
#
#   observeEvent(input$run_btn, {
#     inputDF <- myTable()
#
#     cat(inputDF)
#     cat(class(inputDF))
#
#     })
#

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
#   # })
# }
# }
# shinyApp(ui, server)
