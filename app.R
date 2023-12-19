# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# input .txt file w/common psychiatric medicaions on each line
medsPsych <- as.data.frame(read.table(file = "medicationsPsychiatry.txt", header = FALSE, sep = "\n"))
names(medsPsych) <- "meds"

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Medications"),

    # Sidebar 
    sidebarLayout(
      
      sidebarPanel(
        
        # input box to enter number of combinations
        numericInput(
          inputId = "n_combo",
          label = "Number of medications to combine",
          value = 2,
          min = 2,
          max = 10
        ),
        
        # input box to enter medications
        selectizeInput(
          inputId = "medicationsPsychiatry",
          label = "Enter list of medications, separated by commas",
          choices = medsPsych$meds,
          selected = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(
            'plugins' = list('remove_button'),
            'create' = TRUE,
            'persist' = TRUE
            )
          )
      ),
      
      # main panel output
      mainPanel(
        
        # show text as output
        tableOutput("table")
        
      )
    )
)

# Define server logic required to create table of combinations
server <- function(input, output, session) {

  # run each time a user changes text
  output$table <- renderTable({
 
    # source vector       
    meds <- as.character(input$medicationsPsychiatry)
    
    # size of source vector
    n <- length(meds)
    
    # size of target vectors
    r <- input$n_combo
    
    # as.data.frame(t(combn(vec, 2, simplify = TRUE)))
    tibble_out <- tibble::as_tibble(
      gtools::combinations(n=n, r=r, v = meds, set = TRUE, repeats.allowed = FALSE)
    )
    
    names(tibble_out) <- paste("Medication", 1:r)
    
    tibble_out
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
