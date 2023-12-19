library(shiny)  # http://shiny.rstudio.com/

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
    
    # create combinations of all possible entries
    tibble_combo <- tibble::as_tibble(
      gtools::combinations(n=n, r=r, v = meds, set = TRUE, repeats.allowed = FALSE)
    )
    names(tibble_combo) <- paste("Medication", 1:r)
    
    # format table for document
    tibble_out <- dplyr::mutate(
      # combine multiple medications into single column
      tidyr::unite(
        data = tibble_combo, 
        col = med_combo, sep = " + "
      ), 
      # capitalize first letter of each line
      med_combo = gsub("^([a-z])", "\\U\\1", med_combo, perl=TRUE)
    )
    
    # final output
    tibble_out
  
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
