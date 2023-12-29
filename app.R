library(shiny)  # http://shiny.rstudio.com/
library(tidyverse)
library(gtools)

# input .txt file w/common psychiatric medicaions on each line
medsPsych <- as.data.frame(read.table(file = "medicationsPsychiatry.txt", header = FALSE, sep = "\n"))
names(medsPsych) <- "meds"

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Medication Interactions"),

    # Sidebar 
    sidebarLayout(
      
      sidebarPanel(
        
        # input box to enter medications
        selectizeInput(
          inputId = "medicationsPsychiatry",
          label = "Enter 2 or more medications, separated by commas",
          choices = medsPsych$meds,
          selected = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(
            'plugins' = list('remove_button'),
            'create' = TRUE,
            'persist' = TRUE,
          placeholder = "e.g., abilify, haloperidol, quetiapine"
          )
          ),

        # input box to enter number of combinations
        numericInput(
          inputId = "n_combo",
          label = "Number of medications to combine",
          value = 2,
          min = 2,
          max = 10
        ),
        
        # text to filter combinations by
        textInput(
          inputId = "filter_med",
          label = "Filter combinations by medicine",
          value = NULL,
          placeholder = "type the name of a single medicaiton (not case sensitive)"
        )
      ),
      
      # main panel output
      mainPanel(
        
        p("Your treatment team may prescribe more than one medication at a time."),
        
        h3("Possible medication combinations include:"),
        tableOutput("table")
        
      )
    )
)

# Define server logic required to create table of combinations
server <- function(input, output, session) {

  observeEvent(input$medicationsPsychiatry, {
    
    # initialize user input
    n <- length(as.character(input$medicationsPsychiatry))
    r <- input$n_combo
    
    if(n >= r){

      # run each time a user changes text
      output$table <- renderTable({
        
        # source vector       
        meds <- as.character(input$medicationsPsychiatry)
        
        # size of source vector
        n <- length(meds)
        
        # size of target vectors
        r <- input$n_combo
    
        # string to filter medication names
        filter_med1 <- str_to_lower(input$filter_med)
        
        # create combinations of all possible entries
        tibble_combo <- as_tibble(
          combinations(n=n, r=r, v = meds, set = TRUE, repeats.allowed = FALSE)
        )
        names(tibble_combo) <- paste("Medication", 1:r)
        
        # if text is provided for filtering by medication...
        if(!is.null(filter_med)){
          # ...then filter medicine by that medication
          tibble_combo <- filter(
            .data = tibble_combo,
            if_any(.cols = everything(), .fns = ~ grepl(filter_med1, .))
          )
        }
 
        # format table for document
        tibble_out <- mutate(
          # combine multiple medications into single column
          unite(
            data = tibble_combo, 
            col = med_combo, sep = " + "
          ), 
          # capitalize first letter of each line
          med_combo = gsub("^([a-z])", "\\U\\1", med_combo, perl=TRUE)
        )
        
        # final output
        names(tibble_out) <- paste("Combinations of", r, "Medications")
        tibble_out
        
      })
      
    }
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
