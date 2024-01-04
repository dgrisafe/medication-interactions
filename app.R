library(shiny)  # http://shiny.rstudio.com/
library(tidyverse)
library(gtools)
library(DT)
library(markdown)

# input .txt file w/common psychiatric medications on each line
medsPsych <- as.data.frame(read.table(file = "data/medicationsPsychiatry.txt", header = FALSE, sep = "\n"))
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
          label = "Search combinations for a specific medicine",
          value = NULL,
          placeholder = "type the name of a single medicaiton (not case sensitive)"
        )
        
      ),
      
      # main panel output
      mainPanel(
        
        p("Your treatment team may prescribe more than one medication at a time."),
        
        h3("Possible medication combinations include:"),
       
        DTOutput("valu", width = "100%"),
        
      )
      
    ),
    
    # add a row at the bottom with the readme .md file
    fluidRow(column(12, 
        includeMarkdown("README.md")
        )
    )
    
)

# Define server logic required to create table of combinations
server <- function(input, output, session) {
  
  # widget input medications
  medicationsPsychiatry <- reactive({
    as.character(input$medicationsPsychiatry)
  })

  # widget input number of combinations
  n_combo <- reactive({
    input$n_combo # size of target vectors
  })  
  
  # widget input search text to filter all medication combinations
  filter_med <- reactive({
    input$filter_med # size of target vectors
  })  
  
  combine_meds <- reactive({

      # create combinations of all possible entries
      tibble_combo <- as_tibble(
        combinations(
          n=length(medicationsPsychiatry()), 
          r=n_combo(), 
          v = medicationsPsychiatry(), 
          set = TRUE, 
          repeats.allowed = FALSE
          )
      )
      names(tibble_combo) <- paste("Medication", 1:n_combo())
      
      # if text is provided for filtering by medication...
      if(!is.null(filter_med())){
        # ...then filter medicine by that medication
        tibble_combo <- filter(
          .data = tibble_combo,
          if_any(.cols = everything(), .fns = ~ grepl(str_to_lower(filter_med()), .))
        )
      }
      
      # format table for document
      tibble_out <- mutate(
        # combine multiple medications into single column
        unite(
          data = tibble_combo, 
          col = med_combo, 
          sep = " + "
        ), 
        # capitalize first letter of each line
        med_combo = gsub("^([a-z])", "\\U\\1", med_combo, perl=TRUE)
      )
      
      # final output
      names(tibble_out) <- paste("Combinations of", n_combo(), "Medications")
      return(tibble_out)
      
  })

  output$valu <- renderDT({
    
    # run the app only if appropriate number of medications
    # and combination numbers provided
    if(length(medicationsPsychiatry()) >= n_combo()) { 
      
      data <- combine_meds()
      
      DT::datatable(
        data,
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = NULL,
        extensions = c("Buttons", "Select"),
        selection = 'none',
        fillContainer = FALSE,
        options = list(
          pageLength = 100,
          select = TRUE,
          dom = "Brtip",  ##  remove f to remove search  ## Brftip
          scrollX = TRUE,
          buttons = list(
            list(
              title = NULL,
              extend = "copy",
              text = 'Copy'#,
              #exportOptions = list(modifier = list(selected = TRUE))
              )
            )
          )) %>% 
        formatStyle(0, target = "row", fontWeight = styleEqual(1, "bold")) 
      }

  }, server = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
