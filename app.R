library(shiny)  # http://shiny.rstudio.com/
library(tidyverse)
library(gtools)

# input .txt file w/common psychiatric medicaions on each line
medsAntiPsy <- as.data.frame(read.table(file = "medication_lists/medlis_antipsychotics.txt", header = FALSE, sep = "\n"))
names(medsAntiPsy) <- "meds"
medsLAIs <- as.data.frame(read.table(file = "medication_lists/medlis_lais.txt", header = FALSE, sep = "\n"))
names(medsLAIs) <- "meds"
medsMood <- as.data.frame(read.table(file = "medication_lists/medlis_moodstabilizers.txt", header = FALSE, sep = "\n"))
names(medsMood) <- "meds"
medsAntiDep <- as.data.frame(read.table(file = "medication_lists/medlis_antidepressants.txt", header = FALSE, sep = "\n"))
names(medsAntiDep) <- "meds"
medsBenzo <- as.data.frame(read.table(file = "medication_lists/medlis_benzos.txt", header = FALSE, sep = "\n"))
names(medsBenzo) <- "meds"
medsMisc <- as.data.frame(read.table(file = "medication_lists/medlis_misc.txt", header = FALSE, sep = "\n"))
names(medsMisc) <- "meds"
label_meds <- "Type in medications, separated by commas"

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Medication Interactions"),

    # Sidebar 
    sidebarLayout(
      
      # antipsychotics
      sidebarPanel(
        
        div(
          h3("Antipsychotics"),
          # input box to enter medications
          selectizeInput(
            inputId = "meds_antipsychotics",
            label = label_meds,
            choices = medsAntiPsy$meds,
            selected = NULL,
            multiple = TRUE,
            width = "100%",
            options = list(
              'plugins' = list('remove_button'),
              'create' = TRUE,
              'persist' = TRUE,
            placeholder = paste(medsAntiPsy[[1]], collapse = ", ")
            )
            ),
          # input box to enter number of combinations
          numericInput(
            inputId = "n_combo",
            label = "Number of combinations",
            width = "50%",
            value = 2,
            min = 2,
            max = 10
          )
          ),

        # long-acting injectables
        div(
          h3("Long-Acting Injectables"),
          # input box to enter medications
          selectizeInput(
            inputId = "meds_lais",
            label = label_meds,
            choices = medsLAIs$meds,
            selected = NULL,
            multiple = TRUE,
            width = "100%",
            options = list(
              'plugins' = list('remove_button'),
              'create' = TRUE,
              'persist' = TRUE,
            placeholder = paste(medsLAIs[[1]], collapse = ", ")
            )
            ),
          # input box to enter number of combinations
          numericInput(
            inputId = "n_combo",
            label = "Number of combinations",
            width = "50%",
            value = 2,
            min = 2,
            max = 10
          )
        ),
        
        div(
          h3("Mood Stabilizers"),
          # input box to enter medications
          selectizeInput(
            inputId = "meds_lais",
            label = label_meds,
            choices = medsMood$meds,
            selected = NULL,
            multiple = TRUE,
            width = "100%",
            options = list(
              'plugins' = list('remove_button'),
              'create' = TRUE,
              'persist' = TRUE,
              placeholder = paste(medsMood[[1]], collapse = ", ")
            )
          ),
          # input box to enter number of combinations
          numericInput(
            inputId = "n_combo",
            label = "Number of combinations",
            width = "50%",
            value = 2,
            min = 2,
            max = 10
          )
        ),
        
        div(
          h3("Antidepressants"),
          # input box to enter medications
          selectizeInput(
            inputId = "meds_lais",
            label = label_meds,
            choices = medsAntiDep$meds,
            selected = NULL,
            multiple = TRUE,
            width = "100%",
            options = list(
              'plugins' = list('remove_button'),
              'create' = TRUE,
              'persist' = TRUE,
              placeholder = paste(medsAntiDep[[1]], collapse = ", ")
            )
          ),
          # input box to enter number of combinations
          numericInput(
            inputId = "n_combo",
            label = "Number of combinations",
            width = "50%",
            value = 2,
            min = 2,
            max = 10
          )
        ),
        
        div(
          h3("Benzodiazepines"),
          # input box to enter medications
          selectizeInput(
            inputId = "meds_lais",
            label = label_meds,
            choices = medsBenzo$meds,
            selected = NULL,
            multiple = TRUE,
            width = "100%",
            options = list(
              'plugins' = list('remove_button'),
              'create' = TRUE,
              'persist' = TRUE,
            placeholder = paste(medsBenzo[[1]], collapse = ", ")
            )
            ),
          # input box to enter number of combinations
          numericInput(
            inputId = "n_combo",
            label = "Number of combinations",
            width = "50%",
            value = 2,
            min = 2,
            max = 10
          )
        ),
                
        div(
          h3("Miscellaneous"),
          # input box to enter medications
          selectizeInput(
            inputId = "meds_lais",
            label = label_meds,
            choices = medsMisc$meds,
            selected = NULL,
            multiple = TRUE,
            width = "100%",
            options = list(
              'plugins' = list('remove_button'),
              'create' = TRUE,
              'persist' = TRUE,
            placeholder = paste(medsMisc[[1]], collapse = ", ")
            )
            ),
          # input box to enter number of combinations
          numericInput(
            inputId = "n_combo",
            label = "Number of combinations",
            width = "50%",
            value = 2,
            min = 2,
            max = 10
          )
        )

      ),
      
      # main panel output
      mainPanel(
        
        # h3(""),
        # text to filter combinations by
        textInput(
          inputId = "filter_med",
          label = "Filter combinations by medicine",
          value = NULL,
          placeholder = "type the name of a single medicaiton (not case sensitive)",
          width = "100%"
        ),
        
        h2("Polypharmacy Information Sheet"),
        p("Your treatment team may prescribe more than one medication at a time."),
        
        h3("Possible medication combinations include:"),
        tableOutput("table")
        
      )
    )
)

# Define server logic required to create table of combinations
server <- function(input, output, session) {

  # run each time a user changes text
  output$table <- renderTable({
 
    # source vector       
    meds <- as.character(input$meds_antipsychotics)
    
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
    if(!is.null(filter_med1)){
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

# Run the application 
shinyApp(ui = ui, server = server)
