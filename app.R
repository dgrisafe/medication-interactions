library(shiny)  # http://shiny.rstudio.com/
library(tidyverse)
library(gtools)

# input .txt files of psychiatric medications classes with each line listing a common medications
medsList <- sapply(
  X = c("antipsychotics", "lais", "moodstabilizers", "antidepressants", "benzos", "misc"),
  FUN = function(x){list(read.table(file = paste0("medication_lists/medlis_", x, ".txt"), col.names = "meds", header = FALSE, sep = "\n"))}
  )
names(medsList) <- c("medsAntiPsy", "medsLAIs", "medsMood", "medsAntiDep", "medsBenzo", "medsMisc")

# input medications to UI
inputMedClass <- function(inTitle, inId,inMeds){
  div(
    h3(inTitle),
    # input box to enter medications
    selectizeInput(
      inputId = inId,
      label = "Type in medications, separated by commas",
      choices = inMeds[["meds"]],
      selected = NULL,
      multiple = TRUE,
      width = "100%",
      options = list(
        'plugins' = list('remove_button'),
        'create' = TRUE,
        'persist' = TRUE,
        placeholder = paste(inMeds[[1]], collapse = ", ")
      )
    ),
    # input box to enter number of combinations
    numericInput(
      inputId = "n_combo",
      label = "Number of combinations",
      width = "50%",
      value = 1,
      min = 1,
      max = 5
    )
  )
  
}

# Define UI for combinations of psychiatric medications
ui <- fluidPage(

    # Application title
    titlePanel("Medication Interactions"),

    # Sidebar 
    sidebarLayout(
      
      sidebarPanel(
        
        inputMedClass(inTitle = "Antipsychotics", inId = "meds_antipsychotics", inMeds = medsList$medsAntiPsy),
        inputMedClass(inTitle = "Long-Acting Injectables", inId = "meds_lais", inMeds = medsList$medsLAIs),
        inputMedClass(inTitle = "Mood Stabilizers", inId = "meds_moodstabs", inMeds = medsList$medsMood),
        inputMedClass(inTitle = "Antidepressants", inId = "meds_antideps", inMeds = medsList$medsAntiDep),
        inputMedClass(inTitle = "Benzodiazepines", inId = "meds_benzos", inMeds = medsList$medsBenzo),
        inputMedClass(inTitle = "Miscellaneous", inId = "meds_misc", inMeds = medsList$medsMisc),

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
