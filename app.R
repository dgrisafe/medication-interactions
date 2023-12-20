library(shiny)  # http://shiny.rstudio.com/

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
          label = "Enter list of medications, separated by commas",
          choices = medsPsych$meds,
          selected = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(
            'plugins' = list('remove_button'),
            'create' = TRUE,
            'persist' = TRUE,
          placeholder = "e.g., abilify PO, haloperidol, quetiapine"
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
        
        h2("Antipsychotic/Mood Stabilizer Polypharmacy Information Sheet"),
        p("Your treatment team may prescribe more than one medication at a time."),
        
        # h3("Why would I receive two similar types of medication?"),
        # p("1. Improve or optimize your treatment by using medications that work in different ways to treat the same condition. This can be a useful strategy if one medication used alone was not adequately treating your symptoms."),
        # p("2. Your provider may be attempting to switch from one medication to another. In order to safely switch to another medication, they may have to slowly decrease the dose of one and increase the dose of another medication."),
        # 
        # h3("What are the potential risks of taking two or more antipsychotic medications at the same time?"),
        # p("- Increased sedation: Taking two or more antipsychotics can increase the amount of tiredness that you are feeling."),
        # p("- Lower seizure threshold: This combination of medication may possibly increase risk of seizures for patients who already have a seizure disorder."),
        # p("- QT prolongation: This can occur when the heart muscle takes a longer time to contract than to relax. Taking two or more medications with this risk can lead to an abnormal heart rhythm. Report any chest pain or palpitations to your physician immediately."),
        # p("- Extrapyramidal symptoms: This combination of medications may lead to increased risk of developing a movement disorder. Report any abnormal or uncontrolled movements to your prescriber."),
        # p("- Peripheral edema: Depakote and risperidone (a type of antipsychotic) together can lead to swelling of the limbs. Contact your prescriber if you notice any swelling in your limbs."),
        
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
    meds <- as.character(input$medicationsPsychiatry)
    
    # size of source vector
    n <- length(meds)
    
    # size of target vectors
    r <- input$n_combo

    # string to filter medication names
    filter_med1 <- stringr::str_to_lower(input$filter_med)
    
    # create combinations of all possible entries
    tibble_combo <- tibble::as_tibble(
      gtools::combinations(n=n, r=r, v = meds, set = TRUE, repeats.allowed = FALSE)
    )
    names(tibble_combo) <- paste("Medication", 1:r)
    
    # if text is provided for filtering by medication...
    if(!is.null(filter_med1)){
      # ...then filter medicine by that medication
      tibble_combo <- dplyr::filter(
        .data = tibble_combo,
        dplyr::if_any(.cols = dplyr::everything(), .fns = ~ grepl(filter_med1, .))
      )
    }
    
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
    names(tibble_out) <- paste("Combinations of", r, "Medications")
    tibble_out
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
