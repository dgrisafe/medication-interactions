# # create combinations of all possible entries
# tibble_combo <- tibble::as_tibble(
#   gtools::combinations(n=n, r=r, v = meds, set = TRUE, repeats.allowed = FALSE)
# )
# names_meds_cols <- paste("Medication", 1:r)
# names(tibble_combo) <- names_meds_cols
# 
# # if text is provided for filtering by medication
# if(!is.null(filter_med1)){
#   # then filter medicine by that medication
#   tibble_combo <- dplyr::filter(
#     .data = tibble_combo,
#     dplyr::if_any(.cols = dplyr::everything(), .fns = ~ grepl(filter_med1, .))
#     )
# }
# 
# tibble_combo
# 
# # format table for document
# tibble_out <- dplyr::mutate(
#   # combine multiple medications into single column
#   tidyr::unite(
#     data = tibble_combo, 
#     col = med_combo, sep = " + "
#     ), 
#   # capitalize first letter of each line
#   med_combo = gsub("^([a-z])", "\\U\\1", med_combo, perl=TRUE)
#   )
# 
# tibble_out


# source vector       
# meds <- as.character(input$medicationsPsychiatry)
meds <- c("verapamil", "finasteride", "aspirin", "quetiapine", "tylenol")

# size of source vector
n <- length(meds)

# size of target vectors
# r <- input$n_combo
r <- 3

filter_med <- NULL

if(n >= 2){
  
  # # run each time a user changes text
  # output$table <- renderTable({
    
    # string to filter medication names
    # filter_med1 <- str_to_lower(input$filter_med)
  filter_med1 <- str_to_lower(filter_med)
  
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
        if_any(.cols = everything(), .fns = ~ grepl(pattern = filter_med1, x = .x))
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
    
  # })
  
}