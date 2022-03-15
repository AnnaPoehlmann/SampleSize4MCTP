#------------------------------------------------------------------------------#
# UI
#------------------------------------------------------------------------------#
nonpar_N_mainPanelUI <- function(id){
  
  ns_nonparN <- NS(id) # namespace shared by inputs and outputs of this module
  
  tagList(
    wellPanel(h4(dataTableOutput(ns_nonparN("nonpar_N_table"))))
  )
  
}

#------------------------------------------------------------------------------#
# Server
#------------------------------------------------------------------------------#
nonpar_N_mainPanel <- function(input, output, session, nonpar_N_inputs){
  
  # calculate output table
  nonpar_N_table <- eventReactive(nonpar_N_inputs$run(), {
    
    SingleSim_Nfix(p = nonpar_N_inputs$p()$`p`,
                   N = nonpar_N_inputs$N(),
                   C = nonpar_N_inputs$C(),
                   power_inp = as.numeric(nonpar_N_inputs$power()),
                   approx = nonpar_N_inputs$approx(),
                   datPilot = nonpar_N_inputs$pilot_data(),
                   seed = nonpar_N_inputs$seed())
  })
  
  # format output table
  output$nonpar_N_table <- renderDataTable({ 
    datatable(nonpar_N_table(),
              options(list(searching = FALSE, paging = FALSE, width = 1200, info = FALSE)),
              rownames = FALSE)
  })

}
