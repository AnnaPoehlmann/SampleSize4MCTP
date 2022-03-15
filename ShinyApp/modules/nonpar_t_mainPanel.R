#------------------------------------------------------------------------------#
# UI
#------------------------------------------------------------------------------#
nonpar_t_mainPanelUI <- function(id){
  
  ns_nonpart <- NS(id) # namespace shared by inputs and outputs of this module
  
  tagList(
    wellPanel(h4(dataTableOutput(ns_nonpart("nonpar_t_table"))))
  )
  
}

#------------------------------------------------------------------------------#
# Server
#------------------------------------------------------------------------------#
nonpar_t_mainPanel <- function(input, output, session, nonpar_t_inputs){
  
  # calculate output table
  nonpar_t_table <- eventReactive(nonpar_t_inputs$run(), {
    
    SingleSim_tfix(p = nonpar_t_inputs$p()$`p`,
                   t = nonpar_t_inputs$t()$`t`,
                   C = nonpar_t_inputs$C(),
                   power_inp = as.numeric(nonpar_t_inputs$power()),
                   approx = nonpar_t_inputs$approx(),
                   datPilot = nonpar_t_inputs$pilot_data(),
                   seed = nonpar_t_inputs$seed())
  })
  
  # format output table
  output$nonpar_t_table <- renderDataTable({ 
    datatable(nonpar_t_table(),
              options(list(searching = FALSE, paging = FALSE, width = 1200, info = FALSE)),
              rownames = FALSE)
  })

}
