#------------------------------------------------------------------------------#
# UI
#------------------------------------------------------------------------------#
par_N_mainPanelUI <- function(id){
  
  ns_parN <- NS(id)
  
  tagList(
    wellPanel(h4(dataTableOutput(ns_parN("par_N_table"))))
  )
  
}

#------------------------------------------------------------------------------#
# Server
#------------------------------------------------------------------------------#
par_N_mainPanel <- function(input, output, session, par_N_inputs){
  
  # calculate output table
  par_N_table <- eventReactive(par_N_inputs$run(), {

    if(par_N_inputs$heteroscedastic() == "Heteroscedastic"){
      
      SingleSimHet_Nfix(mu = par_N_inputs$mu()$`mu`,
                        sigma = c(par_N_inputs$sigma()[,2]),
                        C = contrMat(rep(x = 10, times = nrow(par_N_inputs$mu())), type = par_N_inputs$C()),
                        N = par_N_inputs$N(),
                        power_inp = as.numeric(par_N_inputs$power_inp()),
                        seed = par_N_inputs$seed())
    }else{
      SingleSimHom_Nfix(mu = par_N_inputs$mu()$`mu`,
                        sigma = par_N_inputs$sigma()[1,1],
                        C = contrMat(rep(x = 10, times = nrow(par_N_inputs$mu())), type = par_N_inputs$C()),
                        N = par_N_inputs$N(),
                        power_inp = as.numeric(par_N_inputs$power_inp()),
                        seed = par_N_inputs$seed())
      
    }

  })
  
  # format output table
  output$par_N_table <- renderDataTable({ 
    datatable(par_N_table(),
              options(list(searching = FALSE, paging = FALSE, width = 1200, info = FALSE)),
              rownames = FALSE)
  })

}
