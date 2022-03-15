#------------------------------------------------------------------------------#
# UI
#------------------------------------------------------------------------------#
par_t_mainPanelUI <- function(id){
  
  ns_part <- NS(id)
  
  tagList(
    wellPanel(h4(dataTableOutput(ns_part("par_t_table"))))
  )
  
}

#------------------------------------------------------------------------------#
# Server
#------------------------------------------------------------------------------#
par_t_mainPanel <- function(input, output, session, par_t_inputs){
  
  # calculate output table
  par_t_table <- eventReactive(par_t_inputs$run(), {

    if(par_t_inputs$heteroscedastic() == "Heteroscedastic"){
      
      SingleSimHet_tfix(mu = par_t_inputs$mu()$`mu`,
                        sigma = c(par_t_inputs$sigma()[,2]),
                        C = contrMat(rep(10, nrow(par_t_inputs$mu())), type = par_t_inputs$C()),
                        t = par_t_inputs$t()$`t`,
                        power_inp = as.numeric(par_t_inputs$power_inp()),
                        seed = par_t_inputs$seed())
    }else{
      SingleSimHom_tfix(mu = par_t_inputs$mu()$`mu`,
                        sigma = par_t_inputs$sigma()[1,1],
                        C = contrMat(rep(10, nrow(par_t_inputs$mu())), type = par_t_inputs$C()),
                        t = par_t_inputs$t()$`t`,
                        power_inp = as.numeric(par_t_inputs$power_inp()),
                        seed = par_t_inputs$seed())
    }
   
  })
  
  # format output table
  output$par_t_table <- renderDataTable({ 
    datatable(par_t_table(),
              options(list(searching = FALSE, paging = FALSE, width = 1200, info = FALSE)),
              rownames = FALSE)
  })

}
