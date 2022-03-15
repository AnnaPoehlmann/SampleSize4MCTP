server <- function(input, output, session) {
  
  # load functions
  source("./source/functions_par.R", local = TRUE)
  source("./source/functions_nonpar.R", local = TRUE)
  
  # load modules
  source("./modules/par_t_sidebarPanel.R", local = TRUE)
  source("./modules/par_t_mainPanel.R", local = TRUE)
  source("./modules/par_N_sidebarPanel.R", local = TRUE)
  source("./modules/par_N_mainPanel.R", local = TRUE)
  source("./modules/nonpar_t_sidebarPanel.R", local = TRUE)
  source("./modules/nonpar_t_mainPanel.R", local = TRUE)
  source("./modules/nonpar_N_sidebarPanel.R", local = TRUE)
  source("./modules/nonpar_N_mainPanel.R", local = TRUE)

  # PARAMETRIC CASE  ----------------------------------------------
  
  # t KNOWN ---------------------------------------
  
  # INPUT ------------------------
  par_t_inputs <- callModule(module = par_t_sidebarPanel, id = "par_t_sidebarPanel")

  # OUTPUT -----------------------
  callModule(module = par_t_mainPanel, id = "par_t_mainPanel", par_t_inputs = par_t_inputs)
  
  # N KNOWN ---------------------------------------
  
  # INPUT ------------------------
  par_N_inputs <- callModule(module = par_N_sidebarPanel, id = "par_N_sidebarPanel")
  
  # OUTPUT -----------------------
  callModule(module = par_N_mainPanel, id = "par_N_mainPanel", par_N_inputs = par_N_inputs)

  # NON-PARAMETRIC CASE  -------------------------------------------
  
  # t KNOWN ---------------------------------------
  
  # INPUT ------------------------
  nonpar_t_inputs <- callModule(module = nonpar_t_sidebarPanel, id = "nonpar_t_sidebarPanel")
  
  # OUTPUT -----------------------
  callModule(module = nonpar_t_mainPanel, id = "nonpar_t_mainPanel", nonpar_t_inputs = nonpar_t_inputs)
  
  # N KNOWN ---------------------------------------
  
  # INPUT ------------------------
  nonpar_N_inputs <- callModule(module = nonpar_N_sidebarPanel, id = "nonpar_N_sidebarPanel")
  
  # OUTPUT -----------------------
  callModule(module = nonpar_N_mainPanel, id = "nonpar_N_mainPanel", nonpar_N_inputs = nonpar_N_inputs)
  
}
