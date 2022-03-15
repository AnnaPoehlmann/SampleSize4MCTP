rm(list = ls())
# load /install packages
pacman::p_load(shiny, rhandsontable, DT, readxl, multcomp, rankFD, tidyr, purrr)

# load modules
source("./modules/par_t_sidebarPanel.R", local = TRUE)    
source("./modules/par_t_mainPanel.R", local = TRUE)
source("./modules/par_N_sidebarPanel.R", local = TRUE)
source("./modules/par_N_mainPanel.R", local = TRUE)
source("./modules/nonpar_t_sidebarPanel.R", local = TRUE)    
source("./modules/nonpar_t_mainPanel.R", local = TRUE)
source("./modules/nonpar_N_sidebarPanel.R", local = TRUE)
source("./modules/nonpar_N_mainPanel.R", local = TRUE)



navbarPage( 'Sample size planning for multiple contrast tests',
  #img(src="img/ibike-logo.jpg", width = "50px"),
  # titlePanel(div(img(src="img/ibike-logo.jpg", width = "100px"),"Sample size planning for multiple contrast tests")),
  # tags$hr(style="border:solid #939395 1px;"),
  # #          
           
    tabPanel("Parametric - t known",
             sidebarLayout(
               
               sidebarPanel(
                 style = "background-color: #dcdcdc" ,
                 par_t_sidebarPanelUI("par_t_sidebarPanel")
               ),

               mainPanel(
                 par_t_mainPanelUI("par_t_mainPanel")
               )
             )
    ),
    tabPanel("Parametric - N known",
             sidebarLayout(
               
               sidebarPanel(
                  style = "background-color: #dcdcdc" ,
                  par_N_sidebarPanelUI("par_N_sidebarPanel")
               ),
               
               mainPanel(
                  par_N_mainPanelUI("par_N_mainPanel")
               )
             )
    ),

    tabPanel("Nonparametric - t known",
             sidebarLayout(

               sidebarPanel(

                 style = "background-color: #dcdcdc" ,
                 nonpar_t_sidebarPanelUI("nonpar_t_sidebarPanel")

               ),

               mainPanel(

                 nonpar_t_mainPanelUI("nonpar_t_mainPanel")

               )
             )
             
    ),
    tabPanel("Nonparametric - N known",
             sidebarLayout(
               
               sidebarPanel(
                 style = "background-color: #dcdcdc" ,
                 nonpar_N_sidebarPanelUI("nonpar_N_sidebarPanel")
               ),
               
               mainPanel(
                 nonpar_N_mainPanelUI("nonpar_N_mainPanel")
               )
             )
    )

)


