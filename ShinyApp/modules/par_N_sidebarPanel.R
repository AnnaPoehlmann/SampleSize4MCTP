#------------------------------------------------------------------------------#
# UI
#------------------------------------------------------------------------------#
par_N_sidebarPanelUI <- function(id){
  
  ns_parN <- NS(id)
  
  tagList(
   # verbatimTextOutput("debug"),
    wellPanel(
      h4("Input", style =  "font-weight: bold", align = "center"),

      radioButtons(inputId = ns_parN("C"), 
                   label = "Contrast",
                   choiceNames = c("Many-to-one (Dunnett)", "All-pairs (Tukey)", "Sequen", "AVE", "Changepoint", 
                                   "Williams", "Marcus", "McDermott", "UmbrellaWilliams", "GrandMean"),
                   choiceValues = c("Dunnett", "Tukey", "Sequen", "AVE", "Changepoint",
                                    "Williams", "Marcus", "McDermott", "UmbrellaWilliams", "GrandMean")),
      
      numericInput(inputId = ns_parN("a"), 
                   label = "Number of groups",
                   value = 3, min = 3, max = 8),
      
      h5("Mean", style =  "font-weight: bold"),
      rHandsontableOutput(ns_parN("mu")), HTML("<br>"),
      
      radioButtons(inputId = ns_parN("heteroscedastic"), 
                   label = "Variance",
                   choices = c("Homoscedastic", "Heteroscedastic"), selected = "Heteroscedastic"),

      rHandsontableOutput(ns_parN("sigma")), HTML("<br>"),

      numericInput(inputId = ns_parN("N"),
                   label = "Total sample size", 
                   value = 100, 
                   min = 10, max = 1000),

      numericInput(inputId = ns_parN("power_inp"),
                   label = "Power [%]", 
                   value = 80, 
                   min = 1, max = 99),
      
      numericInput(inputId = ns_parN("seed"), 
                   label = "Seed",
                   value = 2)
      
    ),
    wellPanel(
      actionButton(inputId = ns_parN("run"), label = " Run", icon("play")) 
    )
    
  )
  
}

#------------------------------------------------------------------------------#
# Server
#-----------------------------------------------------------------------------#
par_N_sidebarPanel <- function(input, output, session){
  
  # INPUT ------------------------

  default_sigma <- reactive({
    if(input$heteroscedastic == "Heteroscedastic"){
      data.frame(Group = LETTERS[1:input$a], sigma = c(1,1,0.5,0.5,0.5,1,0.2,0.2)[1:input$a],  
                 stringsAsFactors = FALSE)
    }else{
      data.frame(sigma = 1)
    }
  })
  output$sigma <- renderRHandsontable({
    rhandsontable(default_sigma(), rowHeaders = NULL, readOnly = FALSE)  
  })
  
  # create object that contains default values for mu
  default_mu <- reactive({
    data.frame(Group = LETTERS[1:input$a], mu = c(0,0.5,1,0.25,0.2,0.4,0.6,0.8)[1:input$a],  
               stringsAsFactors = FALSE)
  })
  output$mu <- renderRHandsontable({
    rhandsontable(default_mu(), rowHeaders = NULL, readOnly = FALSE)  
  })

  return(
    list(
      mu = reactive({hot_to_r(input$mu)}),
      sigma = reactive({hot_to_r(input$sigma)}),
      heteroscedastic = reactive({input$heteroscedastic}),
      N = reactive({input$N}),
      C = reactive({input$C}),
      power_inp = reactive({input$power_inp/100}),
      seed = reactive({input$seed}),
      run = reactive({input$run})
    )
  
  )
  
}
