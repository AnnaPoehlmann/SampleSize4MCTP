#------------------------------------------------------------------------------#
# UI
#------------------------------------------------------------------------------#
par_t_sidebarPanelUI <- function(id){
  
  ns_part <- NS(id)
  
  tagList(
    verbatimTextOutput("debug"),
    wellPanel(
      h4("Input", style =  "font-weight: bold", align = "center"),

      radioButtons(inputId = ns_part("C"), 
                   label = "Contrast",
                   choiceNames = c("Many-to-one (Dunnett)", "All-pairs (Tukey)", "Sequen", "AVE", "Changepoint", 
                                   "Williams", "Marcus", "McDermott", "UmbrellaWilliams", "GrandMean"),
                   choiceValues = c("Dunnett", "Tukey", "Sequen", "AVE", "Changepoint",
                                    "Williams", "Marcus", "McDermott", "UmbrellaWilliams", "GrandMean")),
      
      numericInput(inputId = ns_part("a"), 
                   label = "Number of groups",
                   value = 3, min = 3, max = 8),
      
      h5("Mean", style =  "font-weight: bold"),
      rHandsontableOutput(ns_part("mu")), HTML("<br>"),
      
      radioButtons(inputId = ns_part("heteroscedastic"), 
                   label = "Variance",
                   choices = c("Homoscedastic", "Heteroscedastic"), selected = "Heteroscedastic"),

      rHandsontableOutput(ns_part("sigma")), HTML("<br>"),

      h5("Allocation rate", style =  "font-weight: bold"),
      h5("The values of t must sum up to 1.", color = "grey"),
      rHandsontableOutput(ns_part("t")), HTML("<br>"),

      numericInput(inputId = ns_part("power_inp"),
                   label = "Power [%]", 
                   value = 80, 
                   min = 1, max = 99),
      
      numericInput(inputId = ns_part("seed"), 
                   label = "Seed",
                   value = 2)
    ),
    wellPanel(
      actionButton(inputId = ns_part("run"), label = " Run", icon("play")) 
    )
    
  )
  
}

#------------------------------------------------------------------------------#
# Server
#-----------------------------------------------------------------------------#
par_t_sidebarPanel <- function(input, output, session){
  
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
  
  # create object that contains default values for t
  default_t <- reactive({
    data.frame(Group = LETTERS[1:input$a], t = get_t_input(input$a),  stringsAsFactors = FALSE)
  })
  output$t <- renderRHandsontable({
    rhandsontable(default_t(), rowHeaders = NULL,readOnly = FALSE)  
  })

  return(
    list(
      mu = reactive({hot_to_r(input$mu)}),
      sigma = reactive({hot_to_r(input$sigma)}),
      heteroscedastic = reactive({input$heteroscedastic}),
      t = reactive({hot_to_r(input$t)}),
      C = reactive({input$C}),
      power_inp = reactive({input$power_inp/100}),
      seed = reactive({input$seed}),
      run = reactive({input$run})
    )
  
  )
  
}
