#------------------------------------------------------------------------------#
# UI
#------------------------------------------------------------------------------#
# only show the pilot data when approximation methods B,C,D are chosen 
t_pilotUI <- function(id){
  ns_nonpart <- NS(id) # namespace shared by inputs and outputs of this module
  radioButtons(inputId = ns_nonpart("approx"),
               label = "Method",
               choiceNames = c("A Noether",
                               "B Noether, but account for ties by estimating the variance under H_0 from pilot data",
                               "C estimate the covariance from pilot data",
                               "D like C, but use a t-distribution when calculating the power", 
                               "E estimate the covariance under H_A from a normal distribution"),
               choiceValues = LETTERS[1:5], selected = "A")
  
}

# global UI
nonpar_t_sidebarPanelUI <- function(id){
  
  ns_nonpart <- NS(id)
  
  tagList(
    wellPanel(
      h4("Input", style =  "font-weight: bold", align = "center"),
      
      
      radioButtons(inputId = ns_nonpart("C"), 
                   label = "Contrast",
                   choiceNames = c("Many-to-one (Dunnett)"), #, "All-pairs (Tukey)"),
                   choiceValues = c("Dunnett")), # "Tukey")),
      
      numericInput(inputId = ns_nonpart("a"), 
                   label = "Number of groups",
                   value = 3, min = 3, max = 8),
      
      h5("Relative effect", style = "font-weight: bold"),
      rHandsontableOutput(ns_nonpart("input_p")), HTML("<br>"),
      
      h5("Allocation rate", style = "font-weight: bold"),
      h5("The values of t must sum up to 1.", color = "grey"),
      rHandsontableOutput(ns_nonpart("input_t")), HTML("<br>"),

      numericInput(inputId = ns_nonpart("power_inp"),
                   label = "Power [%]", 
                   value = 80, 
                   min = 1, max = 99),
      
      wellPanel( 
        style = "background-color: white",
        h4("Approximation", style =  "font-weight: bold"),
        t_pilotUI(ns_nonpart('id1')),
        uiOutput(ns_nonpart("dynamic1"))
      ),
      
      numericInput(inputId = ns_nonpart("seed"), 
                   label = "Seed",
                   value = 2)
    ),
    wellPanel(
      actionButton(inputId = ns_nonpart("run"), label = " Run", icon("play")) 
    )
    
  )
  
}

#------------------------------------------------------------------------------#
# Server
#------------------------------------------------------------------------------#
# only show the pilot data when approximation methods B,C,D are chosen 
t_display_pilot <- function(input, output, session){
  reactive(input$approx)
}

# global server
nonpar_t_sidebarPanel <- function(input, output, session){
  
  # INPUT ------------------------

  # relative effect p
  default_p <- reactive({
    if(input$C == "Dunnett"){
      data.frame(Group = paste("A -", LETTERS[2:input$a]),
                 p = c(0.7,0.8,0.4,0.6,0.3,0.2,0.6)[1:(input$a-1)], stringsAsFactors = FALSE)      
    }else if(input$C == "Tukey"){
      data.frame(Group = apply(combn(LETTERS[1:input$a],2), 2, function(x) paste(x, collapse = "-")),
                 p = c(0.7,0.8,0.4,0.6,0.3,0.2,0.6)[1:(input$a)], stringsAsFactors = FALSE) 
    }
  })
  output$input_p <- renderRHandsontable({
    rhandsontable(default_p(), rowHeaders = NULL, readOnly = FALSE)  
  })
  
  # allocation rate t
  default_t <- reactive({
    data.frame(Group = LETTERS[1:input$a], t = get_t_input(input$a), stringsAsFactors = FALSE)
  })
  output$input_t <- renderRHandsontable({
    rhandsontable(default_t(), rowHeaders = NULL, readOnly = FALSE)  
  })
  
  # only show the pilot data when approximation methods B,C,D are chosen
  condition <- callModule(t_display_pilot, ('id1'))
  output$dynamic1 <- renderUI({
    ns <- session$ns
    if (condition() %in% c('B', 'C', 'D')){
      # pilot data for approximation methods B, C, D
      tagList(
        h5("Pilot data for methods B, C, D", style = "font-weight: bold"),
        h5("Please upload the pilot data from the template or enter it into the table.", color = "grey"),
        fileInput(inputId = ns("pilot_upload"), label = "Input File", accept = ".xlsx"),
        h5("Pilot data", style = "font-weight: bold"),
        rHandsontableOutput(ns("pilot_data")), HTML("<br>")
      )
    }
  })
  
  pilot_upload <- eventReactive(input$pilot_upload, {
    read_xlsx(path = input$pilot_upload$datapath)
  })
  
  # create an object that contains the uploaded data or default values
  pilot_default_or_uploaded <- reactive({
    
    if(is.null(input$pilot_upload)){
      # if no data is uploaded: show default data
      data.frame(A = c(3.78,3.40,4.14,3.14,5.00,4.76,3.23,3.31),
                 B = c(5.00,3.80,4.01,3.62,3.95,4.12,4.54, NA),
                 C = c(4.14,4.11,4.50,4.21,4.81,3.91,4.19,5.00))
      
    }else{
      pilot_upload()
    }
  })
  
  # pilot data displayed in the app
  output$pilot_data <- renderRHandsontable({
    rhandsontable(pilot_default_or_uploaded(), rowHeaders = NULL, readOnly = FALSE)
  })
  
  return(
    list(
      a = reactive({input$a}),
      p = reactive({hot_to_r(input$input_p)}),
      t = reactive({hot_to_r(input$input_t)}),
      power_inp = reactive({input$power_inp/100}),
      C = reactive({input$C}),
      approx = reactive({condition()}),
      pilot_data = reactive({hot_to_r(input$pilot_data)}),
      seed = reactive({input$seed}),
      run = reactive({input$run})
    )
  )
  
}
