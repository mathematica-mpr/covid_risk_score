# Define the server code
server <- function(input, output, session) {

  disclaimer_message <- modalDialog(
    title = "Disclaimer",
    disclaimerpopupHTML()
    )
  
  # Show the model on start up ...
  showModal(disclaimer_message)
  
  # function to make request to API
  hit_api<- eventReactive(input$go, {
    
    api_return <- calculateRisk(input)
    
    # if a message is returned, display the message
    validate(need(is.null(api_return$message), gsub("Bad Request: ","\nPlease Correct:\n",as.character(api_return$message))))
    
    return(api_return$results)
    })

  # update some input values when input$go is pressed
  calc_input_vals <- eventReactive(input$go, {
    
    val_list <- list("hand" = input$hand, 
                     "ppe" = input$ppe, 
                     "symptoms" = input$symptoms)
    
    return(val_list)
  })
  
  # Sidebar Collapse updates
  observeEvent(input$next0, {
    updateCollapse(session, id = "collapse_main", open = "1. About You", close = "Introduction")
  })
  observeEvent(input$next1, {
    updateCollapse(session, id = "collapse_main", open = "2. Pre-existing Conditions", 
                   close = "1. About You")
  })
  observeEvent(input$next2, {
    updateCollapse(session, id = "collapse_main", open = "3. Your Behavior", 
                   close = "2. Pre-existing Conditions")
    if (!input$is_sick) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "symptoms", selected = character(0))
    }
  })
  
  observe({
    # handle special cases (immune and other) in chronic conditions 
    if ("is_other" %in% input$conditions) {
      # if other chronic condition is selected, clear all other selections
      updateCheckboxGroupInput(session, "conditions", selected = "is_other")
    }
    if (!input$has_preexisting) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "conditions", selected = character(0))
    }
  })
  
  get_risk_info <- reactive({
    
    # hit the API
    api_out <- hit_api()
    
    # number of outputs
    n_out <- length(api_out)

    # if the length of output is more than one have user select one county
    if(n_out > 1 ){
      output$select_county <- renderUI({
      validate(need(is.null(input$ordinal_county), ""))
      tagList(
        tags$h4("There is more than one county that matches your 5-digit zip code."),
        tags$h4("Please choose a county:")
      )})
      output$zipcontrol <- renderUI({
        validate(need(is.null(input$ordinal_county), ""))
        list_opts <- as.character(1:length(api_out))
        county_names<-sapply(api_out, `[`, "name") %>% as.character()
        radioButtons("ordinal_county", label = "Counties:", 
                     choiceNames = county_names, choiceValues  = list_opts, selected = character(0))
        })
      
      # stop if county is not selected
      validate(need(!is.null(input$ordinal_county), ""))
      
      which_county<- as.numeric(input$ordinal_county)
      
    } else {
      # if there is only one output county, select the first output county
      which_county <- 1}
    
    one_county <- api_out[[which_county]]
    
    return (one_county)
  })

  output$gauge <-renderGauge({
    
    risk<-get_risk_info()
    
    validate(need(!is.null(risk), ""))
    
    output$output_intro <- renderUI({
      # in app/results.R
      renderOutputIntroHtml()
    })
    
    gauge(case_when(risk$score<1 ~ 1,
                    risk$score>100 ~ 100,
                TRUE ~round(risk$score)), 
          min = 0, max = 100, 
          sectors = gaugeSectors(success = c(0, 30),
                                 warning = c(30, 70),
                                 danger = c(70, 100)),
          label = "")
  })
  
  output$res <-renderUI({
    risk <- get_risk_info()
    input_vals <- calc_input_vals()
    # in app/results.R
    renderResultsHtml(risk, input_vals$symptoms, input_vals$hand, input_vals$ppe)
  })
  
  output$methods <-renderUI({
    # in app/info_html.R
    renderMethodsHtml()
  })
  
  output$faq <- renderUI({
    # in app/info_html.R
    renderFaqHtml()
  })
  
}