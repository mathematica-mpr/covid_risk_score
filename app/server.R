# Define the server code
server <- function(input, output, session) {

  disclaimer_message <- modalDialog(
    title = "Disclaimer",
    disclaimerpopupHTML()
    )
  
  # Show the model on start up ...
  showModal(disclaimer_message)
  
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
  })
  
  observe({
    
    if (!input$is_sick) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "symptoms", selected = character(0))
    }
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
    # do not procced if go was not clicked at all
    validate(need(input$go > 0, ""))
    validate(need(nchar(input$zip) >= 5, ""))
    api_return <- calculateRisk(input)

    if (is.null(api_return$message)) {
      api_out <- api_return$results
      
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
    } else  {
      return(api_return)
    }   
  })
  
  output$output_intro <- renderUI({
    risk<-get_risk_info()
    
    if (!is.null(risk$message)) {
      HTML(paste0(risk$message, collapse = "<br/>"))
    } else {
      renderOutputIntroHtml()
    }
  })
  
  output$gauge <-renderGauge({
    
    risk<-get_risk_info()
    
    if (is.null(risk$message)) {
      validate(need(!is.null(risk), ""))
      
      gauge(case_when(risk$score<1 ~ 1,
                      risk$score>100 ~ 100,
                      TRUE ~round(risk$score)), 
            min = 0, max = 100, 
            sectors = gaugeSectors(success = c(0, 30),
                                   warning = c(30, 70),
                                   danger = c(70, 100)),
            label = "")
    }
  })
  
  output$res <-renderUI({
    risk <- get_risk_info()
    # in app/results.R
    if (is.null(risk$message)) {
      renderResultsHtml(risk, input$symptoms, input$hand, input$ppe)
    } 
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