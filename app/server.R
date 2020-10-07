# Define the server code
server <- function(input, output, session) {

  disclaimer_message <- modalDialog(
    title = "Disclaimer",
    disclaimerpopupHTML()
    )
  
  # Show the model on start up ...
  showModal(disclaimer_message)
  
  hit_api<- eventReactive(input$go, {
    
    return(calculateRisk(input))
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

    # if the length of output is more than one
    if(n_out > 1 ){
      #deal with zipcode mapping to >1 counties 
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
      validate(need(!is.null(input$ordinal_county), ""))
      
      which_county<- as.numeric(input$ordinal_county)
      
      #eventReactive(input$ordinal_county, {
      #  updateRadioButtons(session, "ordinal_county", selected = which_county)
      #})
      
    } else {
      # if there is only one output county, select the first output county
      which_county <- 1}
    
    one_county <- api_out[[which_county]]
    
    return (one_county)
  })

  output$gauge <-renderGauge({
    
    risk<-get_risk_info()
    
    output$output_intro <- renderUI({
      # in src/results.R
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
    # in src/results.R
    renderResultsHtml(risk, input$symptoms, input$hand, input$ppe)
  })
  
  output$methods <-renderUI({
    # in src/info_html.R
    renderMethodsHtml()
  })
  
  output$faq <- renderUI({
    # in src/info_html.R
    renderFaqHtml()
  })
  
}