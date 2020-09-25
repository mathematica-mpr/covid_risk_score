# Define the server code
server <- function(input, output, session) {

  disclaimer_message <- modalDialog(
    title = "Disclaimer",
    disclaimerpopupHTML()
    )
  
  # Show the model on start up ...
  showModal(disclaimer_message)
  
  hit_api<- eventReactive(input$go, {

    output$output_intro <-renderUI({
      # in src/results.R
      renderOutputIntroHtml()
      })
    return(calculateRisk(input))
    })

  
  # Sidebar Collapse updates
  updateInputCollapse1 <- eventReactive(input$next0, {
    updateCollapse(session, id = "collapse_main", open = "1. About You", close = "Introduction")
  })
  updateInputCollapse2 <- eventReactive(input$next1, {
    updateCollapse(session, id = "collapse_main", open = "2. Pre-existing Conditions", 
                   close = "1. About You")
  })
  updateInputCollapse3 <- eventReactive(input$next2, {
    updateCollapse(session, id = "collapse_main", open = "3. Your Behavior", 
                   close = "2. Pre-existing Conditions")
  })
  updateInputCollapses <- function() {
    updateInputCollapse1()
    updateInputCollapse2()
    updateInputCollapse3()
  }
  
  updateRisk <- reactive({
    updateInputCollapses()
    if (!input$is_sick) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "symptoms", selected = character(0))
    }
    
    if (!input$has_preexisting) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "conditions", selected = character(0))
    }
    risk_data <- hit_api()
    
    # in results.R
    return (risk_data)
  })

  
  output$gauge <-renderGauge({
    risk<-updateRisk()
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
    risk <- updateRisk()
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