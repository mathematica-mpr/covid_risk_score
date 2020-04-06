library(shiny)
source("src/global_var.R")

getMethodsHtml <- function() {
  tagList(
    tags$p(""),
    tags$p('Our "Risk Score" visualization is the quantity {Exposure * Susceptibility}, normalized by the average disease burden of flu, logarithmically scaled.'),
    tags$p("Exposure represents how likely it is that you've been infected with the virus. It's a function of the prevalence of active cases in your
             community and ",
           tags$a("transmissibility estimates.", href = urls$cdc_mm6909e1),
           "You can reduce your exposure by ",
           tags$a("social distancing, practicing good hygiene, and closely following the directives of your local public health officials.",
                  href = urls$cdc_prevention),
           "Your personal susceptibility to COVID-19 is quantified by {P(hospitalization) + P(ICU) + P(death)}.",
           "Please remember that even if your personal susceptibility is low, you can still help by preventing spread to others."
    ),
    tags$p(""),
    tags$h4("Assumptions:"),
    tags$ol(
      tags$li(
        "To calculate exposure, we used ",
        tags$a("the New York Times's published data on COVID-19 cases & deaths", href = urls$nytimes_data_article),
        "to estimate the prevalence of infected people within your county. 
        For the five boroughs in New York City, we use the overall New York City COVID-19 data.",
      ),
      tags$li(
        "Due to rapid spread and insufficient testing during the COVID-19 pandemic, there are likely additional unreported cases beyond the officially reported cases.",
        "We followed methodology reported by", tags$a("Russell et al (2020)", href = urls$russel_etal_2020),
        "to calculate the percentage of cases that are currently known, and presumably quarantined, versus the number of cases still distributed throughout the community."),
      tags$li("Other methods of becoming infected (e.g. touching an infected surface) are not accounted for by this calculator."),
      tags$li(
        "Estimations of the probability of hospitalization, ICU and death among all infected cases, stratified by age groups, were obtained from a Lancet article authored by ",
        tags$a("Verity et al (2020).", href = urls$verity_etal_2020),
        "We chose this study over US CDC reports because this study is larger and more thorough. We do not account for differences between Chinese population and US population."
      ),
      tags$li("Estimations of risk factors associated with underlying medical conditions were obtained from",
              tags$a("China CDC weekly, 2020 Vol No.2", href = urls$ccdc_vol2_2020), "and gender from this preprint by", 
              tags$a("Caramelo et al (2020).", href = urls$caramelo_etal_2020)
      ),
    ),
    tags$p(""),
    tags$p("We'll be doing our best to update these assumptions as additional knowledge about the virus becomes available.")
  )
}