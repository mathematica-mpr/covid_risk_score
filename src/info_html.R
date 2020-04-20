library(shiny)
source("src/global_var.R")

# disclaimer popup
disclaimerpopupHTML <- function(){
  tagList(
    tags$p("This tool works best on Google Chrome and mobile.", class = "text-warning"),
    tags$p("Your use of this tool is subject to these ", tags$a("Terms of Use", href="")),
    tags$p(style="color:#DF691A", "THE INFORMATION PROVIDED BY THIS TOOL IS NOT MEDICAL ADVICE AND CANNOT BE 
             USED TO DIAGNOSE OR TREAT ANY MEDICAL CONDITION.  See FAQ for more information.", class = "text-warning")
  )
}

# render the Methodology
renderMethodsHtml <- function() {
  # return
  tagList(
    tags$p(""),
    tags$p('Our "Risk Score" visualization is the quantity {Exposure * Susceptibility}, normalized by the average disease burden of flu for the average American, logarithmically scaled.'),
    tags$p('In 2018-2019 flu season, US had 35 million cases, 0.5 million hospitalization, and almost 35,000 deaths (',
           tags$a('Source: CDC', href = urls$cdc_flu),
           ').'),
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
        For the five boroughs in New York City, we use the overall New York City COVID-19 data."),
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
      )
    ),
    tags$p(""),
    tags$p("We'll be doing our best to update these assumptions as additional knowledge about the virus becomes available.")
  )
}

# helper function for rendering FAQ's
faqQuestion<- function(string) {
  return (tags$li(tags$b(tags$span(style="color:#DF691A", string))))
}

# render the FAQ's
renderFaqHtml <- function() {
  # return
  tagList(
    tags$h3("Frequently Asked Questions:"),
    faqQuestion("I understand that the information provided by the tool is not medical advice and cannot be 
                used to diagnose or treat any medical condition, so how should I best use the information provided 
                by the tool?"),
    tags$p("This tool provides you with an estimation of your personal susceptibility or risk of contracting 
           COVID 19 based on the information you input into the tool.  We believe that having this information 
           can help you make better decisions when going about your daily activities.  Whatever your personal 
           risk of contracting COVID 19 may be, you should always follow the ", 
           tags$a("CDC’s guidelines", href="https://www.cdc.gov/coronavirus/2019-ncov/communication/guidance-list.html?Sort=Date%3A%3Adesc"), 
           " and any other guidelines provided by your state or local public health officials.  It is also very important to remember that even 
           if your risk is low, following the ",  
           tags$a("CDC’s guidelines", href="https://www.cdc.gov/coronavirus/2019-ncov/communication/guidance-list.html?Sort=Date%3A%3Adesc"), 
           " will help prevent spreading COVID 19 to others."),
    faqQuestion("Why is my score so high?"),
    tags$p("We wanted our tool to be sensitive to the wide variety of circumstances encountered in the US right now;",
           "as a result, it's calibrated around a score of 50. A score of 50 is defined as an equal disease burden as ",
           "the flu, estimated based on total number of flu cases, hospitalizations, ICU admission, and deaths in the ",
           "2018-2019 flu season. For every 10x change in (Exposure*Susceptibility), the score will change by 50/3.",
           "Thus, for two users, one with a score of 20, and one with a score of 70, the user with a score of 70 is",
           "1000x more likely to have been exposed to COVID-19 and experience a serious consequence (hospitalization, ",
           "ICU admission, or death)."),
    faqQuestion("My family is sheltering in place with me. Should I count them as exposure risks?"),
    tags$p("As long as your family has been sheltering in place with you, you should be able to think of your family",
           "as a single \"user\" of the tool. However, bear in mind that their exposure risks become yours, and vice",
           "versa."),
    faqQuestion("My county only has a few (tens, hundreds, thousands) of cases. Why is my exposure risk so high?"),
    tags$p("Probably the most difficult/controversial/inaccurate part of our calculator is our estimation of the",
           "underreporting factor, the factor we use to estimate the true, larger, community prevalence of COVID-19",
           "in your community. In some places, our tool may be overestimating this factor, and in some places, it",
           "may be underestimating. Even so, it's probably good enough to get you a ballpark estimate of your risk."),
    tags$p("If your community has seen a huge increase in testing, has a \"test positive\" rate < 5%, and if you",
           "feel like anyone that wants to be tested is being tested promptly, then I think there is reason to",
           "believe that the authorities are tracking most of the community cases of COVID-19 in your area.",
           "Unfortunately, that is not true of most of the US at present."),
    faqQuestion("My specific medical condition isn't listed. What do I do?"),
    tags$p("Try using \"other conditions\" to get a catch-all estimate of your susceptibility."),
    faqQuestion("My hospitalization/ICU/death risk seems out of whack."),
    tags$p("A lot is still unknown about the disease, and data sets are sparse, so our susceptibility scores are",
           "good for ballpark estimates only. We'll update our tool with better numbers as they become available."),
    faqQuestion("I have suggestion X, or know of data set Y, or want feature Z..."),
    tags$p("Let us know at", tags$a("covid.risk.score@gmail.com", href="mailto:covid.risk.score@gmail.com"), 
    "or visit us on ", tags$a("GitHub", href="https://github.com/mathematica-mpr/covid_risk_score"))
  )
}
