library(shiny)
library(markdown)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  headerPanel("Bayes’s Rule and Disease Testing"),
  sidebarPanel(
    numericInput("p_D", "Prevalence of a disease (or prior probability):", 0.01, min = 0, max = 1, step = 0.01),
    numericInput("sensitivity", "Test's Sensitivity:", 0.95, min = 0, max = 1, step = 0.01),
    numericInput("specificity", "Test's Specificity:", 0.95, min = 0, max = 1, step = 0.01),
    # selectInput("test_result", "Test result", c("Positive", "Negative"), selected = "Positive"),
    # selectInput("statistic", "Check for:", c("Has a disease", "Doesn't have a disease"), selected = "Has a disease"),
    # sliderInput("p_D", "Prevalence of a disease (or prior probability):", min = 0, max = 1, value = 0.01, step = 0.01),
    # sliderInput("sensitivity", "Sensitivity:", min = 0, max = 1, value = 0.95, step = 0.01),
    # sliderInput("specificity", "Specificity:", min = 0, max = 1, value = 0.95, step = 0.01),
    radioButtons("test_result", "Test result:",
                 choices = list("Positive", "Negative"),selected = "Positive"),
    radioButtons("statistic", "Check for:",
                 choices = list("Has a disease", "Doesn't have a disease"),selected = "Has a disease"),
    helpText("Probabilities are  presented in decimal format. For example, 1% = 0.01, 25.6% = 0.256 and so on."), 
    helpText(a(href="https://github.com/ruslan-kl/shiny-apps/tree/master/bayes", target="_blank", "Code at GitHub"))
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Results", htmlOutput("calc_proba")),
                tabPanel("Probability Tree", plotOutput("graphPlot"))
                # tabPanel("Help & Info", withMathJax(includeMarkdown("overview.html")))
                )
  )
)
)
