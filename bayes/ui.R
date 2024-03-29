library(shiny)
library(markdown)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  headerPanel("Bayes Rule and Disease Testing"),
  sidebarPanel(
    # numericInput("p_D", "Prevalence of a disease (or prior probability):", 0.01, min = 0, max = 1, step = 0.01),
    # numericInput("sensitivity", "Test's Sensitivity:", 0.95, min = 0, max = 1, step = 0.01),
    # numericInput("specificity", "Test's Specificity:", 0.95, min = 0, max = 1, step = 0.01),
    # selectInput("test_result", "Test result", c("Positive", "Negative"), selected = "Positive"),
    # selectInput("statistic", "Check for:", c("Has a disease", "Doesn't have a disease"), selected = "Has a disease"),
    sliderInput("p_D", "Prevalence of a disease (or prior probability):", min = 0, max = 1, value = 0.01, step = 0.01),
    sliderInput("sensitivity", "Test's Sensitivity:", min = 0.5, max = 1, value = 0.95, step = 0.01),
    sliderInput("specificity", "Test's Specificity:", min = 0.5, max = 1, value = 0.95, step = 0.01),
    radioButtons("test_result", "Test result:",
                 choices = list("Positive", "Negative"),selected = "Positive"),
    radioButtons("statistic", "Check for:",
                 choices = list("Has a disease", "Doesn't have a disease"),selected = "Has a disease"),
    helpText("Probabilities are  presented in decimal format. For example, 1% = 0.01, 25.6% = 0.256 and so on."), 
    br(),
    helpText(HTML("<b>Created by </b> <a href='https://github.com/rklymentiev'>Ruslan Klymentiev</a>")),
    helpText(a(href="https://github.com/rklymentiev/shiny-apps/tree/master/bayes", target="_blank", "Code at GitHub")),
    width = 3),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Results", htmlOutput("calc_proba")),
                tabPanel("Probability Tree", plotOutput("graphPlot"))
                # tabPanel("Help & Info", withMathJax(includeMarkdown("overview.html")))
                )
  )
)
)
