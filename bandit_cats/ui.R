library(shiny)

shinyUI(fluidPage(
    theme = shinythemes::shinytheme("journal"),

    titlePanel("Bandit Cats and Exploration-Exploitation Trade-Off"),

    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "n", label = "Number of trials (n):",
                min = 100, max = 1000, value = 100, step = 50),
            sliderInput(
                inputId = "rate_A", label = "Probability of Reward A:",
                min = 0, max = 1, value = 0.2, step = 0.01),
            sliderInput(
                inputId = "rate_B", label = "Probability of Reward B:",
                min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput(
                inputId = "rate_C", label = "Probability of Reward C:",
                min = 0, max = 1, value = 0.8, step = 0.01),
            sliderInput(
                inputId = "epsilon", label = "Epsilon Greedy Value:",
                min = 0, max = 1, value = 0.05, step = 0.01),
            checkboxInput("seed", "Set the Random Seed", value = FALSE),
            actionButton("go", "Go!"),
            br(),
            br(),
            helpText(HTML("<b>Created by <a href='https://github.com/ruslan-kl'>Ruslan Klymentiev</a></b>")),
            helpText(a(href="https://github.com/ruslan-kl/shiny-apps/tree/master/bandit_cats", target="_blank", "Code at GitHub")),
            width = 3
            
        ),

        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Game",
                    includeHTML("machines.html")),
                tabPanel(
                    "Players",
                    br(),
                    includeHTML("cats.html")),
                tabPanel(
                    "Results",
                    br(),
                    htmlOutput("helper_text"),
                    plotOutput("picks_plot", width = 650, height = 500),
                    br(),
                    plotOutput("reward_plot", width = 600, height = 500)),
                tabPanel(
                    "Animated Results",
                    br(),
                    includeHTML("animations.html")),
                tabPanel(
                    "References",
                    br(),
                    includeHTML("refs.html")))
        )
    )
))
