library(shiny)
library(shinythemes)
library(ggplot2)
library(markdown)

#### HELPER FUNCTIONS ####

define_sign <- function(area, type, correction) {
    if (area == "lower" & type == "strict") {
        sign <- paste0("<= ", -correction/2, " +")
    } else if (area == "lower" & type == "not_strict") {
        sign <- paste0("<= ", correction/2, " +")
    } else if (area == "upper" & type == "strict") {
        sign <- paste0(">= ", correction/2, " +")
    } else {sign <- paste0(">= ", -correction/2, " +")}
    return(sign)
}

check_conditions <- function(n, p) {
    # check if conditions for approximations are met
    if (n*p >= 10 & n*(1-p) >= 10) {
        return(TRUE)
        } else {return(FALSE)}
}

#### UI ####
ui <- fluidPage(
    
    theme = shinytheme("flatly"),

    headerPanel("Normal Approximation to Binomial Distribution"),

    sidebarPanel(
        sliderInput(
            inputId = "p", label = "Probability of Success (p):",
            min = 0, max = 1, value = 0.5, step = 0.01),
        sliderInput(
            inputId = "n", label = "Number of Trials (n):",
            value = 50, min = 1, max = 300),
        uiOutput("k"),
        selectInput(
            inputId = "area",
            label = "Area of Interest:",
            choices = c("Lower Tail" = "lower",
                        "Upper Tail" = "upper"),
            selected = "lower"),
        selectInput(
            inputId = "type",
            label = "Type:",   
            choices = c("Strict (<, >)" = "strict",
                        "Not strict (≤, ≥)" = "not_strict"),
            selected = "strict"),
        checkboxInput("correction", HTML("<b>Use correction of 0.5</b>"), value = TRUE),
        HTML("<b>Shade Area:</b>"),
        checkboxInput("shade_curve", "Under the Normal Curve", value = TRUE),
        checkboxInput("shade_bar", "On the Bar Chart", value = FALSE),
        br(),
        helpText(HTML("<b>Created by <a href='https://github.com/ruslan-kl'>Ruslan Klymentiev</a></b>")),
        helpText(a(href="https://github.com/ruslan-kl/shiny-apps/tree/master/norm-binom", target="_blank", "Code at GitHub")),
        width = 4
        
    ),

    mainPanel(
        tabsetPanel(
            type = "tabs",
            tabPanel("Results",
                     htmlOutput("conditions"),
                     br(),
                     plotOutput("distplot"),
                     htmlOutput("results")),
            tabPanel("Info", withMathJax(includeMarkdown("info.html")))
            )
        )
)

#### SERVER ####
server <- function(input, output) {

    #### RENDER INPUT VALUES ####
    output$k <- renderUI({
        n <- input$n
        sliderInput(
            inputId = "k", label = "Number of Successes (k):",
            value = floor(n/2), min = 0, max = n, step = 1)
        })
    
    #### CONDITIONS CHECK ####
    output$conditions <- renderText({
        p <- input$p
        n <- input$n
        
        flag <- check_conditions(n, p)
        if (flag) {
            text <- "Conditions are met:"
        } else {text <- "Conditions are <b>not</b> met:"}
        
        paste0(text, "<br><i>np = ", n*p, "<br>n(1-p) = ", n*(1-p))
    })
    
    #### DISTRITBUTION PLOT ####
    output$distplot <- renderPlot({
        p <- input$p
        n <- input$n
        k <- as.integer(input$k)
        area <- input$area
        type <- input$type
        correction <- input$correction

        x <- as.integer(0:n)
        x_norm <- seq(min(x), max(x), by = 0.1)
        binom_dist <- dbinom(x, size = n, prob = p)
        norm_dist <- dnorm(x = x_norm, mean = n*p, sd = sqrt(n*p*(1-p)))
        
        sign <- define_sign(area, type, correction)
        cndtn_for_area <- eval(parse(text = paste("x_norm",sign,"k")))
        
        shade_curve <- input$shade_curve
        shade_bar <- input$shade_bar
        
        fill <- rep("not_selected", n+1)
        if(shade_bar) {
            if(!is.null(k)) {
                if(area == "lower" & type == "strict") {
                    fill[x < k] <- "selected"
                } else if(area == "lower" & type == "not_strict") {
                    fill[x <= k] <- "selected"
                } else if(area == "upper" & type == "strict") {
                    fill[x > k] <- "selected"
                } else {
                    fill[x >= k] <- "selected"
                }
            }
        }
        
        if(shade_curve) {
            alpha <- 0.5
        } else {alpha <- 0}
        
        if (n*p >= 10 & n*(1-p) >= 10) {
            ggplot() +
                geom_bar(
                    mapping = aes(x = x, y = binom_dist, fill = fill),
                    stat="identity", color = "black") +
                geom_line(
                    mapping = aes(x = x_norm, y = norm_dist),
                    color = "red", size = 1) +
                geom_area(
                    mapping = aes(x = x_norm[cndtn_for_area],
                                  y = norm_dist[cndtn_for_area]),
                    fill="red", colour=NA, alpha=alpha) +
                labs(
                    x = "x",
                    y = "Probability") +
                theme_bw() +
                theme(legend.position = "none") +
                scale_fill_manual(values = c("not_selected" = "lightblue",
                                             "selected" = "blue"))
        } else {
            ggplot() +
                geom_bar(
                    mapping = aes(x = x, y = binom_dist, fill = fill),
                    stat="identity", color = "black") +
                labs(
                    x = "x",
                    y = "Probability") +
                theme_bw() +
                theme(legend.position = "none") +
                scale_fill_manual(values = c("not_selected" = "lightblue",
                                             "selected" = "blue"))
            }
    })

    #### CALCULATIONS ####
    output$results <- renderText({
        n <- input$n
        p <- input$p
        k <- input$k
        area <- input$area
        type <- input$type
        flag <- check_conditions(n, p)
        correction <- input$correction
        
        if (!is.null(k)) {
            if (area == "lower" & type == "strict") {
                p_binom <- pbinom(k-1, size = n, prob = p)
                p_norm <- pnorm(q = k-correction/2, mean = n*p, sd = sqrt(n*p*(1-p)))
                sign <- "<"
            } else if (area == "lower" & type == "not_strict") {
                p_binom <- pbinom(k, size = n, prob = p)
                p_norm <- pnorm(q = k+correction/2, mean = n*p, sd = sqrt(n*p*(1-p)))
                sign <- "≤"
            } else if (area == "upper" & type == "strict") {
                p_binom <- 1 - pbinom(k, size = n, prob = p)
                p_norm <- 1 - pnorm(q = k+correction/2, mean = n*p, sd = sqrt(n*p*(1-p)))
                sign <- ">"
            } else {
                p_binom <- 1 - pbinom(k-1, size = n, prob = p)
                p_norm <- 1 - pnorm(q = k-correction/2, mean = n*p, sd = sqrt(n*p*(1-p)))
                sign <- "≥"
            }
            
            binom_results <- paste0("P(X", sign, k, ") = ", round(p_binom, 4))
            norm_results <- paste0("P(X", sign, k, ") = ", round(p_norm, 4))
            
            if(flag) {
                paste0("<center><b>Binomial Distribution</b>: ", binom_results, 
                       "<br><b>Normal Distribution</b>: ",norm_results, "</center>")
                } else {paste0("<center><b>Binomial Distribution</b>: ", binom_results, "</center>")}
            }
        
        })
}

#### RUN ####
shinyApp(ui = ui, server = server)
