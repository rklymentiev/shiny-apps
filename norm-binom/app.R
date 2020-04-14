library(shiny)
library(shinythemes)
library(ggplot2)


define_sign <- function(area, type) {
    if (area == "lower" & type == "strict") {
        sign <- "<="
    } else if (area == "lower" & type == "not_strict") {
        sign <- "<= 0.5 +"
    } else if (area == "upper" & type == "strict") {
        sign <- ">="
    } else {sign <- ">= -0.5 +"}
    return(sign)
}

check_conditions <- function(n, p) {
    if (n*p >= 10 & n*(1-p) >= 10) {
        return(TRUE)
        } else {return(FALSE)}
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),

    # Application title
    titlePanel("Normal Approximation to Binomial Distrtibution"),

    # Sidebar with a slider input for number of bins 
    sidebarPanel(
        sliderInput(
            inputId = "p", label = "Probability of Success (p)",
            min = 0, max = 1, value = 0.5, step = 0.01),
        sliderInput(
            inputId = "n", label = "Number of Trials (n)",
            value = 50, min = 1, max = 300),
        uiOutput("k"),
        selectInput(
            inputId = "area",
            label = "Area of Interest:",
            choices = c("Lower" = "lower",
                        "Upper" = "upper"),
            selected = "lower"),
        selectInput(
            inputId = "type",
            label = "Type:",   
            choices = c("Strict (<, >)" = "strict",
                        "Not strict (≤, ≥)" = "not_strict"),
            selected = "strict"),
        HTML("<b>Shade Area:</b>"),
        checkboxInput("shade_curve", "Under the Normal Curve", value = TRUE),
        checkboxInput("shade_bar", "On the Bar Chart", value = FALSE)
        
    ),

    # Show a plot of the generated distribution
    mainPanel(
        htmlOutput("conditions"),
        br(),
        plotOutput("distplot"),
        htmlOutput("results"))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$k <- renderUI({
        n <- input$n
        sliderInput(
            inputId = "k", label = "Number of Successes (k)",
            value = floor(n/2), min = 0, max = n, step = 1)
        })
    
    
    output$conditions <- renderText({
        p <- input$p
        n <- input$n
        
        flag <- check_conditions(n, p)
        if (flag) {
            text <- "Conditions are met:"
        } else {text <- "Conditions are <b>not</b> met:"}
        
        paste0(text, "<br><i>np = ", n*p, "<br>n(1-p) = ", n*(1-p))
    })
    
    
    output$distplot <- renderPlot({
        p <- input$p
        n <- input$n
        k <- as.integer(input$k)
        area <- input$area
        type <- input$type

        x <- as.integer(1:n)
        x_norm <- seq(min(x), max(x), by = 0.1)
        binom_dist <- dbinom(x, size = n, prob = p)
        norm_dist <- dnorm(x = x_norm, mean = n*p, sd = sqrt(n*p*(1-p)))
        
        sign <- define_sign(area, type)
        cndtn_for_area <- eval(parse(text = paste("x_norm",sign,"k")))
        
        shade_curve <- input$shade_curve
        shade_bar <- input$shade_bar
        
        fill <- rep("not_selected", n)
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
                    mapping = aes(x = x, y = binom_dist),
                    stat="identity", fill = "lightblue", color = "black") +
                labs(
                    x = "x",
                    y = "Probability") +
                theme_bw()
            }
    })

    output$results <- renderText({
        n <- input$n
        p <- input$p
        k <- input$k
        area <- input$area
        type <- input$type
        flag <- check_conditions(n, p)
        
        
        
        
        if (!is.null(k)) {
            if (area == "lower" & type == "strict") {

                p_binom <- sum(dbinom(0:k-1, size = n, prob = p))
                p_norm <- pnorm(q = k, mean = n*p, sd = sqrt(n*p*(1-p)))
                sign <- "<"
            } else if (area == "lower" & type == "not_strict") {
                p_binom <- sum(dbinom(0:k, size = n, prob = p))
                p_norm <- pnorm(q = k+0.5, mean = n*p, sd = sqrt(n*p*(1-p)))
                sign <- "≤"
            } else if (area == "upper" & type == "strict") {
                p_binom <- sum(dbinom(k+1:n, size = n, prob = p))
                p_norm <- 1 - pnorm(q = k, mean = n*p, sd = sqrt(n*p*(1-p)))
                sign <- ">"
            } else {
                p_binom <- sum(dbinom(k:n, size = n, prob = p))
                p_norm <- 1 - pnorm(q = k+0.5, mean = n*p, sd = sqrt(n*p*(1-p)))
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


shinyApp(ui = ui, server = server)
