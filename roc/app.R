library(shiny)
library(plotly)
library(ROCit)
library(dplyr)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme("flatly"),

    titlePanel("ROC Curve Simulation"),

    sidebarPanel(
        sliderInput("n", "Number of Observations (n):", min = 5, max = 25, value = 8, step = 1),
        sliderInput("threshold", "Threshold:", min = 0, max = 1, value = 0.5, step = 0.01),
        helpText("If predicted probability of class 'Yes' is greater or equal to the threshold value,
                 then the predicted class will be  defined as 'Yes'."),
        helpText("Set of random observations is generated each time the number n is changed."),
        helpText("Use threshold slider to find the best values of TPR and FPR. Correctly predicted observations
        are painted in green, incorrectly predicted are painted in red. The point corresponding to that
                 treshold value is plotted in red on the ROC plot."),
        br(),
        helpText(HTML("<b>Created by <a href='https://github.com/ruslan-kl'>Ruslan Klymentiev</a></b>")),
        helpText(a(href="https://github.com/ruslan-kl/shiny-apps/tree/master/roc", target="_blank", "Code at GitHub")),
        width = 3),

    mainPanel(
        # set the tabs
        tabsetPanel(type = "tabs",
                    tabPanel("Simulation", 
                             plotlyOutput('barChart'),
                             hr(), # line
                             # set the grid layout
                             fluidRow(
                                 fluidRow(column(5, tableOutput('ConfMatrix'),
                                                 fluidRow(column(12, htmlOutput("perf")))),
                                          column(width = 7, plotlyOutput('ROCplot'))))
                             ),
                    tabPanel("Info", "Coming soon.."))
        )
    )


server <- function(input, output) {

    observeEvent(input$n, {
        n <- as.integer(input$n)
        
        actual_probs <- rbinom(n, 1, 0.5)
        
        actual_class <- list(labels = rep("No", n),
                             values = actual_probs)
        actual_class$labels[actual_class$values == 1] <- "Yes"
        actual_class$plot_labels <- paste(1:n, actual_class$labels, sep = ": ")
        
        predicted_class <- list(probs = NA,
                                labels = rep("No", n),
                                values = rep(0, n))
        predicted_class$probs[actual_class$values == 1] <- round(runif(sum(actual_class$values == 1), 0.25, 0.95), 2)
        predicted_class$probs[actual_class$values != 1] <- round(runif(sum(actual_class$values != 1), 0.05, 0.75), 2)
        
        observeEvent(input$threshold, {
            threshold <- as.numeric(input$threshold)

            predicted_class$labels[predicted_class$probs >= threshold] <- "Yes"
            predicted_class$values[predicted_class$probs >= threshold] <- 1

            colors <- rep("rgba(120,255,120,0.9)", n)
            colors[predicted_class$values != actual_class$values] <- "rgba(255,120,120,0.9)"
            
            output$barChart <- renderPlotly(
                fig <- plot_ly() %>%
                    add_trace(
                        x = actual_class$plot_labels, y = predicted_class$probs,
                        type = 'bar', name = "Pred. Proba",
                        marker = list(color = colors),
                        text = predicted_class$labels,
                        hovertemplate = paste('<b>Actual Class</b>: %{x}',
                                              '<br><b>Probability of Yes Class</b>: %{y}',
                                              '<br><b>Predicted Class</b>: %{text}')) %>%
                    add_trace(
                        x = actual_class$plot_labels, y = predicted_class$probs, type = 'scatter',
                        mode = 'text', text = paste0("<b>", predicted_class$labels, "</b>"),
                        textposition = 'middle top',
                        name = "Pred. Class", textfont = list(color = 'black', size = 16)) %>%
                    add_trace(
                        x = c(actual_class$plot_labels[1], actual_class$plot_labels[n]),
                        y = c(threshold, threshold),
                        name = "Threshold", type = 'scatter', mode = 'lines',
                        line = list(color = 'black', width = 2, dash = 'dot')) %>%
                    layout(
                        showlegend = FALSE,
                        margin = list(t = 75),
                        title = "<b>Predicted Positive Class Probability</b>",
                        xaxis = list(title = "Actual Class",
                                     categoryorder = "array",
                                     categoryarray = 1:n),
                        yaxis = list(title = "Predicted Probabilities<br>(for Class 'Yes')",
                                     range = c(0, 1.1)))
            )
            
            conf_matrix <- as.data.frame.matrix(table(predicted_class$labels, actual_class$labels))
            colnames(conf_matrix) <- paste0("Actual: ", colnames(conf_matrix))
            rownames(conf_matrix) <- paste0("Predicted: ", rownames(conf_matrix))
            
            
            `%notin%` <- Negate(`%in%`)

            if ("Predicted: No" %notin% rownames(conf_matrix)) {
                temp <- data.frame(0, 0)
                rownames(temp) <- "Predicted: No"
                colnames(temp) <- colnames(conf_matrix)
                conf_matrix <- rbind(conf_matrix, temp)
            } else if ("Predicted: Yes" %notin% rownames(conf_matrix)) {
                temp <- data.frame(0, 0)
                rownames(temp) <- "Predicted: Yes"
                colnames(temp) <- colnames(conf_matrix)
                conf_matrix <- rbind(conf_matrix, temp)
            } else if ("Actual: Yes" %notin% colnames(conf_matrix)) {
                conf_matrix$`Actual: Yes` <- c(0,0)
            } else if ("Actual: No" %notin% colnames(conf_matrix)) {
                conf_matrix$`Actual: No` <- c(0,0)
            }
            
            conf_matrix$Predicted <- rownames(conf_matrix)
            conf_matrix <- select(conf_matrix, Predicted, "Actual: No", "Actual: Yes")
            conf_matrix$`Actual: No` <- as.integer(conf_matrix$`Actual: No`)
            conf_matrix$`Actual: Yes` <- as.integer(conf_matrix$`Actual: Yes`)
            conf_matrix <- arrange(conf_matrix, Predicted)

            tpr <- conf_matrix$`Actual: Yes`[2] / sum(conf_matrix$`Actual: Yes`)
            fpr <- conf_matrix$`Actual: No`[2] / sum(conf_matrix$`Actual: No`)
            
            output$perf <- renderText(paste0("<b>False Positive Rate</b> = ", round(fpr, 3), 
                                             "<br><b>True Positive Rate</b> = ", round(tpr, 3)))
            
            ROCit_obj <- rocit(score = predicted_class$probs, class = actual_class$values)
            
            output$ConfMatrix <- renderTable(conf_matrix, caption = "<b>Confusion Matrix</b>",
                                             caption.placement = getOption("xtable.caption.placement", "top"))
            
            output$ROCplot <- renderPlotly(
                plot_ly() %>%
                    add_lines(x = round(ROCit_obj$FPR,2), y = round(ROCit_obj$TPR,2),
                              fill = 'tozeroy', name = "ROC", text = ROCit_obj$Cutoff,
                              hovertemplate = paste('<b>FPR</b>: %{x}',
                                                    '<br><b>TPR</b>: %{y}',
                                                    '<br><b>Threshold</b>: %{text}')) %>%
                    add_text(x = 0.75, y = 0.25, 
                             text = paste0("<b>AUC = ", as.character(round(ROCit_obj$AUC, 2)), "</b>"),
                             textfont = list(color = 'black', size = 16)) %>% 
                    add_lines(x = c(0, 1), y = c(0, 1), name = "Baseline",
                              line = list(color = 'red', width = 2, dash = 'dot')) %>%
                    add_markers(x = round(fpr,2), y = round(tpr,2), name = "",
                                marker = list(size = 10, color = "red")) %>%
                    layout(title = "<b>ROC Curve</b>",
                           xaxis = list(title = "FPR", range = c(-0.1, 1.1)),
                           yaxis = list(title = "TPR", range = c(-0.1, 1.1)),
                           margin = list(t = 100),
                           showlegend = FALSE)
            )
        }
        )
    })
    


}


# Run the application 
shinyApp(ui = ui, server = server)



