options(repos = BiocManager::repositories())

library(shiny)
library(Rgraphviz)
options("scipen" = 5)
options(shiny.deprecation.messages=FALSE)


function(input, output, session) {
    
    observeEvent(input$p_D, { 
        updateNumericInput(session, "p_D", value = ({ if(input$p_D < 0 | !is.numeric(input$p_D)) {0}
            else if(input$p_D > 1) {1}
            else {input$p_D}
            })
        )
    })
    
    observeEvent(input$sensitivity, { 
        updateNumericInput(session, "sensitivity", value = ({ if(input$sensitivity < 0 | !is.numeric(input$sensitivity)) {0}
            else if(input$sensitivity > 1) {1}
            else {input$sensitivity}
        })
        )
    })
    
    observeEvent(input$specificity, { 
        updateNumericInput(session, "specificity", value = ({ if(input$specificity < 0 | !is.numeric(input$specificity)) {0}
            else if(input$specificity > 1) {1}
            else {input$specificity}
        })
        )
    })
    
    output$graphPlot <- reactivePlot(function() {
        # input parameters
        p_D <- input$p_D
        p_neg_given_noD <- input$specificity
        p_pos_given_D <- input$sensitivity
        
        # calculations for probability tree
        p_noD <- 1 - input$p_D
        p_pos_given_noD = round(1 - p_neg_given_noD, 4)
        p_neg_given_D = round(1 - p_pos_given_D, 4)
        p_neg_and_D <- p_D * p_neg_given_D
        p_pos_and_D <- p_D * p_pos_given_D
        p_neg_and_noD <- p_noD * p_neg_given_noD
        p_pos_and_noD <- p_noD * p_pos_given_noD
        
        
        # specify the nodes names
        node0 <- "Subject"
        node1 <- "Disease"
        node2 <- "no Disease"
        node3 <- "'+' given Disease"
        node4 <- "'-' given Disease"
        node5 <- "'+' given no Disease"
        node6 <- "'-' given no Disease"
        node7 <- "'+' & Disease"
        node8 <- "'-' & Disease"
        node9 <- "'+' & no Disease"
        node10 <- "'-' & no Disease"
        nodeNames <- c(node0, node1, node2, node3, node4, node5, node6, node7, node8, node9, node10)
        
        # create new graph object
        rEG <- new("graphNEL", nodes=nodeNames, edgemode="directed")
        
        rEG <- addEdge(node0, node1, rEG)
        rEG <- addEdge(node0, node2, rEG)
        rEG <- addEdge(node1, node3, rEG)
        rEG <- addEdge(node1, node4, rEG)
        rEG <- addEdge(node2, node5, rEG)
        rEG <- addEdge(node2, node6, rEG)
        rEG <- addEdge(node3, node7, rEG)
        rEG <- addEdge(node4, node8, rEG)
        rEG <- addEdge(node5, node9, rEG)
        rEG <- addEdge(node6, node10, rEG)
        
        # specify the attributes of the object (color, shape, etc)
        at <- list(node=list(label = "foo", fillcolor="lightgreen", 
                             fontsize=15, shape="ellipse", fixedsize=FALSE), 
                   edge=list(color="red"), 
                   graph=list(rankdir="LR"))
        
        
        plot(rEG, attrs = at)
        
        text(100, 450, paste0("P(", node1, ")=", p_D), col="black")
        text(100, 300, paste0("P(", node2, ")=", p_noD), col="black")
        
        text(360, 650, paste0("P(", node3, ")=", p_pos_given_D), col="black")
        text(360, 450, paste0("P(", node4, ")=", p_neg_given_D), col="black")
        
        text(680, 650, paste0("P(", node7, ")=", p_pos_and_D), col="black")
        text(680, 450, paste0("P(", node8, ")=", p_neg_and_D), col="black")
        
        text(360, 300, paste0("P(", node5, ")=", p_pos_given_noD), col="black")
        text(360, 110, paste0("P(", node6, ")=", p_neg_given_noD), col="black")
        
        text(680, 300, paste0("P(", node9, ")=", p_pos_and_noD), col="black")
        text(680, 110, paste0("P(", node10, ")=", p_neg_and_noD), col="black")
    }, width = 950, height = 750)

    output$calc_proba <- renderText({

        # input parameters
        p_D <- input$p_D
        p_neg_given_noD <- input$specificity
        p_pos_given_D <- input$sensitivity
        statistic = input$statistic
        test_result = input$test_result

        # calculations for probability tree
        p_noD <- 1 - p_D
        p_pos_given_noD = round(1 - p_neg_given_noD, 4)
        p_neg_given_D = round(1 - p_pos_given_D, 4)
        p_neg_and_D <- p_D * p_neg_given_D
        p_pos_and_D <- p_D * p_pos_given_D
        p_neg_and_noD <- p_noD * p_neg_given_noD
        p_pos_and_noD <- p_noD * p_pos_given_noD
        
        p_D_given_neg <- p_neg_and_D / (p_neg_and_D + p_neg_and_noD)
        p_D_given_pos <- p_pos_and_D / (p_pos_and_D + p_pos_and_noD)
        p_noD_given_neg <- p_neg_and_noD / (p_neg_and_noD + p_neg_and_D)
        p_noD_given_pos <- p_pos_and_noD / (p_pos_and_D + p_pos_and_noD)

        if (statistic == "Has a disease" & test_result == "Negative") {
            p <- p_D_given_neg
        } else if (statistic == "Has a disease" & test_result == "Positive") {
            p <- p_D_given_pos
        } else if (statistic == "Doesn't have a disease" & test_result == "Negative") {
            p <- p_noD_given_neg
        } else {
            p <- p_noD_given_pos
        }
        
        prior_odds <- p_D/p_noD
        if (test_result == "Negative") {
            posterior_odds <- p_D_given_neg / p_noD_given_neg
        } else {posterior_odds <- p_D_given_pos / p_noD_given_pos}
        
        bayes_factor <- posterior_odds / prior_odds
        
        # if (bayes_factor > 100) {
        #     bayes_interpret <- "Decisive evidence for H_1"
        # } else if (bayes_factor > 30) {
        #     bayes_interpret <- "Very strong evidence for H_1"
        # } 
        # bayes_interpret <- "No"

        paste0("<br>Probability that subject <B>", tolower(statistic), "</B> given that the test result was <B>",
               tolower(test_result), "</B> is: ", round(p, 4), " ≈ ", round(p, 3) * 100, "%",
               "<br><br>",
               "<header><h3>Hypothesis Testing</h3></header>",
               "<br><b>Hypothesis 1 (i)</b>: Subject has a disease. P(H_1) = ", p_D,
               "<br><b>Hypothesis 2 (j)</b>: Subject doesn't have a disease. P(H_2) = ", p_noD,
               "<br><br><b>Bayes Factor</b> = ", round(bayes_factor, 1),
               "<br><b>ln(Bayes Factor)</b> = ", round(log(bayes_factor), 2),
               "<center><br><img src='https://www.researchgate.net/publication/320376135/figure/tbl1/AS:614127873568776@1523430903196/The-Evidence-Categories-for-the-Bayes-Factor-BF-ij-as-given-by-Jeffreys-1961.png' width=500></img></center>")
        
    })
    
    # output$info <- renderText({paste0("<b>Prevalence</b> in epidemiology is the proportion 
    #                       of a particular population found to be affected by a medical condition.")})
    
}
