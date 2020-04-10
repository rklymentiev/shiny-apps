library(plotly)

# 
# function(input, output) {
#     
#     x <- reactive({
#         mtcars[,input$xcol]
#     })
#     
#     y <- reactive({
#         mtcars[,input$ycol]
#     })
#     
#     
#     output$plot <- renderPlotly(
#         plot1 <- plot_ly(
#             x = x(),
#             y = y(), 
#             type = 'scatter',
#             mode = 'markers')
#     )
#     
# }
