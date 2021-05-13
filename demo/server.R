library(plotly)


function(input, output) {
  
    x <- reactive({
        mtcars[,input$xcol]
    })

    y <- reactive({
        mtcars[,input$ycol]
    })
    
    n <- eventReactive(input$go,{
      x()
      y()
    })


    output$plot <- renderPlotly(
        plot1 <- plot_ly(
            x = n(),
            y = y(),
            type = 'scatter',
            mode = 'markers')
    )

}
