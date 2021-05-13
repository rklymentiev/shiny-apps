# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
library(plotly)

fluidPage(
  headerPanel('Example'),
  sidebarPanel(
    actionButton("go", "Go"),
    selectInput('xcol','X Variable', names(mtcars)),
    selectInput('ycol','Y Variable', names(mtcars)),
    selected = names(mtcars)[[2]]),
  mainPanel(
    plotlyOutput('plot')
  )
)