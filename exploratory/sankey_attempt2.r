# Sankey in plotly

library(plotly)
library(rjson)

json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

p <- plot_ly(
  type = "sankey",
  domain = list(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh"
) %>%
  layout(
    title = "Energy forecast for 2050, UK - Department of Energy & Climate Change",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F)
  )
