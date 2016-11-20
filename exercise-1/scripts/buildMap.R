# BuildMap file: write function that returns a map

# This function requires plotly
library(plotly)

# BuildMap function: fill this in with a function that returns a map:
# Try parameterize a few options, such as the title
# I suggest: https://plot.ly/r/bubble-maps/
BuildMap <- function(data, title) {
  
  data$q <- with(data, cut(pop, quantile(pop)))
  levels(data$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
  data$q <- as.ordered(data$q)
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray85"),
    subunitwidth = 1,
    countrywidth = 1,
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white")
  )
  
  p <- plot_geo(data, locationmode = 'USA-states', sizes = c(1, 250)) %>%
    add_markers(
      x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
      text = ~paste(data$name, "<br />", data$pop/1e6, " million")
    ) %>%
    layout(title = 'title', geo = g)
}