
# =============================================================================
# Ecología de la herpetofauna en el ambiente R para no programadores
# =============================================================================
# GRÁFICOS INTERACTIVOS
# =============================================================================
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/

# Interative plots --------------------------------------------------------

# https://r-graph-gallery.com/414-map-multiple-charts-in-ggiraph.html


# Instalation -------------------------------------------------------------

# install.packages("ggiraph")
library("ggiraph")

# install.packages("tidyverse")
library("tidyverse")

# install.packages("patchwork")
library("patchwork")

# install.packages("sf")
library("sf")

# install.packages("leaflet")
library("leaflet")


# Basic usage -------------------------------------------------------------

mtcars_db <- tibble::rownames_to_column(mtcars, var = "carname")


# geom_point_interactive()


myplot <- ggplot(
  data = mtcars_db,
  mapping = aes(
    x = disp, y = qsec,
    # here we add interactive aesthetics
    tooltip = carname, data_id = carname
  )
) +
  geom_point_interactive(
    size = 3, hover_nearest = TRUE
  )

interactive_plot <- girafe(ggobj = myplot)

htmltools::save_html(interactive_plot, "ggiraph-2.html")



# Combine 2 charts --------------------------------------------------------


mtcars_db <- tibble::rownames_to_column(mtcars, var = "carname")

# First plot: Scatter plot
scatter <- ggplot(
  data = mtcars_db,
  mapping = aes(
    x = disp, 
    y = qsec,
    tooltip = carname, 
    data_id = carname
  )
) +
  geom_point_interactive(
    size = 3, 
    hover_nearest = TRUE
  ) +
  labs(
    title = "Displacement vs Quarter Mile",
    x = "Displacement", 
    y = "Quarter Mile"
  ) +
  theme_bw()

# Second plot: Bar plot
bar <- ggplot(
  data = mtcars_db,
  mapping = aes(
    x = reorder(carname, mpg), 
    y = mpg,
    tooltip = paste("Car:", carname, "<br>MPG:", mpg),
    data_id = carname
  )
) +
  geom_col_interactive(fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Miles per Gallon by Car",
    x = "Car", y = "Miles per Gallon"
  ) +
  theme_bw()

# Combine the plots using patchwork
combined_plot <- scatter + bar +
  plot_layout(ncol = 2)

# Create a single interactive plot with both subplots
interactive_plot <- girafe(ggobj = combined_plot)

# Set options for the interactive plot
interactive_plot <- girafe_options(
  interactive_plot,
  opts_hover(css = "fill:cyan;stroke:black;cursor:pointer;"),
  opts_selection(type = "single", css = "fill:red;stroke:black;")
)

htmltools::save_html(interactive_plot, "ggiraph-3.html")



# Chart and map -----------------------------------------------------------


## Creating dataset --------------------------------------------------------

# Read the full world map
world_sf <- sf::read_sf("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/world.geojson")
world_sf <- world_sf %>%
  filter(!name %in% c("Antarctica", "Greenland"))

# Create a sample dataset
happiness_data <- data.frame(
  Country = c(
    "France", "Germany", "United Kingdom",
    "Japan", "China", "Vietnam",
    "United States of America", "Canada", "Mexico"
  ),
  Continent = c(
    "Europe", "Europe", "Europe",
    "Asia", "Asia", "Asia",
    "North America", "North America", "North America"
  ),
  Happiness_Score = rnorm(mean = 30, sd = 20, n = 9),
  GDP_per_capita = rnorm(mean = 30, sd = 20, n = 9),
  Social_support = rnorm(mean = 30, sd = 20, n = 9),
  Healthy_life_expectancy = rnorm(mean = 30, sd = 20, n = 9)
)

# Join the happiness data with the full world map
world_sf <- world_sf %>%
  left_join(happiness_data, by = c("name" = "Country"))



## Creating charts ---------------------------------------------------------

# Create the first chart (Scatter plot)
p1 <- ggplot(world_sf, aes(
  GDP_per_capita,
  Happiness_Score,
  tooltip = name,
  data_id = name,
  color = name
)) +
  geom_point_interactive(data = filter(world_sf, !is.na(Happiness_Score)), size = 4) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# Create the second chart (Bar plot)
p2 <- ggplot(world_sf, aes(
  x = reorder(name, Happiness_Score),
  y = Happiness_Score,
  tooltip = name,
  data_id = name,
  fill = name
)) +
  geom_col_interactive(data = filter(world_sf, !is.na(Happiness_Score))) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# Create the third chart (choropleth)
p3 <- ggplot() +
  geom_sf(data = world_sf, fill = "lightgrey", color = "lightgrey") +
  geom_sf_interactive(
    data = filter(world_sf, !is.na(Happiness_Score)),
    aes(fill = name, tooltip = name, data_id = name)
  ) +
  coord_sf(crs = st_crs(3857)) +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# Combine the plots
combined_plot <- (p1 + p2) / p3 + plot_layout(heights = c(1, 2))

# Create the interactive plot
interactive_plot <- girafe(ggobj = combined_plot)
interactive_plot <- girafe_options(
  interactive_plot,
  opts_hover(css = "fill:red;stroke:black;")
)

# save as an html widget
htmltools::save_html(interactive_plot, "multiple-ggiraph-2.html")


## CSS adjusts -------------------------------------------------------------
tooltip_css <- "
  border-radius: 12px;
  color: #333;
  background-color: white;
  padding: 10px;
  font-size: 14px;
  transition: all 0.5s ease-out;
"

hover_css <- "
  filter: brightness(75%);
  cursor: pointer;
  transition: all 0.5s ease-out;
  filter: brightness(1.15);
"

# Add interactivity
interactive_plot <- interactive_plot %>%
  girafe_options(
    opts_hover(css = hover_css),
    opts_tooltip(css = tooltip_css),
    opts_hover_inv(css = "opacity:0.3; transition: all 0.2s ease-out;")
  )

# Save the interactive plot
htmltools::save_html(interactive_plot, "multiple-ggiraph-6.html")


# Leaflet interactive -----------------------------------------------------

# load example data (Fiji Earthquakes) + keep only 100 first lines
data(quakes)
quakes <-  head(quakes, 100)

# Create a color palette with handmade bins.
mybins <- seq(4, 6.5, by = 0.5)

mypalette <- colorBin( palette = "YlOrBr", 
                       domain = quakes$mag, 
                       na.color = "transparent", 
                       bins = mybins)

# Prepare the text for the tooltip:
mytext <- paste(
  "Depth: ", quakes$depth, "<br/>", 
  "Stations: ", quakes$stations, "<br/>", 
  "Magnitude: ", quakes$mag, sep = "") %>%
  lapply(htmltools::HTML)

# Final Map
m <- leaflet(quakes) %>% 
  addTiles()  %>% 
  setView( lat = -27, lng = 170, zoom = 4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                   fillColor = ~mypalette(mag), 
                   fillOpacity = 0.7, 
                   color = "white", 
                   radius = 8, 
                   stroke = FALSE,
                   label = mytext,
                   labelOptions = labelOptions( 
                     style = list("font-weight" = "normal", 
                                  padding = "3px 8px"), 
                     textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal = mypalette, 
             values = ~mag, 
             opacity = 0.9, 
             title = "Magnitude", 
             position = "bottomright" )

m 

# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/bubblemapQuakes.html"))

# References -------------------------------------------------------------

citation("ggiraph")
citation("tidyverse")
citation("patchwork")
citation("sf")
citation("leaflet")

citation()
RStudio.Version()

### Fin :)
