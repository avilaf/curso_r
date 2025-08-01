
#############################################################################
############################## script mágico ################################
#############################################################################
# =============================================================================
# Ecología de la herpetofauna en el ambiente R para no programadores
# =============================================================================
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/

# Ploting a simple map with "mapdata" -------------------------


#install.packages("mapdata")

library(mapdata)

RegHR <- map("worldHires", namesonly = TRUE, plot = FALSE)

map('worldHires', fill = TRUE, col = terrain.colors(6),  bg = "#CCEEFF",
    region=RegHR[-grep("Antarctica", RegHR)])



#############################################################################

# R version 4.5.0 (2025-04-11 ucrt)

# Dra. Fernanda Rodrigues de Avila 
# <https://avilaf.github.io/>
# fernandar.avila@gmail.com