
# Bioinformatics for biogeography ------------------------------------------------------------------

# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/

# More information here -> https://rpubs.com/mvillalobos/L01_Phylogeny
# and here -> https://cran.r-project.org/web/packages/rhierbaps/vignettes/introduction.html


# Prepare r -------------------------------------------------------------


## Clean memory ------------------------------------------------------------
 
rm(list = ls())


## Packages ----------------------------------------------------------------


### Molecular biology: ------------------------------------------------------

#install.packages("BiocManager")
library("BiocManager") 

#BiocManager::install("msa")
library("msa") 

# Load the 'msa' package into the R session
library("msa")

# BiocManager::install("Biostrings")
library("Biostrings")

# BiocManager::install("phangorn")
library("phangorn")

# install.packages("ape")
library("ape")

# install.packages("stringdist")
library("stringdist")

# install.packages("rhierbaps")
library("rhierbaps")


### Edit data ---------------------------------------------------------------

# install.packages("dplyr")
library("dplyr")

# install.packages("plyr")
require("plyr")


### Graphics ----------------------------------------------------------------

# bioconductor::install("ggtree")
library("ggtree")

# install.packages("ggplot2")
library("ggplot2")

# install.packages("gridExtra")
library("gridExtra")

# install.packages("plotly")
library("plotly")

# install.packages("reshape2")
library("reshape2")

# install.packages("pheatmap")
library("pheatmap")



### Maps --------------------------------------------------------------------

# install.packages("rnaturalearth")
library("rnaturalearth")


# install.packages("sf")
library("sf")

# install.packages("leaflet")
library("leaflet")

## Define directory --------------------------------------------------------

path <- "D:/biologa/00_beca_posdoctoral_UY/curso_R"

setwd(path)

dir() # check files



# Data --------------------------------------------------------------------

sequences <- Biostrings::readDNAStringSet("datos/bioinfo/sequence_boanas.fa")


names <- c(rep("seq", 29))
names(sequences) <- names

# Perform multiple sequence alignment on the DNAStringSet object using the 'msa' package
msa_result_sample <- msa::msa(sequences,
                              method = "ClustalW",
                              verbose = T)

## Distances --------------------------------------------------------------


### Hamming -----------------------------------------------------------------


# Define a vector containing four DNA sequences
# Convert the multiple sequence alignment result to a phyDat object for downstream analyses in phangorn
phyDat_msa_sample <- phangorn::as.phyDat(msa_result_sample)

# Assign custom names to the sequences
names(phyDat_msa_sample) <- names(sequences)


# Compute the Hamming distance matrix for the aligned sequences
D_hamming <- phangorn::dist.hamming(phyDat_msa_sample, 
                                    ratio = FALSE)

# Round the values in the distance matrix to two decimal places
D_hamming <- round(as.matrix(D_hamming), 2)


# plot distance
pheatmap::pheatmap(D_hamming, 
                   show_rownames = F,
                   show_colnames = F)


### P dist --------------------------------------------------------------------

# Compute the distance matrix using the 'dist.hamming' function (assuming it's from a specific package)
D_p <- phangorn::dist.hamming(phyDat_msa_sample, 
                              ratio = T)

# Convert the distance matrix to a matrix and round the values
D_p <- round(as.matrix(D_p), 2)

# plot distance
pheatmap::pheatmap(D_p,
                   show_rownames = F,
                   show_colnames = F)

### Edit distance -----------------------------------------------------------

# Convert the sequences into character strings (if they are not already).
seq_chars <- as.character(sequences)

# Calculate the Levenshtein distance matrix (edit distance) between the character sequences.
D_Edit <- stringdist::stringdistmatrix(seq_chars, seq_chars, method = "lv")

# Assign custom names to the sequences
colnames(D_Edit) <- names(sequences)
rownames(D_Edit) <- names(sequences)

# Round the values in the distance matrix to two decimal places.
D_Edit <- round(D_Edit, 2)

# plot distance
pheatmap::pheatmap(D_Edit,
                   show_rownames = F,
                   show_colnames = F)

### Junkes-Cantor -----------------------------------------------------------

# Compute the p distance matrix for the aligned sequences
D_JK69 <- ape::dist.dna(as.DNAbin(phyDat_msa_sample), 
                        model = "JC69")

# Convert the distance matrix to a matrix and round the values
D_JK69 <- round(as.matrix(D_JK69),2)

# plot distance
pheatmap::pheatmap(D_JK69,
                   show_rownames = F,
                   show_colnames = F)

### Kimura 2 parameter ------------------------------------------------------

# Calculate the distance matrix with the K80 model and store it in 'D_K80'
D_K80 <- ape::dist.dna(as.DNAbin(phyDat_msa_sample), 
                       model = "K80")

# Convert the distance matrix to a matrix and round the values to two decimal places
D_K80 <- round(as.matrix(D_K80), 2)

# Replace any NA (Not Available) values in the matrix with 0
D_K80[which(is.na(D_K80))] = 0

# plot distance
pheatmap::pheatmap(D_K80, 
                   show_rownames = F,
                   show_colnames = F)

## Ultrametric tree -----------------------------------------------------
  
# Distance Calculation
# Calculate the Hamming distance matrix for the given aligned sequences
# This serves as a measure of pairwise sequence dissimilarity for tree construction
D_hamming <- phangorn::dist.hamming(phyDat_msa_sample)

# Nearest Neighbor Clustering (NNC)
# Construct a phylogenetic tree using the Nearest Neighbor Clustering method
# This method groups sequences based on the nearest (smallest) pairwise distance
tree_NNC <- phangorn::upgma(D_hamming, "single")
tree_NNC <- phangorn::midpoint(tree_NNC)

# Furthest Neighbor (FN)
# Construct a phylogenetic tree using the Furthest Neighbor method
# This method groups sequences based on the furthest (largest) pairwise distance
tree_FN <- phangorn::upgma(D_hamming, "complete")
tree_FN <- phangorn::midpoint(tree_FN)

# Weighted Pair Group Method with Arithmetic Mean (WPGMA)
# Construct a phylogenetic tree using the WPGMA method
# This method considers all pairwise distances for clustering and calculates average distances
tree_WPGMA <- phangorn::wpgma(D_hamming)
tree_WPGMA <- phangorn::midpoint(tree_WPGMA)

# Unweighted Pair-Group Centroid Method (UPGMC)
# Construct a phylogenetic tree using the UPGMC method
# This method clusters sequences based on the centroid distance without considering the number of sequences in each cluster
tree_UPGMC <- phangorn::upgma(D_hamming, "centroid")
tree_UPGMC <- phangorn::midpoint(tree_UPGMC)

# Weighted Pair-Group Centroid Method (WPGGMC)
# Construct a phylogenetic tree using the WPGGMC method
# This method clusters sequences based on the centroid distance and considers the number of sequences in each cluster
tree_WPGGMC <- phangorn::wpgma(D_hamming, "centroid")
tree_WPGGMC <- phangorn::midpoint(tree_WPGGMC)

# Unweighted Pair Group Method with Arithmetic Mean (UPGMA)
# Construct a phylogenetic tree using the UPGMA method
# This method is similar to WPGMA but does not consider the number of sequences in each cluster for centroid calculation
tree_UPGMA <- phangorn::upgma(D_hamming)
tree_UPGMA <- phangorn::midpoint(tree_UPGMA)



# Function to plot trees
plot_tree <- function(tree_plot, title_plot, max_x) {
  g <- ggtree(tree_plot, color = "#00A499", size = 1)
  
  # Customize the appearance of the plot and tip labels
  g <- g +
    geom_nodepoint(size = 1, color = "#c7254e", show.legend = F) +
    labs(title = title_plot) +
    xlim(0, max_x) +
    theme(
      plot.title = element_text(size = 11),
      # Remove axis lines and text
      axis.line = element_blank(),
      axis.text = element_blank(),
      # Remove all grids
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Adjust margins and legend position
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      legend.position = 'top'
    )
  
  
  return(g)
}

# Generate plots for each tree
g_NNC    <- plot_tree(tree_NNC, "NNC", .07)
g_FN     <- plot_tree(tree_FN, "FN", .07)
g_WPGMA  <- plot_tree(tree_WPGMA, "WPGMA", 0.07)
g_UPGMC  <- plot_tree(tree_UPGMC, "UPGMC", 0.07)
g_WPGGMC <- plot_tree(tree_WPGGMC, "WPGGMC", 0.07)
g_UPGMA  <- plot_tree(tree_UPGMA, "UPGMA", 0.07)

# Arrange the plots in a grid
gridExtra::grid.arrange(g_NNC, g_FN, g_WPGMA, 
                        g_UPGMC, g_WPGGMC, g_UPGMA, 
                        nrow = 2, ncol = 3)



# Hierarchical Clustering -------------------------------------------------------------------
# hierarchical Bayesian analysis of populations structure


# Loading data:
snp_matrix <- rhierbaps::load_fasta("datos/bioinfo/sequence_boanas.fa")


# Running hierBAPS
hb_results <- rhierbaps::hierBAPS(snp_matrix,
                                  max.depth = 2,
                                  n.pops = 2,
                                  quiet = T,
                                  n.extra.rounds = Inf,
                                  assignment.probs = T)

# Exporting results:
clust <- hb_results$partition.df
prob <- hb_results$cluster.assignment.prob[[1]]

hb_results_export <- data.frame(clust,prob)
hb_results_export

# export
readr::write_csv(hb_results_export, "datos/bioinfo/sequence_boanas_rhierbaps.csv")


## Bar graph --------------------------------------------------------------


names(hb_results_export)
hb_results$cluster.assignment.prob


fig <- plotly::plot_ly(hb_results_export,
                       x = ~Isolate,
                       y = ~Cluster.1,
                       type = 'bar',
                       name = 'Cluster 1',
                       marker = list(color = '#cc0000')
                       ) 
# second layer:
fig <- fig %>% plotly::add_trace(y = ~Cluster.2, name = 'Cluster 2',
                                 marker = list(color = '#82A92D'))
# Adjust layout:
fig <- fig %>% plotly::layout(yaxis = list(title = FALSE),
                              xaxis = list(title = FALSE,
                                           type = "category",
                                           categoryorder = "array",
                                           categoryarray = unique(hb_results_export$Isolate)),
                              bargap = 0.01,
                              barmode = 'stack')

# View:
fig




##  BAPS + Tree ---------------------------------------------------------


# tree_UPGMA
tree_UPGMA$tip.label <- (hb_results$partition.df$Isolate)

{
p <- ggtree(tree_UPGMA, branch.length = "none") + 
  geom_tiplab(geom = "text", size = 3) + theme(legend.position='none')

df <- cbind.data.frame(hb_results$partition.df$Isolate,
                       hb_results$cluster.assignment.prob[[1]])


df <- melt(df, id = "hb_results$partition.df$Isolate")

p2 <- p + geom_facet(panel = 'bar', 
                     data = df, 
                     geom = geom_bar, 
                     mapping = aes(x = value, 
                                   fill = as.factor(variable)), 
                     orientation = 'y', 
                     width = 0.8, 
                     stat='identity') + 
  xlim_tree(14)


facet_widths(p2, widths = c(3, 1))

}


# Bootstraped tree ------------------------------------------------------------

# Calculate the Hamming distance for the given phylogenetic data
names(phyDat_msa_sample) <- rownames(snp_matrix)
distance <- phangorn::dist.hamming(phyDat_msa_sample)

# Construct a Neighbor Joining tree based on the calculated distance
nj_tree <- phangorn::NJ(distance)

# Resolve multifurcations in the tree
nj_tree <- ape::multi2di(nj_tree)

# Place the root of the tree at its midpoint
nj_tree <- phangorn::midpoint(nj_tree)

# Define a function that computes the NJ tree based on Hamming distance
f <- function(x) nj(dist.hamming(x))

# Perform bootstrap analysis on the tree
bootstrapped_trees <- ape::boot.phylo(nj_tree,
                                      B = 999,
                                      as.DNAbin(phyDat_msa_sample),
                                      f, 
                                      trees = T)

# Add bootstrap confidence values to the nodes of the original tree
bootstrapped_conf_tree <- phangorn::addConfidences(nj_tree, 
                                                   bootstrapped_trees$trees)

# Define a color palette for visualizing bootstrap support
color_palette <- colorRampPalette(c("red3", "mediumseagreen"))(100)
color_positions <- round(bootstrapped_conf_tree$node.label * 97, 0)
colors <- color_palette[color_positions]

# Create a plot of the bootstrapped tree using ggtree
g <- ggtree(bootstrapped_conf_tree, 
            layout = "dendrogram", 
            ladderize = T)

g <- g + 
  geom_tiplab(size = 4, 
              color = "black")  # Add labels to terminal nodes

g <- g + 
  theme(plot.margin = margin(t = 0, r = 0, b = 150, 
                             l = 0, unit = "pt"), 
        legend.position = "top")

g <- g + geom_nodepoint(size = 4, 
                        color = colors) + 
  theme(legend.position='top')


g <- g + 
  labs(title = "NJ tree Bootstrapping (999 trees)")

# Create a data frame with values from 1 to 100 for color mapping
color_data <- data.frame(x = 1:100, y = 1)
color_data$colors <- color_palette

# Create a color bar plot using ggplot2
g1 <- ggplot(color_data, aes(x = x, y = y, fill = colors)) +
  
  geom_bar(stat = "identity", width = 1, position = "identity") + 
  
  scale_fill_manual(values = rev(color_data$colors)) +
  
  labs(title = NULL,
       x = "% of node support",
       y = NULL) +
  
  theme_minimal() + 
  
  theme(axis.text.y = element_blank(),  # Remove y-axis labels
                          axis.title.y = element_blank(),
                          panel.grid = element_blank())  # Remove y-axis title

g1 <- g1 + 
  guides(fill = "none")

# Arrange the tree plot and the color bar plot
grid.arrange(g, g1, 
             nrow = 2, ncol = 1, 
             heights = c(0.8, 0.2))



## BAPS + bootstraped tree -------------------------------------------------

# Level 1
{
  p <- ggtree(bootstrapped_conf_tree, branch.length = "none") +
    
    geom_tiplab(geom = "text", size = 2) + 
    
    theme(legend.position = 'none')
  
  
  df <- cbind.data.frame(hb_results$partition.df$Isolate,
                         hb_results$cluster.assignment.prob[[1]])
  
  
  df <- melt(df, id = "hb_results$partition.df$Isolate")
  
  p2 <- p + geom_facet(panel = 'bar', 
                       data = df, 
                       geom = geom_bar, 
                       mapping = aes(x = value, 
                                     fill = as.factor(variable)), 
                       orientation = 'y', 
                       width = 0.8,
                       stat='identity') + 
    xlim_tree(14)
  
  
  facet_widths(p2, widths = c(3, 1))
  
}

# Level 2
{
  p <- ggtree(bootstrapped_conf_tree, branch.length = "none") +
    
    geom_tiplab(geom = "text", size = 2) + 
    
    theme(legend.position = 'none')
  
  
  df <- cbind.data.frame(hb_results$partition.df$Isolate,
                         hb_results$cluster.assignment.prob[[2]])
  
  
  df <- melt(df, id = "hb_results$partition.df$Isolate")
  
  p2 <- p + geom_facet(panel = 'bar', 
                       data = df, 
                       geom = geom_bar, 
                       mapping = aes(x = value, 
                                     fill = as.factor(variable)), 
                       orientation = 'y', 
                       width = 0.8,
                       stat='identity') + 
    xlim_tree(14)
  
  
  facet_widths(p2, widths = c(3, 1))
  
}



# Saving trees: -----------------------------------------------------------

# Saving to Newick
write.tree(bootstrapped_conf_tree, file = "datos/bioinfo/output_newick_file.nwk")

# Saving to Nexus (using 'ape' package)
write.nexus(bootstrapped_conf_tree, file = "datos/bioinfo/output_nexus_file.nex")



# For Newick
tree_newick <- read.tree("datos/bioinfo/output_newick_file.nwk")

# For Nexus
tree_nexus <- read.nexus("datos/bioinfo/output_nexus_file.nex")



# Boigeography: Boana pulchella ------------------------------------------------------------------


## BAPS --------------------------------------------------------------------

data_pulc <- rhierbaps::load_fasta("datos/bioinfo/sequence_boana_pulchella.fa")
class(data_pulc)

hb_pulchella <- rhierbaps::hierBAPS(data_pulc,
                                    max.depth = 2,
                                    n.pops = 19,
                                    quiet = T,
                                    n.extra.rounds = Inf,
                                    assignment.probs = T
                                    )


clust <- hb_pulchella$partition.df
prob <- data.frame(hb_pulchella$cluster.assignment.prob[[1]])

# View(prob) 

hb_results_export <- data.frame(cbind(clust, prob))
hb_results_export



# export
readr::write_csv(hb_results_export, "datos/bioinfo/hb_pulchella.csv")



# Map ---------------------------------------------------------------------


## Occurrences --------------------------------------------------------------


# Load occurences data
occ_data <- read.csv("datos/maps/datos/mapas/complete_data_pulchella.csv", 
                     header = T)
head(occ_data) # view data

occ_data2 <- base::merge(occ_data, hb_results_export, 
                         by = "Isolate") # merge occ data + cluster data
head(occ_data2) # view

# export
readr::write_csv(occ_data2, "datos/mapas/complete_data_pulchella.csv")



# Load base maps
worldmap <- rnaturalearth::ne_countries(scale = 'medium', 
                                        type = 'map_units',
                         returnclass = 'sf') # world countries


ggplot() + geom_sf(data = worldmap) + theme_bw() # plot basemap


sa <- worldmap[worldmap$continent == 'South America',] # select South America

ggplot() + geom_sf(data = sa) + theme_bw() # plot sa


sa_cropped <- sf::st_crop(worldmap, 
                          xmin = -65, xmax = -45,
                          ymin = -40, ymax = -26) # crop sa to our study area

ggplot() + geom_sf(data = sa_cropped) + theme_bw() # plot cropped sa


# Plot sa + sample occ

(map <- ggplot() + 
  
  geom_sf(data = worldmap) +
  
  coord_sf(xlim = c(-65, -45), 
           ylim = c(-40, -26), 
           expand = FALSE) +
  
  geom_point(data = occ_data2, 
             aes(x = long, y = lat,
                 colour = factor(level.1)),
             size = 3) +
    
    scale_colour_brewer(palette = "Set1") +
    
  theme_bw()

)


## Neotropical provinces --------------------------------------------------------


# load shapefile
neotropical <- sf::st_read(dsn = "datos/mapas/neotropical_provinces_crop/neotropical_provinces_crop.shp")

neotrop <- as.data.frame(neotropical$Provincias)

neotrop_valid <- sf::st_make_valid(neotropical) # adjust shapefile

neotrop_croped <- sf::st_crop(neotrop_valid,
                              xmin = -64, ymin = -40,
                              xmax = -45, ymax = -26) # crop shapefile


# Plot Neotropical Provinces:

(mapa_neotrop <- ggplot(neotrop_croped) + 
  
  geom_sf(data = neotrop_croped,
          aes(fill = Provincias)) +
  
  scale_fill_viridis_d(alpha = 0.4) +
  
  coord_sf(xlim = c(-64, -48), 
           ylim = c(-38.5, -26.5), 
           expand = FALSE) +

  labs(fill = "Provinces",
       colour = "Clusters") +
  
  theme_bw()
)


# Plot Neotropical map + samples clusters:

(mapa_neotrop_amostras <-   
  
  mapa_neotrop +
  
  geom_point(data = occ_data2, 
             aes(x = long, y = lat,
             colour = factor(level.1)),
             size = 4,
             show.legend = T) +
  
  scale_colour_brewer(palette = "Set1") +
  
  guides(fill = guide_legend(override.aes = list(colour = NA)))
  )


# Interative plots --------------------------------------------------------

# https://r-graph-gallery.com/414-map-multiple-charts-in-ggiraph.html
dir()

data <- read.csv("datos/mapas/complete_data_pulchella.csv", 
                 h = T)

df <- data %>%
  dplyr::mutate_at(vars(level.1, level.2), factor)


summary(data)
summary(df)


# get the palette
my_colors <- c("#af7ac5", 
               "#a2d9ce", 
               "#fcf3cf", 
               "#e6b0aa")

# Prepare the text for the tooltip:
mytext <- paste(
  "Local: ", data$local, "<br/>",
  "Level 1: ", data$level.1, "<br/>", 
  "Level 2: ", data$level.2, "<br/>"
) %>%
  lapply(htmltools::HTML)


# Create a proper color palette function
color_pal <- leaflet::colorFactor(palette = my_colors, 
                                  domain = df$level.1)



# Full map ----------------------------------------------------------------


m <- leaflet:: leaflet(data = df) %>%
  
  leaflet::addTiles() %>% 
  
  leaflet::setView(lat = -35, lng = -50, zoom = 4) %>%
  
  leaflet::addProviderTiles("Esri.WorldImagery") %>%
  
  leaflet::addCircleMarkers(~long, ~lat,
                            fillColor = ~color_pal(level.1),
                            fillOpacity = 0.9,
                            color = "white",
                            radius = 8,
                            stroke = FALSE,
                            label = mytext,
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", 
                                           padding = "3px 8px"),
                              textsize = "13px",
                              direction = "auto")) %>%
  
  leaflet::addLegend(pal = color_pal,
                     values = ~df$level.1,
                     opacity = 0.9,
                     title = "Cluster",
                     position = "bottomright")

# Display the map
m


# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/bubblemap.html"))



# Citations ---------------------------------------------------------------

citation("leaflet")

citation("dplyr")

citation("tibble")

citation()

RStudio.Version()

# Citations ---------------------------------------------------------------

## Molecular biology ------------------------------------------------------

citation("BiocManager") 
citation("msa") 
citation("msa")
citation("Biostrings")
citation("phangorn")
citation("ape")
citation("stringdist")
citation("rhierbaps")


## Edit data ---------------------------------------------------------------

citation("dplyr")
citation("plyr")


## Graphics ----------------------------------------------------------------

citation("ggtree")
citation("ggplot2")
citation("gridExtra")
citation("plotly")
citation("reshape2")


## Maps --------------------------------------------------------------------

citation("rnaturalearth")
citation("sf")
citation("leaflet")


## R and RStudio -----------------------------------------------------------

citation()
RStudio.Version()

# end ---------------------------------------------------------------------

