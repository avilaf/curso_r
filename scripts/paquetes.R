
# =============================================================================
# Ecología de la herpetofauna en el ambiente R para no programadores
# INSTALACIÓN DE PAQUETES 
# =============================================================================
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/
# Fecha: 2025
# Objetivo: Análisis morfométrico de datos de anuros con diferentes técnicas



# -----------------------------
# Paquetes del Tidyverse
# -----------------------------

install.packages("tidyverse")         # Conjunto de paquetes para ciencia de datos: dplyr, ggplot2, tidyr, readr, etc.
install.packages("readr")             # Lectura eficiente de archivos de texto/tablas (.csv, .tsv)
install.packages("dplyr")             # Manipulación de datos con gramática intuitiva (filter, select, mutate, etc.)
install.packages("ggplot2")           # Sistema de gráficos basado en capas

# -----------------------------
# Visualización y gráficos
# -----------------------------

install.packages("ggiraph")           # Visualizaciones interactivas con ggplot2 (tooltip, enlaces, etc.)
install.packages("GGally")            # Extensión de ggplot2 para crear matrices de gráficos (pairs plots)
install.packages("patchwork")         # Composición de múltiples gráficos ggplot en un solo layout
install.packages("plotly")            # Gráficos interactivos basados en ggplot o nativos
install.packages("gridExtra")         # Organización de gráficos en grillas (grid.arrange, etc.)
install.packages("corrplot")          # Visualización de matrices de correlación
install.packages("RColorBrewer")      # Paletas de colores predefinidas y estéticas para mapas y gráficos
install.packages("pheatmap")          # Gráficos de calor (heatmaps) con anotaciones y clustering
install.packages("leaflet")           # Mapas interactivos basados en JavaScript (OpenStreetMap, etc.)

# -----------------------------
# Análisis estadístico y multivariado
# -----------------------------

install.packages("car")               # Herramientas para regresión lineal (tests, VIF, etc.)
install.packages("lme4")              # Modelos lineales y generalizados mixtos
install.packages("MASS")              # Métodos y datos estadísticos clásicos (stepAIC, etc.)
install.packages("FactoMineR")        # Análisis multivariado: ACP, MCA, clustering, etc.
install.packages("factoextra")        # Visualización de resultados de FactoMineR
install.packages("cluster")           # Algoritmos de clustering (k-means, PAM, etc.)
install.packages("broom")             # Convierte resultados estadísticos en data frames ordenados (tidy output)
install.packages("betapart")          # Análisis de beta diversidad (ecología)
install.packages("vegan")             # Análisis multivariado en ecología (NMDS, PERMANOVA, etc.)

# -----------------------------
# Fechas, texto y otros datos
# -----------------------------

install.packages("lubridate")         # Manejo de fechas y horas de forma sencilla
install.packages("stringdist")        # Cálculo de distancias entre cadenas de texto (Levenshtein, Jaro, etc.)
install.packages("plyr")              # Versión antigua de dplyr; útil para código heredado

# -----------------------------
# Datos geográficos y mapas
# -----------------------------

install.packages("sf")                # Lectura y manipulación de datos espaciales (shapefiles, geometrías)
install.packages("mapdata")           # Datos geográficos para mapas básicos (costas, países, etc.)
install.packages("rnaturalearth")     # Mapas base de países y continentes
install.packages("rnaturalearthdata") # Datos acompañantes para rnaturalearth
install.packages("rgbif")             # Descarga de datos de biodiversidad desde GBIF (Global Biodiversity Info)

# -----------------------------
# Genética y filogenia
# -----------------------------

install.packages("ape")               # Análisis filogenético y evolutivo
install.packages("rhierbaps")         # Agrupamiento jerárquico de secuencias genómicas
install.packages("BiocManager")       # Gestor de paquetes Bioconductor

BiocManager::install("ggtree")        # Visualización de árboles filogenéticos con ggplot2
BiocManager::install("msa")           # Alineamiento múltiple de secuencias (con Clustal, MUSCLE, etc.)
BiocManager::install("Biostrings")    # Manipulación eficiente de secuencias de ADN/RNA
BiocManager::install("phangorn")      # Reconstrucción y evaluación de árboles filogenéticos

# -----------------------------
# Audio
# -----------------------------

install.packages("SoundShape")        # Análisis de morfología acústica (sonidos, bioacústica)




install.packages("beepr")             # ¡sorpresa! :)



# Checkear instalación: ---------------------------------------------------



# Lista de pacotes CRAN
cran_packages <- c(
  # Tidyverse
  "tidyverse", "readr", "dplyr", "ggplot2",
  
  # Visualización y gráficos
  "ggiraph", "GGally", "patchwork", "plotly", "gridExtra", "corrplot",
  "RColorBrewer", "pheatmap", "leaflet",
  
  # Análisis estadístico y multivariado
  "car", "lme4", "MASS", "FactoMineR", "factoextra", "cluster", 
  "broom", "betapart", "vegan",
  
  # Fechas, texto y otros datos
  "lubridate", "stringdist", "plyr",
  
  # Datos geográficos y mapas
  "sf", "mapdata", "rnaturalearth", "rnaturalearthdata", "rgbif",
  
  # Genética y filogenia
  "ape", "rhierbaps", "BiocManager",
  
  # Audio
  "SoundShape", "beepr"
)

# Instalar pacotes ausentes (CRAN)
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  } else {
    message(paste(pkg, "ya está instalado"))
  }
}

# Pacotes Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Lista de pacotes Bioconductor
bioc_packages <- c("ggtree", "msa", "Biostrings", "phangorn")

for (pkg in bioc_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    BiocManager::install(pkg, ask = FALSE)
  } else {
    message(paste(pkg, "ya está instalado"))
  }
}

# Verificação final: Mostrar pacotes não carregáveis
message("\nVerificación final de instalación:")

all_packages <- c(cran_packages, bioc_packages)
not_loaded <- all_packages[!sapply(all_packages, requireNamespace, quietly = TRUE)]

if (length(not_loaded) == 0) {
  message("✅ Todos los paquetes fueron instalados correctamente.")
} else {
  message("❌ Los siguientes paquetes no se pudieron instalar o cargar:")
  print(not_loaded)
}

# ============================================================================
#
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/
#
# ============================================================================

