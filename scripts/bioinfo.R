# =============================================================================
# Ecología de la herpetofauna en el ambiente R para no programadores
# =============================================================================
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/

# ============================================================================
# ANÁLISIS FILOGENÉTICO Y BIOGEOGRÁFICO DE SECUENCIAS DE ADN
# ============================================================================
#
# Este script te enseñará paso a paso cómo:
# 1. Alinear secuencias de ADN
# 2. Calcular distancias evolutivas
# 3. Construir árboles filogenéticos 
# 4. Analizar estructura poblacional
# 5. Crear mapas biogeográficos
#
# Ejemplo: Estudio de ranas del género Boana en Sudamérica
# ============================================================================

# PASO 1: INSTALACIÓN Y CARGA DE PAQUETES
# ============================================================================

# ¿Qué necesitamos?
# - Paquetes para biología molecular (alineamiento, árboles)
# - Paquetes para gráficos y mapas
# - Paquetes para manipular datos

# Instalar BiocManager (solo la primera vez)
# install.packages("BiocManager")

# Cargar paquetes de biología molecular
library("BiocManager")     # Gestor de paquetes biológicos
library("msa")             # Alineamiento múltiple de secuencias
library("Biostrings")      # Manipulación de secuencias de ADN
library("phangorn")        # Análisis filogenéticos
library("ape")             # Análisis filogenéticos y evolutivos
library("rhierbaps")       # Análisis de estructura poblacional

# Cargar paquetes para manipular datos
library("dplyr")           # Manipulación de datos
library("plyr")            # Manipulación de datos

# Cargar paquetes para gráficos
library("ggtree")          # Visualización de árboles
library("ggplot2")         # Gráficos elegantes
library("gridExtra")       # Combinar gráficos
library("plotly")          # Gráficos interactivos
library("reshape2")        # Transformar datos
library("pheatmap")        # Mapas de calor

# Cargar paquetes para mapas
library("rnaturalearth")   # Mapas del mundo
library("sf")              # Datos espaciales
library("leaflet")         # Mapas interactivos

# ============================================================================
# PASO 2: CONFIGURAR DIRECTORIO DE TRABAJO
# ============================================================================

# Cambia esta ruta por la carpeta donde tienes tus datos
path <- "D:/curso_r-main"
setwd(path)
dir() # Ver qué archivos hay en la carpeta

# ============================================================================
# PASO 3: CARGAR SECUENCIAS DE ADN
# ============================================================================

print("=== CARGANDO SECUENCIAS DE ADN ===")

# Las secuencias están en formato FASTA (estándar para ADN)
# Cada secuencia tiene un nombre y una cadena de nucleótidos (A, T, G, C)
secuencias <- Biostrings::readDNAStringSet("datos/bioinfo/sequence_boanas.fa")

# Darles nombres simples a las secuencias
nombres <- paste("secuencia", 1:length(secuencias), sep="_")
names(secuencias) <- nombres

print(paste("Cargadas", length(secuencias), "secuencias de ADN"))
print("Primeras 3 secuencias:")
print(secuencias[1:3])

# ============================================================================
# PASO 4: ALINEAMIENTO MÚLTIPLE DE SECUENCIAS
# ============================================================================

print("=== ALINEANDO SECUENCIAS ===")

# ¿Por qué alinear?
# Las secuencias pueden tener diferentes longitudes o inserciones/deleciones
# El alineamiento nos permite comparar posición por posición

# Usar el algoritmo ClustalW (muy usado en biología)
alineamiento <- msa::msa(secuencias,
                         method = "ClustalW",
                         verbose = TRUE)

print("¡Alineamiento completado!")
print("Ahora todas las secuencias tienen la misma longitud")

# ============================================================================
# PASO 5: CALCULAR DISTANCIAS EVOLUTIVAS
# ============================================================================

print("=== CALCULANDO DISTANCIAS EVOLUTIVAS ===")

# Convertir el alineamiento a formato especial para análisis
datos_filogeneticos <- phangorn::as.phyDat(alineamiento)
names(datos_filogeneticos) <- names(secuencias)

# DISTANCIA HAMMING
# Cuenta cuántas posiciones difieren entre dos secuencias
print("1. Calculando distancia Hamming...")
dist_hamming <- phangorn::dist.hamming(datos_filogeneticos, ratio = FALSE)
matriz_hamming <- round(as.matrix(dist_hamming), 2)

# Visualizar como mapa de calor
print("Creando mapa de calor de distancias Hamming...")
pheatmap::pheatmap(matriz_hamming, 
                   main = "Distancias Hamming entre secuencias",
                   show_rownames = FALSE,
                   show_colnames = FALSE)

# DISTANCIA P (proporción de sitios diferentes)
print("2. Calculando distancia P...")
dist_p <- phangorn::dist.hamming(datos_filogeneticos, ratio = TRUE)
matriz_p <- round(as.matrix(dist_p), 2)

pheatmap::pheatmap(matriz_p,
                   main = "Distancia P (proporción de diferencias)",
                   show_rownames = FALSE,
                   show_colnames = FALSE)

# DISTANCIA JUKES-CANTOR
# Considera múltiples sustituciones en el mismo sitio
print("3. Calculando distancia Jukes-Cantor...")
dist_jc <- ape::dist.dna(as.DNAbin(datos_filogeneticos), model = "JC69")
matriz_jc <- round(as.matrix(dist_jc), 2)

pheatmap::pheatmap(matriz_jc,
                   main = "Distancia Jukes-Cantor",
                   show_rownames = FALSE,
                   show_colnames = FALSE)

# ============================================================================
# PASO 6: CONSTRUIR ÁRBOLES FILOGENÉTICOS
# ============================================================================

print("=== CONSTRUYENDO ÁRBOLES FILOGENÉTICOS ===")

# Un árbol filogenético muestra las relaciones evolutivas
# Diferentes métodos pueden dar diferentes árboles

print("Construyendo árboles con diferentes métodos...")

# Método UPGMA (muy usado)
arbol_upgma <- phangorn::upgma(dist_hamming)
arbol_upgma <- phangorn::midpoint(arbol_upgma)

# Método de Neighbor-Joining
arbol_wpgma <- phangorn::wpgma(dist_hamming)
arbol_wpgma <- phangorn::midpoint(arbol_wpgma)

# Función para dibujar árboles de forma bonita
dibujar_arbol <- function(arbol, titulo, max_distancia = 50) {
  grafico <- ggtree::ggtree(arbol, color = "#00A499", size = 1)
  
  grafico <- grafico +
    ggtree::geom_nodepoint(size = 2, color = "#c7254e") +
    labs(title = titulo) +
    xlim(0, max_distancia) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.line = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  return(grafico)
}

# Dibujar los árboles
grafico_upgma <- dibujar_arbol(arbol_upgma, "Árbol UPGMA")
grafico_wpgma <- dibujar_arbol(arbol_wpgma, "Árbol WPGMA")

# Mostrar árboles lado a lado
gridExtra::grid.arrange(grafico_upgma, grafico_wpgma, 
                        nrow = 1, ncol = 2)

print("¡Árboles filogenéticos creados!")

# ============================================================================
# PASO 7: ANÁLISIS DE BOOTSTRAPPING
# ============================================================================

print("=== ANÁLISIS DE CONFIANZA (BOOTSTRAPPING) ===")

# El bootstrapping nos dice qué tan confiables son las ramas del árbol
# Valores altos (>70%) indican ramas confiables

print("Calculando árbol Neighbor-Joining...")
distancia <- phangorn::dist.hamming(datos_filogeneticos)
arbol_nj <- phangorn::NJ(distancia)
arbol_nj <- ape::multi2di(arbol_nj)
arbol_nj <- phangorn::midpoint(arbol_nj)

print("Realizando análisis de bootstrap (puede tomar unos minutos)...")

# Función para el bootstrap
funcion_bootstrap <- function(x) phangorn::NJ(phangorn::dist.hamming(x))

# Hacer 100 réplicas de bootstrap (en investigación real se usan 1000+)
bootstrap_resultado <- ape::boot.phylo(arbol_nj,
                                       B = 100,  # 100 réplicas para ser rápido
                                       as.DNAbin(datos_filogeneticos),
                                       funcion_bootstrap, 
                                       trees = TRUE)

# Agregar valores de confianza al árbol
arbol_con_bootstrap <- phangorn::addConfidences(arbol_nj, 
                                                bootstrap_resultado$trees)

# Visualizar árbol con valores de bootstrap
print("Creando visualización del árbol bootstrapped...")

# Paleta de colores para los valores de bootstrap
paleta_colores <- colorRampPalette(c("red", "orange", "green"))(100)
valores_bootstrap <- as.numeric(arbol_con_bootstrap$node.label)
valores_bootstrap[is.na(valores_bootstrap)] <- 0
posiciones_color <- pmax(1, round(valores_bootstrap, 0))
colores_nodos <- paleta_colores[posiciones_color]

# Dibujar árbol con bootstrap
grafico_bootstrap <- ggtree::ggtree(arbol_con_bootstrap, 
                                    layout = "rectangular")

grafico_bootstrap <- grafico_bootstrap + 
  ggtree::geom_tiplab(size = 3, color = "black") +
  ggtree::geom_nodepoint(size = 3, color = colores_nodos) +
  labs(title = "Árbol Neighbor-Joining con valores de Bootstrap",
       subtitle = "Verde = alta confianza, Rojo = baja confianza") +
  theme_minimal()

print(grafico_bootstrap)

# ============================================================================
# PASO 8: ANÁLISIS DE ESTRUCTURA POBLACIONAL
# ============================================================================

print("=== ANÁLISIS DE ESTRUCTURA POBLACIONAL DE Boana pulchella ===")

# hierBAPS identifica grupos genéticamente similares
print("Analizando estructura poblacional con hierBAPS...")

# Cargar datos para hierBAPS
matriz_snp <- rhierbaps::load_fasta("datos/bioinfo/sequence_boana_pulchella.fa")

# Ejecutar análisis (buscar 2-3 grupos)
resultado_baps <- rhierbaps::hierBAPS(matriz_snp,
                                      max.depth = 2,
                                      n.pops = 10,
                                      quiet = TRUE,
                                      assignment.probs = TRUE)

# Extraer resultados
clusters <- resultado_baps$partition.df
probabilidades <- resultado_baps$cluster.assignment.prob[[1]]

resultados_finales <- data.frame(clusters, probabilidades)
print("Primeros resultados del análisis de poblaciones:")
print(head(resultados_finales))

# Crear gráfico de barras con las probabilidades de asignación
print("Creando gráfico de estructura poblacional...")

grafico_estructura <- plotly::plot_ly(resultados_finales,
                                      x = ~Isolate,
                                      y = ~Cluster.1,
                                      type = 'bar',
                                      name = 'Grupo 1',
                                      marker = list(color = '#FF6B6B'))

grafico_estructura <- grafico_estructura %>% 
  plotly::add_trace(y = ~Cluster.2, 
                    name = 'Grupo 2',
                    marker = list(color = '#4ECDC4'))


grafico_estructura <- grafico_estructura %>% 
  plotly::add_trace(y = ~Cluster.3, 
                    name = 'Grupo 3',
                    marker = list(color = '#900C3F'))

grafico_estructura <- grafico_estructura %>% 
  plotly::add_trace(y = ~Cluster.4, 
                    name = 'Grupo 4',
                    marker = list(color = '#DAF7A6'))

grafico_estructura <- grafico_estructura %>% 
  plotly::layout(
    title = "Estructura Poblacional - Probabilidad de Asignación a Grupos",
    yaxis = list(title = "Probabilidad"),
    xaxis = list(title = "Muestras"),
    barmode = 'stack'
  )

print(grafico_estructura)

# ============================================================================
# PASO 9: CREAR MAPAS BIOGEOGRÁFICOS
# ============================================================================

print("=== CREANDO MAPAS BIOGEOGRÁFICOS ===")

# Si tienes datos de localidades geográficas
if(file.exists("datos/mapas/complete_data_pulchella.csv")) {
  
  print("Cargando datos geográficos...")
  datos_geograficos <- read.csv("datos/mapas/complete_data_pulchella.csv")
  
  # Cargar mapa base de Sudamérica
  mapa_mundo <- rnaturalearth::ne_countries(scale = 'medium', 
                                            returnclass = 'sf')
  
  sudamerica <- mapa_mundo[mapa_mundo$continent == 'South America',]
  
  # Crear mapa estático
  mapa_estatico <- ggplot() + 
    geom_sf(data = sudamerica, fill = "lightgray", color = "white") +
    coord_sf(xlim = c(-80, -30), ylim = c(-60, 15)) +
    geom_point(data = datos_geograficos, 
               aes(x = long, y = lat, color = factor(level.1)),
               size = 3) +
    scale_color_brewer(palette = "Set1", name = "Grupo Genético") +
    labs(title = "Distribución Geográfica de Grupos Genéticos",
         subtitle = "Boana pulchella en Sudamérica") +
    theme_minimal() +
    theme(axis.text = element_text(size = 8))
  
  print(mapa_estatico)
  
  # Crear mapa interactivo
  print("Creando mapa interactivo...")
  
  colores_mapa <- c("#FF6B6B", "#4ECDC4", "#900C3F", "#DAF7A6")
  pal_colores <- leaflet::colorFactor(palette = colores_mapa, 
                                      domain = datos_geograficos$level.1)
  
  # Preparar texto para tooltips
  texto_tooltip <- paste(
    "<b>Localidad:</b>", datos_geograficos$local, "<br/>",
    "<b>Grupo Genético:</b>", datos_geograficos$level.1, "<br/>",
    "<b>Coordenadas:</b>", datos_geograficos$lat, ",", datos_geograficos$long
  )
  
  mapa_interactivo <- leaflet::leaflet(data = datos_geograficos) %>%
    leaflet::addTiles() %>% 
    leaflet::setView(lat = -15, lng = -60, zoom = 4) %>%
    leaflet::addProviderTiles("OpenStreetMap") %>%
    leaflet::addCircleMarkers(~long, ~lat,
                              fillColor = ~pal_colores(level.1),
                              fillOpacity = 0.8,
                              color = "white",
                              radius = 8,
                              stroke = TRUE,
                              popup = texto_tooltip) %>%
    leaflet::addLegend(pal = pal_colores,
                       values = ~level.1,
                       opacity = 0.9,
                       title = "Grupo Genético",
                       position = "topright")
  
  print("¡Mapa interactivo creado! Haz clic en los puntos para ver detalles.")
  print(mapa_interactivo)
  
} else {
  print("Datos geográficos no encontrados. Saltando creación de mapas.")
}



if(file.exists("datos/mapas/complete_data_pulchella.csv")) {
  
  print("Cargando datos geográficos...")
  datos_geograficos <- read.csv("datos/mapas/complete_data_pulchella.csv")
  
  # Cargar mapa base de Sudamérica
  mapa_mundo <- rnaturalearth::ne_countries(scale = 'medium', 
                                            returnclass = 'sf')
  
  sudamerica <- mapa_mundo[mapa_mundo$continent == 'South America',]
  
  # Crear mapa estático
  mapa_estatico <- ggplot() + 
    geom_sf(data = sudamerica, fill = "lightgray", color = "white") +
    coord_sf(xlim = c(-80, -30), ylim = c(-60, 15)) +
    geom_point(data = datos_geograficos, 
               aes(x = long, y = lat, color = factor(level.1)),
               size = 3) +
    scale_color_brewer(palette = "Set1", name = "Grupo Genético") +
    labs(title = "Distribución Geográfica de Grupos Genéticos",
         subtitle = "Boana pulchella en Sudamérica") +
    theme_minimal() +
    theme(axis.text = element_text(size = 8))
  
  print(mapa_estatico)
  
  # Crear mapa interactivo
  print("Creando mapa interactivo...")
  
  colores_mapa <- c("#FF6B6B", "#4ECDC4", "#900C3F", "#DAF7A6")
  pal_colores <- leaflet::colorFactor(palette = colores_mapa, 
                                      domain = datos_geograficos$level.1)
  
  # Preparar texto para tooltips
  texto_tooltip <- paste(
    "<b>Localidad:</b>", datos_geograficos$local, "<br/>",
    "<b>Grupo Genético:</b>", datos_geograficos$level.1, "<br/>",
    "<b>Coordenadas:</b>", datos_geograficos$lat, ",", datos_geograficos$long
  )
  
  mapa_interactivo <- leaflet::leaflet(data = datos_geograficos) %>%
    leaflet::addTiles() %>% 
    leaflet::setView(lat = -15, lng = -60, zoom = 4) %>%
    leaflet::addProviderTiles("Esri.WorldImagery") %>%
    leaflet::addCircleMarkers(~long, ~lat,
                              fillColor = ~pal_colores(level.1),
                              fillOpacity = 0.8,
                              color = "white",
                              radius = 8,
                              stroke = TRUE,
                              popup = texto_tooltip) %>%
    leaflet::addLegend(pal = pal_colores,
                       values = ~level.1,
                       opacity = 0.9,
                       title = "Grupo Genético",
                       position = "topright")
  
  print("¡Mapa interactivo creado! Haz clic en los puntos para ver detalles.")
  print(mapa_interactivo)
  
} else {
  print("Datos geográficos no encontrados. Saltando creación de mapas.")
}



# ============================================================================
# PASO 10: GUARDAR RESULTADOS
# ============================================================================

print("=== GUARDANDO RESULTADOS ===")

# Guardar árbol en formato estándar
if(exists("arbol_con_bootstrap")) {
  ape::write.tree(arbol_con_bootstrap, file = "datos/arbol_resultado.nwk")
  ape::write.nexus(arbol_con_bootstrap, file = "datos/arbol_resultado.nex")
  print("Árbol guardado en formatos Newick (.nwk) y Nexus (.nex)")
}

# Guardar resultados de estructura poblacional
if(exists("resultados_finales")) {
  write.csv(resultados_finales, "datos/estructura_poblacional.csv", row.names = FALSE)
  print("Resultados de estructura poblacional guardados en estructura_poblacional.csv")
}

# ============================================================================
# PASO 11: RESUMEN Y INTERPRETACIÓN
# ============================================================================


mostrar_resumen <- function() {
  
  # Limpiar consola y crear separador
  cat("\n")
  cat(rep("=", 80), sep = "")
  cat("\n")
  cat("                       RESUMEN DEL ANÁLISIS FILOGENÉTICO")
  cat("\n")
  cat(rep("=", 80), sep = "")
  cat("\n\n")
  
  # Sección 1: ¿Qué hemos aprendido?
  cat("🧬 ¿QUÉ HEMOS APRENDIDO?\n")
  cat(rep("-", 50), sep = "")
  cat("\n\n")
  
  cat("1️⃣  ALINEAMIENTO DE SECUENCIAS:\n")
  cat("   ✓ Comparamos secuencias de ADN posición por posición\n")
  cat("   ✓ Identificamos regiones similares y diferentes\n\n")
  
  cat("2️⃣  DISTANCIAS EVOLUTIVAS:\n")
  cat("   ✓ Hamming: Diferencias absolutas entre secuencias\n")
  cat("   ✓ Distancia P: Proporción de sitios diferentes\n")
  cat("   ✓ Jukes-Cantor: Considera múltiples mutaciones\n\n")
  
  cat("3️⃣  ÁRBOLES FILOGENÉTICOS:\n")
  cat("   ✓ Muestran relaciones evolutivas entre especies\n")
  cat("   ✓ Ramas cortas = especies muy relacionadas\n")
  cat("   ✓ Ramas largas = especies más divergentes\n\n")
  
  cat("4️⃣  ANÁLISIS DE CONFIANZA (Bootstrap):\n")
  cat("   ✓ Verde (>70%): rama muy confiable\n")
  cat("   ✓ Amarillo (50-70%): moderadamente confiable\n")
  cat("   ✓ Rojo (<50%): poco confiable\n\n")
  
  cat("5️⃣  ESTRUCTURA POBLACIONAL:\n")
  cat("   ✓ Identificamos grupos genéticamente similares\n")
  cat("   ✓ Cada color representa un linaje evolutivo\n\n")
  
  cat("6️⃣  MAPAS BIOGEOGRÁFICOS:\n")
  cat("   ✓ Visualizamos dónde vive cada grupo genético\n")
  cat("   ✓ Entendemos patrones de dispersión y evolución\n\n")
  
  # Sección 2: Interpretación práctica
  cat("📊 INTERPRETACIÓN PRÁCTICA:\n")
  cat(rep("-", 50), sep = "")
  cat("\n\n")
  
  cat("🔍 Cómo leer los resultados:\n")
  cat("   • Árboles más ramificados = mayor diversidad evolutiva\n")
  cat("   • Grupos en el mapa = poblaciones genéticamente distintas\n")
  cat("   • Distancias altas = especies evolucionaron hace mucho tiempo\n\n")
  
  cat("🎯 Aplicaciones en la vida real:\n")
  cat("   • Conservación: identificar poblaciones únicas\n")
  cat("   • Medicina: rastrear origen de enfermedades\n")
  cat("   • Agricultura: mejorar cultivos usando diversidad genética\n\n")
  
  require(beepr, quietly = T)
  beepr::beep(8)
  }

# Mostrar el resumen
mostrar_resumen()

# Sección opcional: Citar paquetes utilizados
cat("📄 CÓMO CITAR ESTE ANÁLISIS:\n")
cat(rep("-", 30), sep = "")
cat("\n")
citation("ape")
citation("phangorn") 
citation("ggtree")
citation("rhierbaps")
citation()
RStudio.Version()

# ============================================================================
#
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/
#
# ============================================================================
