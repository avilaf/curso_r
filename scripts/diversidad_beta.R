
# =============================================================================
# Ecología de la herpetofauna en el ambiente R para no programadores
# =============================================================================
# ECOLOGÍA DE COMUNIDADES
# =============================================================================
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/

# ===============================================================
# Análisis de Partición de Diversidad Beta (Sorensen)
# con comparación entre grupos
# ===============================================================

# 1. CARGAR PAQUETES -------------------------------------------
# Instalar si es necesario:
# install.packages("betapart")
# install.packages("vegan")
library(betapart)  # para diversidad beta
library(vegan)     # para ecología

# 2. IMPORTAR Y PREPARAR DATOS ----------------------------------

#  Importar datos del enlace
# rl_datos <- "https://raw.githubusercontent.com/avilaf/curso_r/refs/heads/main/datos/dados_beta.csv"
# biologica <- read.csv(url_datos, header = TRUE, sep = ",")

#  Importar datos del directorio local
path <- "D:/curso_r-main"

setwd(path)

dir() # check files

biologica <- read.csv("datos/dados_beta.csv", header = TRUE, sep = ",")


# Verificar estructura
str(biologica)

# Preparar matriz de datos (remover columnas no numéricas)
datos <- biologica[,-c(1,2)]

# Convertir a presencia/ausencia
bio <- decostand(datos, method = "pa")

# Crear vector de grupos
grupos <- factor(biologica$factor)

# 3. CALCULAR PARTICIÓN DE DIVERSIDAD BETA ----------------------
# Índice de Sorensen (total, recambio, anidamiento)
dist <- beta.pair(bio, index.family = "sorensen")

# Calcular dispersión de las distancias al centroide
bd_total <- betadisper(dist[[3]], grupos)
bd_recambio  <- betadisper(dist[[1]], grupos)
bd_anidamiento  <- betadisper(dist[[2]], grupos)

# Crear marco de datos con las distancias
datos_estadist <- data.frame(
  grupo = grupos,
  total = bd_total$distances,
  recambio = bd_recambio$distances,
  anidamiento = bd_anidamiento$distances
)

# ===============================================================
# 4. PRUEBAS PARA BETA TOTAL
# ===============================================================
# Prueba permutacional para diferencia en dispersión (total) entre grupos
permutest(bd_total, permutations = 999, pairwise = TRUE)

# ===============================================================
# 5. PRUEBAS PARA RECAMBIO (TURNOVER)
# ===============================================================
# Prueba permutacional para diferencia en dispersión (recambio) entre grupos
permutest(bd_recambio, permutations = 999, pairwise = TRUE)

# ===============================================================
# 6. PRUEBAS PARA ANIDAMIENTO (NESTEDNESS)
# ===============================================================
# Prueba permutacional para diferencia en dispersión (anidamiento) entre grupos
permutest(bd_anidamiento, permutations = 999, pairwise = TRUE)

# ===============================================================
# 7. VISUALIZACIÓN (OPCIONAL)
# ===============================================================
# PCoAs (Análisis de Coordenadas Principales)
par(mfrow = c(1, 3))
graphics::plot(bd_total, main = "PCoA - Beta Total", sub = "")
graphics::plot(bd_recambio, main = "PCoA - Recambio")
graphics::plot(bd_anidamiento, main = "PCoA - Anidamiento")

# Gráficos de cajas (boxplots)
par(mfrow = c(1, 3))
boxplot(total ~ grupo, data = datos_estadist, main = "Beta Total")
boxplot(recambio ~ grupo, data = datos_estadist, main = "Recambio")
boxplot(anidamiento ~ grupo, data = datos_estadist, main = "Anidamiento")

# Restablecer diseño
par(mfrow = c(1, 1))

# ===============================================================
# 8. REFERENCIAS
# ===============================================================

citation()

RStudio.Version()

citation("betapart")
citation("vegan")

# R version 4.5.0 (2025-04-11 ucrt)

# Dra. Fernanda Rodrigues de Avila 
# <https://avilaf.github.io/>
# fernandar.avila@gmail.com


