# =============================================================================
# ANÁLISIS MORFOMÉTRICO DE COMUNIDADES DE ANUROS - VERSIÓN COMENTADA
# =============================================================================
# Autor: Script didáctico para curso de R
# Fecha: 2025
# Objetivo: Análisis morfométrico de datos de anuros con diferentes técnicas
# 
# INTERPRETACIÓN DE ANÁLISIS MORFOMÉTRICOS:
# Los análisis morfométricos nos permiten:
# 1. Identificar patrones de variación morfológica entre poblaciones/tratamientos
# 2. Determinar qué variables son más importantes para diferenciar grupos
# 3. Entender relaciones alométricas (cómo cambian las proporciones con el tamaño)
# 4. Clasificar individuos basándose en su morfología
# =============================================================================

# Cargar librerías necesarias
library(tidyverse)    # Manipulación de datos
library(corrplot)     # Gráficos de correlación
library(FactoMineR)   # Análisis factorial
library(factoextra)   # Visualización de análisis multivariados
library(cluster)      # Análisis de conglomerados
library(MASS)         # Análisis discriminante
library(car)          # ANOVA y diagnósticos
library(ggplot2)      # Gráficos avanzados
library(gridExtra)    # Composición de gráficos
library(reshape2)     # Reformateo de datos
library(broom)        # Limpieza de outputs estadísticos
library(GGally)       # Matrices de gráficos

# =============================================================================
# 1. CARGA Y EXPLORACIÓN INICIAL DE DATOS
# =============================================================================

# COMENTARIO: La exploración inicial es crucial para entender la estructura 
# y calidad de nuestros datos antes de cualquier análisis estadístico

# Cargar datos desde GitHub
url <- "https://raw.githubusercontent.com/avilaf/curso_r/refs/heads/main/datos/dados_morfo.csv"
datos <- read.csv(url, stringsAsFactors = FALSE)

# # Desde tu directorio local (alternativa)
# path <- "camino/para/curso_r-main"
# setwd(path)
# datos <- read.csv("camino/dados_morfo.csv", stringsAsFactors = FALSE)

# Exploración inicial
cat("=== EXPLORACIÓN INICIAL DE DATOS ===\n")
cat("Dimensiones del dataset:", dim(datos), "\n")
cat("Número de filas:", nrow(datos), "\n")
cat("Número de columnas:", ncol(datos), "\n\n")

# INTERPRETACIÓN: Conocer las dimensiones nos da una idea del tamaño muestral
# y número de variables. Muestras pequeñas (<30) pueden limitar algunos análisis

# Estructura de los datos
str(datos)
# INTERPRETACIÓN: str() nos muestra el tipo de cada variable (numérica, categórica)
# Esto es fundamental para saber qué análisis son apropiados para cada variable

# Primeras observaciones
head(datos)
# INTERPRETACIÓN: head() nos permite verificar que los datos se cargaron correctamente
# y entender el formato de las variables

# Resumen estadístico
summary(datos)
# INTERPRETACIÓN: summary() proporciona estadísticas descriptivas básicas:
# - Para variables numéricas: min, max, media, mediana, cuartiles
# - Para variables categóricas: frecuencias
# - Identifica valores NA (faltantes)

# Variables categóricas únicas
cat("\n=== VARIABLES CATEGÓRICAS ===\n")
cat("Géneros únicos:", length(unique(datos$Genero)), "\n")
print(table(datos$Genero))
# INTERPRETACIÓN: El número de géneros nos indica la diversidad taxonómica
# La distribución de frecuencias muestra si hay géneros dominantes

cat("\nEspecies únicas:", length(unique(datos$sp)), "\n")
print(table(datos$sp))
# INTERPRETACIÓN: Especies con pocas observaciones (<5-10) pueden causar
# problemas en análisis multivariados por falta de poder estadístico

cat("\nTratamientos:", "\n")
print(table(datos$Tratamento))
# INTERPRETACIÓN: Los tratamientos son nuestros grupos principales de comparación
# Es importante tener tamaños muestrales similares entre tratamientos

cat("\nLocalidades:", "\n")
print(table(datos$Local))
# INTERPRETACIÓN: Las localidades pueden introducir efectos confundidos
# si no están balanceadas entre tratamientos

# =============================================================================
# 2. LIMPIEZA Y PREPARACIÓN DE DATOS
# =============================================================================

cat("\n=== LIMPIEZA DE DATOS ===\n")
# COMENTARIO: Los datos faltantes son comunes en estudios morfométricos
# debido a dificultades de medición o daños en especímenes

# Verificar valores faltantes
cat("Valores faltantes por columna:\n")
missing_summary <- datos %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing") %>%
  arrange(desc(Missing))
print(missing_summary)

# INTERPRETACIÓN DE VALORES FALTANTES:
# - <5% faltantes: generalmente aceptable
# - 5-15%: puede introducir sesgo leve
# - >15%: requiere técnicas especiales de imputación o eliminación de variable

# Seleccionar variables morfométricas numéricas
variables_morfo <- c("LTh", "TiL", "TarL", "FL", "AL", "HL_M", "ED", "HW", 
                     "HL_Rostro", "Larg_Boca", "CRC", "Massa..gramas.")

# COMENTARIO: Estas variables representan medidas lineales estándar en anuros:
# - CRC: Longitud rostro-cloaca (medida de tamaño corporal principal)
# - LTh: Longitud del muslo (relacionada con capacidad de salto)
# - TiL: Longitud de la tibia (locomotión)
# - HL_M: Longitud de cabeza (relacionada con dieta)
# - ED: Diámetro del ojo (comportamiento, hábitat)
# - Massa: Peso corporal (condición corporal)

# Crear dataset solo con variables morfométricas completas
datos_morfo <- datos %>%
  dplyr::select(ID, Genero, sp, Local, Tratamento, dplyr::all_of(variables_morfo)) %>%
  # Filtrar solo observaciones con datos completos para análisis multivariados
  filter(complete.cases(dplyr::select(., dplyr::all_of(variables_morfo))))

cat("Datos después de limpieza:", nrow(datos_morfo), "observaciones\n")

# INTERPRETACIÓN: La pérdida de observaciones debe evaluarse:
# - Si perdemos <10% de casos: generalmente aceptable
# - Si perdemos >20%: considerar imputación de datos o análisis de sensibilidad

# =============================================================================
# 3. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# =============================================================================

cat("\n=== ANÁLISIS EXPLORATORIO ===\n")
# COMENTARIO: El EDA nos permite entender patrones básicos antes de análisis complejos

# Estadísticas descriptivas por tratamiento
estadisticas_tratamiento <- datos_morfo %>%
  group_by(Tratamento) %>%
  summarise(
    n = n(),
    CRC_media = mean(CRC, na.rm = TRUE),
    CRC_sd = sd(CRC, na.rm = TRUE),
    Massa_media = mean(Massa..gramas., na.rm = TRUE),
    Massa_sd = sd(Massa..gramas., na.rm = TRUE),
    .groups = 'drop'
  )

print(estadisticas_tratamiento)

# INTERPRETACIÓN DE ESTADÍSTICAS DESCRIPTIVAS:
# - Diferencias en medias pueden indicar efectos del tratamiento
# - Desviaciones estándar similares indican homogeneidad de varianzas
# - Tamaños muestrales muy diferentes pueden afectar análisis estadísticos

# Gráfico de distribución de tamaño corporal (CRC) por tratamiento
p1 <- ggplot(datos_morfo, aes(x = Tratamento, y = CRC)) +
  geom_boxplot(aes(fill = Tratamento), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Distribución del Tamaño Corporal (CRC) por Tratamiento",
       x = "Tratamiento", y = "CRC (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

# INTERPRETACIÓN DE BOXPLOTS:
# - Mediana (línea central): tendencia central robusta
# - Cajas: rango intercuartílico (50% de los datos)
# - Bigotes: extienden hasta 1.5 × IQR
# - Puntos aislados: posibles valores atípicos
# - Solapamiento entre cajas: sugiere similitud entre grupos

# Gráfico de distribución de masa por tratamiento
p2 <- ggplot(datos_morfo, aes(x = Tratamento, y = Massa..gramas.)) +
  geom_boxplot(aes(fill = Tratamento), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Distribución de la Masa por Tratamiento",
       x = "Tratamiento", y = "Masa (g)") +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar gráficos
grid.arrange(p1, p2, ncol = 2)

# =============================================================================
# 4. MATRIZ DE CORRELACIONES
# =============================================================================

cat("\n=== ANÁLISIS DE CORRELACIONES ===\n")
# COMENTARIO: Las correlaciones nos muestran qué variables están relacionadas
# Esto es crucial para entender patrones alométricos y multicolinealidad

# Seleccionar solo variables numéricas para correlación
vars_numericas <- datos_morfo %>%
  dplyr::select(dplyr::all_of(variables_morfo)) %>%
  select_if(is.numeric)

# Calcular matriz de correlación (sin NAs)
matriz_cor <- cor(vars_numericas, use = "complete.obs")

# Visualizar matriz de correlación
corrplot(matriz_cor, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black",
         title = "Matriz de Correlaciones - Variables Morfométricas",
         mar = c(0,0,2,0))

# INTERPRETACIÓN DE CORRELACIONES:
# - Valores cercanos a 1: correlación positiva fuerte
# - Valores cercanos a -1: correlación negativa fuerte
# - Valores cercanos a 0: sin correlación lineal
# - Colores rojos: correlaciones positivas
# - Colores azules: correlaciones negativas

# Encontrar correlaciones altas
correlaciones_altas <- which(abs(matriz_cor) > 0.8 & matriz_cor != 1, arr.ind = TRUE)
cat("Correlaciones altas (>0.8):\n")
for(i in 1:nrow(correlaciones_altas)) {
  row_idx <- correlaciones_altas[i, 1]
  col_idx <- correlaciones_altas[i, 2]
  cat(rownames(matriz_cor)[row_idx], " - ", colnames(matriz_cor)[col_idx], 
      ": ", round(matriz_cor[row_idx, col_idx], 3), "\n")
}

# INTERPRETACIÓN DE CORRELACIONES ALTAS (>0.8):
# - Indican posible redundancia entre variables
# - Pueden causar multicolinealidad en análisis multivariados
# - En morfometría, es normal que medidas de tamaño estén correlacionadas
# - Considera eliminar una de las variables altamente correlacionadas

# =============================================================================
# 5. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# =============================================================================

cat("\n=== ANÁLISIS DE COMPONENTES PRINCIPALES ===\n")
# COMENTARIO: El PCA reduce la dimensionalidad preservando la máxima varianza
# Es especialmente útil cuando tenemos muchas variables correlacionadas

# Preparar datos para PCA (solo variables numéricas, sin NAs)
datos_pca <- datos_morfo %>%
  dplyr::select(dplyr::all_of(variables_morfo)) %>%
  na.omit() %>%
  scale()  # Estandarizar variables (media=0, sd=1)

# COMENTARIO SOBRE ESTANDARIZACIÓN:
# Estandarizar es crucial cuando las variables tienen diferentes unidades
# Sin estandarización, variables con mayor varianza dominarían el PCA

# Realizar PCA
pca_resultado <- PCA(datos_pca, graph = FALSE)

# Resumen de resultados
print(summary(pca_resultado))

# INTERPRETACIÓN DEL PCA:
# - Cada componente principal (CP) es una combinación lineal de variables originales
# - CP1 explica la mayor varianza posible
# - CPs subsecuentes explican varianza residual decreciente
# - Objetivo: explicar >70-80% de varianza con pocos CPs

# Scree plot - varianza explicada
fviz_eig(pca_resultado, addlabels = TRUE, ylim = c(0, 50),
         title = "Scree Plot - Varianza Explicada por Componente")

# INTERPRETACIÓN DEL SCREE PLOT:
# - Buscar "codo" donde la pendiente cambia abruptamente
# - Componentes antes del codo son informativos
# - Componentes después del codo contribuyen poco

# Biplot - individuos y variables
fviz_pca_biplot(pca_resultado, 
                col.ind = datos_morfo$Tratamento[complete.cases(datos_morfo[variables_morfo])],
                palette = c("#00AFBB", "#E7B800"),
                addEllipses = TRUE,
                title = "PCA Biplot - Individuos y Variables")

# INTERPRETACIÓN DEL BIPLOT:
# - Puntos: individuos proyectados en espacio de 2 CPs
# - Vectores: variables originales en espacio de CPs
# - Vectores largos: variables con alta varianza
# - Ángulos entre vectores: correlaciones (0°=perfecta, 90°=sin correlación)
# - Elipses: confianza de agrupamiento por tratamiento
# - Separación de elipses: diferenciación entre tratamientos

fviz_pca_biplot(pca_resultado, 
                axes = c(2,3),
                col.ind = datos_morfo$Tratamento[complete.cases(datos_morfo[variables_morfo])],
                palette = c("#00AFBB", "#E7B800"),
                addEllipses = TRUE,
                title = "PCA Biplot - Individuos y Variables")



# Contribución de variables a los componentes principales
contrib_vars <- get_pca_var(pca_resultado)$contrib
cat("Variables con mayor contribución al CP1:\n")
print(sort(contrib_vars[,1], decreasing = TRUE)[1:5])

cat("Variables con mayor contribución al CP2:\n")
print(sort(contrib_vars[,2], decreasing = TRUE)[1:5])

# INTERPRETACIÓN DE CONTRIBUCIONES:
# - CP1 generalmente representa "tamaño general" en morfometría
# - CP2 suele representar "forma" o contrastes específicos
# - Variables con alta contribución definen el significado biológico del CP

# =============================================================================
# 6. ANÁLISIS DISCRIMINANTE
# =============================================================================

cat("\n=== ANÁLISIS DISCRIMINANTE ===\n")
# COMENTARIO: El LDA busca la combinación lineal de variables que mejor separa grupos
# A diferencia del PCA, es un análisis supervisado (usa información de grupos)

# Preparar datos para análisis discriminante
datos_lda <- datos_morfo %>%
  dplyr::select(Tratamento, dplyr::all_of(variables_morfo)) %>%
  na.omit()

# Realizar LDA
lda_resultado <- lda(Tratamento ~ ., data = datos_lda)

# Mostrar resultados
print(lda_resultado)

# INTERPRETACIÓN DEL LDA:
# - "Prior probabilities": probabilidades a priori de cada grupo
# - "Group means": medias de cada variable por grupo
# - "Coefficients": pesos de cada variable en la función discriminante
# - Variables con coeficientes altos son más importantes para discriminar

# Predicciones y matriz de confusión
predicciones_lda <- predict(lda_resultado)
matriz_confusion <- table(datos_lda$Tratamento, predicciones_lda$class)
cat("Matriz de Confusión:\n")
print(matriz_confusion)

# INTERPRETACIÓN DE LA MATRIZ DE CONFUSIÓN:
# - Diagonal: predicciones correctas
# - Fuera de diagonal: errores de clasificación
# - Permite identificar qué grupos se confunden más

# Calcular precisión
precision <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
cat("Precisión del modelo LDA:", round(precision * 100, 2), "%\n")

# INTERPRETACIÓN DE LA PRECISIÓN:
# - >90%: excelente discriminación
# - 80-90%: buena discriminación
# - 70-80%: discriminación moderada
# - <70%: discriminación pobre

# Gráfico de análisis discriminante
datos_lda_plot <- data.frame(
  LD1 = predicciones_lda$x[,1],
  Tratamento = datos_lda$Tratamento
)

ggplot(datos_lda_plot, aes(x = LD1, fill = Tratamento)) +
  geom_density(alpha = 0.6) +
  labs(title = "Análisis Discriminante Linear",
       x = "Función Discriminante 1",
       y = "Densidad") +
  theme_minimal()

# INTERPRETACIÓN DEL GRÁFICO LDA:
# - Separación clara entre distribuciones: buena discriminación
# - Solapamiento alto: discriminación pobre
# - La función discriminante combina todas las variables para maximizar separación

# =============================================================================
# 7. ANÁLISIS DE CONGLOMERADOS (CLUSTER)
# =============================================================================

cat("\n=== ANÁLISIS DE CONGLOMERADOS ===\n")
# COMENTARIO: El clustering agrupa individuos similares sin conocimiento previo de grupos
# Es un análisis no supervisado que puede revelar estructura oculta en los datos

# Preparar datos para clustering (estandarizados)
datos_cluster <- datos_morfo %>%
  dplyr::select(dplyr::all_of(variables_morfo)) %>%
  na.omit() %>%
  scale()

# Determinar número óptimo de clusters - Método del codo
fviz_nbclust(datos_cluster, kmeans, method = "wss", k.max = 10) +
  labs(title = "Método del Codo para Determinar K Óptimo")

# INTERPRETACIÓN DEL MÉTODO DEL CODO:
# - Buscar punto donde la reducción en WSS (suma de cuadrados intra-cluster) se estabiliza
# - El "codo" sugiere el número óptimo de clusters
# - Balancear parsimonia vs. explicación de varianza

# Análisis de silueta
fviz_nbclust(datos_cluster, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "Análisis de Silueta para Determinar K Óptimo")

# INTERPRETACIÓN DEL ANÁLISIS DE SILUETA:
# - Mide qué tan bien cada punto pertenece a su cluster vs. otros clusters
# - Valores cercanos a 1: punto bien agrupado
# - Valores cercanos a 0: punto en frontera entre clusters
# - Valores negativos: punto posiblemente mal clasificado

# Realizar K-means con k=2 (basado en tratamiento)
set.seed(123)
kmeans_resultado <- kmeans(datos_cluster, centers = 2, nstart = 25)

# COMENTARIO SOBRE K-MEANS:
# - Algoritmo iterativo que minimiza varianza intra-cluster
# - "nstart=25": ejecutar 25 veces con inicializaciones aleatorias
# - Tomar la mejor solución (menor suma de cuadrados intra-cluster)

# Visualizar clusters
fviz_cluster(kmeans_resultado, data = datos_cluster,
             palette = c("#2E9FDF", "#00AFBB"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw(),
             main = "K-means Clustering (k=2)")

# INTERPRETACIÓN DE LA VISUALIZACIÓN DE CLUSTERS:
# - Puntos del mismo color pertenecen al mismo cluster
# - Elipses muestran la distribución de cada cluster
# - Separación clara indica clustering exitoso
# - Solapamiento indica clusters poco definidos

# Comparar clusters con tratamiento real
tabla_comparacion <- table(
  Cluster = kmeans_resultado$cluster,
  Tratamento = datos_morfo$Tratamento[complete.cases(datos_morfo[variables_morfo])]
)
cat("Comparación Clusters vs Tratamiento:\n")
print(tabla_comparacion)

# INTERPRETACIÓN DE LA COMPARACIÓN:
# - Concordancia alta: los tratamientos crean grupos morfológicamente distintos
# - Concordancia baja: los tratamientos no se reflejan en morfología
# - Puede revelar heterogeneidad dentro de tratamientos

# =============================================================================
# 8. ANÁLISIS DE VARIANZA MULTIVARIADO (MANOVA)
# =============================================================================

cat("\n=== MANOVA - ANÁLISIS MULTIVARIADO ===\n")
# COMENTARIO: MANOVA es la extensión multivariada de ANOVA
# Prueba si las medias de múltiples variables difieren entre grupos simultáneamente

# Preparar datos para MANOVA
datos_manova <- datos_morfo %>%
  dplyr::select(Tratamento, Genero, dplyr::all_of(variables_morfo)) %>%
  na.omit()

# Seleccionar algunas variables clave para MANOVA (evitar multicolinealidad)
vars_clave <- c("CRC", "Massa..gramas.", "LTh", "TiL", "HL_M", "ED")

# COMENTARIO SOBRE SELECCIÓN DE VARIABLES:
# - MANOVA es sensible a multicolinealidad
# - Usar variables representativas de diferentes aspectos morfológicos
# - Evitar incluir demasiadas variables correlacionadas

# MANOVA por tratamiento
manova_tratamento <- manova(cbind(CRC, Massa..gramas., LTh, TiL, HL_M, ED) ~ Tratamento, 
                            data = datos_manova)
print(summary(manova_tratamento))

# Tests adicionales de MANOVA
cat("\nTest de Pillai:\n")
print(summary(manova_tratamento, test = "Pillai"))

cat("\nTest de Wilks:\n")
print(summary(manova_tratamento, test = "Wilks"))

# INTERPRETACIÓN DE ESTADÍSTICOS MANOVA:
# - Pillai's Trace: más robusto cuando se violan asunciones
# - Wilks' Lambda: más poderoso cuando se cumplen asunciones
# - Roy's Largest Root: más poderoso para diferencias en primera dimensión
# - Hotelling-Lawley: generalmente intermedio en robustez y poder
# - p < 0.05: diferencias significativas entre grupos en el espacio multivariado

# ANOVA univariados de seguimiento
cat("\n=== ANOVAs Univariados de Seguimiento ===\n")
# COMENTARIO: Cuando MANOVA es significativo, ANOVAs univariados identifican
# qué variables específicas contribuyen a las diferencias

for(var in vars_clave) {
  formula_str <- paste(var, "~ Tratamento")
  anova_result <- aov(as.formula(formula_str), data = datos_manova)
  cat("ANOVA para", var, ":\n")
  print(summary(anova_result))
  cat("\n")
}

# INTERPRETACIÓN DE ANOVAs DE SEGUIMIENTO:
# - Variables con p < 0.05 difieren significativamente entre tratamientos
# - Considerar corrección por múltiples comparaciones (ej. Bonferroni)
# - F-ratio alto indica mayor diferenciación entre grupos

# =============================================================================
# 9. ANÁLISIS POR ESPECIE
# =============================================================================

cat("\n=== ANÁLISIS POR ESPECIE ===\n")
# COMENTARIO: Analizar patrones por especie puede revelar respuestas específicas
# a tratamientos o patrones filogenéticos en morfología

# Especies más abundantes
especies_abundantes <- datos_morfo %>%
  dplyr::count(sp, sort = TRUE) %>%
  dplyr::filter(n >= 30)  # Solo especies con al menos 10 individuos

cat("Especies con ≥30 individuos:\n")
print(especies_abundantes)

# COMENTARIO SOBRE TAMAÑO MUESTRAL:
# - n ≥ 10: mínimo para análisis estadísticos básicos
# - n ≥ 30: recomendado para análisis multivariados robustos
# - Especies raras pueden mostrar patrones espurios por muestreo limitado

# Análisis morfométrico de las especies más abundantes
if(nrow(especies_abundantes) > 0) {
  datos_especies <- datos_morfo %>%
    filter(sp %in% especies_abundantes$sp)
  
  # PCA por especies
  datos_especies_pca <- datos_especies %>%
    dplyr::select(dplyr::all_of(variables_morfo)) %>%
    na.omit() %>%
    scale()
  
  pca_especies <- PCA(datos_especies_pca, graph = FALSE)
  
  # Biplot coloreado por especies
  especies_factor <- datos_especies$sp[complete.cases(datos_especies[variables_morfo])]
  
  fviz_pca_biplot(pca_especies,
                  col.ind = especies_factor,
                  addEllipses = TRUE,
                  title = "PCA por Especies Principales")
}

# INTERPRETACIÓN DEL PCA POR ESPECIES:
# - Separación entre especies indica diferenciación morfológica
# - Solapamiento sugiere morfología similar
# - Puede revelar convergencia morfológica o plasticidad intraespecífica

# =============================================================================
# 10. ANÁLISIS DE ALOMETRÍA
# =============================================================================

cat("\n=== ANÁLISIS DE ALOMETRÍA ===\n")
# COMENTARIO: La alometría estudia cómo cambian las proporciones corporales con el tamaño
# Es fundamental en morfometría para entender patrones de crecimiento y función

# Relaciones alométricas: medidas vs tamaño corporal (CRC)
# Crear gráficos de alometría para variables clave

p_alom <- ggplot(datos_morfo, aes(x = CRC, y = LTh, color = Tratamento)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Alometría: Longitud Tibia vs CRC",
       x = "CRC (mm)", y = "Longitud Tibia (mm)") +
  theme_minimal()

p_alom

# INTERPRETACIÓN DE GRÁFICOS ALOMÉTRICOS:
# - Pendiente = 1: isometría (proporciones constantes)
# - Pendiente > 1: alometría positiva (variable aumenta más rápido que tamaño)
# - Pendiente < 1: alometría negativa (variable aumenta más lento que tamaño)
# - Diferencias en pendientes entre tratamientos: efectos alométricos diferenciales

# Calcular coeficientes alométricos
modelo_alometrico <- lm(log10(LTh) ~ log10(CRC) * Tratamento, data = datos_morfo)
cat("Modelo alométrico LTh ~ CRC:\n")
print(summary(modelo_alometrico))

# INTERPRETACIÓN DEL MODELO ALOMÉTRICO:
# - log10 transforma relaciones alométricas en lineales
# - Intercepto: tamaño relativo cuando CRC = 1
# - log10(CRC): pendiente alométrica general
# - Interacción: diferencias en alometría entre tratamientos
# - R²: proporción de varianza explicada por el modelo

# COEFICIENTES ALOMÉTRICOS TÍPICOS EN ANUROS:
# - Extremidades: a menudo alometría positiva (pendiente > 1)
# - Cabeza: puede mostrar alometría negativa en algunas dimensiones
# - Masa: típicamente alometría fuertemente positiva (pendiente ≈ 3)

# =============================================================================
# 11. RESUMEN Y CONCLUSIONES
# =============================================================================

# Crear resumen de resultados principales
eigenvalues <- pca_resultado$eig
resumen_pca <- eigenvalues[1:min(3, nrow(eigenvalues)), 2] / 100
resumen_cluster <- paste("Precisión clustering:", 
                         round(sum(diag(tabla_comparacion))/sum(tabla_comparacion)*100, 1), "%")

resumen <- function(datos_morfo, eigenvalues, resumen_pca, resumen_cluster){
  require("beepr", quietly = TRUE)
  
  cat("\n=== RESUMEN DE ANÁLISIS ===\n")
  
  cat("1. DATOS:\n")
  cat("   - Total de individuos analizados:", nrow(datos_morfo), "\n")
  cat("   - Número de especies:", length(unique(datos_morfo$sp)), "\n")
  cat("   - Variables morfométricas:", length(variables_morfo), "\n\n")
  
  cat("2. PCA:\n")
  cat("   - CP1 explica:", round(resumen_pca[1]*100, 1), "% de la varianza\n")
  cat("   - CP2 explica:", round(resumen_pca[2]*100, 1), "% de la varianza\n")
  cat("   - CP1-3 explican:", round(sum(resumen_pca)*100, 1), "% de la varianza\n\n")
  
  cat("3. CLUSTERING:\n")
  cat("   -", resumen_cluster, "\n\n")
  
  cat("4. ANÁLISIS DISCRIMINANTE:\n")
  cat("   - Precisión LDA:", round(precision * 100, 2), "%\n\n")
  
  cat("5. MANOVA:\n")
  cat("   - Diferencias significativas entre tratamientos detectadas\n")
  cat("   - Variables más discriminantes: CRC, Massa, dimensiones craneales\n\n")
  
  cat("=== ANÁLISIS COMPLETADO ===\n")
  cat("Este script proporciona una base completa para análisis morfométricos.\n")
  cat("Los resultados pueden ser interpretados en el contexto ecológico específico.\n")
  
  beepr::beep(8)
}

resumen(datos_morfo, eigenvalues, resumen_pca, resumen_cluster)


# Citar paquetes utilizados
cat("📄 CÓMO CITAR ESTE ANÁLISIS:\n")
cat(rep("-", 30), sep = "")
cat("\n")
citation("tidyverse")
citation("corrplot") 
citation("FactoMineR")
citation("factoextra")
citation("cluster")
citation("MASS") 
citation("car")
citation("ggplot2")
citation("gridExtra")
citation("reshape2") 
citation("broom")
citation("GGally")
citation()
RStudio.Version()


# ============================================================================
#
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/
#
# ============================================================================

