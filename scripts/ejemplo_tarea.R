# ===============================================================
# Curso R :)
# Ejemplo Tarea con datos
# ===============================================================

# Dra. Fernanda Rodrigues de Avila 
# <https://avilaf.github.io/>
# fernandar.avila@gmail.com

# Fuente de datos:
# https://avilaf.github.io/meus_artigos/Diet%20of%20Odontophrynus%20americanus%20in%20southern%20Atlantic%20Forest%20of%20Brazi.pdf

## 1. ANÁLISIS EXPLORATORIOS INICIALES

### 1.1 Estadísticas Descriptivas

# Cargar bibliotecas necesarias
library(tidyverse)    # Manipulación y transformación de datos (dplyr, ggplot2, tidyr)
library(vegan)        # Análisis de diversidad ecológica y estadísticas multivariadas
library(ggplot2)      # Creación de gráficos avanzados y visualizaciones
library(gridExtra)    # Organización y combinación de múltiples gráficos
library(corrplot)     # Visualización de matrices de correlación
library(RColorBrewer) # Paletas de colores predefinidas para gráficos

# Cargar datos
datos <- read.csv("https://raw.githubusercontent.com/avilaf/curso_r/refs/heads/main/datos/dados_odonto.csv")

# Resumen de las variables por población
estadisticas_resumen <- datos %>%
  group_by(pop) %>%
  summarise_all(list(media = mean, desv_est = sd, mediana = median), na.rm = TRUE)

# Visualizar estructura de los datos
str(datos)
head(datos)

# 1.2 Preparación de los Datos
# Separar variables de volumen y número
columnas_volumen <- c("gastropoda_v", "araneae_v", "coleoptera_v", "hymenoptera_v", 
                      "orthoptera_v", "lepidoptera_larva_v", "isopoda_v", "chilopoda_v", 
                      "diplopoda_v", "lepidoptera_v", "anelida_v", "dermaptera_v", 
                      "blattodea_v", "neoroptera_v")

columnas_numero <- c("gastropoda_n", "araneae_n", "coleoptera_n", "hymenoptera_n", 
                     "orthoptera_n", "lepidoptera_larva_n", "isopoda_n", "chilopoda_n", 
                     "diplopoda_n", "lepidoptera_n", "anelida_n", "dermaptera_n", 
                     "blattodea_n", "neoroptera_n")

# Crear matriz de composición de la dieta (volumen)
matriz_volumen <- datos[, columnas_volumen]
matriz_numero <- datos[, columnas_numero]

# Agregar identificación de la población
matriz_volumen$pop <- datos$pop
matriz_numero$pop <- datos$pop

# 2. GRÁFICOS DE COMPOSICIÓN DE LA DIETA
# 2.1 Gráfico de Barras - Composición Media de la Dieta
# Preparar datos para gráfico de barras
dieta_resumen <- datos %>%
  select(pop, all_of(columnas_volumen)) %>%
  group_by(pop) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  pivot_longer(-pop, names_to = "item_alimentario", values_to = "volumen_medio") %>%
  mutate(item_alimentario = str_remove(item_alimentario, "_v"))

# Gráfico de barras
p1 <- ggplot(dieta_resumen, aes(x = item_alimentario, 
                                y = volumen_medio, 
                                fill = pop)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Composición Media de la Dieta por Volumen",
       x = "Ítem Alimentario", y = "Volumen Medio (mm³)",
       fill = "Población") +
  scale_fill_brewer(type = "qual", palette = "Set2")

p1

# 2.2 Gráfico de Torta/Donut - Proporción de la Dieta
# Calcular proporciones por población

prop_dieta <- datos %>%
  select(pop, all_of(columnas_volumen)) %>%
  group_by(pop) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  pivot_longer(-pop, names_to = "item", values_to = "volumen") %>%
  group_by(pop) %>%
  mutate(proporcion = volumen / sum(volumen),
         item = str_remove(item, "_v")) %>%
  filter(volumen > 0)  # Remover ítems no consumidos

# Gráfico de torta para cada población
p2 <- ggplot(prop_dieta, aes(x = "", y = proporcion, fill = item)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~pop, labeller = labeller(pop = c("A" = "Población A", "B" = "Población B"))) +
  theme_void() +
  labs(title = "Proporción de los Ítems Alimentarios por Población",
       fill = "Ítem Alimentario")

p2

# 3. ANÁLISIS DE DIVERSIDAD
# 3.1 Índices de Diversidad
# Calcular índices de diversidad

diversidad_shannon <- datos %>%
  select(pop, all_of(columnas_volumen)) %>%
  group_by(pop) %>%
  group_modify(~ {
    matriz <- .x[, -1]
    shannon <- diversity(matriz, index = "shannon", MARGIN = 1)
    simpson <- diversity(matriz, index = "simpson", MARGIN = 1)
    riqueza <- specnumber(matriz, MARGIN = 1)
    data.frame(
      shannon = mean(shannon, na.rm = TRUE),
      simpson = mean(simpson, na.rm = TRUE),
      riqueza = mean(riqueza, na.rm = TRUE)
    )
  })

# Gráfico de diversidad
div_long <- diversidad_shannon %>%
  pivot_longer(-pop, names_to = "indice", values_to = "valor")

p3 <- ggplot(div_long, aes(x = pop, y = valor, fill = pop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~indice, scales = "free_y", 
             labeller = labeller(indice = c("shannon" = "Diversidad Shannon",
                                            "simpson" = "Diversidad Simpson",
                                            "riqueza" = "Riqueza de Ítems"))) +
  theme_minimal() +
  labs(title = "Índices de Diversidad por Población",
       x = "Población", y = "Valor del Índice", fill = "Población") +
  scale_fill_brewer(type = "qual", palette = "Set2")

p3

# 3.2 Curvas de Rarefacción
# Curvas de rarefacción

# install.packages("iNEXT")
library(iNEXT)

# Preparar datos para iNEXT (removiendo individuos con estómagos vacíos)
lista_comunidades <- list()

for(poblacion in unique(datos$pop)) {
  subset_pop <- datos[datos$pop == poblacion, columnas_volumen]
  # Remover individuos con estómagos vacíos
  subset_pop <- subset_pop[rowSums(subset_pop, na.rm = TRUE) > 0, ]
  if(nrow(subset_pop) > 0) {
    lista_comunidades[[paste("Población", poblacion)]] <- colSums(subset_pop, na.rm = TRUE)
  }
}

# Generar curvas de rarefacción
rarefaccion <- iNEXT(lista_comunidades, q = 0, datatype = "abundance")

p_rarefaccion <- ggiNEXT(rarefaccion) + 
  theme_minimal() +
  labs(title = "Curvas de Rarefacción - Riqueza de Ítems Alimentarios",
       x = "Tamaño de la Muestra", y = "Riqueza de Ítems")

# 4. ANÁLISIS MULTIVARIADOS
# 4.1 ACP (Análisis de Componentes Principales)
# ACP basado en volumen (removiendo individuos con estómagos vacíos)

# Filtrar datos con estómagos no vacíos
datos_filtrados <- datos[datos$vol_total > 0, ]

# Crear matriz de composición por volumen
acp_datos <- datos_filtrados[, columnas_volumen]

# Reemplazar NA por 0, sumar 1 y aplicar log
acp_datos[is.na(acp_datos)] <- 0
acp_datos <- acp_datos + 1
acp_datos_log <- log(acp_datos)

# Filtrar filas con suma cero y sin NA
filas_validas <- complete.cases(acp_datos_log) & rowSums(acp_datos_log, na.rm = TRUE) > 0
datos_final <- datos_filtrados[filas_validas, ]
acp_datos_final <- acp_datos_log[filas_validas, ]

# 4.1 ACP
acp_resultado <- prcomp(acp_datos_final, scale. = F, center = TRUE)

# DataFrame para gráfico
acp_df <- data.frame(acp_resultado$x[, 1:2],
                     pop = datos_final$pop,
                     vol_total = datos_final$vol_total)

# Gráfico ACP
p4 <- ggplot(acp_df, aes(x = PC1, 
                         y = PC2, 
                         color = pop, 
                         size = vol_total)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(aes(fill = pop), 
               alpha = 0.2, 
               geom = "polygon", 
               show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "ACP - Composición de la Dieta (datos log-transformados)",
    x = paste0("CP1 (", round(summary(acp_resultado)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("CP2 (", round(summary(acp_resultado)$importance[2, 2] * 100, 1), "%)"),
    color = "Población", size = "Volumen Total"
  ) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_size_continuous(range = c(2, 6))

p4

# 4.2 NMDS
nmds_resultado <- metaMDS(acp_datos_final, distance = "bray", k = 2, trymax = 100)

# DataFrame para gráfico NMDS
nmds_df <- data.frame(
  NMDS1 = nmds_resultado$points[, 1],
  NMDS2 = nmds_resultado$points[, 2],
  pop = datos_final$pop,
  vol_total = datos_final$vol_total
)

# Gráfico NMDS
p5 <- ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, color = pop, size = vol_total)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(aes(fill = pop), alpha = 0.2, geom = "polygon", show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = paste("NMDS - Estrés:", round(nmds_resultado$stress, 3)),
    color = "Población", size = "Volumen Total"
  ) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_size_continuous(range = c(2, 6))

p5

# 5. ANÁLISIS ESTADÍSTICOS

# 5.1 PERMANOVA
permanova_resultado <- adonis2(acp_datos_final ~ pop, data = datos_final, 
                               method = "bray", permutations = 999)
print(permanova_resultado)

# 5.2 Prueba de Homogeneidad de Dispersiones (betadisper)
dist_bray <- vegdist(acp_datos_final, method = "bray")
betadisper_resultado <- betadisper(dist_bray, datos_final$pop)
permutest_resultado <- permutest(betadisper_resultado)
print(permutest_resultado)

# 5.3 ANOSIM
anosim_resultado <- anosim(acp_datos_final, datos_final$pop, distance = "bray")
print(anosim_resultado)

# 6. GRÁFICOS COMPARATIVOS ESPECÍFICOS
# 6.1 Boxplots de los Principales Ítems Alimentarios
# Identificar ítems más abundantes (por volumen total)

items_principales <- datos %>%
  select(all_of(columnas_volumen)) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "item", values_to = "total") %>%
  arrange(desc(total)) %>%
  slice_head(n = 8) %>%  # Top 8 ítems
  pull(item)

# Boxplots de los principales ítems
datos_long <- datos %>%
  select(pop, all_of(items_principales)) %>%
  pivot_longer(-pop, names_to = "item", values_to = "volumen") %>%
  mutate(item = str_remove(item, "_v"),
         item = str_replace_all(item, "_", " "),
         item = str_to_title(item))

p6 <- ggplot(datos_long, aes(x = pop, y = volumen, fill = pop)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  facet_wrap(~item, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Distribución de los Principales Ítems Alimentarios",
       x = "Población", y = "Volumen (mm³)", fill = "Población") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme(legend.position = "bottom")

p6

# 6.2 Correlación entre Volumen y Número
# Correlación volumen vs número para cada ítem

correlaciones <- data.frame()
for(i in 1:length(columnas_volumen)) {
  col_vol <- columnas_volumen[i]
  col_num <- columnas_numero[i]
  
  # Filtrar datos donde tanto volumen como número > 0
  datos_item <- datos[datos[[col_vol]] > 0 & datos[[col_num]] > 0, ]
  
  if(nrow(datos_item) > 3) {  # Mínimo de observaciones para correlación
    cor_total <- cor(datos_item[[col_vol]], datos_item[[col_num]], use = "complete.obs")
    
    # Correlaciones por población
    datos_A <- datos_item[datos_item$pop == "A", ]
    datos_B <- datos_item[datos_item$pop == "B", ]
    
    cor_A <- if(nrow(datos_A) > 2) cor(datos_A[[col_vol]], datos_A[[col_num]], use = "complete.obs") else NA
    cor_B <- if(nrow(datos_B) > 2) cor(datos_B[[col_vol]], datos_B[[col_num]], use = "complete.obs") else NA
    
    correlaciones <- rbind(correlaciones, data.frame(
      item = gsub("_v$", "", col_vol),
      cor_total = cor_total,
      cor_A = cor_A,
      cor_B = cor_B,
      n_obs = nrow(datos_item)
    ))
  }
}

# Gráfico de correlaciones (solo ítems con correlaciones válidas)
if(nrow(correlaciones) > 0) {
  cor_long <- correlaciones %>%
    filter(!is.na(cor_total)) %>%
    pivot_longer(c(cor_total, cor_A, cor_B), names_to = "grupo", values_to = "correlacion") %>%
    mutate(grupo = case_when(
      grupo == "cor_total" ~ "Total",
      grupo == "cor_A" ~ "Población A",
      grupo == "cor_B" ~ "Población B",
      TRUE ~ grupo),
      item = str_replace_all(item, "_", " "),
      item = str_to_title(item)) %>%
    filter(!is.na(correlacion))
  
  p7 <- ggplot(cor_long, aes(x = reorder(item, correlacion), y = correlacion, fill = grupo)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Correlación Volumen vs Número por Ítem Alimentario",
         x = "Ítem Alimentario", y = "Correlación (r)", fill = "Grupo") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
}

p7

# 7. PANEL DE GRÁFICOS FINAL
# Combinar gráficos principales

grid.arrange(p1, p3, p4, p7, ncol = 2, nrow = 2,
             top = "Análisis Comparativo de la Dieta entre Poblaciones A y B")

# Guardar resultados
ggsave("analisis_dieta_anfibios.png", width = 16, height = 12, dpi = 300)

# 8. Referencias Bibliográficas:

citation()

RStudio.Version()

citation("tidyverse")
citation("vegan")
citation("ggplot2")
citation("gridExtra")
citation("corrplot")
citation("RColorBrewer")

# R version 4.5.0 (2025-04-11 ucrt)

# Dra. Fernanda Rodrigues de Avila 
# <https://avilaf.github.io/>
# fernandar.avila@gmail.com