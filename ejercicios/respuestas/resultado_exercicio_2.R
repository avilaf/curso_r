###########################################################################
# Ecologia de la herpetofauna en el ambiente R para no programadores ######
# Exercicios 2 ############################################################
###########################################################################

###########################################################################
# Fernanda R. de Avila ####################################################
# https://avilaf.github.io/ ###############################################
# 21/05/25 ################################################################
###########################################################################



# Instalar y cargar paquetes: --------------------------------------------

# install.packages("readr")
library(readr)


# Directorio --------------------------------------------------------------


path <- "D:/curso_r-main"

setwd(path)

# Cargar datos: ---------------------------------------------------------


# Aqui vamos abrir directamente desde el repositorio en linea de GitHub:

url <- "https://raw.githubusercontent.com/avilaf/curso_r/refs/heads/main/datos/dados_odonto.csv"

datos_odonto <- readr::read_csv(url, locale = locale(encoding = "UTF-8"))


# Verificar visualmente si los datos fueron ingresados correctamente:

head(datos_odonto) # evaluar encabezado

datos_odonto$vol_total # evaluar variable respuesta

datos_odonto$sex # evaluar variable predictora sexo

datos_odonto$pop # evaluar variable predictora poblaci?n



# Visualizar datos: -------------------------------------------------------

# Graficar los diagramas de caja (boxplot) para inspeccion visual de los valores:

# Factor sexo:
(sex_plot <- boxplot(vol_total ~ sex,
                     data = datos_odonto,
                     col = c("indianred", "cadetblue")
)
)

# Factor poblaci?n: 
(pop_plot <- boxplot(vol_total ~ pop,
                     data = datos_odonto,
                     col = c("darkseagreen", "plum4")
)
)

# Analisis: ---------------------------------------------------------------

# Probar los supuestos para el test T:

# Graficar la probabilidad de la distribuci?n normal:

qqnorm(datos_odonto$vol_total, pch = 1, frame = FALSE) # dados
qqline(datos_odonto$vol_total, col = "steelblue", lwd = 2) # reta
# Parece que los datos NO se ajustan bien a la normalidad

# Probar la normalidad con Shapiro-Wilk
# H0: los datos de la muestra provienen de una poblaci?n con distribución normal

shapiro.test(x = datos_odonto$vol_total)
# p < 0,05: RECHAZAMOS el supuesto de normalidad de los datos (H0)

# Ahora la prueba NO PARAM?TRICA

# Factor sexo:
(w_res_sex <- wilcox.test(vol_total ~ sex, 
                          data = datos_odonto,
                          conf.int = TRUE)) # a logical indicating whether a confidence interval should be computed
# No hay diferencia entre los sexos
# p-valor = 0.9838

(w_res_pop <- wilcox.test(vol_total ~ pop, 
                          data = datos_odonto,
                          conf.int = TRUE))
# Hay diferencia entre las poblaciones
# p-valor = 0.005989

# Exportar los graficos: ---------------------------------------------------

# Para el factor sexo:

# 1- Crear la imagen
jpeg('output/grafico_odonto_sex.jpeg')

# 2- Graficar:
sex_plot

# 3- Cerrar el archivo
dev.off()

# Para el factor poblacion:

# 1- Crear la imagen
jpeg('output/grafico_odonto_pop.jpeg')

# 2- Graficar:
pop_plot 

# 3- Cerrar el archivo
dev.off()

# Citas: ---------------------------------------------------------------

citation("readr")

citation()

RStudio.Version()


###########################################################################
# Bonus: Otra opcion de analisis ##########################################
###########################################################################

# Instalar y cargar paquetes: --------------------------------------------

# install.packages(c("ggplot2", "car", "MASS", "lme4"))

library(ggplot2)
library(car)
library(MASS)

require(lme4) # require?


# Datos: ------------------------------------------------------------------

head(datos_odonto)
datos_odonto$pop



# Distribuci?n de los datos: ----------------------------------------------

# Sabemos que la distribucion de los datos no es normal
# Vamos a visualizar los datos antes que nada

ggplot(datos_odonto, 
       aes(pop, vol_total)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(x = "Poblaci?n", 
       y = "Volumen total") +
  theme_bw()

# Verificar distribucion:

# Poisson:
poisson <- MASS::fitdistr(datos_odonto$vol_total,
                          "Poisson")

car::qqp(datos_odonto$vol_total,
         "pois",
         lambda = poisson$estimate)

# Binomial Negativa:
nbinom <- MASS::fitdistr(datos_odonto$vol_total,
                         "Negative Binomial")

car::qqPlot(datos_odonto$vol_total,
            "nbinom",
            size = nbinom$estimate[[1]],
            mu = nbinom$estimate[[2]])


# Modelar: ----------------------------------------------------------------

modelo <- lme4::glmer.nb(formula = vol_total ~ pop + (1|sex),
                         data = datos_odonto)

summary(modelo)

# Citas adicionales: ------------------------------------------------------

citation("ggplot2")
citation("car")
citation("MASS")
citation("lme4")

###########################################################################
# Fin :) ##################################################################
# Fernanda R. de Avila ####################################################
# https://avilaf.github.io/ ###############################################
###########################################################################