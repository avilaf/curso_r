# Boxplot com os dados de Odontophrynus asper do exercício 2, agora com ggplot2

## Instalar y cargar paquetes: --------------------------------------------

# install.packages("readr")
library(readr)

# install.packages("ggplot2")
library(ggplot2)


# Cargar los datos

# Aqui vamos abrir directamente desde el repositorio en linea de GitHub:

url <- "https://raw.githubusercontent.com/avilaf/curso_r/refs/heads/main/datos/dados_odonto.csv"


datos_odonto <- readr::read_csv(url, locale = locale(encoding = "UTF-8"))


# Verificar visualmente si los datos fueron ingresados correctamente:

head(datos_odonto) # evaluar encabezado

datos_odonto$vol_total # evaluar variable respuesta

datos_odonto$sex # evaluar variable predictora sexo

datos_odonto$pop # evaluar variable predictora poblacion

# Definir el conjunto de datos (data = ) y los ejes (x = población, y = volumen total)

my_g <- ggplot2::ggplot(data = datos_odonto,
                        aes(x = pop, y = vol_total))
my_g

# Defiir geom_boxplot()

my_g + ggplot2::geom_boxplot()


# Cambiar el theme() 

my_g + 
  ggplot2::geom_boxplot() +
  ggplot2::theme_bw()


# Editar colores de linea (color)

ggplot2::ggplot(data = datos_odonto,
                aes(x = pop, 
                    y = vol_total, 
                    color = pop)) + 
  ggplot2::geom_boxplot() +
  ggplot2::theme_bw()

 
# Editar colores de preenchimento (argumento fill)
ggplot2::ggplot(data = datos_odonto,
                aes(x = pop, 
                    y = vol_total,
                    fill = pop)) + 
  ggplot2::geom_boxplot() +
  ggplot2::theme_bw()


# Formatar título, legenda e títulos dos eixos
# install.packages('ggtext')
library(ggtext)

# cambiando la posicion del titulo del grafico con: hjust = 1

ggplot(data = datos_odonto,
                aes(x = pop, 
                    y = vol_total,
                    fill = pop)) + 
  geom_boxplot() +
  
  labs(title ="Dieta de *Odontophrynus asper*", 
       x = "Población", 
       y = "Volumen Total (mm<sup>3</sup>)") +
  theme_bw() +
  theme(plot.title = element_markdown(hjust = 1),
        axis.title = element_markdown())

  

# DESAFIO: É possível ilustrar em um mesmo boxplot os valores 
# para os dois fatores que existem nesse conjunto de dados (sexo e população). 
# Você consegue pensar em uma estratégia para construir esse gráfico?

# definir colores:

my_palete <- c("darkmagenta",
               "darkolivegreen3")

ggplot(data = datos_odonto,
       aes(x = pop,
           y = vol_total,
           fill = sex)) + 
  geom_boxplot() +
  scale_fill_manual(values = my_palete,
                      name = "Sex",
                      labels = c("Female","Male")) +
  labs(title ="Diet of *Odontophrynus asper*", 
       x = "Population", 
       y = "Total volume (mm<sup>3</sup>)") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        axis.title = element_markdown(),
        legend.title = element_text(
          size = 14, face = 2))


# BÔNUS:

# Sugestão para um boxplot mais infromativo :)

mean_rg <- mean(datos_odonto$vol_total)

ggplot(data = datos_odonto,
       aes(x = pop,
           y = vol_total,
           fill = sex)) + 
  geom_boxplot(alpha = 0.7) +
  stat_summary(geom = "point", 
               fun = mean,
               color = "red",
               shape = 8,
               size = 1,
               position =  position_dodge(width = 0.75)) +
  geom_hline(yintercept = mean_rg, linetype = "dashed") + 
  scale_fill_manual(values = my_palete,
                    name = "Sex",
                    labels = c("Female","Male")) +
  labs(title ="Diet of *Odontophrynus asper*", 
       x = "Population", 
       y = "Total volume (mm<sup>3</sup>)") +
  theme_bw() +
  theme(plot.title = element_markdown(),
        axis.title = element_markdown(),
        legend.title = element_text(
          size = 14, face = 2))
