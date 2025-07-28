

# =============================================================================
# Ecología de la herpetofauna en el ambiente R para no programadores
# =============================================================================
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/
# =====================================================================
# ANÁLISIS DE VOCALIZACIONES DE ANUROS CON SOUNDSHAPE
# =====================================================================

# =====================================================================
# ¿QUÉ VAMOS A HACER?
# =====================================================================
# Este script nos permite:
# 1. Cargar grabaciones de cantos de ranas (archivos .wav)
# 2. Visualizar espectrogramas (representación visual del sonido)
# 3. Analizar la forma de estos sonidos usando métodos geométricos
# 4. Comparar diferentes especies usando análisis estadísticos

# =====================================================================
# PASO 1: INSTALAR Y CARGAR PAQUETES
# =====================================================================

# ¿Qué es un paquete? Es como una "caja de herramientas" con funciones
# especializadas que otros programadores crearon para nosotros

# Si es la primera vez que usas SoundShape, descomenta la siguiente línea:
# install.packages("SoundShape")

# Cargamos el paquete principal para nuestro análisis
library(SoundShape)

# Mensaje para el usuario
cat("✅ Paquete SoundShape cargado correctamente!\n")
cat("Este paquete nos permite analizar la forma de los sonidos\n\n")

# =====================================================================
# PASO 2: CONFIGURAR DIRECTORIO DE TRABAJO
# =====================================================================

# ¿Qué es el directorio de trabajo? Es la carpeta donde R va a buscar
# nuestros archivos y donde guardará los resultados

# IMPORTANTE: Cambia esta ruta por la carpeta donde tienes tus archivos .wav
path <- "D:/curso_r-main/datos/vocalizaciones"

# Le decimos a R cuál es nuestra carpeta de trabajo
setwd(path)

# Verificamos qué archivos tenemos en la carpeta
archivos_disponibles <- dir()
cat("📁 Archivos encontrados en la carpeta:\n")
print(archivos_disponibles)

# Definimos dónde están nuestros archivos de sonido
wav_at <- path

# Creamos una subcarpeta para guardar los resultados
store_at <- file.path(getwd(), "output")
dir.create(store_at, showWarnings = FALSE) # showWarnings = FALSE evita mensajes si ya existe

cat("\n📂 Carpeta 'output' creada para guardar resultados\n\n")

# =====================================================================
# PASO 3: IMPORTAR Y VISUALIZAR LOS SONIDOS
# =====================================================================

cat("🎵 COMENZANDO EL ANÁLISIS DE VOCALIZACIONES\n")
cat("==========================================\n\n")

# ---------------------------------------------------------------------
# ESPECIE 1: Boana pardalis
# ---------------------------------------------------------------------

cat("🐸 Analizando Boana pardalis (muestra 1)...\n")

# 1. IMPORTAR el archivo de sonido
# tuneR::readWave() lee archivos .wav y los convierte en datos que R puede usar
boana_pardalis <- tuneR::readWave("boana_pardalis.wav")

# Vemos información básica del archivo
cat("Información del archivo:\n")
print(boana_pardalis)

# 2. CREAR ESPECTROGRAMA
# Un espectrograma muestra cómo cambia la frecuencia del sonido a lo largo del tiempo
# Es como una "huella dactilar" del sonido

cat("Creando espectrograma...\n")
seewave::spectro(boana_pardalis,
                 flim = c(0, 1.8),    # flim: rango de frecuencias a mostrar (0 a 1.8 kHz)
                 wl = 512,            # wl: tamaño de ventana para el análisis
                 f = 44100,           # f: frecuencia de muestreo del archivo
                 ovlp = 70,           # ovlp: porcentaje de solapamiento entre ventanas
                 grid = FALSE)        # grid: no mostrar líneas de cuadrícula

# ---------------------------------------------------------------------
# ESPECIE 1: Boana pardalis (segunda muestra)
# ---------------------------------------------------------------------

cat("\n🐸 Analizando Boana pardalis (muestra 2)...\n")

boana_pardalis_2 <- tuneR::readWave("boana_pardalis_2.wav")
print(boana_pardalis_2)

seewave::spectro(boana_pardalis_2,
                 flim = c(0, 1.8),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)

# ---------------------------------------------------------------------
# ESPECIE 2: Boana marginata
# ---------------------------------------------------------------------

cat("\n🐸 Analizando Boana marginata...\n")

boana_marginata <- tuneR::readWave("boana_marginata.wav")
print(boana_marginata)

# Nota: Esta especie tiene frecuencias más altas, por eso flim va hasta 4 kHz
seewave::spectro(boana_marginata,
                 flim = c(0, 4),      # Rango más amplio de frecuencias
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)

# ---------------------------------------------------------------------
# ESPECIE 3: Boana faber (muestra 1)
# ---------------------------------------------------------------------

cat("\n🐸 Analizando Boana faber (muestra 1)...\n")

boana_faber <- tuneR::readWave("boana_faber.wav")
print(boana_faber)

seewave::spectro(boana_faber,
                 flim = c(0, 1.8),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)

# ---------------------------------------------------------------------
# ESPECIE 3: Boana faber (muestra 2)
# ---------------------------------------------------------------------

cat("\n🐸 Analizando Boana faber (muestra 2)...\n")

boana_faber_2 <- tuneR::readWave("boana_faber_2.wav")
print(boana_faber_2)

seewave::spectro(boana_faber_2,
                 flim = c(0, 1.8),
                 wl = 512,
                 f = 44100,
                 ovlp = 70,
                 grid = FALSE)

# =====================================================================
# PASO 4: ALINEAR LOS SONIDOS
# =====================================================================

cat("\n🔄 ALINEANDO LOS SONIDOS\n")
cat("========================\n")
cat("¿Por qué alinear? Para poder comparar sonidos, necesitamos que todos\n")
cat("empiecen en el mismo punto y tengan la misma duración.\n\n")

# align.wave() coloca todos los sonidos al inicio de una ventana temporal
SoundShape::align.wave(wav.at = wav_at,           # Dónde están los archivos originales
                       wav.to = "Aligned",        # Nombre de la carpeta para sonidos alineados
                       time.length = .15,         # Duración en segundos (0.15 seg = 150 ms)
                       flim = c(.1, 5)           # Rango de frecuencias para considerar
)

cat("✅ Sonidos alineados correctamente!\n")
cat("Los archivos alineados están en la carpeta 'Aligned'\n\n")

# Verificamos la alineación creando una visualización 2D
cat("🔍 Verificando la alineación con visualización 2D...\n")

SoundShape::eigensound(analysis.type = "twoDshape",               # Análisis en 2D
                       wav.at = file.path(wav_at, "Aligned"),     # Carpeta con sonidos alineados
                       store.at = store_at,                       # Dónde guardar resultados
                       plot.exp = TRUE,                           # Crear gráficos explicativos
                       flim = c(0, 6),                           # Rango de frecuencias
                       tlim = c(0, .3)                           # Rango de tiempo
)

# =====================================================================
# PASO 5: ANÁLISIS DE FORMA EN 3D (EIGENVECTORES)
# =====================================================================

cat("\n📊 ANÁLISIS DE FORMA EN 3D\n")
cat("==========================\n")
cat("Ahora vamos a tratar cada sonido como una 'superficie 3D'\n")
cat("donde podemos medir su forma geométrica.\n\n")

# eigensound() es la función principal que convierte sonidos en formas 3D
eig_boana <- SoundShape::eigensound(analysis.type = "threeDshape",    # Análisis en 3D
                                    wav.at = file.path(wav_at, "Aligned"), # Sonidos alineados
                                    store.at = store_at,                    # Dónde guardar
                                    plot.exp = TRUE,                        # Crear gráficos
                                    flim = c(0, 6),                        # Frecuencias (0-6 kHz)
                                    tlim = c(0, .3)                        # Tiempo (0-0.3 seg)
)

cat("✅ Análisis 3D completado!\n")
cat("Cada sonido ahora tiene coordenadas geométricas.\n\n")

# Veamos qué contiene nuestro objeto eig_boana
print(eig_boana)

# =====================================================================
# PASO 6: ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# =====================================================================

cat("\n📈 ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)\n")
cat("===============================================\n")
cat("El PCA nos ayuda a encontrar las principales diferencias\n")
cat("entre las formas de los sonidos de diferentes especies.\n\n")

# Primero convertimos los datos 3D a formato 2D para el PCA
# two.d.array() aplana los datos manteniendo la información importante
pca_eig_boana <- stats::prcomp(geomorph::two.d.array(eig_boana))

# Vemos un resumen de los resultados del PCA
cat("📋 Resumen del PCA:\n")
summary(pca_eig_boana)

# Vemos los nombres de nuestras muestras
cat("\n🏷️ Nombres de las muestras:\n")
print(dimnames(eig_boana)[[3]])

# Creamos un vector con los grupos (especies) para cada muestra
sample_gr <- factor(c("faber",      # muestra 1: Boana faber
                      "faber",      # muestra 2: Boana faber  
                      "marginata",  # muestra 3: Boana marginata
                      "pardalis",   # muestra 4: Boana pardalis
                      "pardalis"))  # muestra 5: Boana pardalis

cat("\n🎯 Grupos asignados:\n")
print(sample_gr)

# Configuramos la ventana gráfica
par(mfrow = c(1,1),    # Una sola fila y columna para el gráfico
    mar = c(4,4,1,1))  # Márgenes del gráfico

# Creamos el gráfico del PCA
cat("\n📊 Creando gráfico del PCA...\n")
plot <- SoundShape::pca.plot(pca_eig_boana,           # Datos del PCA
                             groups = sample_gr,       # Grupos de especies
                             conv.hulls = sample_gr,   # Crear envolturas convexas por grupo
                             leg.pos = "bottom",       # Posición de la leyenda
                             cex = 1.2)               # Tamaño de los puntos

cat("✅ Gráfico del PCA creado!\n")
cat("Cada punto representa una vocalización.\n")
cat("Los colores indican las diferentes especies.\n\n")

# =====================================================================
# PASO 7: SUPERFICIES HIPOTÉTICAS
# =====================================================================

cat("\n🏔️ CREANDO SUPERFICIES HIPOTÉTICAS\n")
cat("====================================\n")
cat("Vamos a crear representaciones 3D que muestran:\n")
cat("- La forma 'promedio' de todos los sonidos\n")
cat("- Cómo varían las formas en cada componente principal\n\n")

# SUPERFICIE 1: Forma promedio (consenso)
cat("1️⃣ Creando superficie de la forma promedio...\n")

SoundShape::hypo.surf(eig_boana,
                      PC = "mean",        # "mean" = forma promedio de todas las muestras
                      flim = c(0, 4),     # Rango de frecuencias para mostrar
                      tlim = c(0, 0.8),   # Rango de tiempo para mostrar
                      x.length = 70,      # Resolución en el eje X (tiempo)
                      y.length = 47,      # Resolución en el eje Y (frecuencia)
                      cex.lab = 0.7,      # Tamaño de las etiquetas de los ejes
                      cex.axis = 0.5,     # Tamaño de los números en los ejes
                      cex.main = 1)       # Tamaño del título

# SUPERFICIE 2: Variación en el Componente Principal 1
cat("\n2️⃣ Creando superficies para el Componente Principal 1...\n")
cat("Esto muestra la principal diferencia entre los sonidos.\n")

SoundShape::hypo.surf(eig_boana,
                      PC = 1,             # Componente Principal 1
                      flim = c(0, 4),
                      tlim = c(0, 0.8),
                      x.length = 70,
                      y.length = 47,
                      cex.lab = 0.7,
                      cex.axis = 0.5,
                      cex.main = 1)

# SUPERFICIE 3: Variación en el Componente Principal 2  
cat("\n3️⃣ Creando superficies para el Componente Principal 2...\n")
cat("Esto muestra la segunda diferencia más importante.\n")

SoundShape::hypo.surf(eig_boana,
                      PC = 2,             # Componente Principal 2
                      flim = c(0, 4),
                      tlim = c(0, 0.8),
                      x.length = 70,
                      y.length = 47,
                      cex.lab = 0.7,
                      cex.axis = 0.5,
                      cex.main = 1)

# =====================================================================
# PASO 8: INFORMACIÓN FINAL Y REFERENCIAS
# =====================================================================
informacion <- function(){
  cat("\n🎉 ¡ANÁLISIS COMPLETADO!\n")
  cat("=========================\n")
  cat("Resumen de lo que hicimos:\n")
  cat("1. ✅ Cargamos y visualizamos 5 vocalizaciones de 3 especies de ranas\n")
  cat("2. ✅ Alineamos los sonidos para poder compararlos\n")
  cat("3. ✅ Convertimos cada sonido en una forma geométrica 3D\n")
  cat("4. ✅ Usamos PCA para identificar las principales diferencias\n")
  cat("5. ✅ Creamos superficies que muestran cómo varían las formas\n\n")
  
  cat("📁 Revisa la carpeta 'output' para ver todos los resultados guardados.\n\n")
  
  cat("📚 REFERENCIAS Y CRÉDITOS:\n")
  cat("===========================\n")
  beepr::beep(8)
  }

informacion() 


# Información sobre el paquete SoundShape

citation("SoundShape")
citation()
RStudio.Version()


# ============================================================================
#
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/
#
# ============================================================================