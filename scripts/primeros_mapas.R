# =============================================================================
# Ecología de la herpetofauna en el ambiente R para no programadores
# =============================================================================
# PRIMEROS MAPAS EN R
# =============================================================================
# Dra. Fernanda Rodrigues de Avila
# mail: fernandar.avila@gmail.com
# https://avilaf.github.io/


# ================================================================
# TUTORIAL PASO A PASO: MAPAS DE REPTILES CON DATOS DE GBIF
# 
# Este script te enseña a crear mapas de distribución de especies
# usando datos de biodiversidad de GBIF (Global Biodiversity Information Facility)
# 
# Especie de ejemplo: Salvator merianae (Lagarto overo)
# País: Uruguay
# 
# ================================================================

# ¿QUÉ VAMOS A APRENDER?
# 1. Descargar datos de biodiversidad desde GBIF
# 2. Limpiar y preparar datos geográficos
# 3. Crear mapas básicos y avanzados
# 4. Hacer análisis temporales
# 5. Exportar datos y resultados

cat("=== BIENVENIDO AL TUTORIAL DE MAPAS DE BIODIVERSIDAD ===\n")
cat("Aprenderás a crear mapas profesionales de distribución de especies\n")
cat("usando R y datos reales de GBIF\n\n")

# ================================================================
# PASO 1: PREPARAR EL ENTORNO DE TRABAJO
# ================================================================

cat("PASO 1: Preparando el entorno de trabajo...\n")

# ¿Qué son los paquetes de R?
# Los paquetes son conjuntos de funciones que nos permiten hacer tareas específicas
# Es como tener una caja de herramientas especializada para cada trabajo

# Lista de paquetes que necesitamos (explicación de cada uno):
paquetes_necesarios <- c(
  "rgbif",             # Para descargar datos de GBIF (biodiversidad)
  "ggplot2",           # Para crear gráficos y mapas hermosos
  "sf",                # Para trabajar con datos geográficos (mapas)
  "rnaturalearth",     # Para obtener mapas base de países
  "rnaturalearthdata", # Datos complementarios para mapas
  "dplyr",             # Para manipular y limpiar datos fácilmente
  "lubridate",         # Para trabajar con fechas
  "beepr"              # Sorpresa :)
)

# Función que instala solo los paquetes que no tenemos
instalar_si_falta <- function(paquetes) {
  # Ver cuáles paquetes NO están instalados
  faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
  
  # Si hay paquetes faltantes, instalarlos
  if(length(faltantes) > 0) {
    cat("Instalando paquetes faltantes:", paste(faltantes, collapse = ", "), "\n")
    install.packages(faltantes)
  } else {
    cat("Todos los paquetes ya están instalados ✓\n")
  }
}

# Ejecutar la instalación (descomenta la siguiente línea si es tu primera vez)
# instalar_si_falta(paquetes_necesarios)

# Cargar todas las librerías en memoria
cat("Cargando librerías...\n")
library(rgbif)             # ← Conexión con GBIF
library(ggplot2)           # ← Motor de gráficos
library(sf)                # ← Herramientas geográficas
library(rnaturalearth)     # ← Mapas del mundo
library(rnaturalearthdata) # ← Datos de mapas
library(dplyr)             # ← Manipulación de datos
library(lubridate)         # ← Manejo de fechas
library(beepr)

cat("✓ Todas las librerías cargadas correctamente\n")
cat("Fecha de hoy:", as.character(Sys.Date()), "\n\n")

# ================================================================
# PASO 2: DESCARGAR DATOS DE BIODIVERSIDAD DESDE GBIF
# ================================================================

cat("PASO 2: Descargando datos de biodiversidad...\n")

# ¿Qué es GBIF?
# GBIF es una base de datos mundial gratuita con millones de registros
# de plantas, animales, hongos y microorganismos de todo el planeta

# Información sobre nuestra especie de interés
especie_nombre <- "Salvator merianae"
especie_comun <- "Lagarto overo"
pais_codigo <- "UY"  # Código de Uruguay
pais_nombre <- "Uruguay"

cat("Buscando registros de:", especie_nombre, "(", especie_comun, ")\n")
cat("En el país:", pais_nombre, "\n")

# Descargamos los datos usando la función occ_search()
datos_crudos <- occ_search(
  country = pais_codigo,        # ← País donde buscar
  taxonKey = 5227370,          # ← Código único de la especie en GBIF
  hasCoordinate = TRUE,        # ← Solo registros con coordenadas GPS
  limit = 2000                 # ← Máximo 2000 registros
)

# Verificar si obtuvimos datos
if(is.null(datos_crudos$data) || nrow(datos_crudos$data) == 0) {
  stop("❌ No se encontraron datos. Verifica tu conexión a internet.")
}

# Mostrar información básica
cat("✓ Descarga exitosa:\n")
cat("  - Total de registros encontrados:", nrow(datos_crudos$data), "\n")
cat("  - Información disponible en", ncol(datos_crudos$data), "columnas\n")

# Ver las primeras columnas para entender qué datos tenemos
cat("  - Algunas columnas importantes:\n")
columnas_importantes <- c("species", "decimalLongitude", "decimalLatitude", 
                          "eventDate", "country", "locality",
                          "basisOfRecord")
for(col in columnas_importantes) {
  if(col %in% names(datos_crudos$data)) {
    cat("    ✓", col, "\n")
  } else {
    cat("    ⚠", col, "(no disponible)\n")
  }
}

# ================================================================
# PASO 3: LIMPIAR Y PREPARAR LOS DATOS
# ================================================================

cat("\nPASO 3: Limpiando y preparando los datos...\n")

# ¿Por qué limpiar datos?
# Los datos crudos suelen tener errores, valores faltantes o coordenadas incorrectas
# Necesitamos filtrar solo los datos de buena calidad

cat("Aplicando filtros de calidad...\n")

# Proceso de limpieza paso a paso:
datos_limpios <- datos_crudos$data %>%
  # 1. Seleccionar solo las columnas que necesitamos
  dplyr::select(species, decimalLongitude, decimalLatitude, eventDate, 
         locality, institutionCode, basisOfRecord) %>%
  
  # 2. Filtrar registros con coordenadas válidas
  filter(
    !is.na(decimalLongitude),           # ← Que tenga longitud
    !is.na(decimalLatitude),            # ← Que tenga latitud
    decimalLongitude != 0,              # ← No sea 0,0 (error común)
    decimalLatitude != 0,
    # 3. Que esté dentro de los límites aproximados de Uruguay
    decimalLatitude >= -35 & decimalLatitude <= -30,    # ← Latitud de Uruguay
    decimalLongitude >= -58.5 & decimalLongitude <= -53  # ← Longitud de Uruguay
  ) %>%
  
  # 4. Agregar información de tiempo
  mutate(
    # Extraer el año de la fecha
    year = year(as.Date(eventDate)),
    # Crear períodos de tiempo más fáciles de leer
    periodo = case_when(
      is.na(year) ~ "Sin fecha",
      year < 1990 ~ "Antes de 1990",
      year >= 1990 & year < 2000 ~ "1990-1999",
      year >= 2000 & year < 2010 ~ "2000-2009", 
      year >= 2010 & year < 2020 ~ "2010-2019",
      year >= 2020 ~ "2020 en adelante"
    ),
    # Crear décadas
    decada = ifelse(!is.na(year), paste0(floor(year / 10) * 10, "s"), "Sin fecha")
  )

# Convertir a formato geográfico (muy importante!)
# Esto le dice a R que estos datos tienen coordenadas GPS
datos_geograficos <- datos_limpios %>%
  sf::st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),  # ← Columnas de coordenadas
    crs = 4326  # ← Sistema de coordenadas GPS estándar (WGS84)
  )

# Mostrar resultados de la limpieza
cat("✓ Limpieza completada:\n")
cat("  - Registros originales:", nrow(datos_crudos$data), "\n")
cat("  - Registros después de limpieza:", nrow(datos_geograficos), "\n")
cat("  - Registros eliminados:", nrow(datos_crudos$data) - nrow(datos_geograficos), "\n")

if(nrow(datos_geograficos) > 0) {
  rango_años <- range(datos_limpios$year, na.rm = TRUE)
  cat("  - Período temporal:", rango_años[1], "a", rango_años[2], "\n")
}

# ================================================================
# PASO 4: OBTENER EL MAPA BASE DE URUGUAY
# ================================================================

cat("\nPASO 4: Obteniendo mapa base del país...\n")

# ¿Qué es un mapa base?
# Es el contorno del país donde vamos a mostrar nuestros datos
# Como el fondo de un mapa donde marcaremos los puntos

uruguay_mapa <- rnaturalearth::ne_countries(scale = "medium",           # ← Resolución media (buena calidad)
  country = "Uruguay",        # ← El país que queremos
  returnclass = "sf"          # ← Formato geográfico
)

if(is.null(uruguay_mapa) || nrow(uruguay_mapa) == 0) {
  stop("❌ No se pudo descargar el mapa de Uruguay. Verifica tu conexión.")
}

cat("✓ Mapa base de Uruguay descargado correctamente\n")

# ================================================================
# PASO 5: CREAR NUESTRO PRIMER MAPA
# ================================================================

cat("\nPASO 5: Creando el primer mapa...\n")

# ggplot2 funciona por capas, como un sándwich:
# 1. Base (mapa del país)
# 2. Puntos (nuestros datos)
# 3. Etiquetas y formato

primer_mapa <- ggplot() +
  # CAPA 1: El mapa base (fondo)
  geom_sf(
    data = uruguay_mapa, 
    fill = "gray95",      # ← Color de relleno del país
    color = "darkgray",      # ← Color del borde
    size = 0.5               # ← Grosor del borde
  ) +
  
  # CAPA 2: Nuestros puntos de datos
  geom_sf(
    data = datos_geograficos, 
    color = "red",           # ← Color de los puntos
    size = 2,                # ← Tamaño de los puntos
    alpha = 0.7              # ← Transparencia (0=invisible, 1=opaco)
  ) +
  
  # CAPA 3: Formato y etiquetas
  theme_minimal() +          # ← Estilo limpio y moderno
  labs(
    title = paste("Distribución de", especie_nombre, "\nen", pais_nombre),
    subtitle = paste("Fuente: GBIF |", nrow(datos_geograficos), "registros"),
    caption = paste("Elaborado el", Sys.Date()),
    x = "Longitud (Oeste-Este)", 
    y = "Latitud (Sur-Norte)"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(color = "gray60")
  )

# Mostrar el mapa
print(primer_mapa)

# Guardar el mapa como imagen de alta calidad
ggsave(
  filename = "mi_primer_mapa_especies.png", 
  plot = primer_mapa,
  width = 10,        # ← Ancho en pulgadas
  height = 8,        # ← Alto en pulgadas  
  dpi = 300,         # ← Resolución (300 = calidad publicación)
  bg = "white"       # ← Fondo blanco
)

cat("✓ Primer mapa creado y guardado como 'mi_primer_mapa_especies.png'\n")

# ================================================================
# PASO 6: ANÁLISIS TIPO DE REGISTRO
# ================================================================

cat("\nPASO 6: Creando análisis de bais of record...\n")

mapa_registro_especies <- ggplot() +
  # CAPA 1: El mapa base (fondo)
  geom_sf(
    data = uruguay_mapa, 
    fill = "gray95",      # ← Color de relleno del país
    color = "darkgray",      # ← Color del borde
    size = 0.5               # ← Grosor del borde
  ) +
  
  # CAPA 2: Nuestros puntos de datos
  geom_sf(
    data = datos_geograficos, 
    aes(color = basisOfRecord),   # ← Color de los puntos
    size = 2,                # ← Tamaño de los puntos
    alpha = 0.7              # ← Transparencia (0=invisible, 1=opaco)
  ) +
  
  # CAPA 3: Formato y etiquetas
  theme_minimal() +          # ← Estilo limpio y moderno
  labs(
    title = paste("Distribución de", especie_nombre, "\nen", pais_nombre),
    subtitle = paste("Fuente: GBIF |", nrow(datos_geograficos), "registros"),
    caption = paste("Elaborado el", Sys.Date()),
    x = "Longitud (Oeste-Este)", 
    y = "Latitud (Sur-Norte)"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(color = "gray60")
  )

# Mostrar el mapa
print(mapa_registro_especies)

# Guardar el mapa como imagen de alta calidad
ggsave(
  filename = "mapa_registro_especies.png", 
  plot = mapa_registro_especies,
  width = 10,        # ← Ancho en pulgadas
  height = 8,        # ← Alto en pulgadas  
  dpi = 300,         # ← Resolución (300 = calidad publicación)
  bg = "white"       # ← Fondo blanco
)

cat("✓ Primer mapa creado y guardado como 'mapa_registro_especies.png'\n")


# ================================================================
# PASO 7: ANÁLISIS TEMPORAL - MAPAS POR PERÍODOS
# ================================================================

cat("\nPASO 7: Creando análisis temporal...\n")

# ¿Por qué hacer análisis temporal?
# Para ver cómo ha cambiado la distribución de la especie a lo largo del tiempo
# Esto puede mostrar expansión, retracción o nuevas áreas colonizadas

# Filtrar solo datos con información de tiempo
datos_con_fecha <- datos_geograficos %>%
  filter(!is.na(year))

if(nrow(datos_con_fecha) > 10) {  # Solo si tenemos suficientes datos
  
  cat("Creando mapa por períodos históricos...\n")
  
  mapa_temporal <- ggplot() +
    # Mapa base en cada panel
    geom_sf(data = uruguay_mapa, fill = "gray95", color = "gray60", size = 0.3) +
    
    # Puntos coloreados por período
    geom_sf(
      data = datos_con_fecha, 
      aes(color = periodo),    # ← Color según el período temporal
      size = 1.5, 
      alpha = 0.8
    ) +
    
    # Crear un panel separado para cada período
    facet_wrap(~periodo, ncol = 2) +
    
    # Colores personalizados para cada período
    scale_color_viridis_d(name = "Período") +
    
    theme_minimal(base_size = 11) +
    labs(
      title = paste("Evolución temporal:", especie_nombre),
      subtitle = paste("Distribución por períodos en", pais_nombre),
      x = "Longitud", 
      y = "Latitud"
    ) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),  # ← Títulos de paneles en negrita
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(mapa_temporal)
  
  # Guardar mapa temporal
  ggsave("mapa_temporal_especies.png", mapa_temporal, 
         width = 12, height = 10, dpi = 300, bg = "white")
  
  cat("✓ Mapa temporal guardado como 'mapa_temporal_especies.png'\n")
  
  # Crear tabla resumen por período
  resumen_temporal <- datos_con_fecha %>%
    st_drop_geometry() %>%  # ← Quitar información geográfica
    count(periodo, name = "n_registros") %>%
    mutate(porcentaje = round(n_registros / sum(n_registros) * 100, 1)) %>%
    arrange(desc(n_registros))
  
  cat("Distribución temporal de los registros:\n")
  print(resumen_temporal)
  
} else {
  cat("⚠ Pocos datos con fecha disponibles para análisis temporal\n")
}

# ================================================================
# PASO 8: MAPA ESTILO OSCURO
# ================================================================

cat("\nPASO 8: Creando mapa de oscuro...\n")

# Este mapa usará un estilo más elegante y profesional
# Perfecto para presentaciones o publicaciones

mapa_oscuro <- ggplot() +
  # Fondo oscuro elegante
  geom_sf(
    data = uruguay_mapa, 
    fill = "gray20",         # ← Gris oscuro
    color = "gray50", 
    size = 0.3
  ) +
  
  # Puntos brillantes que destacan
  geom_sf(
    data = datos_geograficos, 
    color = "#00D4FF",       # ← Azul cyan brillante
    size = 2.5, 
    alpha = 0.9
  ) +
  
  # Tema oscuro profesional
  theme_void(base_size = 14) +
  labs(
    title = paste(especie_comun, "en", pais_nombre),
    subtitle = paste("Registros de GBIF:", nrow(datos_geograficos)),
    caption = paste("Elaborado el", Sys.Date(), "| Datos: GBIF.org")
  ) +
  theme(
    # Fondo oscuro para toda la imagen
    plot.background = element_rect(fill = "gray10", color = NA),
    panel.background = element_rect(fill = "gray10", color = NA),
    
    # Texto en colores claros
    plot.title = element_text(color = "white", face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray70", size = 12),
    plot.caption = element_text(color = "gray50", size = 9),
    
    # Márgenes
    plot.margin = margin(20, 20, 20, 20)
  )

print(mapa_oscuro)

# Guardar con fondo oscuro
ggsave("mapa_oscuro_especies.png", mapa_oscuro, 
       width = 10, height = 8, dpi = 300, bg = "gray10")

cat("✓ Mapa profesional guardado como 'mapa_oscuro_especies.png'\n")

# ================================================================
# PASO 9: EXPORTAR DATOS PARA OTROS USOS
# ================================================================

cat("\nPASO 9: Exportando datos a CSV...\n")

# ¿Por qué exportar?
# Para usar los datos en Excel, QGIS, Google Earth u otros programas
# Para compartir con colegas o guardar para análisis futuros

# Preparar datos para exportación
datos_para_excel <- datos_crudos$data %>%
  # Seleccionar columnas más importantes y útiles
  dplyr::select(
    # Información básica de la especie
    species, scientificName, vernacularName,
    
    # Ubicación
    country, stateProvince, locality,
    decimalLatitude, decimalLongitude,
    coordinateUncertaintyInMeters,
    
    # Fecha y tiempo
    eventDate, year, month, day,
    
    # Información del registro
    basisOfRecord, institutionCode, 
    recordedBy, catalogNumber,
    
    # Referencias
    references
  ) %>%
  
  # Agregar nuestros cálculos
  mutate(
    # Información que calculamos
    periodo_temporal = case_when(
      is.na(year) ~ "Sin fecha",
      year < 1990 ~ "Antes de 1990",
      year >= 1990 & year < 2000 ~ "1990-1999",
      year >= 2000 & year < 2010 ~ "2000-2009",
      year >= 2010 & year < 2020 ~ "2010-2019", 
      year >= 2020 ~ "2020 en adelante"
    ),
    
    decada = ifelse(!is.na(year), paste0(floor(year / 10) * 10, "s"), "Sin fecha"),
    
    # Metadatos
    fecha_descarga = Sys.Date(),
    script_version = "Tutorial_Principiantes_v1.0"
  )

# Exportar datos completos
write.csv(
  datos_para_excel,
  file = "datos_completos_salvator_merianae.csv",
  row.names = FALSE,     # ← No incluir números de fila
  na = "",              # ← Celdas vacías en lugar de "NA"
  fileEncoding = "UTF-8" # ← Codificación para caracteres especiales
)

# Exportar solo datos con coordenadas válidas (los que usamos en mapas)
datos_geograficos_tabla <- datos_limpios %>%
  filter(
    !is.na(decimalLatitude),
    !is.na(decimalLongitude),
    decimalLatitude >= -35 & decimalLatitude <= -30,
    decimalLongitude >= -58.5 & decimalLongitude <= -53
  ) %>%
  mutate(
    fecha_descarga = Sys.Date(),
    calidad_coordenadas = "Verificadas",
    uso_en_mapas = "Si"
  )

write.csv(
  datos_geograficos_tabla,
  file = "datos_para_mapas_salvator_merianae.csv", 
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# Crear resumen estadístico simple
if(nrow(datos_geograficos_tabla) > 0) {
  estadisticas <- datos_geograficos_tabla %>%
    summarise(
      especie = first(species),
      pais = "Uruguay",
      total_registros = n(),
      
      # Información temporal
      primer_registro = min(year, na.rm = TRUE),
      ultimo_registro = max(year, na.rm = TRUE),
      anos_con_datos = n_distinct(year, na.rm = TRUE),
      
      # Información geográfica  
      latitud_sur = min(decimalLatitude, na.rm = TRUE),
      latitud_norte = max(decimalLatitude, na.rm = TRUE),
      longitud_oeste = min(decimalLongitude, na.rm = TRUE),
      longitud_este = max(decimalLongitude, na.rm = TRUE),
      
      # Información adicional
      instituciones_diferentes = n_distinct(institutionCode, na.rm = TRUE),
      localidades_diferentes = n_distinct(locality, na.rm = TRUE),
      
      # Metadatos
      fecha_analisis = Sys.Date()
    ) %>%
    mutate(
      # Calcular rangos
      rango_latitudinal_km = round((latitud_norte - latitud_sur) * 111, 0),
      rango_longitudinal_km = round((longitud_este - longitud_oeste) * 111, 0),
      anos_de_muestreo = ultimo_registro - primer_registro
    )
  
  write.csv(
    estadisticas,
    file = "resumen_estadistico.csv",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}

# ================================================================
# PASO 10: ANÁLISIS BÁSICO DE LOS DATOS
# ================================================================

cat("\nPASO 10: Haciendo análisis básico de los datos...\n")

if(nrow(datos_geograficos) > 0) {
  
  # 1. Análisis temporal
  if(any(!is.na(datos_limpios$year))) {
    cat("\n--- ANÁLISIS TEMPORAL ---\n")
    
    # Contar registros por década
    registros_por_decada <- datos_limpios %>%
      filter(!is.na(year)) %>%
      count(decada, sort = TRUE)
    
    cat("Registros por década:\n")
    for(i in 1:nrow(registros_por_decada)) {
      cat("  -", registros_por_decada$decada[i], ":", 
          registros_por_decada$n[i], "registros\n")
    }
    
    # Década con más registros
    decada_max <- registros_por_decada$decada[1]
    registros_max <- registros_por_decada$n[1]
    cat("  → Década con más registros:", decada_max, "(", registros_max, "registros)\n")
  }
  
  # 2. Análisis geográfico básico
  cat("\n--- ANÁLISIS GEOGRÁFICO ---\n")
  
  # Extraer coordenadas para cálculos
  coordenadas <- st_coordinates(datos_geograficos)
  
  cat("Distribución geográfica:\n")
  cat("  - Punto más al norte:", round(max(coordenadas[,2]), 3), "°S\n")
  cat("  - Punto más al sur:", round(min(coordenadas[,2]), 3), "°S\n") 
  cat("  - Punto más al oeste:", round(min(coordenadas[,1]), 3), "°O\n")
  cat("  - Punto más al este:", round(max(coordenadas[,1]), 3), "°O\n")
  
  # Calcular área aproximada de distribución
  rango_lat <- max(coordenadas[,2]) - min(coordenadas[,2])
  rango_lon <- max(coordenadas[,1]) - min(coordenadas[,1])
  area_aprox <- rango_lat * rango_lon * 111 * 111  # Conversión aproximada a km²
  
  cat("  - Área aproximada de distribución:", round(area_aprox, 0), "km²\n")
  
  # 3. Análisis de fuentes de datos
  if("institutionCode" %in% names(datos_limpios)) {
    cat("\n--- FUENTES DE DATOS ---\n")
    
    instituciones <- datos_limpios %>%
      filter(!is.na(institutionCode)) %>%
      count(institutionCode, sort = TRUE) %>%
      head(5)  # Top 5 instituciones
    
    cat("Principales instituciones contribuyentes:\n")
    for(i in 1:min(5, nrow(instituciones))) {
      cat("  -", instituciones$institutionCode[i], ":", 
          instituciones$n[i], "registros\n")
    }
  }
}

# ================================================================
# PASO 11: RESUMEN FINAL Y PRÓXIMOS PASOS
# ================================================================
resumen <- function(){
cat("\n", rep("=", 30), "\n")
cat("✓ ¡ANÁLISIS COMPLETADO EXITOSAMENTE! ✓\n")
cat(rep("=", 30), "\n")

cat("\nRESUMEN DE LO QUE HICIMOS:\n")
cat("1. ✓ Descargamos datos de GBIF para", especie_nombre, "\n")
cat("2. ✓ Limpiamos y preparamos", nrow(datos_geograficos), "registros de calidad\n")
cat("3. ✓ Creamos 3 tipos diferentes de mapas\n")
cat("4. ✓ Hicimos análisis temporal y geográfico\n")  
cat("5. ✓ Exportamos datos a CSV para otros usos\n")

cat("\nARCHIVOS CREADOS:\n")
archivos_creados <- c(
  "mi_primer_mapa_especies.png - Mapa básico de distribución",
  "mapa_registro_especies.png - Mapa con clasificación de registros",
  "mapa_oscuro_especies.png - Mapa estilo oscuro",
  "datos_completos_salvator_merianae.csv - Todos los datos descargados",
  "datos_para_mapas_salvator_merianae.csv - Solo datos con coordenadas",
  "resumen_estadistico.csv - Estadísticas resumidas"
)

if(exists("mapa_temporal")) {
  archivos_creados <- c(archivos_creados, "mapa_temporal_especies.png - Análisis temporal")
}

for(archivo in archivos_creados) {
  cat("  📁", archivo, "\n")
}

cat("\nUBICACIÓN DE ARCHIVOS:", getwd(), "\n")

cat("\n🎓 PRÓXIMOS PASOS PARA SEGUIR APRENDIENDO:\n")
cat("1. Prueba con diferentes especies (cambia el taxonKey)\n")
cat("2. Experimenta con otros países (cambia el código de país)\n") 
cat("3. Personaliza los colores y estilos de los mapas\n")
cat("4. Agrega más análisis estadísticos\n")
cat("5. Combina datos de múltiples especies\n")

cat("\n💡 CONSEJOS ÚTILES:\n")
cat("- Guarda tu script con diferentes nombres para experimentar\n")
cat("- Los archivos CSV se pueden abrir en Excel\n")
cat("- Los mapas PNG se pueden usar en presentaciones\n")
cat("- Visita GBIF.org para entender mejor los datos\n")

cat("\n🔗 RECURSOS PARA SEGUIR APRENDIENDO:\n")
cat("- Documentación de ggplot2: https://ggplot2.tidyverse.org/\n")
cat("- Tutorial de sf: https://r-spatial.github.io/sf/\n")
cat("- GBIF API: https://www.gbif.org/developer/summary\n")

cat("\n", rep("=", 30), "\n")
cat("¡Felicitaciones! Has completado tu primer análisis\n")
cat("de biodiversidad con R y GBIF 🎉\n")
cat(rep("=", 30), "\n")

beepr::beep(8)
}

resumen()
