### Importar datos desde Kobo ###

# Asegurarse de que las credenciales necesarias estén disponibles
if (exists("username") && exists("password")) {
  message("Credenciales de Kobo cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Kobo. Asegúrate de cargarlas desde el script maestro.")
}

# Autenticación en KoboToolbox
message("Autenticando en KoboToolbox...")
token <- kobo_token(username = username, password = password, url = "https://kf.kobotoolbox.org")
kobo_setup(url = "https://kf.kobotoolbox.org", token = token)
kobo_settings()  # Verificar configuración

# Listar proyectos disponibles en KoboToolbox
message("Listando proyectos disponibles en Kobo...")
projects <- kobo_asset_list()
message("Proyectos encontrados:")
print(table(projects$name))

# Seleccionar el proyecto específico
uid <- "agqu9ay6FowsVftT3fLw9V"  # Reemplazar con el UID de tu proyecto
asset <- kobo_asset(uid)
message("Proyecto seleccionado: ", asset$name)

# Descargar los datos del proyecto
message("Descargando datos del proyecto con UID: ", uid)
data <- kobo_data(asset)

# Verificar la estructura inicial de los datos
message("Datos descargados exitosamente. Estructura inicial:")
glimpse(data)

### Procesar datos ###

# Extraer etiquetas de variables
message("Extrayendo etiquetas de las variables...")
variable_codes <- names(data)  # Nombres de las variables
variable_labels <- sapply(data, function(x) attr(x, "label"))  # Etiquetas de las variables

# Crear un dataframe con códigos y etiquetas (para referencia o recontacto)
df_variables_etiquetas <- data.frame(
  codigo_variable = variable_codes,
  etiqueta_variable = variable_labels,
  stringsAsFactors = FALSE
)


# Eliminar etiquetas para facilitar el procesamiento
message("Eliminando etiquetas de las variables...")
data <- remove_labels(data)

# Procesar tiempos de inicio y fin de la encuesta
message("Procesando tiempos de encuesta...")
data <- data %>%
  mutate(
    start = ymd_hms(start),  # Convertir 'start' a formato datetime
    end = ymd_hms(end),      # Convertir 'end' a formato datetime
    start = with_tz(start, tzone = "America/Mexico_City"),  # Ajustar zona horaria
    end = with_tz(end, tzone = "America/Mexico_City"),
    duration_minutes = round(as.numeric(difftime(end, start, units = "mins")), 0)  # Calcular duración
  )

# Renombrar columnas clave para facilitar el uso
message("Renombrando columnas clave...")
data <- data %>%
  rename(id = `_id`)

# Verificar la estructura final de los datos procesados
message("Datos procesados correctamente. Estructura final:")
glimpse(data)





