### Script Maestro para Kobo Data Pipeline
# Este script orquesta la importación de datos, la creación de alertas,
# el procesamiento de datos y la exportación de resultados.

rm(list = ls())  # Limpia el entorno

# Instalar y cargar paquetes necesarios
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(
  haven, labelled, tidyr, dplyr, readr, lubridate, googlesheets4, gargle,
  robotoolbox, openssl, dotenv, stringr, writexl, readxl
)

# Configurar el directorio base del proyecto
project_path <- getwd()  # Asume que el repositorio se clonó correctamente
message("Directorio base: ", project_path)

# Verificar si el archivo .env existe
if (file.exists(".env")) {
  message("Archivo .env encontrado en: ", project_path)
  dotenv::load_dot_env(".env")
} else {
  stop("El archivo .env no se encuentra en la ruta: ", project_path)
}

# Cargar credenciales
username <- Sys.getenv("USERNAME")
password <- Sys.getenv("PASSWORD")
email <- Sys.getenv("EMAIL")
temp_creds_file <- "google-credentials.json"

# Verificar que las credenciales estén cargadas
if (username == "" || password == "" || email == "") {
  stop("Una o más variables del archivo .env están vacías. Verifica su configuración.")
}

# Verificar la existencia del archivo de credenciales de Google
if (!file.exists(temp_creds_file)) {
  stop("El archivo de credenciales de Google no se encuentra en: ", temp_creds_file)
}

# Autenticación con Google Sheets usando la Service Account
message("Autenticando con Google Sheets...")
tryCatch({
  gs4_auth(
    path = temp_creds_file,  # Archivo JSON generado desde el YML
    cache = ".secrets"
  )
  message("Autenticación de Google Sheets completada.")
}, error = function(e) {
  message("Error en la autenticación de Google Sheets: ", e)
  stop(e)
})

# Confirmar que las credenciales se cargaron correctamente
message("Credenciales cargadas correctamente:")
message("- Usuario Kobo: ", username)
message("- Email Google Sheets: ", email)

# Función para cargar scripts secundarios
load_script <- function(script_name) {
  script_path <- file.path(project_path, "scripts", script_name)
  if (file.exists(script_path)) {
    message("Ejecutando script: ", script_name)
    source(script_path)
  } else {
    stop(paste("No se encontró el script:", script_path))
  }
}

# Ejecutar scripts secundarios en orden
load_script("Import_data_IFC.R")         # Importar datos desde Kobo
load_script("Correcciones.R")           # Aplicar correcciones a los datos
load_script("Alertas_IFC.R")            # Crear alertas
load_script("Recontacto_IFC.R")         # Generar reporte de recontacto
load_script("Export_data_IFC.R")        # Exportar datos procesados a Google Sheets
load_script("Validación_Identidad_IFC.R") # Validación de identidad

# Confirmación de finalización
message("Pipeline completado exitosamente.")
