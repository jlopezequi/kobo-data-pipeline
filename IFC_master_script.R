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
# En GitHub Actions, el directorio actual es el root del repositorio.
project_path <- getwd()  # Obtener el directorio actual
message("Directorio base: ", project_path)

# Verificar si el archivo .env existe
env_path <- file.path(project_path, ".env")
if (file.exists(env_path)) {
  message("Archivo .env encontrado en: ", env_path)
  dotenv::load_dot_env(env_path)
} else {
  stop("El archivo .env no se encuentra en la ruta: ", env_path)
}

# Cargar credenciales
username <- Sys.getenv("USERNAME")
password <- Sys.getenv("PASSWORD")
email <- Sys.getenv("EMAIL")
creds_path <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")

# Verificar que las credenciales estén cargadas
if (username == "" || password == "" || email == "" || creds_path == "") {
  stop("Una o más variables del archivo .env están vacías. Verifica su configuración.")
}

# Verificar la existencia del archivo de credenciales de Google
if (!file.exists(creds_path)) {
  stop("El archivo de credenciales de Google no se encuentra en: ", creds_path)
}

# Autenticación de Google Sheets
gs4_auth(path = creds_path, cache = ".secrets")
message("Autenticación de Google Sheets completada.")

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
