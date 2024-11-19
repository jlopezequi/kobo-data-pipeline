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

# Cargar credenciales desde variables de entorno
username <- Sys.getenv("USERNAME")
password <- Sys.getenv("PASSWORD")
email <- Sys.getenv("EMAIL")
creds_path <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")

# Verificar que las credenciales estén cargadas
if (username == "" || password == "" || email == "" || creds_path == "") {
  stop("Una o más variables de entorno están vacías. Verifica su configuración en GitHub Secrets.")
}

# Crear temporalmente el archivo de credenciales de Google desde el Secret
temp_creds_file <- tempfile(fileext = ".json")
writeLines(creds_path, temp_creds_file)

# Autenticación de Google Sheets
gs4_auth(path = temp_creds_file, cache = ".secrets")
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
