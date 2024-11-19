### Validación de identidad ###

# Verificar si las credenciales están disponibles
if (exists("email")) {
  message("Credenciales de Google Sheets cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

# Confirmar autenticación con Google Sheets
message("Autenticando con Google Sheets...")
gs4_auth(email = email, cache = ".secrets")

# Procesar datos para validación de identidad
message("Procesando datos para validación de identidad...")
df_validar <- alertas %>%
  select(
    k1_1_ID,
    k1_nombre_completo,
    k3_contacto_cel,
    b1_edad_anios,
    b5_educ_max_str,
    c1_nacionalidad_str,
    c1_1_otro,
    nombre_encuestador_str
  ) %>%
  rename(
    nombre_completo = k1_nombre_completo,
    numero_telefono = k3_contacto_cel,
    edad = b1_edad_anios,
    nacionalidad = c1_nacionalidad_str,
    maximo_nivel_educ = b5_educ_max_str
  )

# Verificar la estructura del dataframe generado
message("Datos preparados para validación de identidad. Estructura:")
glimpse(df_validar)

# Conectar al Google Sheet correspondiente
sheet_url <- "https://docs.google.com/spreadsheets/d/1kHB1oyExyyDeaVwMJX-tY3DpkdaK3iaiRKyUMkMqop0/edit?gid=0#gid=0"
message("Conectando al Google Sheet de validación de identidad: ", sheet_url)
sheet <- gs4_get(sheet_url)

# Exportar los datos procesados al Google Sheet
message("Exportando datos procesados al Google Sheet...")
sheet_write(df_validar, ss = sheet, sheet = "datos_validacion")

message("Datos de validación de identidad exportados correctamente.")
