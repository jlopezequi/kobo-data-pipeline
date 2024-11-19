### Validación de identidad ###

# Verificar si el archivo de credenciales temporales está disponible
if (!exists("temp_creds_file") || !file.exists(temp_creds_file)) {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

# Autenticación con Google Sheets usando la Service Account
message("Autenticando con Google Sheets...")
tryCatch({
  gs4_auth(
    path = temp_creds_file,  # Archivo temporal de credenciales
    cache = ".secrets"
  )
  message("Autenticación de Google Sheets completada.")
}, error = function(e) {
  message("Error en la autenticación de Google Sheets: ", e)
  stop(e)
})

# Procesar datos para validación de identidad
message("Procesando datos para validación de identidad...")
df_validar <- tryCatch({
  alertas %>%
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
}, error = function(e) {
  message("Error al procesar los datos de validación de identidad: ", e)
  stop(e)
})

message("Datos preparados para validación de identidad. Estructura:")
tryCatch({
  glimpse(df_validar)
}, error = function(e) {
  message("Error al mostrar la estructura de los datos: ", e)
})

# Conectar al Google Sheet correspondiente
sheet_url <- "https://docs.google.com/spreadsheets/d/1kHB1oyExyyDeaVwMJX-tY3DpkdaK3iaiRKyUMkMqop0/edit?gid=0"
message("Conectando al Google Sheet de validación de identidad: ", sheet_url)
sheet <- tryCatch({
  gs4_get(sheet_url)
}, error = function(e) {
  message("Error al conectar al Google Sheet: ", e)
  stop(e)
})

# Exportar los datos procesados al Google Sheet
message("Exportando datos procesados al Google Sheet...")
tryCatch({
  sheet_write(df_validar, ss = sheet, sheet = "datos_validacion")
  message("Datos de validación de identidad exportados correctamente.")
}, error = function(e) {
  message("Error al exportar los datos de validación de identidad: ", e)
  stop(e)
})
