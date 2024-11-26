### Exportar datos a Google Sheets ###

# Verificar si las credenciales están disponibles
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
  stop("Error en la autenticación de Google Sheets: ", e)
})

### Exportar recontacto ###

# URL del Google Sheet para recontacto
sheet_url_recontacto <- "https://docs.google.com/spreadsheets/d/1KAZ_nrcyS6Uwn3Gp_gfd8m0jwdcWbw1fjWq5b8rT5KA/edit"
message("Conectando al Google Sheet de recontacto: ", sheet_url_recontacto)

sheet_recontacto <- tryCatch({
  gs4_get(sheet_url_recontacto)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de recontacto: ", e)
})

# Escribir datos en el Google Sheet
message("Exportando datos de recontacto...")
tryCatch({
  sheet_write(data, ss = sheet_recontacto, sheet = "base de datos")
  sheet_write(datos_recontacto, ss = sheet_recontacto, sheet = "preguntas_recontacto")
  message("Datos de recontacto exportados correctamente.")
}, error = function(e) {
  stop("Error al exportar datos de recontacto: ", e)
})

### Exportar alertas para Looker Studio ###

# URL del Google Sheet para alertas
sheet_url_alertas <- "https://docs.google.com/spreadsheets/d/1UGTkn0NcxeDLnGMSTI1nA7iX2mg8JnP2V2SbNmfbVFs/edit"
message("Conectando al Google Sheet de alertas: ", sheet_url_alertas)

sheet_alertas <- tryCatch({
  gs4_get(sheet_url_alertas)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# Corregir alertas basadas en recontacto resuelto

duplicados_resueltos <- c(411445117, 411454298, 411903659, 411909621, 411893696, 411901343, 
                          411898780, 412868850, 412867223, 412857155, 412863625, 411908364, 
                          411913281, 411913229, 410970825, 410976300, 410967969, 410969079, 
                          411884775, 411881742, 411874075)


message("Corrigiendo alertas basadas en recontacto resuelto...")
tryCatch({
  errores_exitosos <- read_sheet("https://docs.google.com/spreadsheets/d/1YFC-naJiXBpc3vhNFYPveJVUBpopKyUL0BTgNK6gF9U/edit")
  
  errores_exitosos <- errores_exitosos %>%
    filter(
      Estatus == "Resuelto exitosamente" &
        valor_extremo == 1 &
        !is.na(`Respuesta correcta (Ingrese el número que corresponde a la opción)`)
    )
  
  alertas <- alertas %>%
    mutate(
      flag_extreme_values = if_else(id %in% errores_exitosos$id, 0, flag_extreme_values, missing = flag_extreme_values)
    )
  alertas <- alertas %>%
    mutate(flag_duplicated = if_else(id %in% duplicados_resueltos,0,flag_duplicated, missing = flag_duplicated))
  
  
}, error = function(e) {
  stop("Error al corregir alertas: ", e)
})



# Calcular métricas y transformar datos para Looker Studio
message("Procesando datos de alertas...")
tryCatch({
  alertas <- alertas %>%
    mutate(
      total_encuestas = n(),
      Exitos = if_else(
        flag_duration_mas == 0 &
          flag_duration_menos == 0 &
          flag_nsnr == 0 &
          flag_duplicated == 0 &
          flag_missing == 0 &
          flag_saltos == 0 &
          flag_rejected == 0 &
          flag_unrealistic_pattern == 0 &
          flag_extreme_values == 0, 1, 0
      ),
      Alertas = if_else(
        flag_duration_mas == 1 |
          flag_duration_menos == 1 |
          flag_nsnr == 1 |
          flag_duplicated == 1 |
          flag_missing == 1 |
          flag_saltos == 1 |
          flag_extreme_values == 1 |
          flag_unrealistic_pattern == 1, 1, 0
      ),
      Rechazos = if_else(flag_rejected == 1, 1, 0),
      tiempos_anomalos_mas = if_else(flag_duration_mas == 1, "Sí", "No"),
      tiempos_anomalos_menos = if_else(flag_duration_menos == 1, "Sí", "No"),
      exceso_nsnr = if_else(flag_nsnr == 1, "Sí", "No"),
      id_repetido = if_else(flag_duplicated == 1, "Sí", "No"),
      valores_faltantes = if_else(flag_missing == 1, "Sí", "No"),
      saltos_irregulares = if_else(flag_saltos == 1, "Sí", "No"),
      valores_extremos = if_else(flag_extreme_values == 1, "Sí", "No")
    ) %>%
    filter(part_valido == 1)  # Solo registros con consentimiento
}, error = function(e) {
  stop("Error al procesar datos de alertas: ", e)
})

# Escribir datos en el Google Sheet
message("Exportando datos de alertas...")
tryCatch({
  sheet_write(alertas, ss = sheet_alertas, sheet = "alertas")
  message("Datos de alertas exportados correctamente.")
}, error = function(e) {
  stop("Error al exportar datos de alertas: ", e)
})
