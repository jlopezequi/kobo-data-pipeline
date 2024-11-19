### Exportar datos a Google Sheets ###

# Verificar si las credenciales están disponibles
if (exists("email")) {
  message("Credenciales de Google Sheets cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

# Confirmar autenticación con Google Sheets
message("Autenticando con Google Sheets...")
gs4_auth(email = email, cache = ".secrets")

### Exportar recontacto ###

# URL del Google Sheet para recontacto
sheet_url_recontacto <- "https://docs.google.com/spreadsheets/d/1KAZ_nrcyS6Uwn3Gp_gfd8m0jwdcWbw1fjWq5b8rT5KA/edit?gid=0#gid=0"
message("Conectando al Google Sheet de recontacto: ", sheet_url_recontacto)
sheet_recontacto <- gs4_get(sheet_url_recontacto)

# Escribir datos en el Google Sheet
message("Exportando datos de recontacto...")
sheet_write(data, ss = sheet_recontacto, sheet = "base de datos")
sheet_write(datos_recontacto, ss = sheet_recontacto, sheet = "preguntas_recontacto")

message("Datos de recontacto exportados correctamente.")

### Exportar alertas para Looker Studio ###

# URL del Google Sheet para alertas
sheet_url_alertas <- "https://docs.google.com/spreadsheets/d/1UGTkn0NcxeDLnGMSTI1nA7iX2mg8JnP2V2SbNmfbVFs/edit?gid=0#gid=0"
message("Conectando al Google Sheet de alertas: ", sheet_url_alertas)
sheet_alertas <- gs4_get(sheet_url_alertas)

# Corregir alertas basadas en recontacto resuelto
message("Corrigiendo alertas basadas en recontacto resuelto...")
errores_exitosos <- read_sheet("https://docs.google.com/spreadsheets/d/1YFC-naJiXBpc3vhNFYPveJVUBpopKyUL0BTgNK6gF9U/edit?gid=0#gid=0") 
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

# Calcular métricas y transformar datos para Looker Studio
message("Procesando datos de alertas...")
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

# Escribir datos en el Google Sheet
message("Exportando datos de alertas...")
sheet_write(alertas, ss = sheet_alertas, sheet = "alertas")

message("Datos de alertas exportados correctamente.")

