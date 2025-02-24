#### Correcciones 

### Correcciones de fecha para los registros imputados


# Convertir el data frame principal
data <- data %>%
  mutate(
    start = case_when(
      id == 408265222 ~ as.POSIXct('2024-11-05 13:50:21.498000', tz = 'America/Mexico_City'),
      id == 408277746 ~ as.POSIXct('2024-11-06 20:31:09.956000', tz = 'America/Mexico_City'),
      id == 408285386 ~ as.POSIXct('2024-11-04 13:44:39.774000', tz = 'America/Mexico_City'),
      id == 408292477 ~ as.POSIXct('2024-11-05 13:08:21.079000', tz = 'America/Mexico_City'),
      id == 408300946 ~ as.POSIXct('2024-11-04 20:50:48.652000', tz = 'America/Mexico_City'),
      id == 408309124 ~ as.POSIXct('2024-11-04 21:17:13.221000', tz = 'America/Mexico_City'),
      id == 408318904 ~ as.POSIXct('2024-11-04 19:36:39.259000', tz = 'America/Mexico_City'),
      id == 408325646 ~ as.POSIXct('2024-11-01 12:55:05.930000', tz = 'America/Mexico_City'),
      id == 408272424 ~ as.POSIXct('2024-11-03 18:03:39.811000', tz = 'America/Mexico_City'),
      id == 408282037 ~ as.POSIXct('2024-11-01 16:43:35.020000', tz = 'America/Mexico_City'),
      id == 408288887 ~ as.POSIXct('2024-11-03 17:18:25.254000', tz = 'America/Mexico_City'),
      id == 408294499 ~ as.POSIXct('2024-11-03 17:38:26.598000', tz = 'America/Mexico_City'),
      id == 408299971 ~ as.POSIXct('2024-10-31 19:19:07.909000', tz = 'America/Mexico_City'),
      id == 408304815 ~ as.POSIXct('2024-11-01 15:40:23.025000', tz = 'America/Mexico_City'),
      id == 408310699 ~ as.POSIXct('2024-11-07 15:32:48.685000', tz = 'America/Mexico_City'),
      id == 408367371 ~ as.POSIXct('2024-11-05 15:32:21.569000', tz = 'America/Mexico_City'),
      id == 408366475 ~ as.POSIXct('2024-11-05 15:28:37.385000', tz = 'America/Mexico_City'),
      TRUE ~ start
    )
  )

### Crear variable de fecha

data <- data %>%
  mutate(fecha_sin_hora = as.Date(start))


data <- data %>%
  relocate(id_encuestador, .after = end)

#### Filtrar encuestas piloto

data <- data %>%
  filter(end >= "2024-10-25 12:00:00")


