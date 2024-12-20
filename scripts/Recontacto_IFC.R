#### Recontacto

#### Datos recontacto

datos_rc <- alertas %>%
  select(-c(k1_nombre_completo)) %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))



datos_rc <- alertas %>%
  select(nombre_encuestador_str, id_encuestador, k1_nombre_completo, id, k3_contacto_cel, starts_with("s_"), starts_with("m_"), 
         starts_with("ex_"))


datos_recontacto <- datos_rc %>%
  pivot_longer(cols = starts_with("s_") | starts_with("m_") | starts_with("ex_"),
               names_to = "alerta_variable", 
               values_to = "alerta_levantada") %>%
  mutate(tipo_alerta = case_when(
    grepl("^s_", alerta_variable) ~ "salto_irregular",
    grepl("^m_", alerta_variable) ~ "missing",
    grepl("^ex_", alerta_variable) ~ "valor_extremo"
  )) %>%
  pivot_wider(names_from = tipo_alerta, values_from = alerta_levantada)%>%
  filter(salto_irregular == 1 |missing == 1 | valor_extremo == 1 )



datos_recontacto <- datos_recontacto %>%
  mutate(nombres_variables = str_remove(alerta_variable, "^s_|^ex_|^m_")) %>%
  relocate(nombres_variables, .after = k3_contacto_cel)%>%
  select(-alerta_variable)


# Pivot del dataframe_original para agregar respuestas originales

 original_rc <- data %>% 
   select(-c(k1_nombre_completo)) %>%
   mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))


original_long <- original_rc %>%
  pivot_longer(cols = -c( id_encuestador, id),
               names_to = "nombres_variables",
               values_to = "respuesta_original")


# Agregar respuestas originales a datos de recontacto

datos_recontacto <- datos_recontacto %>%
  mutate(id_encuestador = as.numeric(id_encuestador))%>%
  left_join(original_long, by = c("id_encuestador", "nombres_variables", "id") )

datos_recontacto <- datos_recontacto%>%
  left_join(df_variables_etiquetas, by = c("nombres_variables" = "codigo_variable"))%>%
  relocate(etiqueta_variable, .after = nombres_variables)

## Agregar nombres

data_nombres <- data %>%
  select(k1_nombre_completo, id)

datos_recontacto <- datos_recontacto %>%
  left_join(data_nombres, by = c("id")) %>%
  relocate(k1_nombre_completo.x, .after = id_encuestador) %>%
  rename(nombre = k1_nombre_completo.x) %>%
  select(-c(k1_nombre_completo.y))



datos_recontacto <- datos_recontacto %>%
  rename(numero_telefono = k3_contacto_cel)%>%
  relocate(numero_telefono, .before = id) %>%
  relocate(id, .before = nombre_encuestador_str)