#### Alertas ####

alertas <- data %>%
  mutate(part_valido = if_else(as.numeric(a1_acepta_participar) == 1 & as.numeric(a2_mex_nacionalidad) == 2 & as.numeric(a3_vivir_mexico) == 1 & 
                                 as.numeric(b1_mayor_edad) == 1 & as.numeric(b7_estado_actual != 4),1,0 ))

table(alertas$part_valido)

# Flag duración
dev_estandar <- alertas %>%
 filter(duration_minutes <= 100) %>%
  summarise(sd_duracion = sd(duration_minutes),
            median_duracion = mean(duration_minutes)) %>%
  pull(sd_duracion)


alertas <- alertas %>%
  mutate(
    flag_duration_mas = if_else(
      ((duration_minutes - median(duration_minutes)) / dev_estandar > 1) & 
        part_valido == 1, 1, 0, missing = 0
    ),
    flag_duration_menos = if_else(
      ((duration_minutes - median(duration_minutes)) / dev_estandar < -1) & 
        part_valido == 1, 1, 0, missing = 0
    )
  )

alertas <- alertas %>%
  mutate(flag_duration_mas = if_else(duration_minutes >= 130,0,flag_duration_mas))

#### Validación de saltos #####


alertas <- alertas %>%
  mutate(
    s_a2_mex_nacionalidad = if_else(!is.na(a2_mex_nacionalidad) & a1_acepta_participar == 2, 1, 0),
    s_a3_vivir_mexico = if_else(!is.na(a3_vivir_mexico) & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_b7_estado_actual = if_else(!is.na(b7_estado_actual) & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_b8_ciudad_actual = if_else(!is.na(b8_ciudad_actual) & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_b1_mayor_edad = if_else(!is.na(b1_mayor_edad) & is.na(b8_ciudad_actual) & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_b6_educ_titulo = if_else(!is.na(b6_educ_titulo) & b5_educ_max %in% c(1, 2, 3, 4, 5, 13) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_c1_1_otro = if_else(!is.na(c1_1_otro) & c1_nacionalidad != 13 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_c3_1_razon_doc = if_else(!is.na(c3_1_razon_doc) & c3_doc_id_mex != 3 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_c3_2_otra = if_else(!is.na(c3_2_otra) & c3_1_razon_doc != 5 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_c4_1_otro = if_else(!is.na(c4_1_otro) & c4_doc_id_nat != 4 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_c6_1_otro = if_else(!is.na(c6_1_otro) & c6_pais_establecer != 4 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_e2_objetivo_mexico = if_else(!is.na(e2_objetivo_mexico) & c6_pais_establecer != 1 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_c7_condi_vivir_mex= if_else(!is.na(c7_condi_vivir_mex) & c6_pais_establecer == 1 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_c7_1_otro = if_else(!is.na(c7_1_otro) & c7_condi_vivir_mex != 8 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d4_fuente_financi_negocio = if_else(!is.na(d4_fuente_financi_negocio) & d3_ocupacion_actual !=4 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d4_1_otro = if_else(!is.na(d4_1_otro) & d4_fuente_financi_negocio !=8 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d5_monto_inversion = if_else(!is.na(d5_monto_inversion) & d3_ocupacion_actual !=4 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d8_realiza_actividad_negocio = if_else(!is.na(d8_realiza_actividad_negocio) & d3_ocupacion_actual %in% c(5, 6, 7, 8, 99) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d8_1_otro = if_else(!is.na(d8_1_otro) & d8_realiza_actividad_negocio != 4 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d9_ubicacion_job = if_else(!is.na(d9_ubicacion_job) & d3_ocupacion_actual %in% c(5, 6, 7, 8, 99) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d9_1_otro = if_else(!is.na(d9_1_otro) & d9_ubicacion_job !=9 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d10_time_actividad = if_else(!is.na(d10_time_actividad) & d3_ocupacion_actual %in% c(5, 6, 7, 8, 99) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d11_ingreso_ult_mes = if_else(!is.na(d11_ingreso_ult_mes) & d3_ocupacion_actual %in% c(5, 6, 7, 8, 99) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d12_frecu_ingre = if_else(!is.na(d12_frecu_ingre) & is.na(d11_ingreso_ult_mes) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d13_modo_ingreso = if_else(!is.na(d13_modo_ingreso) & is.na(d11_ingreso_ult_mes) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d13_1_otro = if_else(!is.na(d13_1_otro) & d13_modo_ingreso != 5 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d14_1_otro = if_else(!is.na(d14_1_otro) & d14_origen_ingresos_adc != 10 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_d15_monto_ingre = if_else(!is.na(d15_monto_ingre) & d14_origen_ingresos_adc %in% c(11, 99) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_e1_1_otro = if_else(!is.na(e1_1_otro) & e1_rama_actividad_eco != 13 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_e4_motivo_cambio_job = if_else(!is.na(e4_motivo_cambio_job) & e3_cambio_job != 1 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_e4_1_otro = if_else(!is.na(e4_1_otro) & e4_motivo_cambio_job != 7 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_e5_1_otro = if_else(!is.na(e5_1_otro) & e5_barreras_empleo != 7 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f1_1_otro = if_else(!is.na(f1_1_otro) & f1_producto_financiero != 12 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f2_1_otro = if_else(!is.na(f2_1_otro) & f2_mex_prod_financiero != 10 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f3_entidad_financiera = if_else(!is.na(f3_entidad_financiera) & f2_mex_prod_financiero %in% c(9, 99) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f3_1_otro = if_else(!is.na(f3_1_otro) & f3_entidad_financiera != 8 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f4_1_otro = if_else(!is.na(f4_1_otro) & f4_digital_cuenta != 6 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f5_razon_no_cta = if_else(!is.na(f5_razon_no_cta) & f2_mex_prod_financiero %in% c(1, 2, 3, 4) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f5_1_otro = if_else(!is.na(f5_1_otro) & f5_razon_no_cta != 9 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f7_prob_p_financi = if_else(!is.na(f7_prob_p_financi) & c3_doc_id_mex %in% c(1, 3, 4, 10) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f8_solicitud_credito = if_else(!is.na(f8_solicitud_credito) & f9_acceso_credito != 2 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f10_destino_credito = if_else(!is.na(f10_destino_credito) & f9_acceso_credito != 1 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f10_1_otro = if_else(!is.na(f10_1_otro) & f10_destino_credito != 8 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f11_monto_credito = if_else(!is.na(f11_monto_credito) & f9_acceso_credito != 1 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f12_barreras_credito = if_else(!is.na(f12_barreras_credito) & f9_acceso_credito != 1 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f12_1_otro = if_else(!is.na(f12_1_otro) & f12_barreras_credito != 8 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f14_1_otro = if_else(!is.na(f14_1_otro) & f14_uso_credito !=8 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f17_canti_dine_envio = if_else(!is.na(f17_canti_dine_envio) & f16_frecu_dine_envio == 6 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f18_medio_envio = if_else(!is.na(f18_medio_envio) & f16_frecu_dine_envio == 6 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_f20_canti_dine_recibe = if_else(!is.na(f20_canti_dine_recibe) & f19_frecu_recibe_dine == 6 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_g1_1_otra = if_else(!is.na(g1_1_otra) & g1_recurre_prestamo != 6 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_g3_prom_ahorro_mensual = if_else(!is.na(g3_prom_ahorro_mensual) & g2_ahorra_actualmente != 1 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_g4_1_otra = if_else(!is.na(g4_1_otra) & g4_manera_ahorro != 7 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_g5_part_metodo_ahorro = if_else(!is.na(g5_part_metodo_ahorro) & g4_manera_ahorro == 8 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_j2_apps_usa = if_else(!is.na(j2_apps_usa) & j1_tel_propio != 1 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_k5_contac_migrante = if_else(!is.na(k5_contac_migrante) & k4_conoce_migrante <=0 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_k6_perm_cont_migra = if_else(!is.na(k6_perm_cont_migra) & is.na(k5_contac_migrante) & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0),
    s_k7_migra_regular = if_else(!is.na(k7_migra_regular) & k4_conoce_migrante <=0 & b1_mayor_edad == 2 & b7_estado_actual == 4 & a3_vivir_mexico == 2 & a2_mex_nacionalidad == 1 & a1_acepta_participar == 2, 1, 0)
  )

variables_salto <- c(
  "s_a2_mex_nacionalidad", "s_a3_vivir_mexico", "s_b7_estado_actual", "s_b8_ciudad_actual", 
  "s_b1_mayor_edad", "s_b6_educ_titulo", "s_c1_1_otro", "s_c3_1_razon_doc", "s_c3_2_otra",
  "s_c4_1_otro", "s_c6_1_otro", "s_e2_objetivo_mexico", "s_c7_condi_vivir_mex",
  "s_c7_1_otro", "s_d4_fuente_financi_negocio", "s_d4_1_otro", "s_d5_monto_inversion",
  "s_d8_realiza_actividad_negocio", "s_d8_1_otro", "s_d9_ubicacion_job", "s_d9_1_otro",
  "s_d10_time_actividad", "s_d11_ingreso_ult_mes", "s_d12_frecu_ingre", "s_d13_modo_ingreso",
  "s_d13_1_otro", "s_d14_1_otro", "s_d15_monto_ingre", "s_e1_1_otro",
  "s_e4_motivo_cambio_job", "s_e4_1_otro", "s_e5_1_otro", "s_f1_1_otro",
  "s_f2_1_otro", "s_f3_entidad_financiera", "s_f3_1_otro", "s_f4_1_otro",
  "s_f5_razon_no_cta", "s_f5_1_otro", "s_f7_prob_p_financi", "s_f8_solicitud_credito",
  "s_f10_destino_credito", "s_f10_1_otro", "s_f11_monto_credito", "s_f12_barreras_credito",
  "s_f12_1_otro", "s_f14_1_otro", "s_f17_canti_dine_envio", "s_f18_medio_envio",
  "s_f20_canti_dine_recibe", "s_g1_1_otra", "s_g3_prom_ahorro_mensual", "s_g4_1_otra",
  "s_j2_apps_usa", "s_k5_contac_migrante", "s_k5_contac_migrante", "s_k6_perm_cont_migra", 
  "s_k7_migra_regular"
)


alertas <- alertas %>%
  mutate(
    total_saltos = rowSums(alertas[,variables_salto], na.rm = T)
  )



#### Validación de preguntas obligatorias (missings) #####  


alertas <- alertas %>%
  mutate(
    m_b1_edad_anios = if_else(is.na(b1_edad_anios) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_b2_sexo = if_else(is.na(b2_sexo) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_b3_est_conyugal = if_else(is.na(b3_est_conyugal) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_b4_num_hogar = if_else(is.na(b4_num_hogar) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_b5_educ_max = if_else(is.na(b5_educ_max) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_c1_nacionalidad = if_else(is.na(c1_nacionalidad) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_c2_anio_mexico = if_else(is.na(c2_anio_mexico) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_c3_doc_id_mex = if_else(is.na(c3_doc_id_mex) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_c3_z_doc_mex = if_else(is.na(c3_z_doc_mex) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_c4_doc_id_nat = if_else(is.na(c4_doc_id_nat) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_c6_pais_establecer = if_else(is.na(c6_pais_establecer) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_c8_cita_cbp = if_else(is.na(c8_cita_cbp) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_c9_vivir_mexi_time = if_else(is.na(c9_vivir_mexi_time) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_d1_aport_persona = if_else(is.na(d1_aport_persona) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_d2_ingre_mensual = if_else(is.na(d2_ingre_mensual) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_d3_ocupacion_actual = if_else(is.na(d3_ocupacion_actual) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_d7_acces_servi_publicos = if_else(is.na(d7_acces_servi_publicos) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_d14_origen_ingresos_adc = if_else(is.na(d14_origen_ingresos_adc) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_e1_rama_actividad_eco = if_else(is.na(e1_rama_actividad_eco) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_e3_cambio_job = if_else(is.na(e3_cambio_job) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_e5_barreras_empleo = if_else(is.na(e5_barreras_empleo) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f1_producto_financiero = if_else(is.na(f1_producto_financiero) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f2_mex_prod_financiero = if_else(is.na(f2_mex_prod_financiero) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f4_digital_cuenta = if_else(is.na(f4_digital_cuenta) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f9_acceso_credito = if_else(is.na(f9_acceso_credito) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f13_urg_financiero = if_else(is.na(f13_urg_financiero) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f14_uso_credito = if_else(is.na(f14_uso_credito) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f15_monto_ide_credi = if_else(is.na(f15_monto_ide_credi) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f16_frecu_dine_envio = if_else(is.na(f16_frecu_dine_envio) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_f19_frecu_recibe_dine = if_else(is.na(f19_frecu_recibe_dine) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_g1_recurre_prestamo = if_else(is.na(g1_recurre_prestamo) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_g2_ahorra_actualmente = if_else(is.na(g2_ahorra_actualmente) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_g4_manera_ahorro = if_else(is.na(g4_manera_ahorro) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_h1_descrip_ingre_gast = if_else(is.na(h1_descrip_ingre_gast) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_h2_perder_fuente_ingre = if_else(is.na(h2_perder_fuente_ingre) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_j1_tel_propio = if_else(is.na(j1_tel_propio) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_k1_nombre_completo = if_else(is.na(k1_nombre_completo) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_k3_contacto_cel = if_else(is.na(k3_contacto_cel) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_k1_1_ID = if_else(is.na(k1_1_ID) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_k2_contact_despues = if_else(is.na(k2_contact_despues) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0),
    m_k4_conoce_migrante = if_else(is.na(k4_conoce_migrante) & b1_mayor_edad == 1 & b7_estado_actual != 4 & a3_vivir_mexico == 1 & a2_mex_nacionalidad == 2 & a1_acepta_participar == 1, 1, 0)
  )



variables_missing <- c(
  "m_b1_edad_anios", "m_b2_sexo", "m_b3_est_conyugal", "m_b4_num_hogar",
  "m_b5_educ_max", "m_c1_nacionalidad", "m_c2_anio_mexico", "m_c3_doc_id_mex",
  "m_c3_z_doc_mex", "m_c4_doc_id_nat", "m_c6_pais_establecer", "m_c8_cita_cbp",
  "m_c9_vivir_mexi_time", "m_d1_aport_persona", "m_d2_ingre_mensual", "m_d3_ocupacion_actual",
  "m_d7_acces_servi_publicos", "m_d14_origen_ingresos_adc", "m_e1_rama_actividad_eco", "m_e3_cambio_job",
  "m_e5_barreras_empleo", "m_f1_producto_financiero", "m_f2_mex_prod_financiero", "m_f4_digital_cuenta",
  "m_f9_acceso_credito", "m_f13_urg_financiero", "m_f14_uso_credito", "m_f15_monto_ide_credi",
  "m_f16_frecu_dine_envio", "m_f19_frecu_recibe_dine", "m_g1_recurre_prestamo", "m_g2_ahorra_actualmente",
  "m_g4_manera_ahorro", "m_h1_descrip_ingre_gast", "m_h2_perder_fuente_ingre", "m_j1_tel_propio",
  "m_k1_nombre_completo", "m_k3_contacto_cel", "m_k1_1_ID", "m_k2_contact_despues",
  "m_k4_conoce_migrante"
)


alertas <- alertas %>%
  mutate(
    total_missing = rowSums(alertas[,variables_missing], na.rm = T)
  )


#### Validación de preguntas (No sabe no responde) ##### 

alertas <- alertas %>%
  mutate(
    nn_c9_vivir_mexi_time = if_else(c9_vivir_mexi_time== 99,1,0),
    nn_d3_ocupacion_actual = if_else(d3_ocupacion_actual==99,1,0),
    nn_d14_origen_ingresos_adc = if_else(d14_origen_ingresos_adc_99==1,1,0),
    nn_e1_rama_actividad_eco = if_else(e1_rama_actividad_eco==99,1,0),
    nn_e2_objetivo_mexico = if_else(e2_objetivo_mexico==99,1,0),
    nn_e5_barreras_empleo = if_else(e5_barreras_empleo_99==1,1,0),
    nn_d9_ubicacion_job = if_else(d9_ubicacion_job_99==1,1,0),
    nn_f1_producto_financiero = if_else(f1_producto_financiero_99==1,1,0),
    nn_f2_mex_prod_financiero = if_else(f2_mex_prod_financiero_99==1,1,0),
    nn_f3_entidad_financiera = if_else(f3_entidad_financiera_99==1,1,0),
    nn_f4_digital_cuenta = if_else(f4_digital_cuenta_99==1,1,0),
    nn_d4_fuente_financi_negocio = if_else(d4_fuente_financi_negocio_99==1,1,0),
    nn_f7_prob_p_financi = if_else(f7_prob_p_financi==99,1,0),
    nn_g4_manera_ahorro = if_else(g4_manera_ahorro_99==1,1,0),
    nn_h1_descrip_ingre_gast = if_else(h1_descrip_ingre_gast==99,1,0),
    nn_h2_perder_fuente_ingre = if_else(h2_perder_fuente_ingre==99,1,0),
    nn_d7_acces_servi_publicos = if_else(d7_acces_servi_publicos==99,1,0),
    nn_e3_cambio_job = if_else(e3_cambio_job==99,1,0),
    nn_f9_acceso_credito = if_else(f9_acceso_credito==99,1,0),
    nn_g2_ahorra_actualmente = if_else(g2_ahorra_actualmente==99,1,0),
    nn_g5_part_metodo_ahorro = if_else(g5_part_metodo_ahorro==99,1,0)
  )


# Vector con los nombres de las variables
    variables_nn <- c(
      "nn_c9_vivir_mexi_time",
      "nn_d3_ocupacion_actual",
      "nn_d14_origen_ingresos_adc",
      "nn_e1_rama_actividad_eco",
      "nn_e2_objetivo_mexico",
      "nn_e5_barreras_empleo",
      "nn_d9_ubicacion_job",
      "nn_f1_producto_financiero",
      "nn_f2_mex_prod_financiero",
      "nn_f3_entidad_financiera",
      "nn_f4_digital_cuenta",
      "nn_d4_fuente_financi_negocio",
      "nn_f7_prob_p_financi",
      "nn_g4_manera_ahorro",
      "nn_h1_descrip_ingre_gast",
      "nn_h2_perder_fuente_ingre",
      "nn_d7_acces_servi_publicos",
      "nn_e3_cambio_job",
      "nn_f9_acceso_credito",
      "nn_g2_ahorra_actualmente",
      "nn_g5_part_metodo_ahorro"
    )


alertas <- alertas %>%
  mutate(
    total_nsnr = rowSums(alertas[,variables_nn], na.rm = T), #Crear score de NS/NR
    flag_nsnr = if_else(((total_nsnr - median(total_nsnr, na.rm = TRUE))/sd(total_nsnr, na.rm = TRUE))>2 & total_nsnr >= 5,1,0, missing = 0)) # Crear FLAG para NSNR


# Alerta de valores numéricos extremos ####



alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para d2_ingre_mensual
    ex_d2_ingre_mensual = if_else(
      abs((d2_ingre_mensual - median(d2_ingre_mensual, na.rm = T)) / sd(d2_ingre_mensual, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_d2_ingre_mensual = if_else(
      (d2_ingre_mensual == 9999 | d2_ingre_mensual == 9998) & ex_d2_ingre_mensual == 1, 
      0, 
      ex_d2_ingre_mensual
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para d5_monto_inversion
    ex_d5_monto_inversion = if_else(
      abs((d5_monto_inversion - median(d5_monto_inversion, na.rm = T)) / sd(d5_monto_inversion, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_d5_monto_inversion = if_else(
      (d5_monto_inversion == 9999 | d5_monto_inversion == 9998) & ex_d5_monto_inversion == 1, 
      0, 
      ex_d5_monto_inversion
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para d11_ingreso_ult_mes
    ex_d11_ingreso_ult_mes = if_else(
      abs((d11_ingreso_ult_mes - median(d11_ingreso_ult_mes, na.rm = T)) / sd(d11_ingreso_ult_mes, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_d11_ingreso_ult_mes = if_else(
      (d11_ingreso_ult_mes == 9999 | d11_ingreso_ult_mes == 9998) & ex_d11_ingreso_ult_mes == 1, 
      0, 
      ex_d11_ingreso_ult_mes
    )
  )


alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para d15_monto_ingre
    ex_d15_monto_ingre = if_else(
      abs((d15_monto_ingre - median(d15_monto_ingre, na.rm = T)) / sd(d15_monto_ingre, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_d15_monto_ingre = if_else(
      (d15_monto_ingre == 9999 | d15_monto_ingre == 9998) & ex_d15_monto_ingre == 1, 
      0, 
      ex_d15_monto_ingre
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para f11_monto_credito
    ex_f11_monto_credito = if_else(
      abs((f11_monto_credito - median(f11_monto_credito, na.rm = T)) / sd(f11_monto_credito, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_f11_monto_credito = if_else(
      (f11_monto_credito == 9999 | f11_monto_credito == 9998) & ex_f11_monto_credito == 1, 
      0, 
      ex_f11_monto_credito
    )
  )


alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para f15_monto_ide_credi
    ex_f15_monto_ide_credi = if_else(
      abs((f15_monto_ide_credi - median(f15_monto_ide_credi, na.rm = T)) / sd(f15_monto_ide_credi, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_f15_monto_ide_credi = if_else(
      (f15_monto_ide_credi == 9999 | f15_monto_ide_credi == 9998) & ex_f15_monto_ide_credi == 1, 
      0, 
      ex_f15_monto_ide_credi
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para f17_canti_dine_envio
    ex_f17_canti_dine_envio = if_else(
      abs((f17_canti_dine_envio - median(f17_canti_dine_envio, na.rm = T)) / sd(f17_canti_dine_envio, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_f17_canti_dine_envio = if_else(
      (f17_canti_dine_envio == 9999 | f17_canti_dine_envio == 9998) & ex_f17_canti_dine_envio == 1, 
      0, 
      ex_f17_canti_dine_envio
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para f20_canti_dine_recibe
    ex_f20_canti_dine_recibe = if_else(
      abs((f20_canti_dine_recibe - median(f20_canti_dine_recibe, na.rm = T)) / sd(f20_canti_dine_recibe, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_f20_canti_dine_recibe = if_else(
      (f20_canti_dine_recibe == 9999 | f20_canti_dine_recibe == 9998) & ex_f20_canti_dine_recibe == 1, 
      0, 
      ex_f20_canti_dine_recibe
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para g3_prom_ahorro_mensual
    ex_g3_prom_ahorro_mensual = if_else(
      abs((g3_prom_ahorro_mensual - median(g3_prom_ahorro_mensual, na.rm = T)) / sd(g3_prom_ahorro_mensual, na.rm = T)) > 2, 1, 0, missing = 0
    ),
    # Ajuste para excluir 9999 y 9998
    ex_g3_prom_ahorro_mensual = if_else(
      (g3_prom_ahorro_mensual == 9999 | g3_prom_ahorro_mensual == 9998) & ex_g3_prom_ahorro_mensual == 1, 
      0, 
      ex_g3_prom_ahorro_mensual
    )
  )


alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para k4_conoce_migrante
    ex_k4_conoce_migrante = if_else(
      abs((k4_conoce_migrante - median(k4_conoce_migrante, na.rm = T)) / sd(k4_conoce_migrante, na.rm = T)) > 2 & k4_conoce_migrante >= 100,
      1,
      0,
      missing = 0
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para k5_contac_migrante
    ex_k5_contac_migrante = if_else(
      abs((k5_contac_migrante - median(k5_contac_migrante, na.rm = T)) / sd(k5_contac_migrante, na.rm = T)) > 2 & k5_contac_migrante >= 100,
      1,
      0,
      missing = 0
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para k6_perm_cont_migra
    ex_k6_perm_cont_migra = if_else(
      abs((k6_perm_cont_migra - median(k6_perm_cont_migra, na.rm = T)) / sd(k6_perm_cont_migra, na.rm = T)) > 2 & k6_perm_cont_migra >= 100,
      1,
      0,
      missing = 0
    )
  )

alertas <- alertas %>%
  mutate(
    # Cálculo de valores extremos para k7_migra_regular
    ex_k7_migra_regular = if_else(
      abs((k7_migra_regular - median(k7_migra_regular, na.rm = T)) / sd(k7_migra_regular, na.rm = T)) > 2 & k7_migra_regular >= 100,
      1,
      0,
      missing = 0
    )
  )



## DUPLICADOS ----

caract_especi <- c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u",
                   "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U",
                   "ñ" = "n", "Ñ" = "N")

alertas <- alertas %>% 
  mutate(
    nombre =  str_squish(str_replace_all(toupper(k1_nombre_completo), caract_especi)))


alertas <- alertas %>%
  mutate(
    dup_nombre = if_else((duplicated(nombre) | duplicated(nombre, fromLast = TRUE)) & part_valido == 1, 1, 0),
    dup_celular = if_else((duplicated(k3_contacto_cel) | duplicated(k3_contacto_cel, fromLast = TRUE)) & part_valido == 1, 1, 0),
    dup_nombre = if_else((dup_nombre == 1 & dup_celular == 1),1,0),
    dup_id = if_else((duplicated(k1_1_ID) | duplicated(k1_1_ID, fromLast = TRUE)) & part_valido == 1, 1, 0)
  )


patrones_poco_realistas <- c(
  "1111111111", "2222222222", "3333333333", "4444444444", "5555555555", 
  "6666666666", "7777777777", "8888888888", "9999999999", "0000000000", 
  "1234567890", "9876543210", "1231231234", "1234567891"
)

# Detectar números poco realistas en la columna 'numero_celular'
alertas <- alertas %>%
  mutate(
    numero_celular_str = as.character(k3_contacto_cel),  # Asegurarse de que es character
    flag_unrealistic_pattern = if_else(
      numero_celular_str %in% patrones_poco_realistas, 1, 0
    )
  )


# Encuestas rechazadas y duplicados, valores faltante y atípicos


alertas <- alertas %>%
  mutate(
    flag_rejected = if_else(part_valido == 0, 1, 0),
    flag_saltos = if_else(total_saltos > 0, 1, 0),
    flag_duplicated = if_else(dup_celular == 1 | dup_nombre == 1 | dup_id == 1, 1, 0),
    flag_missing = if_else(total_missing > 0, 1, 0),
    flag_extreme_values = if_else((
      ex_d2_ingre_mensual == 1 | ex_d5_monto_inversion == 1 | ex_d11_ingreso_ult_mes == 1 |
        ex_d15_monto_ingre == 1 | ex_f11_monto_credito == 1 | ex_f15_monto_ide_credi == 1 |
        ex_f17_canti_dine_envio == 1 | ex_f20_canti_dine_recibe == 1 | ex_g3_prom_ahorro_mensual == 1 |
        # Nuevas variables de valores extremos
        ex_k4_conoce_migrante == 1 | ex_k5_contac_migrante == 1 |
        ex_k6_perm_cont_migra == 1 | ex_k7_migra_regular == 1
    ), 1, 0, missing = 0)
  )



### Datos demográficos labels

genero_labels <- c("Masculino" = 1, "Femenino" = 2)
alertas$b2_sexo_str <- factor(alertas$b2_sexo, levels = c(1, 2), labels = names(genero_labels))
attr(alertas$b2_sexo_str, "label") <- "Sexo de nacimiento"



# Definir las etiquetas para los niveles de educación
educ_labels <- c(
  "Preescolar" = 1,
  "Primaria incompleta" = 2,
  "Primaria completa" = 3,
  "Secundaria completa" = 4,
  "Secundaria incompleta" = 5,
  "Técnico o tecnológico incompleto" = 6,
  "Técnico o tecnológico completo" = 7,
  "Universitario completo" = 8,
  "Universitario incompleto" = 9,
  "Especialidad o especialización" = 10,
  "Maestria" = 11,
  "Doctorado" = 12,
  "Ninguno" = 13
)

# Convertir la variable educ_max en factor y asignar las etiquetas
alertas$b5_educ_max_str <- factor(alertas$b5_educ_max, levels = c(1:13), labels = names(educ_labels))

# Asignar una etiqueta general a la variable
attr(alertas$b5_educ_max_str, "label") <- "Máximo nivel educativo alcanzado"


# Definir las etiquetas para los niveles de nacionalidad
# Actualizar el vector de etiquetas de nacionalidad con todos los valores
nacionalidad_labels <- c(
  "Venezuela" = 1,
  "Honduras" = 2,
  "Haití" = 3,
  "Cuba" = 4,
  "El Salvador" = 5,
  "Guatemala" = 6,
  "Nicaragua" = 7,
  "Chile" = 8,
  "Brasil" = 9,
  "Colombia" = 10,
  "Ecuador" = 11,
  "Argentina" = 12,
  "Otro, especifique" = 13
)

# Convertir la variable c1_nacionalidad en factor y asignar las etiquetas correspondientes
alertas$c1_nacionalidad_str <- factor(alertas$c1_nacionalidad, levels = nacionalidad_labels, labels = names(nacionalidad_labels))

# Asignar una etiqueta general a la variable
attr(alertas$c1_nacionalidad_str, "label") <- "Nacionalidad"


# Definir las etiquetas para los niveles de ocupación
# Nuevas etiquetas para ocupación
ocupacion_labels <- c(
  "Empleado formal con contrato escrito" = 1,
  "Empleado informal sin contrato" = 2,
  "Trabajador independiente o autoempleado" = 3,
  "Dueño de un micronegocio (incluso informal o ambulante)" = 4,
  "Buscar empleo" = 5,
  "Oficios del hogar" = 6,
  "Persona pensionada" = 7,
  "Persona incapacitada para trabajar" = 8,
  "No sabe / No responde" = 99
)

# Convertir la variable d3_ocupacion_actual en factor y asignar las etiquetas
alertas$d3_ocupacion_actual_str <- factor(alertas$d3_ocupacion_actual, levels = c(1:8, 99), labels = names(ocupacion_labels))

# Asignar una etiqueta general a la variable
attr(alertas$d3_ocupacion_actual_str, "label") <- "Ocupación actual"


encuestador_labels <- c(
  "Ericka Aguirre" = 1,
  "Julio Cruz" = 2,
  "Fátima Alonzo" = 3,
  "Larissa Amaya" = 4,
  "Antonio Tovar" = 5,
  "Stephany Muñoz" =6,
  "Sandra Rocha"=7,
  "Angela Martínez"=8,
  "Mixuli López"=9,
  "Fátima Arellano Muñoz"=10
)

  
alertas$nombre_encuestador_str <- factor(alertas$id_encuestador, levels = c(1:10), labels = names(encuestador_labels))

estado_labels <- c(
  "Chiapas" = 1,
  "Nuevo León" = 2,
  "San Luis Potosí" = 3
)

alertas$b7_estado_actual_str <- factor(alertas$b7_estado_actual, level = c(1:3), labels = names(estado_labels))

table(alertas$b7_estado_actual_str)

### Conteo de regulares y refugiados


alertas <- alertas %>%
  mutate(dummy_refugiado = if_else(c3_doc_id_mex_8 == 1 | c3_doc_id_mex_1 == 1 | c3_1_razon_doc == 3,1,0,missing = 0),
         dummy_irregular = if_else(c3_doc_id_mex_2 == 1 | c3_doc_id_mex_5 == 1 | c3_doc_id_mex_7 == 1 | 
                                     c3_doc_id_mex_11 == 1,1,0, missing = 0))

### Crear alertas LOOKER


alertas <- alertas %>%
  mutate(total_encuestas = n(),
         Exitos = if_else(flag_duration_mas == 0 & flag_duration_menos == 0 & flag_nsnr == 0 & flag_duplicated == 0 &  
                            flag_missing == 0 &  flag_saltos == 0 & flag_unrealistic_pattern == 0 & flag_extreme_values == 0,1,0),
         Alertas = if_else(flag_duration_mas == 1 | flag_duration_menos == 1 | flag_nsnr == 1 | flag_duplicated == 1 |   
                             flag_missing == 1 | flag_saltos == 1 | flag_extreme_values == 1 | flag_unrealistic_pattern == 1,1,0),
         Rechazos = if_else(flag_rejected == 1,1,0),
         tiempos_anomalos_mas = if_else(flag_duration_mas == 1,"Sí","No"),
         tiempos_anomalos_menos = if_else(flag_duration_menos == 1,"Sí","No"),
         exceso_nsnr = if_else(flag_nsnr == 1,"Sí","No"),
         id_repetido = if_else(flag_duplicated == 1, "Sí","No"),
         valores_faltantes = if_else(flag_missing == 1,"Sí","No"),
         saltos_irregulares = if_else(flag_saltos == 1,"Sí","No"),
         valores_extremos = if_else(flag_extreme_values == 1, "Sí", "No")) 

table(alertas$Exitos)



