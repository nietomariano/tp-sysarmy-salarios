# -----------------------------------------------------------------------------
# 03_EDA.R
# -----------------------------------------------------------------------------

library(tidyverse)

#INDICE

# 1. CARGA DEL DATASET 
# 2. ESTRUCTURA GENERAL 
# 3. CALIDAD DE DATOS 
# 4. DISTRIBUCIONES UNIVARIADAS 
# 5. DISTRIBUCIÓN DE LA VARIABLE OBJETIVO 
# 6. VARIABLES NUMÉRICAS 
# 7. VARIABLES CATEGÓRICAS 
# 8. VARIABLES POTENCIALMENTE CONFUNDIDORAS 
# 9. REGIÓN Y ROLES 
# 10. CONCLUSIONES DEL EDA

# -----------------------------------------------------------------------------
# 1. CARGA DEL DATASET LIMPIO
# -----------------------------------------------------------------------------

df <- read_csv(
  "data/processed/df_sysarmy.csv")


# 2. ESTRUCTURA GENERAL
# -----------------------------------------------------------------------------

glimpse(df) # 60.356
colnames(df)

# -----------------------------------------------------------------------------
# 3. CALIDAD DE DATOS
# -----------------------------------------------------------------------------


colSums(is.na(df))
sort(colSums(is.na(df)), decreasing = TRUE)



df %>%
  group_by(anio,semestre) %>%
  summarise(
    registros = n(),
    seniority_na = sum(is.na(seniority)),
    dolarizado_na = sum(is.na(sueldo_dolarizado)),
    modalidad_na = sum(is.na(modalidad)),
    uso_ia_na = sum(is.na(uso_ia))
  )


# CONCLUSION:
# Las variables PRINCIPALES utilizadas para el analisis no presentan
# valores faltantes.
#
# Seniority, sueldo_dolarizado y uso_ia representan aproximadamente
# un 67% de valores faltantes. Sin embargo, en el ultimo analisis que hicimos
# (por periodos) nos muestra que estos faltantes no son aleatorios.
#
# Las variables seniority, sueldo_dolarizado y uso_ia se encuentran
# disponibles a partir de la encuesta del periodo 2024.1,
# mientras que modalidad aparece a partir de 2022.2.
#
# Por lo tanto, los valores faltantes responden a cambios en el
# cuestionario de Sysarmy entre distintos períodos y no a errores
# de carga o falta de respuesta de los encuestados.



# -----------------------------------------------------------------------------
# 4. DISTRIBUCIONES UNIVARIADAS
# -----------------------------------------------------------------------------

### VARIABLES CATEGORICAS

# grupo_rol — los grupos ahora son: Datos/AI, Infraestructura, Ciberseguridad,
# Desarrollo/QA representan el ~52% del dataset
df %>%
  count(grupo_rol)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# Distribución de seniority — Nota: ~67% NA corresponden a períodos 2019-2023
df %>%
  count(seniority)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# genero_simple — Nota: ~82% Hombre, representación femenina baja (14%)
df %>%
  count(genero_simple)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# modalidad — Nota: ~46% NA (períodos pre-2022); entre los disponibles, 50% remoto
df %>%
  count(modalidad)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# gente_a_cargo_grupo — Nota: ~80% sin equipo a cargo
df %>%
  count(gente_a_cargo_grupo)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )


# VARIABLES NUMERICAS

# sal_bruto: distribucion asimetrica en pesos — no comparable entre periodos por inflacion
summary(df$sal_bruto)

# sal_usd_blue: salario deflactado por dolar blue — comparable entre 2019 y 2026
summary(df$sal_usd_blue)

# log_sal_usd: media y mediana mas cercanas — confirma uso de log como variable objetivo
summary(df$log_sal_usd)

# experiencia: mediana 6 anios, máximo 65 — posible outlier en el maximo
summary(df$experiencia)

# edad: rango 18-70, algunos NA — filtro aplicado en limpieza (16-70)
summary(df$edad)

# gente_a_cargo: mediana 0, media 0.74 — 80% sin equipo, distribucion muy sesgada
summary(df$gente_a_cargo)



# -----------------------------------------------------------------------------
# 5. DISTRIBUCION DE LA VARIABLE OBJETIVO
# -----------------------------------------------------------------------------


# sal_usd_blue: distribucion asimetrica — se confirma necesidad de transformacion log
ggplot(df, aes(x = sal_usd_blue)) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución del salario en dólares blue",
    x = "Salario (USD)",
    y = "Frecuencia"
  ) +
  theme_minimal()



# log_sal_usd: distribucion mas simetrica — confirma uso de log como variable objetivo
ggplot(df, aes(x = log_sal_usd)) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución del logaritmo del salario (USD)",
    x = "Logaritmo del salario (USD)",
    y = "Frecuencia"
  ) +
  theme_minimal()



# Histograma con densidad — grafico principal de esta seccion
# Distribucion unimodal aproximadamente normal — confirma que log_sal_usd
# es una buena variable objetivo para el modelo de regresion lineal
# La cola izquierda corresponde a casos con salarios bajos que sobrevivieron el filtro
# Contraste con log_sal en pesos: ahi se observaba bimodalidad por la inflacion
ggplot(df, aes(x = log_sal_usd)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40, fill = "steelblue", color = "white"
  ) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribución de log_sal_usd con curva de densidad",
    x = "Logaritmo del salario (USD)",
    y = "Densidad"
  ) +
  theme_minimal()



# Boxplot
ggplot(df, aes(y = log_sal_usd)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Boxplot del logaritmo del salario (USD)",
    y = "Logaritmo del salario (USD)",
    x = ""
  ) +
  theme_minimal()


# -----------------------------------------------------------------------------
# 6. VARIABLES NUMERICAS
# -----------------------------------------------------------------------------


# Relacion entre Experiencia y salario
# Relacion positiva debil — experiencia sola no determina el salario
# Alta dispersion vertical
# La mayoria de observaciones se encuentran entre 0 y 20 anios de experiencia
ggplot(df, aes(x = experiencia, y = log_sal_usd)) +
  geom_jitter(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relación entre experiencia y salario",
    x = "Años de experiencia",
    y = "Logaritmo del salario (USD)",
    caption = "Línea roja: ajuste lineal"
  ) +
  theme_minimal()



# Relacion entre Edad y salario
# Tendencia positiva similar a experiencia pero con mayor dispersion
# Edad y experiencia probablemente correlacionadas — riesgo de multicolinealidad
# Se evaluara incluir solo una de las dos en el modelo final
ggplot(df, aes(x = edad, y = log_sal_usd)) +
  geom_jitter(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relación entre edad y salario",
    x = "Edad (años)",
    y = "Logaritmo del salario (USD)",
    caption = "Línea roja: ajuste lineal"
  ) +
  theme_minimal()


# Relacion entre Personas a cargo y salario
# 80% de observaciones en x=0 - Aplicamos filtro en el siguiente grafico
ggplot(df, aes(x = gente_a_cargo, y = log_sal_usd)) +
  geom_jitter(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm") +
  labs(
    title = "Relación entre personas a cargo y salario",
    x = "Cantidad de personas a cargo",
    y = "Logaritmo del salario (USD)",
    caption = "Línea roja: ajuste lineal | Zona gris: margen de incertidumbre"
  ) +
  theme_minimal()


# Relacion entre Personas a cargo y salario - con filtro sacando los outliers
# 80% de observaciones en x=0 — se filtran valores > 200 (errores de carga)
# El ajuste lineal muestra tendencia positiva pero con alta incertidumbre
ggplot(df %>% filter(gente_a_cargo <= 200), aes(x = gente_a_cargo, y = log_sal_usd)) +
  geom_jitter(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm") +
  labs(
    title = "Relación entre personas a cargo y salario",
    x = "Cantidad de personas a cargo",
    y = "Logaritmo del salario (USD)",
    caption = "Línea roja: ajuste lineal | Zona gris: margen de incertidumbre"
  ) +
  theme_minimal()

# Relacion entre Grupo de personas a cargo y salario
# Mediana salarial aumenta progresivamente con el tamaño del equipo
# Cajas de tamaño similar — dispersion parecida entre los grupos
# Equipo grande tiene pocos casos
df %>%
  filter(!is.na(gente_a_cargo_grupo)) %>%
  ggplot(aes(x = factor(gente_a_cargo_grupo,
                          levels = c("Sin equipo","Equipo pequeño",
                                     "Equipo mediano","Equipo grande")),
               y = log_sal_usd)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Distribución salarial según tamaño del equipo a cargo",
    x = "Tamaño del equipo a cargo",
    y = "Logaritmo del salario (USD)",
    caption = "Sin equipo: 0 | Equipo pequeño: 1–4 | Equipo mediano: 5–10 | Equipo grande: más de 10"
  ) +
  theme_minimal()



# CONCLUSION SECCION 6:
# Las tres variables numericas muestran relacion positiva con el salario
# pero con alta dispersion — ninguna explica el salario de forma aislada.
# Edad y experiencia probablemente correlacionadas — evaluar multicolinealidad.




# -----------------------------------------------------------------------------
# 7. VARIABLES CATEGORICAS
# -----------------------------------------------------------------------------


# SENIORITY
# Variable con separación clara entre los tres niveles
# Medianas crecen progresivamente: Junior < Semi-Senior < Senior
# Senior tiene levemente la caja con mayor dispersión
ggplot(df %>% filter(!is.na(seniority)), 
       aes(x = seniority, y = log_sal_usd, fill = seniority)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según seniority",
    x = "Seniority",
    y = "Logaritmo del salario (USD)",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()


# MODALIDAD
# Remoto e Híbrido muestran medianas similares y superiores a Presencial
# Presencial queda claramente por debajo — diferencia más marcada que antes
ggplot(df %>% filter(!is.na(modalidad)),
       aes(x = modalidad, y = log_sal_usd, fill = modalidad)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("Remoto", "Hibrido", "Presencial")) +
  labs(
    title = "Distribución salarial según modalidad de trabajo",
    x = "Modalidad de trabajo",
    y = "Logaritmo del salario (USD)"
  ) +
  theme_minimal()


# DOLARIZACION
# Con salario deflactado la brecha se achica — quienes cobran en dolares
# ya estan capturados en la misma escala que el resto
ggplot(df %>% filter(!is.na(sueldo_dolarizado)),
       aes(x = factor(sueldo_dolarizado, 
                      labels = c("No dolarizado", "Dolarizado")),
           y = log_sal_usd,
           fill = factor(sueldo_dolarizado))) +
  scale_fill_discrete(name = "Tipo de sueldo") +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según dolarización del sueldo",
    x = "Tipo de sueldo",
    y = "Logaritmo del salario (USD)",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()

# TABLA DE APOYO
df %>%
  filter(!is.na(sueldo_dolarizado)) %>%
  group_by(sueldo_dolarizado) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    n = n()
  )


# RELACION ENTRE EXPERIENCIA Y SALARIO SEGUN DOLARIZACION
# Los dolarizados parten con un intercepto levemente superior (~7.2 vs ~7.0)
# y la brecha se mantiene a lo largo de toda la trayectoria de experiencia
# La diferencia es menor que en pesos pero no desaparece completamente
# Confirma que la brecha observada en pesos era un efecto de la inflacion
# no una diferencia real de poder adquisitivo
ggplot(df %>% filter(!is.na(sueldo_dolarizado)),
       aes(x = experiencia, 
           y = log_sal_usd,
           color = factor(sueldo_dolarizado, 
                          labels = c("No dolarizado", "Dolarizado")))) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre experiencia y salario según dolarización",
    x = "Años de experiencia",
    y = "Logaritmo del salario (USD)",
    color = "Tipo de sueldo",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()




# TAMANO DE LA EMPRESA
# Medianas similares entre todos los tamaños — sin tendencia clara ni consistente
# Leve variacion en tramos intermedios pero se estabiliza en empresas grandes
# Tamaño de empresa no parece ser un predictor relevante del salario

ggplot(df %>% filter(!is.na(tam_empresa)),
       aes(x = factor(tam_empresa, 
                      levels = c("1", "2-10", "11-50", "51-100", 
                                 "101-200", "201-500", "501-1000", 
                                 "1001-2000", "2001-5000", 
                                 "5001-10000", "+10000")), 
           y = log_sal_usd)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Distribución salarial según tamaño de la organización",
    x = "Cantidad de personas en la organización",
    y = "Logaritmo del salario (USD)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  theme_minimal()



# CONCLUSION SECCION 7:
# Seniority es la variable categórica con mayor poder discriminante.
# Modalidad y tamaño de empresa no muestran diferencias salariales relevantes.
# Dolarización en pesos nominales mostraba brecha del 34%, pero al deflactar
# por dolar blue la diferencia prácticamente desaparece — era un efecto inflacionario.

# Tablas de apoyo
df %>%
  group_by(seniority) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    cantidad = n()
  )

df %>%
  group_by(modalidad) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    cantidad = n()
  )

df %>%
  group_by(sueldo_dolarizado) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    cantidad = n()
  )

# -----------------------------------------------------------------------------
# 8. VARIABLES POTENCIALMENTE CONFUNDIDORAS
# -----------------------------------------------------------------------------



# GENERO
# Sin controlar por seniority — hombres muestran mediana levemente superior
# Podría sugerir una brecha salarial de género, pero es necesario controlar
# por otras variables antes de concluir
ggplot(df,
       aes(x = genero_simple, y = log_sal_usd, fill = genero_simple)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según género",
    x = "Género",
    y = "Logaritmo del salario (USD)"
  ) +
  theme_minimal()


# Controlando por seniority — persiste una brecha salarial a favor de hombres
# especialmente visible en Semi-Senior y Senior
# El genero tiene un efecto independiente del seniority
# Esto sugiere incluir genero_simple en el modelo para cuantificar su efecto real
ggplot(df %>% filter(!is.na(seniority)),
       aes(x = genero_simple, y = log_sal_usd, fill = seniority)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según género y seniority",
    x = "Género",
    y = "Logaritmo del salario (USD)",
    fill = "Seniority",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()



# Hombres tienen mayor proporcion de Senior (~53%) vs mujeres (~35%)
# Mujeres tienen mayor proporcion de Junior y Semi-Senior
# La distribucion de seniority amplifica la brecha pero no la explica completamente
# — la diferencia persiste dentro de cada nivel de seniority
ggplot(df %>% filter(!is.na(seniority)),
       aes(x = genero_simple, fill = seniority)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporción de seniority según género",
    x = "Género",
    y = "Proporción",
    fill = "Seniority",
    caption = "Solo períodos 2024-2026"
  ) +
  theme_minimal()


# Tabla de Apoyo - Mediana de salario por grupo_rol
df %>%
  group_by(grupo_rol) %>%
  summarise(
    mediana_usd = median(sal_usd_blue, na.rm = TRUE),
    cantidad = n()
  ) %>%
  arrange(desc(mediana_usd))
  

# CONCLUSION SECCION 8:
# El genero muestra una brecha salarial a favor de hombres incluso controlando
# por seniority — especialmente en Semi-Senior y Senior.
# La distribucion de seniority amplifica la brecha: hombres ~53% Senior 
# vs mujeres ~35% Senior.
# El genero sera incluido en el modelo para cuantificar su efecto independiente.


# -----------------------------------------------------------------------------
# 9. REGIONES Y ROLES
# -----------------------------------------------------------------------------

# SALARIO SEGUN REGION
# Las tres regiones muestran medianas muy similares en USD
# CABA tiene una leve ventaja pero las diferencias son pequeñas
# El mercado IT argentino es relativamente homogéneo en términos geográficos
# Observaciones: CABA = 8.434, GBA = 2.578, Interior = 4.108
ggplot(df %>% filter(!is.na(region)),
       aes(x = region, y = log_sal_usd, fill = region)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según región",
    x = "Región",
    y = "Logaritmo del salario (USD)"
  ) +
  theme_minimal()

df %>% count(region)



# SALARIO SEGUN GRUPO DE ROL
# Medianas muy similares entre todos los grupos en USD
# Management supera al resto. Ciberseguridad  e Infraestructura tiene una leve ventaja sobre las demas
# Desarrollo tiene la mediana mas baja de todas
# El grupo de rol puede actuar como variable de control en el modelo

ggplot(df,
       aes(x = reorder(grupo_rol, log_sal_usd, median),
           y = log_sal_usd,
           fill = grupo_rol)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según grupo de rol",
    x = "Grupo de rol",
    y = "Logaritmo del salario (USD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

# TOP ROLES
# Manager / Director y Architect lideran con las medianas más altas
# QA / Tester y Consultant quedan en el extremo inferior
# Management (violeta) concentra los roles mejor pagos
# El puesto especifico tiene menor poder explicativo que seniority
top_roles <- df %>%
  count(trabajo_de, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(trabajo_de)

ggplot(df %>% filter(trabajo_de %in% top_roles),
       aes(x = reorder(trabajo_de, log_sal_usd, median),
           y = log_sal_usd,
           fill = grupo_rol)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribución salarial por rol - Top 10",
    x = "Rol",
    y = "Logaritmo del salario (USD)",
    fill = "Grupo de rol"
  ) +
  theme_minimal()


# Desarrollo/QA y Datos/AI muestran la pendiente más pronunciada —
# la experiencia premia más en esos grupos
# Management parte con el intercepto más alto (salario base mayor)
# Infraestructura crece más lento con la experiencia
df %>%
  filter(!is.na(experiencia), !is.na(log_sal_usd)) %>%
  ggplot(aes(x = experiencia, y = log_sal_usd, color = grupo_rol)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Salario vs Experiencia por grupo de rol",
    x     = "Años de experiencia",
    y     = "Logaritmo del salario (USD)",
    color = "Grupo de rol"
  ) +
  theme_minimal()



# CONCLUSION SECCION 9:
# Las diferencias regionales son pequeñas en USD — mercado IT homogeneo geograficamente.
# CABA tiene una leve ventaja pero no es determinante.
# Entre grupos de rol las diferencias tambien son moderadas.
# AI Engineer lidera entre roles individuales, BI Analyst / Data Analyst 
# queda en el extremo inferior.
# Region y grupo de rol seran incluidos como variables de control en el modelo.


# -----------------------------------------------------------------------------
# 9B. USO DE IA — ADOPCIÓN Y PERFIL
# -----------------------------------------------------------------------------


roles_amenazados <- c("BI Analyst / Data Analyst", "Business Analyst")
roles_nativos    <- c("Data Scientist", "AI Engineer", "AI / Prompt / Chatbots")
roles_tecnico    <- c("Data Engineer", "SysAdmin / DevOps / SRE", "DBA")

df_ia_rol <- df %>%
  filter(
    !is.na(uso_ia),
    trabajo_de %in% c(roles_amenazados, roles_nativos, roles_tecnico)
  ) %>%
  mutate(
    categoria_rol = case_when(
      trabajo_de %in% roles_amenazados ~ "Amenazados por IA",
      trabajo_de %in% roles_nativos    ~ "Nativos de IA",
      trabajo_de %in% roles_tecnico    ~ "Técnicos / Infraestructura"
    ),
    categoria_rol = factor(
      categoria_rol,
      levels = c("Nativos de IA", "Amenazados por IA", "Técnicos / Infraestructura")
    )
  )


df_ia_rol %>%
  group_by(anio, categoria_rol) %>%
  summarise(uso_ia_medio = mean(uso_ia, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = anio, y = uso_ia_medio, color = categoria_rol, group = categoria_rol)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Nativos de IA"              = "steelblue",
    "Amenazados por IA"          = "tomato",
    "Técnicos / Infraestructura" = "gray50"
  )) +
  labs(
    title = "¿Los roles amenazados adoptaron IA más rápido?",
    subtitle = "Evolución del uso promedio de IA por categoría de rol",
    x = "Año",
    y = "Uso promedio de IA (escala)",
    color = NULL
  ) +
  theme_minimal()


df_ia_rol %>%
  group_by(trabajo_de) %>%
  mutate(mediana_uso = median(uso_ia, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(
    x    = reorder(trabajo_de, mediana_uso),
    y    = uso_ia,
    fill = categoria_rol
  )) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Nativos de IA"              = "steelblue",
    "Amenazados por IA"          = "tomato",
    "Técnicos / Infraestructura" = "gray70"
  )) +
  labs(
    title = "Uso de IA por rol individual",
    subtitle = "Ordenado por mediana — color según exposición a la automatización",
    x = NULL,
    y = "Uso de IA (escala)",
    fill = NULL
  ) +
  theme_minimal()

# ADOPCION DE IA POR GRUPO DE ROL (2024-2026)
# Todos los grupos parten de 25-37% de uso alto en 2024.1 y convergen hacia 60-70% en 2026.1
# Datos / AI lidera desde el inicio — Desarrollo / QA arranca bajo pero termina liderando
# Ciberseguridad es el grupo que crece más lento y se queda rezagado al final
# La convergencia sugiere que la IA se está volviendo transversal independientemente del rol
df %>%
  filter(!is.na(uso_ia), !is.na(grupo_rol)) %>%
  group_by(grupo_rol, periodo) %>%
  summarise(
    pct_alto_uso = mean(uso_ia >= 4, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(periodo = factor(periodo)) %>%  
  ggplot(aes(x = periodo, y = pct_alto_uso, color = grupo_rol, group = grupo_rol)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  labs(
    title = "¿Qué grupos adoptaron IA más rápido?",
    subtitle = "% de encuestados con uso alto de IA (≥ 4) por período",
    x = "Período",
    y = "% con uso alto de IA",
    color = "Grupo de rol"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
# HALLAZGO CONTRAINTUITIVO: los juniors y Semi-Seniopr usan mas IA
# Junior y Semi-Senior tienen mediana 4 — Senior tiene mediana 3
# Los juniors entraron al mercado laboral con IA ya disponible
# Esto sugiere que la IA está siendo incorporada de abajo hacia arriba en las organizaciones  
df %>%
    filter(!is.na(uso_ia), !is.na(seniority)) %>%
    ggplot(aes(x = seniority, y = uso_ia, fill = seniority)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.15, fill = "white", outlier.alpha = 0.2) +
    labs(
      title = "¿Los seniors usan más IA?",
      subtitle = "Distribución del uso de IA por nivel de seniority — 2024 a 2026",
      x = "Seniority",
      y = "Uso de IA (escala)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  

# Las tres medianas son muy similares — el uso de IA no determina el salario de forma clara
# Alto (4-5) tiene una leve ventaja pero la diferencia es pequeña
# Bajo (1-2) supera a Medio (3) — no hay relación lineal
# INTERPRETACION: el uso de IA puede ser una consecuencia del rol y seniority
# más que una causa directa del salario
  df %>%
    filter(!is.na(uso_ia)) %>%
    mutate(uso_ia_grupo = case_when(
      uso_ia <= 2 ~ "Bajo (1-2)",
      uso_ia == 3 ~ "Medio (3)",
      uso_ia >= 4 ~ "Alto (4-5)"
    ),
    uso_ia_grupo = factor(uso_ia_grupo, 
                          levels = c("Bajo (1-2)", "Medio (3)", "Alto (4-5)"))) %>%
    ggplot(aes(x = uso_ia_grupo, y = log_sal_usd, fill = uso_ia_grupo)) +
    geom_boxplot() +
    labs(
      title = "¿Los que más usan IA ganan más?",
      subtitle = "Distribución salarial según nivel de uso de IA — 2024 a 2026",
      x = "Nivel de uso de IA",
      y = "Logaritmo del salario (USD)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  



# -----------------------------------------------------------------------------
# 10. CONCLUSIONES DEL EDA
# -----------------------------------------------------------------------------

# VARIABLES CON MAYOR PODER EXPLICATIVO:
# - Seniority: separacion clara entre niveles, mayor señal del EDA
# - Experiencia: relacion positiva pero con alta dispersion
# - Grupo de rol: diferencias moderadas entre grupos. Management lidera,
#   Desarrollo / QA queda en el extremo inferior a nivel de grupo.
#   A nivel de rol individual, Manager / Director y Architect son los más altos.
# - Genero: persiste una brecha a favor de hombres incluso controlando por seniority

# VARIABLES CON MENOR PODER EXPLICATIVO:
# - Dolarizacion: en pesos nominales mostraba brecha del 34%, pero al deflactar
#   por dolar blue la diferencia practicamente desaparece
# - Modalidad: medianas similares entre remoto, hibrido y presencial
# - Tamaño de empresa: sin tendencia clara ni consistente
# - Region: mercado IT relativamente homogeneo geograficamente

# VARIABLE CONFUNDIDORA IDENTIFICADA:
# - Seniority confunde parcialmente la relacion genero-salario
# - Hombres tienen mayor proporcion de Senior (~53%) vs mujeres (~35%)
# - Sin embargo la brecha persiste dentro de cada nivel de seniority




# DECISION DE MODELADO:
# - Variable objetivo: log_sal_usd — salario deflactado por dolar blue

# PREGUNTAS para generar hipotesis.. 
#
# Que rol maximiza el salario en dolares en el sector IT argentino, controlando por experiencia, region y genero?
# Que rol y nivel de seniority  maximiza el salario en dolares en el sector IT argentino, controlando por experiencia, region y genero?
#
# Yendo por esas preguntas quizas un modelo podria ser:
# Modelo 1: log_sal_usd ~ experiencia + grupo_rol + genero_simple + region + gente_a_cargo
# Modelo 2: agregar seniority
# Se espera que grupo_rol = Management sea la categoría con coeficiente más alto
# y Desarrollo / QA el de menor efecto marginal