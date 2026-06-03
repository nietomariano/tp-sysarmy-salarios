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
# Desarrollo/QA representan el ~54% del dataset
df %>%
  count(grupo_rol)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# Distribución de seniority — Nota: ~72% NA corresponden a períodos 2019-2023
df %>%
  count(seniority)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# genero_simple — Nota: ~82% Hombres, representacion femenina baja (~14%)
df %>%
  count(genero_simple)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# modalidad — Nota: ~49% NA (períodos pre-2022); entre los disponibles, ~29% remoto
df %>%
  count(modalidad)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )

# gente_a_cargo_grupo — Nota: ~74% sin equipo a cargo
df %>%
  count(gente_a_cargo_grupo)%>%
  mutate(
    porcentaje = round(n/sum(n)*100,1)
  )


# VARIABLES NUMERICAS

# sal_bruto: distribucion asimetrica en pesos — no comparable entre periodos por inflacion
summary(df$sal_bruto)

# sal_usd_blue: salario calculado por dolar blue — comparable entre 2019 y 2026
summary(df$sal_usd_blue)

# log_sal_usd: media y mediana mas cercanas — confirma uso de log como variable objetivo
summary(df$log_sal_usd)

# experiencia: mediana 6 anios, maximo 45
summary(df$experiencia)

# edad: rango 16-70, mediana 32
summary(df$edad)

# gente_a_cargo: mediana 0, media 0  — ~74% sin equipo, distribucion muy sesgada
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



# Boxplot log_sal_usd — mediana ~7, IQR entre 6.5 y 7.5
ggplot(df, aes(y = log_sal_usd)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Boxplot del logaritmo del salario (USD)",
    y = "Logaritmo del salario (USD)",
    x = ""
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


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
# Tendencia positiva similar a experiencia
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
# Senior tiene la caja con mayor dispersión
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


# SUELDOS DOLARIZADOS VS NO
# Dolarizados muestran mediana y Q3 superiores a no dolarizados
# La brecha persiste al deflactar — cobrar en dolares tiene efecto real sobre el salario
# Mayor dispersión en dolarizados — los salarios en ese grupo varían más entre sí
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
    caption = "Solo períodos 2024-2026 | Línea: ajuste lineal | Zona gris: intervalo de confianza"
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
# Sin controlar por seniority — hombres muestran mediana visiblemente superior
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


# Al controlar por seniority las diferencias salariales entre generos se reducen notablemente
# Las cajas de Hombre y Mujer se solapan dentro de cada nivel — medianas muy similares
# Sugiere que la brecha observada sin controlar se explica principalmente por la distribución
# de seniority: más hombres en Senior que mujeres
ggplot(df %>% filter(!is.na(seniority), 
                     genero_simple %in% c("Hombre", "Mujer")),

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



# Hombres tienen mayor proporcion de Senior (~58%) vs mujeres (~40%)
# Mujeres tienen mayor proporcion de Junior y Semi-Senior
# La distribucion de seniority explica gran parte de la brecha salarial entre generos
ggplot(df %>% filter(!is.na(seniority),
                     genero_simple %in% c("Hombre", "Mujer")),
       
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


# Tabla de apoyo
df %>% count(region)



# SALARIO SEGUN GRUPO DE ROL
# Roles de gestión lidera con mediana claramente superior al resto
# Ciberseguridad segunda, seguida de cerca por Infraestructura y Datos / AI
# Desarrollo / QA tiene la mediana más baja de todos los grupos
# Las diferencias entre los grupos del medio son pequeñas
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

# TOP 10 PUESTOS
# Top 10 roles por cantidad de observaciones en el dataset
top_roles <- df %>%
  count(trabajo_de, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(trabajo_de)

# Boxplot de salario por rol — ordenado por mediana, coloreado por grupo
# Los grupos de roles mejor pagos son de gestion e infraestructura: (Manager/Director y Architect)
# Los roles de Datos/AI (Business Analyst, BI Analyst, Data analyst) se ubican en la mitad baja
# QA/Tester tiene la mediana mas baja — Desarrollo/QA concentra los roles peor pagos
# Developer tiene alta dispersion hacia la derecha — outliers de salarios muy altos
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


# Desarrollo/QA tiene la pendiente más pronunciada — la experiencia premia mas en ese grupo
# Roles de gestion arranca con el intercepto mas alto — salario base superior desde el inicio
# Infraestructura tiene la pendiente mas baja — la experiencia premia menos
# Ciberseguridad y Datos/AI muestran pendientes similares y convergen al final
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


# Se excluye Roles de gestion de la comparacion
# Para ver diferencia unicamente entre grupos tecnicos

# Ciberseguridad y Datos/AI muestran pendientes muy similares y arrancan desde el mismo punto
# Infraestructura tiene la pendiente mas baja y el intercepto mas bajo — crece más lento
# Desarrollo/QA tiene el intercepto mas bajo pero la pendiente mas alta —
# arranca peor pagado que el resto pero la experiencia lo premia mas que a cualquier otro grupo
# Un perfil senior de Desarrollo/QA termina igualando o superando a Ciberseguridad y Datos/AI
df %>%
  filter(!is.na(experiencia), !is.na(log_sal_usd),
         grupo_rol != "Roles de gestión") %>%
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
# Roles de gestion lidera entre grupos — Desarrollo/QA queda en el extremo inferior.
# A nivel de rol individual, Manager/Director y Architect son los mejor pagos.
# QA/Tester y Consultant son los peor pagos del top 10.
# La experiencia premia más en Desarrollo/QA — Infraestructura crece mas lento.
# Region y grupo de rol serán incluidos como variables de control en el modelo.


# -----------------------------------------------------------------------------
# 9B. USO DE IA — ADOPCIÓN Y PERFIL
# -----------------------------------------------------------------------------


roles_amenazados <- c("BI Analyst / Data Analyst", "Business Analyst")
roles_nativos    <- c("Data Scientist", "AI Engineer", "AI / Prompt / Chatbots")
roles_tecnico    <- c("Data Engineer", "SysAdmin / DevOps / SRE", "DBA","DBA (Database Administrator)")

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
  # - Seniority: mayor señal del EDA — separacion clara y progresiva entre los tres niveles.
  #   Junior mediana ~7.0, Semi-Senior ~7.5, Senior ~7.8 (log_sal_usd).
  #   Confirmado por el modelo: ascender a Senior implica un incremento del ~70% sobre Junior.
  #
  # - Gente a cargo: tendencia progresiva y consistente — la mediana sube de ~7.0 (sin equipo)
  #   a ~7.8 (equipo grande). Es la variable numerica con la señal más limpia del EDA.
  #
  # - Grupo de rol: Roles de gestion lidera con mediana claramente superior al resto.
  #   Los cuatro grupos tecnicos (Ciberseguridad, Datos/AI, Infraestructura, Desarrollo/QA)
  #   muestran medianas muy comprimidas entre si — las diferencias existen pero son pequeñas.
  #   A nivel de rol individual, Manager/Director y Architect son los mejor pagos;
  #   QA/Tester y Consultant son los peores pagos del top 10.
  #
  # - Experiencia: relacion positiva pero con alta dispersion — no determina el salario de 
  #   forma aislada. Hallazgo destacado: la pendiente es más pronunciada en Desarrollo/QA —
  #   ese grupo arranca con el intercepto más bajo pero es el que más se beneficia
  #   de cada año adicional de experiencia (ver Rplot20).
  #
  # - Genero: brecha a favor de hombres visible sin controlar (Rplot13).
  #   Al controlar por seniority las cajas se solapan dentro de cada nivel,
  #   pero la brecha persiste — especialmente en Semi-Senior y Senior.
  #   Confirmado por el modelo: ser mujer se asocia con un salario ~10% menor (coef. -0.106, ***).
  #
  # - Dolarizacion: su efecto no es directo sino mediado por la experiencia.
  #   Las medianas de arranque son similares entre grupos, pero los dolarizados muestran
  #   mayor dispersion hacia salarios altos (IQR mas amplio — Rplot10).
  #   El efecto principal aparece en la interaccion con experiencia: la pendiente salarial
  #   de los dolarizados es significativamente mas pronunciada — la brecha crece con los
  #   años de carrera (Rplot11). Confirmado en el modelo: la interaccion
  #   experiencia:sueldo_dolarizado es altamente significativa (***) y sube el R²
  #   de 0.2881 a 0.3373.
  #
  #
  #
  #
  # VARIABLES CON MENOR PODER EXPLICATIVO:
  #
  # - Modalidad: Remoto e Hibrido muestran medianas similares (~7.2 en log_sal_usd).
  #   Presencial queda claramente por debajo (~6.9) — diferencia equivalente a ~26% en USD.
  #   La distincion relevante es Presencial vs el resto, no entre Remoto e Hibrido.
  #
  # - Region: CABA y GBA/Prov. BA muestran medianas casi identicas.
  #   Interior es la que queda por debajo de forma consistente.
  #   El modelo confirma: GBA = -0.062, Interior = -0.123 respecto a CABA.
  #   El mercado IT es relativamente homogeneo entre CABA y GBA; Interior muestra
  #   una diferencia real pero moderada (~13% menos que CABA).
  #
  # - Tamaño de empresa: sin tendencia clara ni consistente (Rplot12).
  #   Leve crecimiento hasta empresas medianas pero se estabiliza — no es un predictor relevante.
  #
  # HALLAZGOS SOBRE USO DE IA (2024-2026):
  # - La adopcion crecio en todos los grupos: de ~25-37% a ~60-70% de uso alto en dos años.
  # - Ciberseguridad es el grupo que adopta mas lento y queda rezagado al final.
  # - Datos/AI lidera desde el inicio; Desarrollo/QA arranca bajo pero termina liderando.
  # - Contraintuitivo: Junior y Semi-Senior usan IA mas intensamente que Senior (mediana 4 vs 3).
  #   Interpretacion: los juniors ingresaron al mercado con IA ya disponible —
  #   la adopcion se da de abajo hacia arriba en las organizaciones.
  # - El uso de IA no se traduce directamente en mayor salario — esta mediado por rol y seniority.
  #
  # DECISION DE MODELADO:
  # - Variable objetivo: log_sal_usd — salario deflactado por dolar blue (2019-2026)
  # - Ambos modelos trabajan con datos desde 2024, unico periodo con seniority 
  #   disponible (n = 17.079 tras drop_na)
  #
  # - Modelo 1: log_sal_usd ~ experiencia + grupo_rol + seniority + genero_simple + region
  #   R² = 0.2881 | RSE = 0.552
  #
  # - Modelo 2: log_sal_usd ~ experiencia * sueldo_dolarizado + grupo_rol + seniority
  #             + genero_simple + region
  #   R² = 0.3373 | RSE = 0.533
  #   La interaccion experiencia:sueldo_dolarizado es altamente significativa (***)
  #
  # - Modelo 3: log_sal_usd ~ poly(experiencia, 2) * sueldo_dolarizado + grupo_rol + seniority
  #             + genero_simple + region
  #   R² = 0.3454 | RSE = 0.530
  #   Permite capturar la curvatura de la experiencia (rendimientos decrecientes)
  #
  # - Los tres modelos son comparados con ANOVA — cada uno agrega explicacion significativa (***)
  #
  # - Se espera que seniority sea el predictor con mayor impacto individual
  #   y Roles de gestion el grupo con mayor intercepto relativo
  