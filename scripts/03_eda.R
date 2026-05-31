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

# -----------------------------------------------------------------------------
# 2. ESTRUCTURA GENERAL
# -----------------------------------------------------------------------------

glimpse(df) # 15.288
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

# grupo_rol — "Data Platform / MLOps" representa el ~50% del dataset.
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


### VARIABLES NUMERICAS


# sal_bruto: media (1.049.037) - mediana (270.000) — distribucion asimetrica
# confirma uso de log_sal como variable objetivo
summary(df$sal_bruto)

# log_sal: media (12.74) y mediana (12.51) mas cercanas - buen uso de log()
summary(df$log_sal)

# experiencia: mediana 6 anios, máximo 65 — posible outlier en el maximo
summary(df$experiencia)

# edad: rango 17-70, 5 NA — no afectan el analisis
summary(df$edad)

# gente_a_cargo: mediana 0, media 0.74 — 80% sin equipo, distribucion muy sesgada
summary(df$gente_a_cargo)



# -----------------------------------------------------------------------------
# 5. DISTRIBUCION DE LA VARIABLE OBJETIVO
# -----------------------------------------------------------------------------


  ggplot(df, aes(x = sal_bruto)) +
    geom_histogram(
      bins = 40,
      fill = "steelblue",
      color = "white"
    ) +
    labs(
      title = "Distribución del salario bruto",
      x = "Salario bruto (ARS)",
      y = "Frecuencia"
    ) +
    theme_minimal()



ggplot(df, aes(x = log_sal)) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución del logaritmo del salario",
    x = "Logaritmo del salario",
    y = "Frecuencia"
  ) +
  theme_minimal()



# Histograma con densidad — linea roja muestra curva de densidad estimada
# Grafico principal de esta seccion — se observa distribucion bimodal
# Dos picos sugieren -> dos grupos salariales distintos (pesos vs dolares)
ggplot(df, aes(x = log_sal)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40, fill = "steelblue",color = "white"
  ) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribución de log_sal con curva de densidad",
    x = "Logaritmo del salario",
    y = "Densidad"
  )+
  theme_minimal()



# Boxplot del logarito del salario bruto
ggplot(df, aes(y = log_sal)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Boxplot del logaritmo del salario",
    y = "Logaritmo del salario",
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
ggplot(df, aes(x = experiencia, y = log_sal)) +
  geom_jitter(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relación entre experiencia y salario",
    x = "Años de experiencia",
    y = "Logaritmo del salario bruto (log)",
    caption = "Línea roja: ajuste lineal"
  )+
  theme_minimal()



# Relacion entre Edad y salario
# Tendencia positiva similar a experiencia pero con mayor dispersion
# Edad y experiencia probablemente correlacionadas — riesgo de multicolinealidad
# Se evaluara incluir solo una de las dos en el modelo final
ggplot(df, aes(x = edad, y = log_sal)) +
  geom_jitter(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relación entre edad y salario",
    x = "Edad (años)",
    y = "Logaritmo del salario bruto (log)",
    caption = "Línea roja: ajuste lineal"
  ) +
  theme_minimal()


# Relacion entre Personas a cargo y salario
# 80% de observaciones en x=0 — ajuste lineal poco confiable
# Zona gris del lm se abre hacia la derecha por escasez de datos en valores altos
# El boxplot por grupos probablemente sea mas informativo para esta variable
ggplot(df, aes(x = gente_a_cargo, y = log_sal)) +
  geom_jitter(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm") +
  labs(
    title = "Relación entre personas a cargo y salario",
    x = "Cantidad de personas a cargo",
    y = "Logaritmo del salario bruto (log)",
    caption = "Línea roja: ajuste lineal | Zona gris: margen de incertidumbre"
  ) +
  theme_minimal()

# Relacion entre Grupo de personas a cargo y salario
# Mediana salarial aumenta progresivamente con el tamaño del equipo
# Cajas de tamaño similar — dispersion parecida entre los grupos
# Equipo grande tiene pocos casos
ggplot(df, aes(x = factor(gente_a_cargo_grupo, 
                           levels = c("Sin equipo","Equipo pequeño",
                                      "Equipo mediano","Equipo grande")), 
                y = log_sal)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Distribución salarial según tamaño del equipo a cargo",
    x = "Tamaño del equipo a cargo",
    y = "Logaritmo del salario bruto (log)"
  )+
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
       aes(x = seniority, y = log_sal, fill = seniority)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según seniority",
    x = "Seniority",
    y = "Logaritmo del salario bruto (log)",
    caption = "Solo períodos 2024-2026 | n = 5.091 observaciones"
  )+
  theme_minimal()


# MODALIDAD
# Presencial muestra mediana levemente inferior y menor dispersión
# Medianas similares entre Remoto e Hibrido
# Modalidad no pareciera ser un determinante relevante del salario

ggplot(df %>% filter(!is.na(modalidad)),
       aes(x = modalidad, y = log_sal, fill = modalidad)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según modalidad de trabajo",
    x = "Modalidad de trabajo",
    y = "Logaritmo del salario bruto (log)",
    caption = "Períodos 2019-2022 | n = 8.195 observaciones"
  ) +
  theme_minimal()


# DOLARIZACION
# Diferencia salarial mas pronunciada de todas las variables categóricas
# Dolarizado: mediana 14.9 ($2.916.000) vs No dolarizado: 14.6 ($2.180.000)
# Confirma hipotesis de bimodalidad observada en sección 5

ggplot(df %>% filter(!is.na(sueldo_dolarizado)),
       aes(x = factor(sueldo_dolarizado, 
                      labels = c("No dolarizado", "Dolarizado")),
           y = log_sal,
           fill = factor(sueldo_dolarizado))) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según dolarización del sueldo",
    x = "Tipo de sueldo",
    y = "Logaritmo del salario bruto (log)",
    caption = "Solo períodos 2024-2026 | n = 5.091 observaciones"
  ) +
  theme_minimal()

# Tabla de apoyo para visualizar mediana de log_sal segun sueldo dolarizado 
# TRUE o FALSE

df %>%
  filter(!is.na(sueldo_dolarizado)) %>%
  group_by(sueldo_dolarizado) %>%
  summarise(
    mediana = median(log_sal, na.rm = TRUE),
    mediana_pesos = median(sal_bruto, na.rm = TRUE),
    n = n()
  )


# RELACION ENTRE EXPERIENCIA y SALARIO SEGUN DOLARIZACION
# Brecha salarial entre dolarizados y no dolarizados se mantiene en toda 
# la trayectoria
# Las líneas no son paralelas — brecha crece levemente con la experiencia
# Dolarizar el sueldo tiene mayor impacto salarial a largo plazo

ggplot(df %>% filter(!is.na(sueldo_dolarizado)),
       aes(x = experiencia, 
           y = log_sal,
           color = factor(sueldo_dolarizado, 
                          labels = c("No dolarizado", "Dolarizado")))) +
  geom_point(alpha = 0.4, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre experiencia y salario según dolarización",
    x = "Años de experiencia",
    y = "Logaritmo del salario bruto (log)",
    color = "Tipo de sueldo",
    caption = "Línea: ajuste lineal con intervalo de confianza al 95% | Solo períodos 2024-2026"
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
           y = log_sal)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Distribución salarial según tamaño de la organización",
    x = "Cantidad de personas en la organización",
    y = "Logaritmo del salario bruto (log)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  theme_minimal()



# CONCLUSION SECCION 7:
# Seniority y dolarización son las variables categóricas con mayor poder discriminante.
# Modalidad y tamaño de empresa no muestran diferencias salariales relevantes.
# Dolarización confirma hipótesis de bimodalidad — brecha del 34% en salario bruto.

# Tablas de apoyo

# mediana por seniority

df %>%
  group_by(seniority) %>%
  summarise(
    mediana_salario = median(sal_bruto, na.rm = TRUE),
    cantidad = n()
  )


# mediana por modalidad

df %>%
  group_by(modalidad) %>%
  summarise(
    mediana_salario = median(sal_bruto, na.rm = TRUE),
    cantidad = n()
  )


# mediana por sueldo dolarizado

df %>%
  group_by(sueldo_dolarizado) %>%
  summarise(
    mediana_salario = median(sal_bruto, na.rm = TRUE),
    cantidad = n()
  )


# -----------------------------------------------------------------------------
# 8. VARIABLES POTENCIALMENTE CONFUNDIDORAS
# -----------------------------------------------------------------------------



# GENERO
# Sin controlar por seniority — mujeres muestran mediana levemente superior
# Conclusion apresurada: no hay brecha salarial de genero
# Este grafico solo es el punto de partida — ver gráficos 2 y 3
ggplot(df,
       aes(x = genero_simple, y = log_sal, fill = genero_simple)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según género",
    x = "Género",
    y = "Logaritmo del salario bruto (log)"
  ) +
  theme_minimal()


# Controlando por seniority - distribuciones de hombre y mujer muy similares
# El patron Junior < Semi-Senior < Senior se repite igual en ambos generos.
# La brecha que observamos en el grafico 1 desaparece al controlar por seniority.
ggplot(df %>% filter(!is.na(seniority)),
       aes(x = genero_simple, y = log_sal, fill = seniority)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según género y seniority",
    x = "Género",
    y = "Logaritmo del salario bruto (log)",
    fill = "Seniority",
    caption = "Solo períodos 2024-2026 | n = 5.091 observaciones"
  ) +
  theme_minimal()



# Hombres tienen mayor proporcion de Senior (~50%) vs mujeres (~30%)
# Mujeres tienen mayor proporcion de Junior y Semi-Senior
# Explica porque el genero parece relevante sin controlar si 
# es el seniority el factor real
ggplot(df %>% filter(!is.na(seniority)),
       aes(x = genero_simple, fill = seniority)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporción de seniority según género",
    x = "Género",
    y = "Proporción",
    fill = "Seniority",
    caption = "Solo períodos 2024-2026 | n = 5.091 observaciones"
  ) +
  theme_minimal()


# Tabla de Apoyo - Mediana de salario por grupo_rol
df %>%
  group_by(grupo_rol) %>%
  summarise(
    mediana_salario = median(sal_bruto, na.rm = TRUE),
    cantidad = n()
  ) %>%
  arrange(desc(mediana_salario))


# CONCLUSION SECCION 8:
# El genero no es un determinante salarial independiente.
# Seniority actua como variable confundidora — hombres Senior ~50%  vs 
# mujeres Senior ~30%.
# Al controlar por seniority, las diferencias salariales entre generos desaparecen.


# -----------------------------------------------------------------------------
# 9. REGIONES Y ROLES
# -----------------------------------------------------------------------------

# SALARIO SEGUN REGION
# GBA tiene mediana mas alta y distribucion más compacta que CABA e Interior
# Posiblemente esto refleje profesionales remotos trabajando para empresas 
# de CABA.
# CABA e Interior tienen medianas similares pero mayor dispersion
# Observaciones: CABA=8.461, GBA=915, Interior=5.912
ggplot(df,
       aes(x = region, y = log_sal, fill = region)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según región",
    x = "Región",
    y = "Logaritmo del salario bruto (log)"
  ) +
  theme_minimal()

df %>% count(region)



# SALARIO SEGUN GRUPO DE ROL
# Data Science / AI tiene la mediana mas alta — Data Platform / MLOps la 
# mas baja
# Diferencias entre grupos menores de lo esperado — medianas similares
ggplot(df,
       aes(x = grupo_rol, y = log_sal, fill = grupo_rol)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según grupo de rol",
    x = "Grupo de rol",
    y = "Logaritmo del salario bruto (log)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )+
  theme_minimal()


# TOP DE ROLES CON FILL POR GRUPO — salario por ocupación
# Entre los puestos AI Engineer tiene la mediana mas alta - DBA la mas baja 
# Alta variabilidad dentro de cada grupo de rol
# El puesto (rol) es una etiqueta — seniority y dolarización
# determinan el salario independientemente del puesto especifico
top_roles <- df %>%
  count(trabajo_de, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(trabajo_de)

ggplot(df %>% filter(trabajo_de %in% top_roles),
       aes(x = reorder(trabajo_de, log_sal, median),
           y = log_sal,
           fill = grupo_rol)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribución salarial por rol — Top 10",
    x = "Rol",
    y = "Logaritmo del salario bruto (log)",
    fill = "Grupo de rol"
  ) +
  theme_minimal()


# CONCLUSION SECCION 9:
# Las diferencias regionales son menores de lo esperado — mercado IT relativamente homogeneo.
# Entre roles, AI Engineer lidera y DBA queda en el extremo inferior.
# Grupo de rol y region pueden ser variables de control en el modelado.


# -----------------------------------------------------------------------------
# 10. CONCLUSIONES DEL EDA
# -----------------------------------------------------------------------------


# VARIABLES CON MAYOR PODER EXPLICATIVO:
# - Seniority: separación clara entre niveles, mayor señal del EDA
# - Dolarización: brecha del 34% en salario bruto, confirma bimodalidad
# - Experiencia: relación positiva pero con alta dispersión
# - Grupo de rol: Data Science / AI lidera, diferencias moderadas

# VARIABLES CON MENOR PODER EXPLICATIVO:
# - Modalidad: medianas similares entre remoto, híbrido y presencial
# - Tamaño de empresa: sin tendencia clara ni consistente
# - Región: mercado IT relativamente homogéneo geográficamente

# VARIABLE CONFUNDIDORA IDENTIFICADA:
# - Seniority confunde la relación género-salario
# - Al controlar por seniority, diferencias entre géneros desaparecen

# DECISIÓN DE MODELADO:
# - Modelo 1: datos completos 2019-2026, sin seniority ni dolarización
# - Modelo 2: datos 2024-2026, incorpora seniority y dolarización
# - Comparación mediante ANOVA
