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
  "data/processed/df_sysarmy.csv",
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 2. ESTRUCTURA GENERAL
# -----------------------------------------------------------------------------

glimpse(df)

dim(df)

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
# Las variables principales utilizadas para el análisis no presentan
# valores faltantes.
#
# Seniority, sueldo_dolarizado y uso_ia presentan aproximadamente
# un 67% de valores faltantes. Sin embargo, el analisis por períodos
# nos muestra que estos faltantes no son aleatorios.
#
# Las variables seniority y sueldo_dolarizado se encuentran
# disponibles a partir de la encuesta del periodo 2024.1,
# mientras que uso_ia aparece a partir de 2022.2.
#
# Por lo tanto, los valores faltantes responden a cambios en el
# cuestionario de Sysarmy entre distintos períodos y no a errores
# de carga o falta de respuesta de los encuestados.



# -----------------------------------------------------------------------------
# 4. DISTRIBUCIONES UNIVARIADAS
# -----------------------------------------------------------------------------

# Variables categóricas

# grupo_rol — Data Platform / MLOps representa el 50% del dataset
df %>%
  count(grupo_rol)

# Distribución de seniority — 66.7% NA corresponden a períodos 2019-2023
df %>%
  count(seniority)

# genero_simple — 82% Hombre, representación femenina baja (14%)
df %>%
  count(genero_simple)

# modalidad — 46% NA (períodos pre-2022); entre los disponibles, 50% remoto
df %>%
  count(modalidad)

# gente_a_cargo_grupo — 80% sin equipo a cargo
df %>%
  count(gente_a_cargo_grupo)



# Variables numericas


# sal_bruto: media (1.049.037) muy superior a mediana (270.000)
# la mayoría de las personas gana bastante menos de $1 millon
# existe un grupo relativamente pequeño con salarios muy altos
# esos salarios altos empujan la media hacia arriba

# — distribución asimétrica, confirma uso de log_sal para el modelado
summary(df$sal_bruto)

# log_sal: distribución más simétrica tras transformación logarítmica
summary(df$log_sal)

# experiencia: máximo de 65 años, posible outlier a revisar
summary(df$experiencia)

# edad: rango 17-70, sin valores corruptos. 5 NAs — se mantienen, 
# son pocos y no afectan el análisis
summary(df$edad)

# gente_a_cargo: mediana 0, 75% sin equipo — distribución muy sesgada
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
      x = "salario bruto",
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



ggplot(df, aes(x = log_sal)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30, fill = "steelblue", color = "black", alpha = 0.6
  ) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribución de log_sal con curva de densidad",
    x = "Logaritmo del salario",
    y = "Densidad"
  )


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

# Experiencia vs salario

ggplot(df, aes(x = experiencia, y = log_sal)) +
  geom_jitter(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Salario vs experiencia",
    x = "Años de experiencia",
    y = "Logaritmo del salario"
  )


# Edad vs salario

ggplot(df, aes(x = edad, y = log_sal)) +
  geom_jitter(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Salario vs edad",
    x = "Edad",
    y = "Logaritmo del salario"
  )


# Personas a cargo vs salario

ggplot(df, aes(x = gente_a_cargo, y = log_sal)) +
  geom_jitter(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm") +
  labs(
    title = "Salario vs personas a cargo",
    x = "Personas a cargo",
    y = "Logaritmo del salario"
  )



ggplot(df, aes(x = factor(gente_a_cargo_grupo, 
                           levels = c("Sin equipo","Equipo pequeño",
                                      "Equipo mediano","Equipo grande")), 
                y = log_sal)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Salario según personas a cargo",
    x = "Grupo de personas a cargo",
    y = "Logaritmo del salario"
  )



# Observaciones:



# -----------------------------------------------------------------------------
# 7. VARIABLES CATEGORICAS
# -----------------------------------------------------------------------------


# SENIORITY

# Filtramos NA porque no aporta al grafico y mejoramos la visual.
# Con caption hacemos aclaracion del filtro ( Check List)

ggplot(df %>% filter(!is.na(seniority)), 
       aes(x = seniority, y = log_sal, fill = seniority)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según seniority",
    x = "Seniority",
    y = "Logaritmo del salario bruto (log)",
    caption = "Solo períodos 2024-2026 | n = 5.091 observaciones"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# MODALIDAD

# Filtramos NA porque no aporta al grafico y mejoramos la visual.
# Con caption hacemos aclaracion del filtro ( Check List)


ggplot(df %>% filter(!is.na(modalidad)),
       aes(x = modalidad, y = log_sal, fill = modalidad)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según modalidad de trabajo",
    x = "Modalidad de trabajo",
    y = "Logaritmo del salario bruto (log)",
    caption = "Períodos 2019-2022 | n = 8.195 observaciones"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# SUELDO SEGUN DOLARIZACION

# Filtramos NA porque no aporta al grafico y mejoramos la visual.
# Con caption hacemos aclaracion del filtro ( Check List)



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
  theme_minimal() +
  theme(legend.position = "none")

# SUELDO DOLARIZADO: Experiencia vs Salario segun dolarizacion
# SIN N/A, aclaramos con caption


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



#  Observacionn: 

# PERSONAS A CARGO


ggplot(df,
       aes(x = factor(gente_a_cargo_grupo,
                      levels = c("Sin equipo", "Equipo pequeño",
                                 "Equipo mediano", "Equipo grande")),
           y = log_sal,
           fill = gente_a_cargo_grupo)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según tamaño del equipo a cargo",
    x = "Tamaño del equipo a cargo",
    y = "Logaritmo del salario bruto (log)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# OBSERVACIONES: 



# TAMANO DE LA EMPRESA
# forzamos el orden de los datos en el eje x porque ordena alfabeticamente  las categorias y queda desordenado #.


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
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# OBSERVACIONES: 


# TABLAS RESUMEN

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




# Genero vs salario — sin controlar por seniority

ggplot(df,
       aes(x = genero_simple, y = log_sal, fill = genero_simple)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según género",
    x = "Género",
    y = "Logaritmo del salario bruto (log)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Genero vs salario sin controlar — mediana mujeres levemente superior a hombres

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



# Seniority como confundidora — hombres ~50% Senior vs mujeres ~30% Senior

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


# Tabla de apoyo

df %>%
  group_by(grupo_rol) %>%
  summarise(
    mediana_salario = median(sal_bruto, na.rm = TRUE),
    cantidad = n()
  ) %>%
  arrange(desc(mediana_salario))



# OBSERVACIONES: 

# -----------------------------------------------------------------------------
# 9. REGIONES Y ROLES
# -----------------------------------------------------------------------------

# Salario según región
# Grafico región — GBA/Prov. BA tiene una caja muy compacta y alta. 


ggplot(df,
       aes(x = region, y = log_sal, fill = region)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según región",
    x = "Región",
    y = "Logaritmo del salario bruto (log)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Observamos cantidad de observaciones en GBA para descartar errores
# 915 observaciones son suficientes para comparar
# los salarios en GBA están más concentrados en un rango alto con poca dispersion.
df %>% count(region)




# Salario según grupo de rol
ggplot(df,
       aes(x = grupo_rol, y = log_sal, fill = grupo_rol)) +
  geom_boxplot() +
  labs(
    title = "Distribución salarial según grupo de rol",
    x = "Grupo de rol",
    y = "Logaritmo del salario bruto (log)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )


# Top roles individuales — salario por ocupación
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


# -----------------------------------------------------------------------------
# 10. CONCLUSIONES DEL EDA
# -----------------------------------------------------------------------------


# Principales hallazgos:
