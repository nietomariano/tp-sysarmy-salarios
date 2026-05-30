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
# Buscamos a que periodos pertenecen los N/A o la mayor cantidad



# -----------------------------------------------------------------------------
# 4. DISTRIBUCIONES UNIVARIADAS
# -----------------------------------------------------------------------------

# Variables categóricas

df %>%
  count(grupo_rol)

df %>%
  count(seniority)

df %>%
  count(genero_simple)

df %>%
  count(modalidad)

df %>%
  count(gente_a_cargo_grupo)


# Variables numericas


summary(df$sal_bruto)

summary(df$log_sal)

summary(df$experiencia)

summary(df$edad)

summary(df$gente_a_cargo)



# -----------------------------------------------------------------------------
# 5. DISTRIBUCION DE LA VARIABLE OBJETIVO
# -----------------------------------------------------------------------------

ggplot(df, aes(x = sal_bruto)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribución del salario bruto",
    x = "Salario bruto",
    y = "Frecuencia"
  )

ggplot(df, aes(x = log_sal)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribución de log_sal",
    x = "Logaritmo del salario",
    y = "Frecuencia"
  )

ggplot(df, aes(x = log_sal)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30
  ) +
  geom_density() +
  labs(
    title = "Distribución de log_sal con curva de densidad",
    x = "Logaritmo del salario",
    y = "Densidad"
  )

ggplot(df, aes(y = sal_bruto)) +
  geom_boxplot() +
  labs(
    title = "Boxplot del salario bruto",
    y = "Salario bruto"
  )


# La distribución de sal_bruto es muy asimétrica. Luego de aplicar logaritmo 
# (log_sal) se aproxima mucho más a una distribución normal.


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
  geom_smooth(method = "lm") +
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



ggplot(df, aes(x = gente_a_cargo_grupo, y = log_sal)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Salario según personas a cargo",
    x = "Grupo de personas a cargo",
    y = "Logaritmo del salario"
  )


# Observaciones:
#
# - La experiencia muestra una relación positiva con el salario.
# - La edad también presenta una tendencia positiva.
# - Las personas con equipos a cargo parecen percibir salarios mayores.
# - Existe una dispersión considerable, por lo que estas variables
#   no explican por sí solas las diferencias salariales observadas.
#
# Esto sugiere que será necesario incorporar variables categóricas
# como seniority, modalidad, grupo de rol y sueldo dolarizado.


# -----------------------------------------------------------------------------
# 7. VARIABLES CATEGORICAS
# -----------------------------------------------------------------------------


# SENIORITY


ggplot(df, aes(x = seniority, y = log_sal, fill = seniority)) +
  geom_boxplot() +
  theme(legend.position = "none")+
  labs(
    title = "Salario según seniority",
    x = "Seniority",
    y = "Logaritmo del salario"
  )


# El salario aumenta claramente a medida que aumenta el seniority.
# Se observa una separacion marcada entre Junior, Semi-Senior y Senior.


# MODALIDAD

ggplot(df, aes(x = modalidad, y = log_sal)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Salario según modalidad de trabajo",
    x = "Modalidad",
    y = "Logaritmo del salario"
  )

# Obervaciones entre diferencias entre trabajo remot?
# No encuentro algo claro para marcar. Pareciera que presencial disminuye vs Remoto e hibrido.


# SUELDO SEGUN DOLARIZACION


ggplot(df,
       aes(
         x = sueldo_dolarizado,
         y = log_sal,
         fill = sueldo_dolarizado
       )) +
  geom_boxplot() +
  labs(
    title = "Salario según dolarización",
    x = "Sueldo dolarizado",
    y = "Logaritmo del salario"
  ) +
  theme(legend.position = "none")

# SUELDO DOLARIZADO: Experiencia vs Salario segun dolarizacion

ggplot(df,
       aes(
         x = experiencia,
         y = log_sal,
         color = sueldo_dolarizado
       )) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  labs(
    title = "Experiencia vs salario según dolarización",
    x = "Experiencia",
    y = "Logaritmo del salario"
  )



#  Observacionn: qioenes cobran en dolares parecieran tener un mejor sueldo.

# PERSONAS A CARGO

ggplot(df,
       aes(
         x = gente_a_cargo_grupo,
         y = log_sal,
         fill = gente_a_cargo_grupo
       )) +
  geom_boxplot() +
  labs(
    title = "Salario según personas a cargo",
    x = "Grupo",
    y = "Logaritmo del salario"
  ) +
  theme(legend.position = "none")

# OBSERVACIONES: 



# TAMANO DE LA EMPRESA


ggplot(df,
       aes(
         x = tam_empresa,
         y = log_sal
       )) +
  geom_boxplot() +
  labs(
    title = "Salario según tamaño de empresa",
    x = "Tamaño de empresa",
    y = "Logaritmo del salario"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
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


# SUELDOS EN RELACION AL GENERO

ggplot(df, aes(x = genero_simple, y = log_sal)) +
  geom_boxplot()


# GENERO SIN CONTROLAR SENIORITY

ggplot(
  df,
  aes(
    x = genero_simple,
    y = log_sal,
    fill = genero_simple
  )
) +
  geom_boxplot() +
  theme(
    legend.position = "none"
  )

# Posible variable confundidora seniority
# GENERO Vs SALARIO POR SENIORITY

ggplot(
  df,
  aes(
    x = genero_simple,
    y = log_sal,
    fill = seniority
  )
) +
  geom_boxplot()


# DISTRIBUCION SALARIAL POR GENERO Y SENIORITY

ggplot(
  df,
  aes(
    x = genero_simple,
    y = log_sal,
    fill = seniority
  )
) +
  geom_violin()


# DISTRIBUCION DE SENIORITY POR GENERO


ggplot(
  df,
  aes(
    x = genero_simple,
    fill = seniority
  )
) +
  geom_bar(
    position = "dodge"
  )

# PROPORCION

ggplot(
  df,
  aes(
    x = genero_simple,
    fill = seniority
  )
) +
  geom_bar(
    position = "fill"
  )

# Tabla de apoyo para examinar cantidades y proporciones


df %>%
  count(
    genero_simple,
    seniority
  ) %>%
  group_by(genero_simple) %>%
  mutate(
    porcentaje = n / sum(n)
  )


# OBSERVACIONES: 




# -----------------------------------------------------------------------------
# 9. REGION Y ROLES
# -----------------------------------------------------------------------------

# Boxplot: Salario segun region

ggplot(
  df,
  aes(
    x = region,
    y = log_sal,
    fill = region
  )
) +
  geom_boxplot() +
  labs(
    title = "Salario según región",
    x = "Región",
    y = "Logaritmo del salario"
  ) +
  theme(
    legend.position = "none"
  )


# Violin: Salario segun region

ggplot(
  df,
  aes(
    x = region,
    y = log_sal,
    fill = region
  )
) +
  geom_violin() +
  labs(
    title = "Distribución salarial según región",
    x = "Región",
    y = "Logaritmo del salario"
  ) +
  theme(
    legend.position = "none"
  )

# Tabla de apoyo para examinar datos usados

df %>%
  group_by(region) %>%
  summarise(
    mediana_salario = median(sal_bruto, na.rm = TRUE),
    cantidad = n()
  )

# POR ROLES

# Tabla de apoyo para ver cuales son los roles con mas frecuencia.

df %>%
  count(trabajo_de, sort = TRUE)

# TOP 15 ROLES

top_roles <- df %>%
  count(trabajo_de, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(trabajo_de)


df %>%
  filter(trabajo_de %in% top_roles) %>%
  ggplot(
    aes(
      x = reorder(trabajo_de, log_sal, median),
      y = log_sal,
      fill = trabajo_de
    )
  ) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Salario por ocupación",
    x = "Rol",
    y = "Logaritmo del salario"
  ) +
  theme(
    legend.position = "none"
  )


# Boxplot: Salario segun grupo de rol (sector)


ggplot(
  df,
  aes(
    x = grupo_rol,
    y = log_sal,
    fill = grupo_rol
  )
) +
  geom_boxplot() +
  labs(
    title = "Salario según grupo de rol",
    x = "Grupo de rol",
    y = "Logaritmo del salario"
  ) +
  theme(
    legend.position = "none"
  )


# Violin: Salario segun grupo de rol (sector)


ggplot(
  df,
  aes(
    x = grupo_rol,
    y = log_sal,
    fill = grupo_rol
  )
) +
  geom_violin() +
  labs(
    title = "Distribución salarial según grupo de rol",
    x = "Grupo de rol",
    y = "Logaritmo del salario"
  ) +
  theme(
    legend.position = "none"
  )


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
# 10. CONCLUSIONES DEL EDA
# -----------------------------------------------------------------------------


# Principales hallazgos:
