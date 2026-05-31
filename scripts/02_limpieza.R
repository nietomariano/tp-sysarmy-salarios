library(tidyverse)


# -----------------------------------------------------------------------------
# 1. CARGA DEL DATASET CONSOLIDADO
# -----------------------------------------------------------------------------

# INPUT: Sysarmy_consolidado.csv — 75.649 observaciones, 26 variables
df_sysarmy <- read_csv(
  "data/intermediate/sysarmy_consolidado.csv",
  show_col_types = FALSE
)

# Verificacion de inputs
dim(df_sysarmy)



# -----------------------------------------------------------------------------
# 2. LIMPIEZA DE TIPOS
# -----------------------------------------------------------------------------

# Las variables numéricas llegan como character por formatos inconsistentes
# entre encuestas — parse_number extrae el número ignorando símbolos y separadores

# Guardamos NA antes de la conversión para comparar después
na_antes <- colSums(is.na(df_sysarmy[c("salario_bruto", "salario_neto",
                                       "edad", "experiencia",
                                       "antiguedad_empresa", "antiguedad_puesto",
                                       "gente_a_cargo")]))



df_sysarmy <- df_sysarmy %>%
  mutate(
    salario_bruto = parse_number(as.character(salario_bruto)),
    salario_neto = parse_number(as.character(salario_neto)),
    edad = parse_number(as.character(edad)),
    experiencia = parse_number(as.character(experiencia)),
    antiguedad_empresa = parse_number(as.character(antiguedad_empresa)),
    antiguedad_puesto = parse_number(as.character(antiguedad_puesto)),
    gente_a_cargo = parse_number(as.character(gente_a_cargo))
  )

# NA después de la conversión
na_despues <- colSums(is.na(df_sysarmy[c("salario_bruto", "salario_neto",
                                         "edad", "experiencia",
                                         "antiguedad_empresa", "antiguedad_puesto",
                                         "gente_a_cargo")]))


# NA generados por parse_number — valores que existian pero no eran convertibles
na_despues - na_antes
# Resultado:
# salario_bruto: 47 NA nuevos — texto registrado en la encuesta en lugar de numeros
# salario_neto:   9 NA nuevos — mismo problema que salario_bruto en menor cantidad
# resto de variables: 0 NA nuevos — conversión exitosa
# Los 56 registros afectados van a ser eliminados en el paso 6


# -----------------------------------------------------------------------------
# VERIFICACIÓN Y ELIMINACIÓN DE DUPLICADOS
# -----------------------------------------------------------------------------

filas_totales <- nrow(df_sysarmy)
filas_unicas  <- nrow(distinct(df_sysarmy))
duplicados    <- filas_totales - filas_unicas

cat("Filas totales:", filas_totales, "\n")
cat("Filas únicas:", filas_unicas, "\n")
cat("Duplicados:", duplicados, "\n")
# Resultado: 290 duplicados (0.38%) — encuestados que enviaron el formulario más de una vez
# Decisión: se eliminan con distinct() — no aportan información nueva

df_sysarmy <- df_sysarmy %>%
  distinct()

nrow(df_sysarmy)  # 75.359 observaciones finales


# -----------------------------------------------------------------------------
# 3. DEFINICIÓN DE ROLES A INCLUIR
# -----------------------------------------------------------------------------

# Se definen los roles del sector data y tecnología a incluir en el analisis
# Los strings deben coincidir exactamente con los valores de la columna trabajo_de
# Si no matchean se pierdan observaciones silenciosamente

roles_core <- c(
  "Data Scientist",
  "Data Engineer",
  "BI Analyst / Data Analyst",
  "AI Engineer",
  "AI / Prompt / Chatbots"
)

roles_data_platform <- c(
  "SysAdmin / DevOps / SRE",
  "DBA",
  "Infraestructura"
)

roles_analytics <- c(
  "Business Analyst",
  "Functional Analyst",
  "Automation / RPA",
  "Product Analyst",
  "Analista de Procesos"
)

roles_governance <- c(
  "Data Governance / GRC",
  "Infosec",
  "IT Security",
  "Analista de Cyberseguridad",
  "Analista de seguridad",
  "Business Continuity Analyst"
)

todos_los_roles <- c(
  roles_core,
  roles_data_platform,
  roles_analytics,
  roles_governance
)


# Verificacion — roles definidos que no aparecen en el dataset
roles_no_encontrados <- setdiff(todos_los_roles, unique(df_sysarmy$trabajo_de))
roles_no_encontrados
# Resultado: character(0) — todos los roles matchean correctamente



# -----------------------------------------------------------------------------
# 4. FILTRADO POR ROL
# -----------------------------------------------------------------------------


# Se filtra conservando solo los roles del sector data y tecnologia
# El dataset pasa de 75.359 a aproximadamente 15583 observaciones y luego
# a 1540 post eliminar roles con menos de 10 observaciones.
df_clean <- df_sysarmy %>%
  filter(trabajo_de %in% todos_los_roles)

# Verificacion de registros post-filtro
dim(df_clean) # 15583

# Distribucion por grupo de rol
# Identificamos roles con pocas observaciones
df_clean %>%
  count(trabajo_de, sort = TRUE)

# Roles con menos de 10 observaciones no son representativos para el analisis
# Se eliminan para evitar distorsiones en los grupos
roles_minimos <- df_clean %>%
  count(trabajo_de) %>%
  filter(n >= 10)

df_clean <- df_clean %>%
  filter(trabajo_de %in% roles_minimos$trabajo_de)

# Verificacion final
dim(df_clean) # 15540
df_clean %>%
  count(trabajo_de, sort = TRUE)


# -----------------------------------------------------------------------------
# 5. CREACIÓN DE VARIABLES PARA EL ANÁLISIS
# -----------------------------------------------------------------------------


# Asigna cada rol a uno de los cuatro grupos definidos en el paso 3
# No se necesita TRUE ~ "Otro" porque el paso 4 ya garantiza
# que todas las observaciones pertenecen a alguno de los cuatro grupos
df_clean <- df_clean %>%
  mutate(
    grupo_rol = case_when(
      trabajo_de %in% roles_core ~ "Data Science / AI",
      trabajo_de %in% roles_data_platform ~ "Data Platform / MLOps",
      trabajo_de %in% roles_analytics ~ "Analytics / Automation",
      trabajo_de %in% roles_governance ~ "Governance / Security"
      
    ),
    
    grupo_rol = factor(
      grupo_rol,
      levels = c(
        "Data Science / AI",
        "Data Platform / MLOps",
        "Analytics / Automation",
        "Governance / Security"
      )
    ),
    
    
    
    # ordered = TRUE establece jerarquia: Junior < Semi-Senior < Senior
    # El ~67% de los valores son NA — estructurales, no errores
    # Corresponden a períodos 2019-2023 donde la variable no fue relevada
    seniority = factor(
      seniority,
      levels = c("Junior", "Semi-Senior", "Senior"),
      ordered = TRUE
    ),
    
   
    # Variable continua convertida a grupos para facilitar el analisis
    # El ~80% tiene gente_a_cargo = 0. La variable continua es muy sesgada
    # TRUE ~ NA_character_ asume los NA originales
    # ordered = TRUE establece jerarquía desde Sin equipo < hasta.. < Equipo grande
    gente_a_cargo_grupo = case_when(
      gente_a_cargo == 0 ~ "Sin equipo",
      gente_a_cargo >= 1 & gente_a_cargo <= 4 ~ "Equipo pequeño",
      gente_a_cargo >= 5 & gente_a_cargo <= 10 ~ "Equipo mediano",
      gente_a_cargo > 10 ~ "Equipo grande",
      TRUE ~ NA_character_
    ),
    
    
      gente_a_cargo_grupo = factor(
        gente_a_cargo_grupo,
        levels = c(
          "Sin equipo",
          "Equipo pequeño",
          "Equipo mediano",
          "Equipo grande"
        ),
        ordered = TRUE
      ),
    
    
    
    #  VERIFICACION PREVIA: valores originales detectados antes del mutate
    # - "Full-Time" (32.142 obs) → "Staff" en encuestas antiguas, mismo tipo, distinto nombre
    # - "Remoto (empresa de otro pais)" (3.282 obs)  → Contractor
    # - "Part-Time" (1.989 obs),  no lo calificamos como tipo de contrato → Otro
    # - "Participación societaria en una cooperativa" (285 obs)  → Otro
    contrato = case_when(
      tipo_contrato == "Staff (planta permanente)"                         ~ "Staff",
      tipo_contrato == "Full-Time"                                         ~ "Staff",
      tipo_contrato == "Contractor"                                        ~ "Contractor",
      tipo_contrato == "Remoto (empresa de otro país)"                     ~ "Contractor",
      tipo_contrato == "Tercerizado (trabajo a través de consultora o agencia)" ~ "Tercerizado",
      tipo_contrato == "Freelance"                                         ~ "Freelance",
      tipo_contrato == "Part-Time"                                         ~ "Otro",
      tipo_contrato == "Participación societaria en una cooperativa"       ~ "Otro",
      TRUE ~ "Otro"
    ),
    
    contrato = factor(
      contrato,
      levels = c("Staff", "Contractor", "Tercerizado", "Freelance", "Otro")
    ),,
    
    
    # Estandariza modalidad_trabajo en tres categorias limpias
    # TRUE ~ NA_character_ asume los valores que no matchean, corresponden a periodos
    # donde la variable no fue relevada, a diferencia de contrato aca la mayoria de los 
    # NA corresponden a que la pregunta no se hizo en todas las encuestas
    modalidad = case_when(
      modalidad_trabajo == "100% remoto" ~ "Remoto",
      modalidad_trabajo == "Híbrido (presencial y remoto)" ~ "Hibrido",
      modalidad_trabajo == "100% presencial" ~ "Presencial",
      TRUE ~ NA_character_
    ),
    
    modalidad = factor(
      modalidad,
      levels = c("Remoto", "Hibrido", "Presencial")
    ),
    
    
    # Estandariza los rangos de cantidad de personas en 11 categorías ordenadas
    # ordered = TRUE — El tamaño de empresa tiene jerarquia natural de menor a mayor
    # Los levels fuerzan el orden correcto — sin esto R ordenaria alfabéticamente
    # y "+10000" quedaría primero por el símbolo "+"
    # TRUE ~ NA_character_ — valores no reconocidos van a NA
    tam_empresa = case_when(
      cantidad_personas_organizacion == "1 (solamente yo)" ~ "1",
      cantidad_personas_organizacion == "De 2 a 10 personas" ~ "2-10",
      cantidad_personas_organizacion == "De 11  a 50  personas" ~ "11-50",
      cantidad_personas_organizacion == "De 51 a 100 personas" ~ "51-100",
      cantidad_personas_organizacion == "De 101 a 200 personas" ~ "101-200",
      cantidad_personas_organizacion == "De 201 a 500 personas" ~ "201-500",
      cantidad_personas_organizacion == "De 501 a 1000 personas" ~ "501-1000",
      cantidad_personas_organizacion == "De 1001 a 2000 personas" ~ "1001-2000",
      cantidad_personas_organizacion == "De 2001a 5000 personas" ~ "2001-5000",
      cantidad_personas_organizacion == "De 5001 a 10000 personas" ~ "5001-10000",
      cantidad_personas_organizacion == "Más de 10000 personas" ~ "+10000",
      TRUE ~ NA_character_
    ),
    
    tam_empresa = factor(
      tam_empresa,
      levels = c(
        "1", "2-10", "11-50", "51-100", "101-200",
        "201-500", "501-1000", "1001-2000", "2001-5000",
        "5001-10000", "+10000"
      ),
      ordered = TRUE
    ),
    
    
    
    # Texto libre con muchas variantes, se mapean en tres categorias
    # Se cubren variantes con y sin tilde, mayusculas y minusculas, con sufijo Cis
    # TRUE ~ "Otro/No binarie": identidades que no son Hombre ni Mujer
    # No se usa NA porque no son datos faltantes sino identidades distintas
    # No tiene ordered = TRUE — las categorias no tienen jerarquia
    genero_simple = case_when(
      genero %in% c("Hombre Cis", "Varón Cis", "Hombre", "Varón", "Varon",
                    "Masculino", "masculino", "hombre", "varón") ~ "Hombre",
      genero %in% c("Mujer Cis", "Mujer", "mujer") ~ "Mujer",
      TRUE ~ "Otro/No binarie"
    ),
    
    genero_simple = factor(
      genero_simple,
      levels = c("Hombre", "Mujer", "Otro/No binarie")
    ),
    
    
    # VERIFICACIÓN PREVIA: valores originales detectados antes del mutate
    # - "Posgrado" (1.657 obs) = "Posgrado/Especialización", mismo nivel, distinto nombre entre encuestas
    # - "Primario" (16 obs) — probable error de carga
    # - NA masivos desde 2021 — variable paso a ser opcional en el cuestionario
    # - "Maestría" solo aparece desde 2021 — en 2019-2020 estaba dentro de "Posgrado"
    nivel_estudios = case_when(
      nivel_estudios == "Posgrado" ~ "Posgrado/Especialización",
      nivel_estudios == "Primario" ~ NA_character_,  # muy pocas obs, descartamos
      TRUE ~ nivel_estudios  # el resto ya está bien escrito
    ),
    
    nivel_estudios = factor(
      nivel_estudios,
      levels = c(
        "Secundario", "Terciario", "Universitario",
        "Posgrado/Especialización", "Maestría", "Doctorado", "Posdoctorado"
      ),
      ordered = TRUE # jerarquia natural de menor a mayor formacion academica
    ),
    
    
    # Clasificamos variable provincia en tres regiones geograficas
    # TRUE ~ "Interior" captura todas las provincias que no son CABA ni Buenos Aires
    # No es un valor desconocido, es una categoria valida que agrupa el resto del pais
    # No tiene ordered = TRUE — las regiones no tienen jerarquia natural
    # Se chequeo mediante un count que no existan ubicaciones fuera del pais y que
    # caigan en interior
    region = case_when(
      provincia == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
      provincia %in% c("Buenos Aires", 
                       "Provincia de Buenos Aires",
                       "GBA") ~ "GBA / Prov. BA",
      is.na(provincia) ~ NA_character_,
      TRUE ~ "Interior"
    ),
    
    region = factor(
      region,
      levels = c("CABA", "GBA / Prov. BA", "Interior")
    ),
    
    # Conversion a numerico
    # Variable disponible solo desde 2022.2 — el resto son NA estructurales
    uso_ia = parse_number(as.character(uso_ia)),
    
    
    # Variable de texto libre, se normaliza a TRUE/FALSE
    # str_to_lower() unifica mayusculas y minusculas antes de detectar patrones
    # Patron afirmativo: "si|sí|yes|true|dolarizado|dólares|dolares"
    # captura tanto respuestas directas como descripciones textuales de cobro en dolares
    # "Cobro parte de mi sueldo en otro país" → NA — ambiguo, no implica necesariamente dolares
    # Disponible solo desde 2024.1 — el resto son NA estructurales
    sueldo_dolarizado = case_when(
      str_detect(str_to_lower(as.character(sueldo_dolarizado)), 
                 "si|sí|yes|true|dolarizado|dólares|dolares") ~ TRUE,
      str_detect(str_to_lower(as.character(sueldo_dolarizado)), 
                 "no|false") ~ FALSE,
      TRUE ~ NA
    ),
    
    # sal_bruto,copia de salario_bruto con nombre corto para uso analitico
    sal_bruto = salario_bruto,
    # log_sal, logaritmo natural de sal_bruto
    # La transformacion log normaliza la distribucion asimetrica del salario
    log_sal = log(sal_bruto)
  )

df_clean %>% count(contrato, sort = TRUE)

# -----------------------------------------------------------------------------
# VERIFICACIONES FINALES PASO 5
# -----------------------------------------------------------------------------

# Estructura general — tipos de cada variable
glimpse(df_clean)

# Variables categóricas — distribución y NA
df_clean %>% count(grupo_rol)
df_clean %>% count(seniority)
df_clean %>% count(gente_a_cargo_grupo)
df_clean %>% count(contrato)
df_clean %>% count(modalidad)
df_clean %>% count(tam_empresa)
df_clean %>% count(genero_simple)
df_clean %>% count(nivel_estudios)
df_clean %>% count(region)
df_clean %>% count(sueldo_dolarizado)

# Variables numéricas — resumen estadístico
summary(df_clean$sal_bruto)
summary(df_clean$log_sal)
summary(df_clean$uso_ia)

# -----------------------------------------------------------------------------
# OBSERVACIONES POST-VERIFICACIÓN PASO 5
# -----------------------------------------------------------------------------

# sal_bruto: minimo 0 y máximo 123.123.123
# - Los 0 corresponden a encuestados que no revelaron su sueldo o errores de carga
# - El maximo es claramente un error — nadie gana $123 millones mensuales
# - Se resuelven en paso 6 (filtro sal_bruto > 0) y paso 7 (filtro outliers p1-p99)

# log_sal: minimo -Inf y media -Inf
# - Consecuencia directa de los sal_bruto = 0 — log(0) = -Infinito matematicamente
# - La media queda contaminada con un solo -Inf
# - Se resuelve en paso 6 con el filtro

# tam_empresa: 7.210 NA (46% del dataset)
# - NA estructurales — la pregunta no existia en el cuestionario antes de 2023
# - 2019, 2020, 2021: 100% NA
# - 2022: 51% NA — solo el segundo semestre incorporo la pregunta
# - 2023 en adelante: 0% NA
# - Por esta razon tam_empresa no sera incluida en el modelo de regresion




# -----------------------------------------------------------------------------
# 6. FILTROS BASICOS DE CALIDAD
# -----------------------------------------------------------------------------

# Eliminamos observaciones con salario invalido
# !is.na(sal_bruto) -> 12 NA generados por parse_number en paso 2
# sal_bruto > 0 —> registros con salario = 0, no representan salarios reales
# !is.na(log_sal) —> elimina los -Inf generados por log(0)
#
# Las tres condiciones estan relacionadas pero se filtran explicitamente
# para documentar cada tipo de problema
df_clean <- df_clean %>%
  filter(
    !is.na(sal_bruto),
    sal_bruto > 0,
    !is.na(log_sal)
  )

# Verificación post-filtro de cantidad de observaciones finales
cat("Observaciones después del filtro de calidad:", nrow(df_clean), "\n")
# Resultado: 15.521 — se eliminaron 19 registros
# 12 NA de sal_bruto + registros con sal_bruto = 0

# -----------------------------------------------------------------------------
# 7. JOIN CON BLUE 
# -----------------------------------------------------------------------------

dolar_blue <- read.csv(
  "https://api.bluelytics.com.ar/v2/evolution.csv"
)

dolar_blue_s<-dolar_blue %>%
  filter(type == "Blue", day >= as.Date("2019-01-01")) %>%
  mutate(
    day        = as.Date(day),
    valor_blue = (value_sell + value_buy) / 2,
    anio       = year(day),
    semestre   = if_else(month(day) <= 6, 1, 2),
    periodo = as.character(paste(anio, semestre, sep = "."))
    ) %>%
  group_by(periodo) %>%
  summarise(blue_promedio = mean(valor_blue))

df_clean <- df_clean %>%
  mutate(periodo = as.character(periodo))

df_clean <- df_clean %>%
  left_join(dolar_blue_s, by = "periodo") %>%
  mutate(
    sal_usd_blue = sal_bruto / blue_promedio,
    log_sal_usd  = log(sal_usd_blue),
    valor_blue = blue_promedio
  )




# -----------------------------------------------------------------------------
# 8. FILTRO DE OUTLIERS EN SALARIO Y EDAD
# -----------------------------------------------------------------------------

# Filtramos sobre sal_usd_blue (dólares constantes) en lugar de sal_bruto nominal
# para que el criterio sea comparable entre períodos

q1  <- quantile(df_clean$sal_usd_blue, 0.001, na.rm = TRUE)
q99 <- quantile(df_clean$sal_usd_blue, 0.999, na.rm = TRUE)

cat("Percentil 0.1 (USD):", q1, "\n")
cat("Percentil 99.9 (USD):", q99, "\n")

df_clean <- df_clean %>%
  filter(
    sal_usd_blue >= q1,
    sal_usd_blue <= q99,
    is.na(edad) | (edad >= 16 & edad <= 70)
  )

cat("Observaciones después del filtro de outliers:", nrow(df_clean), "\n")


# -----------------------------------------------------------------------------
# 9. SELECCION FINAL DE COLUMNAS
# -----------------------------------------------------------------------------


# Seleccionamos las 23 variables analíticas finales
# Se eliminan las columnas originales reemplazadas por versiones estandarizadas
df_clean <- df_clean %>%
  select(
    anio,
    semestre,
    periodo,
    
    trabajo_de,
    grupo_rol,
    dedicacion,
    
    sal_bruto,
    log_sal,
    sueldo_dolarizado,
    log_sal_usd,
    sal_usd_blue,
    
    
    seniority,
    experiencia,
    antiguedad_empresa,
    antiguedad_puesto,
    gente_a_cargo,
    gente_a_cargo_grupo,
    
    contrato,
    modalidad,
    tam_empresa,
    
    genero_simple,
    edad,
    nivel_estudios,
    region,
    uso_ia
  )


# Verificación final de estructura
glimpse(df_clean) # 15.474 registros , 23 columnas



# -----------------------------------------------------------------------------
# 10. VERIFICACIONES FINALES
# -----------------------------------------------------------------------------

# Estructura general
glimpse(df_clean)

# Distribucion por período
df_clean %>% count(periodo)

# Variables categoricas clave
df_clean %>% count(grupo_rol)
df_clean %>% count(seniority)
df_clean %>% count(genero_simple)
df_clean %>% count(contrato)
df_clean %>% count(region)
df_clean %>% count(sueldo_dolarizado)

# Resumen del salario — confirmamos que no hay valores problematicos
df_clean %>%
  summarise(
    filas                  = n(),
    salario_min            = min(sal_bruto, na.rm = TRUE),
    salario_max            = max(sal_bruto, na.rm = TRUE),
    salario_mediana        = median(sal_bruto, na.rm = TRUE),
    salario_promedio       = mean(sal_bruto, na.rm = TRUE),
    faltantes_salario      = sum(is.na(sal_bruto)),
    log_sal_min            = min(log_sal, na.rm = TRUE),
    log_sal_infinitos      = sum(is.infinite(log_sal)),
    usd_min                = min(sal_usd_blue, na.rm = TRUE),
    usd_infinitos          = sum(is.infinite(log_sal_usd))
  )


# Verificando que no queden log_usd negativos -> log de cualquier nro <1 da negativo
df_clean %>%
  filter(log_sal_usd < 0) %>%
  summarise(n = n(), min_usd = min(sal_usd_blue), max_usd = max(sal_usd_blue))

# Filtrando esos log usd negativos
df_clean <- df_clean %>%
  filter(
    sal_usd_blue >= q1,
    sal_usd_blue <= q99,
    sal_usd_blue >= 1,          # elimina salarios menores a 1 USD
    is.na(edad) | (edad >= 16 & edad <= 70)
  )



# -----------------------------------------------------------------------------
# 11. GUARDADO DEL DATASET FINAL
# -----------------------------------------------------------------------------

write_csv(
  df_clean,
  "data/processed/df_sysarmy.csv"
)
