library(tidyverse)

# -----------------------------------------------------------------------------
# 1. CARGA DEL DATASET CONSOLIDADO
# -----------------------------------------------------------------------------

df_sysarmy <- read_csv(
  "data/intermediate/sysarmy_consolidado.csv",
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# 2. LIMPIEZA DE TIPOS
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# 3. DEFINICIÓN DE ROLES A INCLUIR
# -----------------------------------------------------------------------------

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
  "Infrastruture Engineer"
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

# -----------------------------------------------------------------------------
# 4. FILTRADO POR ROL
# -----------------------------------------------------------------------------

df_clean <- df_sysarmy %>%
  filter(trabajo_de %in% todos_los_roles)

# -----------------------------------------------------------------------------
# 5. CREACIÓN DE VARIABLES PARA EL ANÁLISIS
# -----------------------------------------------------------------------------

df_clean <- df_clean %>%
  mutate(
    grupo_rol = case_when(
      trabajo_de %in% roles_core ~ "Data Science / AI",
      trabajo_de %in% roles_data_platform ~ "Data Platform / MLOps",
      trabajo_de %in% roles_analytics ~ "Analytics / Automation",
      trabajo_de %in% roles_governance ~ "Governance / Security",
      TRUE ~ "Otro"
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
    
    seniority = factor(
      seniority,
      levels = c("Junior", "Semi-Senior", "Senior"),
      ordered = TRUE
    ),
    
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
    
    contrato = case_when(
      tipo_contrato == "Staff (planta permanente)" ~ "Staff",
      tipo_contrato == "Contractor" ~ "Contractor",
      tipo_contrato == "Tercerizado (trabajo a través de consultora o agencia)" ~ "Tercerizado",
      tipo_contrato == "Freelance" ~ "Freelance",
      TRUE ~ "Otro"
    ),
    
    contrato = factor(
      contrato,
      levels = c("Staff", "Contractor", "Tercerizado", "Freelance", "Otro")
    ),
    
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
    
    nivel_estudios = factor(
      nivel_estudios,
      levels = c(
        "Secundario", "Terciario", "Universitario",
        "Posgrado/Especialización", "Maestría", "Doctorado", "Posdoctorado"
      ),
      ordered = TRUE
    ),
    
    region = case_when(
      provincia == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
      provincia == "Buenos Aires" ~ "GBA / Prov. BA",
      TRUE ~ "Interior"
    ),
    
    region = factor(
      region,
      levels = c("CABA", "GBA / Prov. BA", "Interior")
    ),
    
    uso_ia = parse_number(as.character(uso_ia)),
    
    sueldo_dolarizado = case_when(
      str_detect(str_to_lower(as.character(sueldo_dolarizado)), "si|sí|yes|true") ~ TRUE,
      str_detect(str_to_lower(as.character(sueldo_dolarizado)), "no|false") ~ FALSE,
      TRUE ~ NA
    ),
    
    sal_bruto = salario_bruto,
    log_sal = log(sal_bruto)
  )

# -----------------------------------------------------------------------------
# 6. FILTROS BÁSICOS DE CALIDAD
# -----------------------------------------------------------------------------

df_clean <- df_clean %>%
  filter(
    !is.na(sal_bruto),
    sal_bruto > 0,
    !is.na(log_sal)
  )

# -----------------------------------------------------------------------------
# 7. FILTRO DE OUTLIERS EN SALARIO Y EDAD
# -----------------------------------------------------------------------------

q1 <- quantile(df_clean$sal_bruto, 0.01, na.rm = TRUE)
q99 <- quantile(df_clean$sal_bruto, 0.99, na.rm = TRUE)

df_clean <- df_clean %>%
  filter(
    sal_bruto >= q1,
    sal_bruto <= q99,
    is.na(edad) | (edad >= 16 & edad <= 70)  # elimina datos corruptos como 20000
  )


# -----------------------------------------------------------------------------
# 8. SELECCIÓN FINAL DE COLUMNAS
# -----------------------------------------------------------------------------

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



# -----------------------------------------------------------------------------
# 9. VERIFICACIONES
# -----------------------------------------------------------------------------

glimpse(df_clean)

df_clean %>%
  count(periodo)

df_clean %>%
  count(grupo_rol)

df_clean %>%
  count(seniority)

df_clean %>%
  count(genero_simple)

df_clean %>%
  summarise(
    filas = n(),
    salario_min = min(sal_bruto, na.rm = TRUE),
    salario_max = max(sal_bruto, na.rm = TRUE),
    salario_promedio = mean(sal_bruto, na.rm = TRUE),
    faltantes_salario_bruto = sum(is.na(sal_bruto))
  )

# -----------------------------------------------------------------------------
# 10. GUARDADO DEL DATASET FINAL
# -----------------------------------------------------------------------------

write_csv(
  df_clean,
  "data/processed/df_sysarmy.csv"
)
