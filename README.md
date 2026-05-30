# TP Final - Análisis de Salarios IT en Argentina (Sysarmy)

## Descripción

Este proyecto analiza la evolución y los determinantes de los salarios del sector IT en Argentina utilizando las encuestas públicas de remuneraciones de Sysarmy.

Se consolidaron múltiples ediciones semestrales de la encuesta (2019–2026), construyendo un dataset unificado para realizar tareas de limpieza, análisis exploratorio de datos (EDA) y modelado estadístico.

## Objetivos

* Consolidar múltiples encuestas históricas de Sysarmy.
* Estandarizar variables con diferentes nombres entre períodos.
* Construir un dataset analítico consistente.
* Realizar análisis exploratorio de datos (EDA).
* Identificar factores asociados al salario.
* Construir modelos estadísticos para explicar las diferencias salariales observadas.


## Pregunta de investigación

¿Qué factores explican mejor las diferencias salariales entre los profesionales del área de datos en Argentina?


## Hipótesis

H1. A mayor experiencia laboral, mayor salario.

H2. El seniority tiene un efecto positivo sobre el salario.

H3. Los profesionales con salario dolarizado perciben mayores ingresos.

H4. Existen diferencias salariales entre los distintos grupos de roles.

H5. Las personas con personal a cargo perciben salarios superiores.


## Variable objetivo

La variable objetivo utilizada para el modelado es:

log_sal = log(salario_bruto)

Se utiliza la transformación logarítmica para reducir la asimetría presente en la distribución salarial y cumplir mejor los supuestos de los modelos de regresión lineal.


## Metodología

El proyecto se desarrolla en cuatro etapas:

1. Consolidación de encuestas históricas.
2. Limpieza y transformación de variables.
3. Análisis exploratorio de datos (EDA).
4. Modelado estadístico mediante regresión lineal.


## Resultados esperados

Identificar qué variables tienen mayor capacidad explicativa sobre los salarios del sector IT argentino y cuantificar su impacto relativo.

---

## Estructura del proyecto

```text
tp-sysarmy-salarios/
│
├── data/
│   ├── raw/
│   ├── intermediate/
│   └── processed/
│
├── scripts/
│   ├── 01_consolidacion.R
│   ├── 02_limpieza.R
│   ├── 03_eda.R
│   └── 04_modelado.R
│
├── outputs/
│   ├── figures/
│   └── tables/
│
├── README.md
└── tp-sysarmy-salarios.Rproj
```

---

## Pipeline de datos

```text
Encuestas Sysarmy (raw)
            ↓
01_consolidacion.R
            ↓
sysarmy_consolidado.csv
            ↓
02_limpieza.R
            ↓
df_sysarmy.csv
            ↓
03_eda.R
            ↓
04_modelado.R
```

---

## Fuente de datos

Las encuestas utilizadas fueron obtenidas de las publicaciones públicas de Sysarmy:

https://sueldos.openqube.io/encuesta-sueldos-2026.01/
https://sysarmy.com/

---

## Variables analizadas

Entre las principales variables utilizadas se encuentran:

* Salario bruto
* Salario neto
* Seniority
* Experiencia
* Edad
* Modalidad de trabajo
* Tipo de contrato
* Sueldo dolarizado
* Grupo de rol
* Personas a cargo
* Región
* Nivel de estudios

---

## Tecnologías utilizadas

* R
* tidyverse
* ggplot
* Git
* GitHub

---

## Integrantes


* Valle Candela
* Aguirre Santiago
* Mariano Nieto
