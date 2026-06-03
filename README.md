# TP Final - Análisis de Salarios IT en Argentina (Sysarmy)

## Descripción

Este proyecto analiza la evolución y los determinantes de los salarios del sector IT en Argentina utilizando las encuestas públicas de remuneraciones de Sysarmy.

Se consolidaron múltiples ediciones semestrales de la encuesta (2019–2026), construyendo un dataset unificado para realizar tareas de limpieza, análisis exploratorio de datos (EDA) y modelado estadístico.

---

## Objetivos

- Consolidar múltiples encuestas históricas de Sysarmy.
- Estandarizar variables con diferentes nombres entre períodos.
- Construir un dataset analítico consistente.
- Realizar análisis exploratorio de datos (EDA).
- Identificar factores asociados al salario.
- Construir modelos estadísticos para explicar las diferencias salariales observadas.

---

## Pregunta de investigación

¿Qué variables — seniority, dolarización del sueldo, experiencia, género y región — tienen mayor capacidad explicativa sobre los salarios de los profesionales IT en Argentina durante el período 2024-2026?

---

## Hipótesis

- **H1.** A mayor experiencia laboral, mayor salario.
- **H2.** El seniority tiene un efecto positivo sobre el salario, siendo el predictor individual de mayor impacto.
- **H3.** Los profesionales con salario dolarizado perciben mayores ingresos, y ese efecto se amplifica con los años de experiencia.
- **H4.** Existen diferencias salariales entre los distintos grupos de roles, con Roles de gestión en el extremo superior y Desarrollo/QA en el inferior.
- **H5.** Las personas con personal a cargo perciben salarios superiores.
- **H6.** La modalidad de trabajo tiene efecto sobre el salario: los trabajadores presenciales perciben ingresos menores que los remotos e híbridos.

---

## Variable objetivo

La variable objetivo utilizada para el modelado es:

```
log_sal_usd = log(salario_bruto / dolar_blue)
```

Se utiliza la transformación logarítmica para reducir la asimetría presente en la distribución salarial y cumplir mejor los supuestos de los modelos de regresión lineal. La deflactación por dólar blue permite comparar salarios entre períodos con alta inflación.

---

## Metodología

El proyecto se desarrolla en cuatro etapas:

1. Consolidación de encuestas históricas.
2. Limpieza y transformación de variables.
3. Análisis exploratorio de datos (EDA).
4. Modelado estadístico mediante regresión lineal múltiple (tres modelos comparados con ANOVA).

---

## Modelos

Se ajustaron tres modelos de regresión lineal, todos sobre el subconjunto 2024–2026 (n = 17.079), único período donde seniority y sueldo_dolarizado están disponibles.

| Modelo | Fórmula | R² | RSE |
|--------|---------|-----|-----|
| modp1 | log_sal_usd ~ experiencia + grupo_rol + seniority + genero_simple + region | 0.288 | 0.552 |
| modp2 | modp1 + experiencia * sueldo_dolarizado | 0.337 | 0.533 |
| modp3 | modp1 + poly(experiencia, 2) * sueldo_dolarizado | 0.345 | 0.530 |

La comparación con ANOVA confirma que cada modelo agrega poder explicativo significativo (p < 2.2e-16 en ambas comparaciones).

---

## Resultados principales

- **Seniority** es el predictor con mayor impacto individual: ser Senior implica ~64% más de salario que Junior (modp3), controlando por el resto de las variables.
- **Dolarización** interactúa con la experiencia: la brecha entre dolarizados y no dolarizados crece con los años de carrera — la experiencia rinde ~4.4 veces más para quienes cobran en dólares.
- **Grupo de rol**: Roles de gestión lidera (~12% más que Ciberseguridad); Desarrollo/QA es el grupo peor pago (~15% menos).
- **Género**: ser mujer se asocia con un salario ~10% menor, controlando por seniority, rol, región y experiencia.
- **Región**: Interior queda ~13% por debajo de CABA; GBA muestra diferencias menores (~8%).
- **Experiencia**: la relación es positiva pero con rendimientos decrecientes — el crecimiento salarial se desacelera a partir de los 20-30 años (capturado en modp3).

---

## Calidad de los datos

Las variables principales utilizadas para el modelado (salario, experiencia, rol, género y región) no presentan valores faltantes.

Las variables seniority, sueldo_dolarizado y uso_ia presentan ~72% de valores faltantes, pero estos son **estructurales**: corresponden a períodos anteriores a 2024, cuando esas preguntas no existían en el cuestionario de Sysarmy. No son errores de carga ni falta de respuesta.

Por este motivo, los modelos se restringen al período 2024–2026, donde todas las variables están disponibles.

---

## Estructura del proyecto

```
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
├── docs/
│   ├── Documentacion_eda.docx
│   └── Documentacion_limpieza.docx
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

```
Encuestas Sysarmy (raw)
            ↓
01_consolidacion.R
            ↓
sysarmy_consolidado.csv  (75.649 registros)
            ↓
02_limpieza.R
            ↓
df_sysarmy.csv  (60.356 registros)
            ↓
03_eda.R
            ↓
04_modelado.R  (subconjunto 2024–2026: n = 17.079)
```

---

## Fuente de datos

Las encuestas utilizadas fueron obtenidas de las publicaciones públicas de Sysarmy:

- https://sueldos.openqube.io/encuesta-sueldos-2026.01/
- https://sysarmy.com/

---

## Variables analizadas

Entre las principales variables utilizadas se encuentran:

- Salario bruto (en pesos y deflactado por dólar blue → `log_sal_usd`)
- Seniority (Junior / Semi-Senior / Senior)
- Experiencia (años)
- Sueldo dolarizado (sí/no)
- Grupo de rol
- Género
- Región
- Personas a cargo
- Modalidad de trabajo (remoto, híbrido, presencial)
- Uso de IA
- Nivel de estudios

---

## Tecnologías utilizadas

- R
- tidyverse
- ggplot2
- modelr
- janitor
- Git / GitHub

---

## Integrantes

- Valle Candela
- Aguirre Santiago
- Nieto Mariano│
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
│── docs/
│   ├── Documentacion_eda.docx
│   └── Documentacion_limpieza.docx
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
# TP Final - Análisis de Salarios IT en Argentina (Sysarmy)

## Descripción

Este proyecto analiza la evolución y los determinantes de los salarios del sector IT en Argentina utilizando las encuestas públicas de remuneraciones de Sysarmy.

Se consolidaron múltiples ediciones semestrales de la encuesta (2019–2026), construyendo un dataset unificado para realizar tareas de limpieza, análisis exploratorio de datos (EDA) y modelado estadístico.

---

## Objetivos

- Consolidar múltiples encuestas históricas de Sysarmy.
- Estandarizar variables con diferentes nombres entre períodos.
- Construir un dataset analítico consistente.
- Realizar análisis exploratorio de datos (EDA).
- Identificar factores asociados al salario.
- Construir modelos estadísticos para explicar las diferencias salariales observadas.

---

## Pregunta de investigación

¿Qué variables — seniority, dolarización del sueldo, experiencia, género y región — tienen mayor capacidad explicativa sobre los salarios de los profesionales IT en Argentina durante el período 2024-2026?

---

## Hipótesis

- **H1.** A mayor experiencia laboral, mayor salario.
- **H2.** El seniority tiene un efecto positivo sobre el salario, siendo el predictor individual de mayor impacto.
- **H3.** Los profesionales con salario dolarizado perciben mayores ingresos, y ese efecto se amplifica con los años de experiencia.
- **H4.** Existen diferencias salariales entre los distintos grupos de roles, con Roles de gestión en el extremo superior y Desarrollo/QA en el inferior.
- **H5.** Las personas con personal a cargo perciben salarios superiores.
- **H6.** La modalidad de trabajo tiene efecto sobre el salario: los trabajadores presenciales perciben ingresos menores que los remotos e híbridos.

---

## Variable objetivo

La variable objetivo utilizada para el modelado es:

```
log_sal_usd = log(salario_bruto / dolar_blue)
```

Se utiliza la transformación logarítmica para reducir la asimetría presente en la distribución salarial y cumplir mejor los supuestos de los modelos de regresión lineal. La deflactación por dólar blue permite comparar salarios entre períodos con alta inflación.

---

## Metodología

El proyecto se desarrolla en cuatro etapas:

1. Consolidación de encuestas históricas.
2. Limpieza y transformación de variables.
3. Análisis exploratorio de datos (EDA).
4. Modelado estadístico mediante regresión lineal múltiple (tres modelos comparados con ANOVA).

---

## Modelos

Se ajustaron tres modelos de regresión lineal, todos sobre el subconjunto 2024–2026 (n = 17.079), único período donde seniority y sueldo_dolarizado están disponibles.

| Modelo | Fórmula | R² | RSE |
|--------|---------|-----|-----|
| modp1 | log_sal_usd ~ experiencia + grupo_rol + seniority + genero_simple + region | 0.288 | 0.552 |
| modp2 | modp1 + experiencia * sueldo_dolarizado | 0.337 | 0.533 |
| modp3 | modp1 + poly(experiencia, 2) * sueldo_dolarizado | 0.345 | 0.530 |

La comparación con ANOVA confirma que cada modelo agrega poder explicativo significativo (p < 2.2e-16 en ambas comparaciones).

---

## Resultados principales

- **Seniority** es el predictor con mayor impacto individual: ser Senior implica ~64% más de salario que Junior (modp3), controlando por el resto de las variables.
- **Dolarización** interactúa con la experiencia: la brecha entre dolarizados y no dolarizados crece con los años de carrera — la experiencia rinde ~4.4 veces más para quienes cobran en dólares.
- **Grupo de rol**: Roles de gestión lidera (~12% más que Ciberseguridad); Desarrollo/QA es el grupo peor pago (~15% menos).
- **Género**: ser mujer se asocia con un salario ~10% menor, controlando por seniority, rol, región y experiencia.
- **Región**: Interior queda ~13% por debajo de CABA; GBA muestra diferencias menores (~8%).
- **Experiencia**: la relación es positiva pero con rendimientos decrecientes — el crecimiento salarial se desacelera a partir de los 20-30 años (capturado en modp3).

---

## Calidad de los datos

Las variables principales utilizadas para el modelado (salario, experiencia, rol, género y región) no presentan valores faltantes.

Las variables seniority, sueldo_dolarizado y uso_ia presentan ~72% de valores faltantes, pero estos son **estructurales**: corresponden a períodos anteriores a 2024, cuando esas preguntas no existían en el cuestionario de Sysarmy. No son errores de carga ni falta de respuesta.

Por este motivo, los modelos se restringen al período 2024–2026, donde todas las variables están disponibles.

---

## Estructura del proyecto

```
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
├── docs/
│   ├── Documentacion_eda.docx
│   └── Documentacion_limpieza.docx
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

```
Encuestas Sysarmy (raw)
            ↓
01_consolidacion.R
            ↓
sysarmy_consolidado.csv  (75.649 registros)
            ↓
02_limpieza.R
            ↓
df_sysarmy.csv  (60.356 registros)
            ↓
03_eda.R
            ↓
04_modelado.R  (subconjunto 2024–2026: n = 17.079)
```

---

## Fuente de datos

Las encuestas utilizadas fueron obtenidas de las publicaciones públicas de Sysarmy:

- https://sueldos.openqube.io/encuesta-sueldos-2026.01/
- https://sysarmy.com/

---

## Variables analizadas

Entre las principales variables utilizadas se encuentran:

- Salario bruto (en pesos y deflactado por dólar blue → `log_sal_usd`)
- Seniority (Junior / Semi-Senior / Senior)
- Experiencia (años)
- Sueldo dolarizado (sí/no)
- Grupo de rol
- Género
- Región
- Personas a cargo
- Modalidad de trabajo (remoto, híbrido, presencial)
- Uso de IA
- Nivel de estudios

---

## Tecnologías utilizadas

- R
- tidyverse
- ggplot2
- modelr
- janitor
- Git / GitHub

---

## Integrantes

- Valle Candela
- Aguirre Santiago
- Nieto Mariano
## Tecnologías utilizadas

* R
* tidyverse
* janitor
* ggplot
* Git
* GitHub

---

## Integrantes


* Valle Candela
* Aguirre Santiago
* Nieto Mariano
