library(tidyverse)
library(modelr)

df <- read_csv(
  "data/processed/df_sysarmy.csv")


# Ajustando datasets y NA (toma desde el año 2024)
df_modelo <- df %>%
  drop_na(log_sal_usd, experiencia, grupo_rol, seniority, 
          sueldo_dolarizado, genero_simple, region)

dim(df_modelo)


# -----------------------------------------------------------------------------
#### 10. MODELADO ####
# -----------------------------------------------------------------------------

# DECISION DE MODELADO:
# - Variable objetivo: log_sal_usd — salario deflactado por dolar blue

#Preguntas a responder con los modelos:

# - Que rol maximiza el salario en dolares en el sector IT argentino,
#   controlando por experiencia, region y genero?
# - El valor de un anioo de experiencia cambia segun si se cobra en dolares o no?
# - La relacion entre experiencia y salario es lineal o presenta rendimientos decrecientes?



# ---
#### Modelo 1: log_sal_usd ~ experiencia + grupo_rol + seniority + genero_simple + region ####
modp1 <- lm(log_sal_usd ~ experiencia + grupo_rol + seniority + genero_simple + region,
            data = df_modelo)

summary(modp1)

#### Observaciones modp1: ####
# R2 = 0.2881: el modelo explica el 28.81% de la variabilidad de los salarios.
# El ~71% restante se debe a dispersion natural y factores no incluidos (como dolarizacion).

# RSE = 0.552: las predicciones se desvian en promedio 0.552 unidades logaritmicas.

# Intercepto = 7.1084: salario base estimado para un hombre Junior de Ciberseguridad en CABA
# con 0 anios de experiencia.

# Experiencia: coeficiente 0.0052 — por cada anio adicional, el salario sube ~0.52%.

# Seniority (mayores saltos del modelo):
# - Semi-Senior: coef 0.4770 → sube ~61% respecto a Junior  (e^0.477 - 1)
# - Senior:      coef 0.7716 → sube ~116% respecto a Junior (e^0.772 - 1)

# Grupo de rol (referencia = Ciberseguridad):
# - Datos / AI:      coef -0.050 → ~4.9% menos  (no significativo al 5%)
# - Infraestructura: coef -0.137 → ~12.8% menos (***)
# - Desarrollo / QA: coef -0.135 → ~12.6% menos (***)
# - Roles de gestion: coef +0.138 → ~14.8% mas  (***)

# Genero:
# - Mujer: coef -0.111 → ~10.5% menos que hombres (***)

# Region (referencia = CABA):
# - GBA / Prov. BA: coef -0.062 → ~6.0% menos (***)
# - Interior:       coef -0.123 → ~11.6% menos (***)





#### GRAFICOS MODELO 1 ####

# ---------------------------------------------------------
# GRÁFICO 1: VISUALIZACIÓN DEL MODELO (Predicciones)
# ---------------------------------------------------------

# Grilla fijando genero = Hombre y region = CABA para aislar el efecto de rol y seniority
grilla_modp1 <- df_modelo %>%
  data_grid(
    experiencia = seq_range(experiencia, n = 50),
    grupo_rol,
    seniority,
    genero_simple = "Hombre",
    region = "CABA"
  ) %>%
  add_predictions(modp1, var = "pred") 


# Rectas paralelas por seniority dentro de cada grupo de rol —
# el modelo lineal asume que la pendiente de experiencia es igual para todos los grupos
ggplot(df_modelo, aes(x = experiencia, y = log_sal_usd)) +
  geom_point(alpha = 0.2, color = "black") +
  geom_line(data = grilla_modp1, aes(y = pred, color = seniority), size = 1) +
  facet_wrap(~ grupo_rol) +
  labs(
    title = "Modelo P1: Salario vs Experiencia según Rol y Seniority",
    subtitle = "Género: Hombre | Región: CABA",
    x = "Años de experiencia",
    y = "Logaritmo del salario (USD)",
    color = "Seniority"
  ) +
  theme_bw()



# ---------------------------------------------------------
# GRÁFICO 2: DIAGNÓSTICO DE RESIDUOS (Residuos vs Predicciones)
# ---------------------------------------------------------

# Residuos centrados en 0 — sin patron sistematico claro
# La dispersion aumenta levemente en predicciones intermedias (~7.5)
# El modelo 1 deja estructura sin capturar en la zona de valores medios
df_residuos_p1 <- df_modelo %>%
  add_predictions(modp1, var = "pred") %>%
  add_residuals(modp1, var = "resid")

ggplot(df_residuos_p1, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(
    title = "Residuos vs Predicciones (modp1)",
    x = "Predicciones (log_sal_usd estimado)",
    y = "Residuos"
  ) +
  theme_minimal()



#### Modelo 2: experiencia * sueldo_dolarizado + grupo_rol + seniority + genero_simple + region ####
modp2 <- lm(log_sal_usd ~ experiencia * sueldo_dolarizado + grupo_rol + seniority + genero_simple + region,
            data = df_modelo)

summary(modp2)

#### Observaciones modp2: ####
# R² sube de 0.2881 a 0.3373: agregar la dolarizacion y su interaccion con experiencia
# aporta ~5 puntos porcentuales de explicacion adicional.

# RSE baja de 0.552 a 0.533: las predicciones mejoran al incorporar la dolarizacion.

# Interaccion experiencia:sueldo_dolarizadoTRUE — altamente significativa (***)
# El efecto de la experiencia sobre el salario depende de si se cobra en dolares o no:

# - Sueldos en pesos: pendiente = 0.003748 → cada año extra sube ~0.37%
# - Sueldos dolarizados: pendiente = 0.003748 + 0.012820 = 0.016568 → cada año extra sube ~1.66%
# La experiencia rinde ~4.4 veces mas rapido para quienes cobran en dolares.

# Efecto base de dolarizacion (sueldo_dolarizadoTRUE = 0.1738):
# A 0 años de experiencia, cobrar en dolares implica ~19% mas de salario (e^0.1738 - 1).

# Seniority:
# - Semi-Senior: coef 0.4512 → ~57% mas que Junior  (e^0.451 - 1)
# - Senior:      coef 0.6992 → ~101% mas que Junior (e^0.699 - 1)

# Grupo de rol (referencia = Ciberseguridad):
# - Datos / AI:      coef -0.068 → ~6.6% menos (*)
# - Infraestructura: coef -0.136 → ~12.7% menos (***)
# - Desarrollo / QA: coef -0.171 → ~15.7% menos (***)
# - Roles de gestion: coef +0.112 → ~11.8% mas  (***)

# Genero: Mujer coef -0.106 → ~10.1% menos (***)
# Region: GBA -0.079 → ~7.6% menos | Interior -0.134 → ~12.6% menos (***)





#### GRAFICOS MODELO 2 ####
# ---------------------------------------------------------
# GRAFICO 1: VISUALIZACION DEL MODELO 2 (Predicciones)
# ---------------------------------------------------------

# La separacion entre curvas rojas (pesos) y turquesa (dolares) crece con la experiencia
# ilustra la interaccion: cobrar en dolares premia cada vez mas con los anios
  data_grid(
    experiencia = seq_range(experiencia, n = 50),
    grupo_rol,
    seniority,
    sueldo_dolarizado,
    genero_simple = "Hombre",
    region = "CABA"
  ) %>%
  add_predictions(modp2, var = "pred")



ggplot(df_modelo, aes(x = experiencia, y = log_sal_usd)) +
  geom_point(alpha = 0.2, color = "black") +
  geom_line(data = grilla_modp2, aes(y = pred, color = sueldo_dolarizado, linetype = seniority), size = 1) +
  facet_wrap(~ grupo_rol) +
  labs(
    title = "Modelo P2: Salario vs Experiencia (interacción dolarización)",
    subtitle = "Género: Hombre | Región: CABA",
    x = "Años de experiencia",
    y = "Logaritmo del salario (USD)",
    color = "Dolarizado",
    linetype = "Seniority"
  ) +
  theme_bw()

# ---------------------------------------------------------
# GRAFICO 2: DIAGNOTICO DE RESIDUOS (mod2)
# ---------------------------------------------------------

# Residuos mas simetricos que modp1, la nube se compacta hacia el centro
# Persiste leve dispersion en predicciones bajas (~7.0)
  add_predictions(modp2, var = "pred") %>%
  add_residuals(modp2, var = "resid")



ggplot(df_residuos_p2, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(
    title = "Residuos vs Predicciones (modp2)",
    x = "Predicciones (log_sal_usd estimado)",
    y = "Residuos"
  ) +
  theme_minimal()


#### Modelo 3: poly(experiencia, 2) * sueldo_dolarizado + grupo_rol + seniority + genero_simple + region ####
modp3 <- lm(log_sal_usd ~ poly(experiencia, 2) * sueldo_dolarizado + grupo_rol + seniority + genero_simple + region,
            data = df_modelo)

summary(modp3)

#### Observaciones modp3: ####
# R² sube de 0.3373 a 0.3454: permitir curvatura en la experiencia agrega ~1 punto adicional.
# RSE baja de 0.533 a 0.530.

# Terminos polinomicos de experiencia:
# - poly(experiencia,2)1 positivo y significativo: tendencia general creciente.
# - poly(experiencia,2)2 = -7.076, negativo y significativo: curva con forma de U invertida.
#   Los salarios crecen rapido en los primeros años pero se desaceleran y alcanzan
#   un techo alrededor de los 20-30 años de experiencia.

# Interaccion polinomica con dolarizacion — ambos terminos altamente significativos (***):
# Los dolarizados no solo tienen una pendiente inicial mas alta, sino que la forma
# de la curva es matematicamente distinta: el techo es mas alto y se alcanza mas tarde.

# Ajuste de seniority al capturar mejor la experiencia:
# - Semi-Senior: coef 0.3790 → ~46% mas que Junior  (e^0.379 - 1)
# - Senior:      coef 0.4949 → ~64% mas que Junior  (e^0.495 - 1)
# Al modelar la curvatura real de la experiencia, el modelo ya no necesita "inflar"
# el coeficiente de seniority para compensar — los saltos son mas precisos.

# Grupo de rol (referencia = Ciberseguridad):
# - Datos / AI:      coef -0.059 → ~5.7% menos (*)
# - Infraestructura: coef -0.140 → ~13.1% menos (***)
# - Desarrollo / QA: coef -0.166 → ~15.3% menos (***)
# - Roles de gestion: coef +0.112 → ~11.8% mas  (***)

# Genero: Mujer coef -0.103 → ~9.8% menos (***)
# Region: GBA -0.082 → ~7.9% menos | Interior -0.136 → ~12.7% menos (***)


#### GRAFICOS MODELO 3 ####


# ---------------------------------------------------------
# GRAFICO 1: VISUALIZACION DEL MODELO 3 (Curvas Polinmicas)
# ---------------------------------------------------------

# Las curvas muestran el techo salarial: crecimiento rapido hasta ~15 años,
# luego desaceleracion. Los dolarizados alcanzan un pico mas alto.
# Dentro de cada panel, la distancia vertical entre Junior/Semi-Senior/Senior
# se mantiene constante — los ascensos siguen garantizando saltos significativos.grilla_modp3 <- df_modelo %>%
  data_grid(
    experiencia = seq_range(c(0,30), n = 50),
    grupo_rol,
    seniority,
    sueldo_dolarizado,
    genero_simple = "Hombre",
    region = "CABA"
  ) %>%
  add_predictions(modp3, var = "pred")



ggplot(df_modelo, aes(x = experiencia, y = log_sal_usd)) +
  geom_point(alpha = 0.2, color = "black") +
  geom_line(data = grilla_modp3, aes(y = pred, color = sueldo_dolarizado, linetype = seniority), size = 1) +
  facet_wrap(~ grupo_rol) +
  labs(
    title = "Modelo P3: Salario vs Experiencia (curvas polinómicas)",
    subtitle = "Género: Hombre | Región: CABA",
    x = "Años de experiencia",
    y = "Logaritmo del salario (USD)",
    color = "Dolarizado",
    linetype = "Seniority"
  ) +
  theme_bw()



# ---------------------------------------------------------
# GRAFICO 2: DIAGNOSTICO DE RESIDUOS (mod3)
# ---------------------------------------------------------

# Residuos bien centrados en 0 a lo largo de todo el rango de predicciones —
# mejor comportamiento que modp1 y modp2.
# La nube es simetrica sin patron visible: el modelo captura bien la estructura principal.df_residuos_p3 <- df_modelo %>%
  add_predictions(modp3, var = "pred") %>%
  add_residuals(modp3, var = "resid")



ggplot(df_residuos_p3, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(
    title = "Residuos vs Predicciones (modp3)",
    x = "Predicciones (log_sal_usd estimado)",
    y = "Residuos"
  ) +
  theme_minimal()



##### Comparacion modp1, modp2 y modp3 con ANOVA ####
anova(modp1, modp2, modp3)

# Resultado ANOVA:
# modp1 -> modp2: F = 640.26, p < 2.2e-16 *** — agregar dolarizacion mejora significativamente
# modp2 -> modp3: F = 105.38, p < 2.2e-16 *** — agregar curvatura mejora significativamente
# Cada modelo agrega poder explicativo real — el modelo 3 es el mas completo.

