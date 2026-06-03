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

#Como afecta la experiencia al salario?
#Como afecta la modalidad al salario?
#El valor de un anio de experiencia cambia segun la modalidad?

# PREGUNTAS para generar hipotesis.. 
#
# Que rol maximiza el salario en dolares en el sector IT argentino, controlando por experiencia, region y genero?
# Que rol y nivel de seniority  maximiza el salario en dolares en el sector IT argentino, controlando por experiencia, region y genero?
#
# Yendo por esas preguntas quizas un modelo podria ser:
# Modelo 1 log_sal_usd ~ experiencia + grupo_rol + genero_simple + region + gente_a_cargo ??
# Modelo 2 —  agregar seniority + sueldo_dolarizado  ??
#


# ---
#### Modelo 1: Salarios según la experiencia, grupo rol, seniority ####
modp1 <- lm(log_sal_usd ~ experiencia + grupo_rol + seniority + genero_simple + region,
            data = df_modelo)

summary(modp1)

#### Observaciones mod1: ####
#   (Multiple R-squared): El valor es 0.2541
#   Esto significa que el modelo logra capturar y explicar el 25.41% de la variabilidad 
#   de los salarios
#   El ~75% restante se debe a la dispersión natural de los sueldos y a factores
#   que aún no se incluyeron (como la dolarización).

# Error residual (Residual standard error): Es 0.552 indica que, en promedio, 
# las predicciones del modelo 1 se desvían de los salarios reales en 0.552 unidades logarítmicas

# El (Intercept) de 7.027 es el salario base estimado, corresponde a un profesional 
# con 0 años de experiencia, con seniority Junior, y de un grupo de rol de referencia 
# (Ciberseguridad)

# Impacto de las variables

# Experiencia: Su coeficiente es 0.0066. Por cada año adicional de experiencia, 
# el salario aumenta en promedio un 0.66%.

# Seniority (los mayores saltos): Comparado con ser Junior, ascender a Semi-Senior
# aumenta el sueldo en aproximadamente un 49.1% (coeficiente 0.4912). Ascender a 
# Senior lo incrementa un 79.4% respecto a la base (coeficiente 0.7940).

# Grupo Rol: Los coeficientes negativos indican que ganan menos que el rol de 
# referencia. El perfil de Datos / AI gana un 6.8% menos (coeficiente -0.068), 
# Infraestructura un 15% menos, y Desarrollo / QA un 15.3% menos.

#### GRAFICOS MODELO 1 ####

# ---------------------------------------------------------
# GRÁFICO 1: VISUALIZACIÓN DEL MODELO (Predicciones)
# ---------------------------------------------------------

# Creamos una grilla con todas las combinaciones posibles de nuestras variables
# Usamos seq_range para generar una secuencia suave de años de experiencia
grilla_modp1 <- df_modelo %>%
  data_grid(
    experiencia = seq_range(experiencia, n = 50),
    grupo_rol,
    seniority,
    genero_simple = "Hombre",
    region = "CABA"
  ) %>%
  add_predictions(modp1, var = "pred") # Agregamos la predicción del modelo 1


# Graficamos los datos reales y le superponemos las rectas de las predicciones
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

# Calculamos predicciones y residuos sobre los datos reales
df_residuos_p1 <- df_modelo %>%
  add_predictions(modp1, var = "pred") %>%
  add_residuals(modp1, var = "resid")

# Graficamos
ggplot(df_residuos_p1, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(
    title = "Residuos vs Predicciones (modp1)",
    x = "Predicciones (log_sal_usd estimado)",
    y = "Residuos"
  ) +
  theme_minimal()


#### Modelo 2: Salarios según la experiencia con interaccion sueldo_dolarizado, grupo rol, seniority ####
modp2 <- lm(log_sal_usd ~ experiencia * sueldo_dolarizado + grupo_rol + seniority + genero_simple + region,
            data = df_modelo)

summary(modp2)

#### Observaciones mod2: ####
# El Multiple R-squared saltó a 0.3109.
# El Modelo 1 explicaba el 25.41% de los salarios, el modelo 2 ahora captura el
# 31.09% de la variabilidad. 

# El Residual standard error bajó de 0.552 a 0.5305
# Esto indica que, al sumar la dolarización y su interacción, las predicciones de 
# este modelo se equivocan menos (los errores se achicaron).

# Interaccion entre variables
# experiencia:sueldo_dolarizadoTRUE: Su p-valor es < 2e-16 (tiene ***), lo que 
# confirma que la interacción es altamente significativa
# El efecto de la experiencia depende definitivamente de si cobrás en dólares o no.

# Para sueldos en Pesos: La pendiente está dada por el coeficiente experiencia 
# (0.003985). Es decir, por cada año extra, el sueldo en pesos crece apenas un ~0.4%.

# Para sueldos Dolarizados: La pendiente es la suma de la base más la interacción
# (0.003985 + 0.017400 = 0.021385). Por cada año extra, el sueldo dolarizado crece 
# en promedio un ~2.14%. La experiencia "paga" o rinde más de 5 veces más rápido 
# a largo plazo si trabajás para el exterior.

# El término sueldo_dolarizadoTRUE (0.157370) también es muy significativo (***).
# Esto dice que, en el punto de partida (a los 0 años de experiencia, comparando 
# dos Juniors del mismo rol), el mero hecho de tener el sueldo dolarizado incrementa 
# el salario en aproximadamente un 15.7% respecto a la base en pesos.

# El resto de las variables (Seniority y Rol):
# Las variables estructurales que traíamos del mod1 se ajustaron levemente pero 
# siguen siendo estadísticamente significativas (***)

# Manteniendo todo constante, ser Senior aumenta el sueldo en un enorme ~71.7% 
# (coeficiente 0.7176) frente a un Junior, y ser Semi-Senior un ~46.6%

# Los roles mantienen la misma lógica: Desarrollo/QA (-19.6%), 
# Infraestructura (-15.3%) y Datos/AI (-8.8%) ganan menos que el rol base 
# "Otros" / Analytics

#### GRAFICOS MODELO 2 ####
# ---------------------------------------------------------
# GRÁFICO 1: VISUALIZACIÓN DEL MODELO 2 (Predicciones)
# ---------------------------------------------------------

# 1. Creamos la grilla agregando 'sueldo_dolarizado'
grilla_modp2 <- df_modelo %>%
  data_grid(
    experiencia = seq_range(experiencia, n = 50),
    grupo_rol,
    seniority,
    sueldo_dolarizado,
    genero_simple = "Hombre",
    region = "CABA"
  ) %>%
  add_predictions(modp2, var = "pred")

# 2. Graficamos superponiendo las rectas al scatter plot
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
# GRÁFICO 2: DIAGNÓSTICO DE RESIDUOS (mod2)
# ---------------------------------------------------------

# Calculamos predicciones y residuos sobre los datos reales
df_residuos_p2 <- df_modelo %>%
  add_predictions(modp2, var = "pred") %>%
  add_residuals(modp2, var = "resid")

# Graficamos los residuos
ggplot(df_residuos_p2, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(
    title = "Residuos vs Predicciones (modp2)",
    x = "Predicciones (log_sal_usd estimado)",
    y = "Residuos"
  ) +
  theme_minimal()

#### Modelo 3: Probando experiencia con poly y su interaccion con dolarizado + grupo rol + seniority ####
modp3 <- lm(log_sal_usd ~ poly(experiencia, 2) * sueldo_dolarizado + grupo_rol + seniority + genero_simple + region,
            data = df_modelo)

summary(modp3)

#### Observaciones mod3: ####
# El Modelo 2 explicaba el 31.09% de la varianza. Al permitir que la experiencia 
# se curve, el Multiple R-squared subió a 0.3189 (casi 32%).
# El error residual bajó aún más, llegando a 0.5275.

## Terminos cuadraticos:
# poly(experiencia, 2) R dividió la experiencia en dos componentes: el componente 
# lineal (la tendencia general de subida) y el componente cuadrático 
# (la concavidad o curvatura).

# poly(experiencia, 2)1 (Positivo y significativo): Confirma que la tendencia general
# a lo largo del tiempo es que el sueldo suba.

# poly(experiencia, 2)2 (-6.534, negativo y significativo): El signo negativo 
# indica que la curva tiene forma de "U invertida" o parábola cóncava. 
# Matemáticamente, demuestra lo que vimos en el gráfico de Salario vs. Experiencia,
# los sueldos crecen muy rápido en los primeros años, pero luego se 
# desaceleran y alcanzan un "techo" a medida que los profesionales llegan a los 
# 20 o 30 años de experiencia.

## La Interacción Polinómica con la Dolarización 
# El modelo demuestra que la forma de la curva cambia según la moneda.
# Los términos poly(...)1:sueldo_dolarizadoTRUE y poly(...)2:sueldo_dolarizadoTRUE 
# son altísimamente significativos (***).
# Esto significa que si cobras en dólares, no solo la pendiente inicial es más 
# agresiva (crece más rápido), sino que la forma en la que el sueldo se va 
# "estancando" a lo largo de los años sigue una curvatura matemáticamente 
# distinta a la de los sueldos en pesos.

## El resto de las variables
# El seniority y el grupo de rol siguen siendo muy significativos. Un detalle 
# interesante es que, al capturar mejor la curvatura de la experiencia, 
# los saltos por Seniority se ajustaron levemente (por ejemplo, el salto por ser 
# Senior pasó de ~71% en el modelo 2 a ~51% en el modelo 3). 

# En los modelos anteriores, como la recta no lograba subir lo suficiente en los
# primeros años, el modelo "inflaba" el coeficiente del Seniority para compensar. 
# Ahora el modelo es mucho más preciso aislando qué aumento es por años de 
# experiencia y qué aumento es por ascenso de rol.


#### GRAFICOS MODELO 3 ####
# ---------------------------------------------------------
# GRÁFICO 1: VISUALIZACIÓN DEL MODELO 3 (Curvas Polinómicas)
# ---------------------------------------------------------

# 1. Creamos la grilla (usamos las mismas variables que en mod2, pero predecimos con mod3)
grilla_modp3 <- df_modelo %>%
  data_grid(
    experiencia = seq_range(c(0,30), n = 50),
    grupo_rol,
    seniority,
    sueldo_dolarizado,
    genero_simple = "Hombre",
    region = "CABA"
  ) %>%
  add_predictions(modp3, var = "pred")

# 2. Graficamos superponiendo las curvas al scatter plot
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
# GRÁFICO 2: DIAGNÓSTICO DE RESIDUOS (mod3)
# ---------------------------------------------------------

# Calculamos predicciones y residuos sobre los datos reales usando mod3
df_residuos_p3 <- df_modelo %>%
  add_predictions(modp3, var = "pred") %>%
  add_residuals(modp3, var = "resid")

# Graficamos los residuos
ggplot(df_residuos_p3, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(
    title = "Residuos vs Predicciones (modp3)",
    x = "Predicciones (log_sal_usd estimado)",
    y = "Residuos"
  ) +
  theme_minimal()

# Observaciones de graficos mod3
# El gráfico muestracómo los sueldos crecen velozmente durante los primeros 
# 10 a 15 años de experiencia, pero luego alcanzan un "techo" a partir 
# de los 20 o 30 años.

# Las curvas color turquesa (Dolarizado = TRUE) no solo tienen una pendiente 
# inicial más alta, sino que llegan a un pico salarial mucho más alto que 
# las curvas rojas (FALSE)
# Esto ilustra que cobrar en moneda extranjera cambia por completo la dinámica 
# del crecimiento a lo largo de los años.

# Dentro de cada panel (como Desarrollo o Datos) y color, las curvas mantienen su
# distancia vertical según sean líneas continuas (Junior), punteadas (Semi-Senior)
# o a rayas (Senior)
# Esto demuestra que los ascensos de categoría siguen garantizando grandes saltos 
# salariales sin importar la etapa de la curva en la que se encuentre.


##### Comparando mod1, mod2 y mod3 con ANOVA ####
anova(modp1,modp2,modp3)
