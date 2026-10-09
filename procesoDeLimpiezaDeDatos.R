# ============================================================

# TRABAJO DE ECONOMETRÍA

# REGRESIÓN LINEAL MÚLTIPLE

# ============================================================

# ------------------------------------------------------------

# 1. CARGAR LOS PAQUETES
# ============================================================

# Instalar paquetes (solo ejecutar la primera vez)
install.packages(c(
  "readxl",
  "ggplot2",
  "dplyr",
  "sandwich",
  "psych",
  "broom",
  "tseries",
  "car",
  "lmtest"
))

# Cargar paquetes
library(readxl)
library(ggplot2)
library(dplyr)
library(sandwich)
library(psych)
library(broom)
library(tseries)
library(car)
library(lmtest)
# -----------------------------------------------------------

# 2. CARGAR LA BASE DE DATOS

# ------------------------------------------------------------

d <- read_excel("C:/Users/Familia/Downloads/d.xlsx")


View(d)

dim(d)

names(d)

head(d)

str(d)


# REVISAR LOS VALORES FALTANTES

# ------------------------------------------------------------

colSums(is.na(d))

na_por_columna <- data.frame(
  Variable = names(d),
  Numero_NA = colSums(is.na(d)),
  Porcentaje_NA = round(
    colSums(is.na(d)) / nrow(d) * 100,
    2
  )
)

print(na_por_columna)

sum(is.na(d))

# ------------------------------------------------------------

# 4. CONVERTIR LAS VARIABLES CATEGÓRICAS EN FACTORES

# ------------------------------------------------------------

d$hombre <- factor(
  d$hombre,
  levels = c("Mujer", "Hombre")
)

d$blanco <- factor(
  d$blanco,
  levels = c("No blanco", "Blanco")
)

levels(d$hombre)

levels(d$blanco)

table(d$hombre)

table(d$blanco)

# ------------------------------------------------------------
# 5. ESTADÍSTICAS DESCRIPTIVAS
# ------------------------------------------------------------

variables_numericas <- c(
  "pe_nacer",
  "pre_cigarrillo",
  "a_esc_padre",
  "a_esc_madre",
  "orden_naci_ninho",
  "n_cig_fum_dia_madre",
  "log_ing",
  "ingreso"
)

# Comprobar que todas sean numéricas
sapply(d[, variables_numericas], class)

# Estadísticas descriptivas básicas
summary(d[, variables_numericas])

# Estadísticas descriptivas detalladas
psych::describe(d[, variables_numericas])
# ------------------------------------------------------------

# 6. GRÁFICOS EXPLORATORIOS

# ------------------------------------------------------------

# Peso al nacer

ggplot(d, aes(x = pe_nacer)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribución del peso al nacer",
    x = "Peso al nacer",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Peso al nacer según sexo

ggplot(d, aes(x = hombre, y = pe_nacer)) +
  geom_boxplot() +
  labs(
    title = "Peso al nacer según sexo",
    x = "Sexo",
    y = "Peso al nacer"
  ) +
  theme_minimal()

# Peso al nacer según condición racial

ggplot(d, aes(x = blanco, y = pe_nacer)) +
  geom_boxplot() +
  labs(
    title = "Peso al nacer según condición racial",
    x = "Condición racial",
    y = "Peso al nacer"
  ) +
  theme_minimal()

# Cigarrillos fumados y peso al nacer

ggplot(
  d,
  aes(
    x = n_cig_fum_dia_madre,
    y = pe_nacer
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Cigarrillos fumados por la madre y peso al nacer",
    x = "Cigarrillos por día",
    y = "Peso al nacer"
  ) +
  theme_minimal()

# Educación de la madre y peso al nacer

ggplot(
  d,
  aes(
    x = a_esc_madre,
    y = pe_nacer
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Educación de la madre y peso al nacer",
    x = "Años de educación",
    y = "Peso al nacer"
  ) +
  theme_minimal()

# Educación del padre y peso al nacer

ggplot(
  d,
  aes(
    x = a_esc_padre,
    y = pe_nacer
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Educación del padre y peso al nacer",
    x = "Años de educación",
    y = "Peso al nacer"
  ) +
  theme_minimal()

# Ingreso y peso al nacer

ggplot(
  d,
  aes(
    x = log_ing,
    y = pe_nacer
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Ingreso y peso al nacer",
    x = "Logaritmo del ingreso",
    y = "Peso al nacer"
  ) +
  theme_minimal()


# 7. CORRELACIONES

correlaciones <- cor(
  d[, variables_numericas],
  use = "complete.obs"
)

round(
  correlaciones,
  3
)


# Correlación de cada variable con el peso al nacer

round(
  correlaciones[, "pe_nacer"],
  3
)

d$n_cig_fum_dia_madre <- as.numeric(d$n_cig_fum_dia_madre)
class(d$n_cig_fum_dia_madre)

round(
  correlaciones[, "pe_nacer"],
  3
)


# ============================================================

# III.B. REGRESIÓN LINEAL MÚLTIPLE

# ============================================================

# ------------------------------------------------------------

# 8. ESTIMACIÓN DEL MODELO

# ------------------------------------------------------------

modelo <- lm(
  pe_nacer ~
    pre_cigarrillo +
    a_esc_padre +
    a_esc_madre +
    orden_naci_ninho +
    hombre +
    blanco +
    n_cig_fum_dia_madre +
    log_ing,
  data = d
)

summary(modelo)


modelo_reducido <- lm(
  pe_nacer ~
    orden_naci_ninho +
    hombre +
    blanco +
    n_cig_fum_dia_madre,
  data = d
)

summary(modelo_reducido)
# ------------------------------------------------------------
# 9. COEFICIENTES E INTERVALOS DE CONFIANZA
# ------------------------------------------------------------

coef(summary(modelo_reducido))

confint(modelo_reducido)

resultados <- tidy(
  modelo_reducido,
  conf.int = TRUE
)

print(resultados)


# ------------------------------------------------------------
# 10. MEDIDAS DE AJUSTE
# ------------------------------------------------------------

summary(modelo_reducido)$r.squared

summary(modelo_reducido)$adj.r.squared

sigma(modelo_reducido)

AIC(modelo_reducido)

BIC(modelo_reducido)


# ============================================================
# 11. RESIDUOS
# ============================================================

residuos <- residuals(modelo_reducido)

valores_ajustados <- fitted(modelo_reducido)

summary(residuos)


# ------------------------------------------------------------
# 12. NORMALIDAD DE LOS RESIDUOS
# ------------------------------------------------------------

hist(
  residuos,
  breaks = 30,
  main = "Histograma de los residuos",
  xlab = "Residuos"
)

qqnorm(
  residuos,
  main = "QQ-Plot de los residuos"
)

qqline(residuos)

shapiro.test(residuos)

jarque.bera.test(residuos)


# ------------------------------------------------------------
# 13. HOMOCEDASTICIDAD
# ------------------------------------------------------------

bptest(modelo_reducido)

plot(
  valores_ajustados,
  residuos,
  main = "Residuos vs valores ajustados",
  xlab = "Valores ajustados",
  ylab = "Residuos"
)

abline(h = 0, lty = 2)


# ------------------------------------------------------------
# 14. MULTICOLINEALIDAD
# ------------------------------------------------------------

vif(modelo_reducido)


# ------------------------------------------------------------
# 15. ERRORES ESTÁNDAR ROBUSTOS
# ------------------------------------------------------------

coeftest(modelo_reducido)

errores_robustos <- coeftest(
  modelo_reducido,
  vcov = vcovHC(
    modelo_reducido,
    type = "HC1"
  )
)

print(errores_robustos)


# ------------------------------------------------------------
# 16. COMPARACIÓN DE ERRORES ESTÁNDAR
# ------------------------------------------------------------

tabla_comparacion <- data.frame(
  Variable = rownames(coef(summary(modelo_reducido))),
  Coeficiente = coef(modelo_reducido),
  Error_Estandar = coef(summary(modelo_reducido))[, "Std. Error"],
  P_valor = coef(summary(modelo_reducido))[, "Pr(>|t|)"],
  Error_Robusto = errores_robustos[, "Std. Error"],
  P_Robusto = errores_robustos[, "Pr(>|t|)"]
)

print(tabla_comparacion)

cor(d$orden_naci_ninho, residuos)
cor(d$n_cig_fum_dia_madre, residuos)

# ------------------------------------------------------------
# 17. GRÁFICOS DE DIAGNÓSTICO
# ------------------------------------------------------------

par(mfrow = c(2, 2))

plot(modelo_reducido)

par(mfrow = c(1, 1))


# ============================================================
# III.D. RESULTADOS FINALES
# ============================================================

# ------------------------------------------------------------
# 18. TABLA FINAL DEL MODELO
# ------------------------------------------------------------

tabla_final <- tidy(
  modelo_reducido,
  conf.int = TRUE,
  conf.level = 0.95
)

print(tabla_final)


# ------------------------------------------------------------
# 19. INFORMACIÓN GENERAL DEL MODELO
# ------------------------------------------------------------

cat("\n")
cat("============================================\n")
cat("RESULTADOS DEL MODELO REDUCIDO\n")
cat("============================================\n")

cat(
  "Número de observaciones:",
  nobs(modelo_reducido),
  "\n"
)

cat(
  "R cuadrado:",
  round(
    summary(modelo_reducido)$r.squared,
    4
  ),
  "\n"
)

cat(
  "R cuadrado ajustado:",
  round(
    summary(modelo_reducido)$adj.r.squared,
    4
  ),
  "\n"
)

cat(
  "Error estándar residual:",
  round(
    sigma(modelo_reducido),
    4
  ),
  "\n"
)


# ------------------------------------------------------------
# 20. GUARDAR LOS RESULTADOS
# ------------------------------------------------------------

write.csv(
  tabla_final,
  "resultados_modelo_reducido.csv",
  row.names = FALSE
)

write.csv(
  tabla_comparacion,
  "errores_estandar_modelo_reducido.csv",
  row.names = FALSE
)

# ============================================================
# FIN DEL SCRIPT
# ============================================================
