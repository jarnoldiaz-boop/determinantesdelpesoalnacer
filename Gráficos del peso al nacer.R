# ============================================================
# Proyecto: Determinantes del Peso al Nacer
# Autor: Jhon Arnold Díaz
# Asignatura: Econometría
# Descripción:
# Este script realiza el análisis exploratorio de la base
# de datos utilizada para estudiar los determinantes del
# peso al nacer mediante estadística descriptiva y
# visualizaciones gráficas.
# ============================================================
# ============================================================
# DETERMINANTES DEL PESO AL NACER
# Análisis exploratorio y visualización de datos
# ============================================================

# ------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# 2. PREPARACIÓN DE DATOS
# ------------------------------------------------------------

# Conversión de variables al tipo adecuado

d$n_cig_fum_dia_madre <- as.numeric(d$n_cig_fum_dia_madre)

# Verificar estructura de la base de datos
sapply(d, class)

# ============================================================
# 3. ANÁLISIS DEL PESO AL NACER (pe_nacer)
# ============================================================

# ------------------------------------------------------------
# 3.1 Histograma del peso al nacer
# ------------------------------------------------------------

media <- mean(d$pe_nacer, na.rm = TRUE)

g <- ggplot(d, aes(x = pe_nacer)) +
  geom_histogram(bins = 30)

datos_hist <- ggplot_build(g)$data[[1]]

max_freq <- max(datos_hist$count)

ggplot(d, aes(x = pe_nacer)) +

  geom_histogram(
    bins = 30,
    fill = "#4F81BD",
    color = "white"
  ) +

  geom_text(
    data = subset(datos_hist, count > 0),
    aes(
      x = x,
      y = count,
      label = count
    ),
    inherit.aes = FALSE,
    vjust = -0.3,
    size = 3
  ) +

  geom_vline(
    xintercept = media,
    color = "red",
    linewidth = 1.2
  ) +

  annotate(
    "text",
    x = media,
    y = max_freq + 12,
    label = round(media, 1),
    color = "red",
    fontface = "bold",
    size = 5
  ) +

  scale_x_continuous(
    breaks = seq(
      floor(min(d$pe_nacer, na.rm = TRUE) / 20) * 20,
      ceiling(max(d$pe_nacer, na.rm = TRUE) / 20) * 20,
      by = 20
    )
  ) +

  scale_y_continuous(
    breaks = seq(
      0,
      max_freq + 20,
      by = 20
    ),
    limits = c(0, max_freq + 25)
  ) +

  labs(
    x = "Peso al nacer",
    y = "Bebés nacidos"
  ) +

  theme_classic()

# ------------------------------------------------------------
# 3.2 Distribución individual de observaciones
# ------------------------------------------------------------

datos_disp <- data.frame(
  observacion = 1:nrow(d),
  pe_nacer = d$pe_nacer
)

media <- mean(d$pe_nacer, na.rm = TRUE)

ggplot(datos_disp, aes(x = observacion, y = pe_nacer)) +

  geom_point(
    color = "#4F81BD",
    size = 2,
    alpha = 0.7
  ) +

  geom_hline(
    yintercept = media,
    color = "red",
    linewidth = 1.2
  ) +

  annotate(
    "text",
    x = max(datos_disp$observacion) * 0.85,
    y = media + 5,
    label = paste("Media =", round(media, 1)),
    color = "red",
    fontface = "bold"
  ) +

  scale_y_continuous(
    breaks = seq(
      floor(min(d$pe_nacer, na.rm = TRUE) / 20) * 20,
      ceiling(max(d$pe_nacer, na.rm = TRUE) / 20) * 20,
      by = 20
    )
  ) +

  labs(
    x = "Observación",
    y = "Peso al nacer"
  ) +

  theme_classic()

# ------------------------------------------------------------
# 3.3 Boxplot del peso al nacer
# ------------------------------------------------------------

ggplot(d, aes(x = "", y = pe_nacer)) +

  geom_boxplot(
    fill = "#4F81BD",
    alpha = 0.9,
    width = 0.5,
    outlier.color = "#4F81BD"
  ) +

  geom_hline(
    yintercept = 141,
    color = "black"
  ) +

  geom_hline(
    yintercept = 88,
    color = "black"
  ) +

  annotate(
    "text",
    x = 1,
    y = 144,
    label = "141",
    fontface = "bold"
  ) +

  annotate(
    "text",
    x = 1,
    y = 91,
    label = "88",
    fontface = "bold"
  ) +

  scale_y_continuous(
    breaks = seq(0, 300, by = 20)
  ) +

  labs(
    x = "",
    y = "Peso al nacer"
  ) +

  theme_classic()

# ============================================================
# 4. PRECIO DEL CIGARRILLO
# ============================================================

# ------------------------------------------------------------
# 4.1 Gráfico de dona
# ------------------------------------------------------------

intervalos <- cut(
  d$pre_cigarrillo,
  breaks = 5,
  include.lowest = TRUE
)

tabla <- data.frame(intervalo = intervalos) %>%
  count(intervalo) %>%
  mutate(
    porcentaje = round(100 * n / sum(n), 1),
    etiqueta = paste0(porcentaje, "%")
  )

ggplot(tabla, aes(x = 2, y = n, fill = intervalo)) +
  ...

# ------------------------------------------------------------
# 4.2 Relación entre precio del cigarrillo y peso al nacer
# ------------------------------------------------------------

modelo <- lm(pe_nacer ~ pre_cigarrillo, data = d)

# Obtención de coeficientes y R²
b0 <- coef(modelo)[1]
b1 <- coef(modelo)[2]
r2 <- summary(modelo)$r.squared

# Construcción del gráfico
ggplot(d, aes(x = pre_cigarrillo, y = pe_nacer)) +
  ...

# ============================================================
# 5. ESCOLARIDAD DE LA MADRE
# ============================================================

# 5.1 Gráfico de dona
# 5.2 Dispersión vs peso al nacer

# ============================================================
# 6. ESCOLARIDAD DEL PADRE
# ============================================================

# 6.1 Gráfico de dona
# 6.2 Dispersión vs peso al nacer

# ============================================================
# 7. ORDEN DE NACIMIENTO
# ============================================================

# 7.1 Boxplot
# 7.2 Relación con peso al nacer

# ============================================================
# 8. VARIABLES CATEGÓRICAS
# ============================================================

# ------------------------------------------------------------
# 8.1 Sexo del bebé (Hombre / Mujer)
# ------------------------------------------------------------

# Código del gráfico de dona

# ------------------------------------------------------------
# 8.2 Raza (Blanco / No blanco)
# ------------------------------------------------------------

# Código del gráfico de dona

# ============================================================
# 9. INGRESO FAMILIAR
# ============================================================

# ------------------------------------------------------------
# 9.1 Boxplot del logaritmo del ingreso
# ------------------------------------------------------------

ggplot(d, aes(y = log_ing)) +
  ...

# ------------------------------------------------------------
# 9.2 Relación entre ingreso y peso al nacer
# ------------------------------------------------------------

modelo <- lm(pe_nacer ~ log_ing, data = d)

ggplot(d, aes(x = log_ing, y = pe_nacer)) +
  ...
