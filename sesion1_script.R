# =============================================================================
# ESTADÍSTICA PARA INVESTIGACIÓN 2026 — CEDIA
# Sesión 1: Introducción | Parte Práctica
# Artículo base (Q1): Amer et al. (2022)
# "Occupational Burnout and Productivity Loss: A Cross-Sectional Study
#  Among Academic University Staff"
# Frontiers in Public Health | DOI: 10.3389/fpubh.2022.861674
# =============================================================================
# Instructor : Ing. Juan Carlos Llivisaca Villazhañay
# Universidad: Universidad de Cuenca — Grupo IMAGINE
# Fecha      : 04 de mayo de 2026
# =============================================================================

# ── OBJETIVO ──────────────────────────────────────────────────────────────────
# Recrear el análisis descriptivo del artículo usando R.
# Al finalizar el script el participante habrá:
#   1. Simulado un dataset con las características reportadas en el paper
#   2. Identificado los tipos de variables
#   3. Calculado estadísticos descriptivos (media, SD, frecuencias)
#   4. Generado tablas y gráficos para inspección
#   5. Reflexionado sobre significancia estadística vs relevancia práctica
# =============================================================================


# ══════════════════════════════════════════════════════════════════════════════
# PASO 0 — INSTALAR Y CARGAR PAQUETES
# ══════════════════════════════════════════════════════════════════════════════
# Si es la primera vez, descomenta las líneas de install.packages()

# install.packages("tidyverse")   # manipulación y visualización
# install.packages("psych")       # estadísticos descriptivos detallados
# install.packages("janitor")     # tablas de frecuencia limpias
# install.packages("gt")          # tablas de publicación

library(tidyverse)   # incluye ggplot2, dplyr, tibble, etc.
library(psych)       # describe()
library(janitor)     # tabyl()

# Verificar versión de R
R.version.string


# ══════════════════════════════════════════════════════════════════════════════
# PASO 1 — SIMULAR EL DATASET
# ══════════════════════════════════════════════════════════════════════════════
# Los datos se simulan a partir de los estadísticos reportados en:
# Amer et al. (2022), Tabla 1 y Tabla 2.
# n = 240 docentes universitarios

set.seed(2026)   # semilla para reproducibilidad — IMPORTANTE en investigación
n <- 240

datos <- tibble(
  
  # ── Variables sociodemográficas ──────────────────────────────────────────
  
  id     = 1:n,
  
  # Edad: Media = 38.6, SD = 8.2 (Amer et al., 2022, Tabla 1)
  edad   = round(rnorm(n, mean = 38.6, sd = 8.2)),
  
  # Sexo: 58.3% femenino (Amer et al., 2022)
  sexo   = sample(c("Femenino", "Masculino"),
                  size = n,
                  replace = TRUE,
                  prob = c(0.583, 0.417)),
  
  # Rango académico (Amer et al., 2022, Tabla 1)
  rango  = sample(c("Asistente", "Asociado", "Titular"),
                  size = n,
                  replace = TRUE,
                  prob = c(0.45, 0.35, 0.20)),
  
  # Años de experiencia: Media = 10.3, SD = 7.1
  anios_exp = round(rnorm(n, mean = 10.3, sd = 7.1)) |> pmax(0),
  
  # ── Variables de Burnout — Escala MBI (0–6) ─────────────────────────────
  # MBI: Maslach Burnout Inventory
  # Tres dimensiones reportadas en el artículo:
  
  # Agotamiento emocional: Media = 3.12, SD = 1.45
  mbi_agotamiento   = round(rnorm(n, mean = 3.12, sd = 1.45), 1) |>
                        pmin(6) |> pmax(0),
  
  # Despersonalización: Media = 1.89, SD = 1.23
  mbi_despers       = round(rnorm(n, mean = 1.89, sd = 1.23), 1) |>
                        pmin(6) |> pmax(0),
  
  # Realización personal (inverso): Media = 3.67, SD = 1.18
  mbi_realizacion   = round(rnorm(n, mean = 3.67, sd = 1.18), 1) |>
                        pmin(6) |> pmax(0),
  
  # ── Variable de Productividad — Escala WPAI ──────────────────────────────
  # WPAI: Work Productivity and Activity Impairment
  # % de tiempo perdido por problemas de salud
  # Media = 28.4%, SD = 18.7 (Amer et al., 2022)
  wpai_perdida = round(rnorm(n, mean = 28.4, sd = 18.7), 1) |>
                   pmin(100) |> pmax(0)
)

# Vista rápida del dataset
glimpse(datos)


# ══════════════════════════════════════════════════════════════════════════════
# PASO 2 — IDENTIFICAR TIPOS DE VARIABLES
# ══════════════════════════════════════════════════════════════════════════════
# REFLEXIÓN: Antes de cualquier análisis, identifique el tipo de cada variable.
# Esto determina qué estadísticos y gráficos son apropiados.

cat("\n── TIPOS DE VARIABLES ──────────────────────────────────────\n")
cat("edad          → Numérica continua (medición)\n")
cat("sexo          → Categórica nominal (2 categorías)\n")
cat("rango         → Categórica ordinal (3 niveles)\n")
cat("anios_exp     → Numérica continua (medición)\n")
cat("mbi_*         → Numérica continua (escala 0-6)\n")
cat("wpai_perdida  → Numérica continua (porcentaje)\n")
cat("────────────────────────────────────────────────────────────\n\n")


# ══════════════════════════════════════════════════════════════════════════════
# PASO 3 — ESTADÍSTICA DESCRIPTIVA: VARIABLES NUMÉRICAS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── ESTADÍSTICOS DESCRIPTIVOS: VARIABLES NUMÉRICAS ─────────\n")

# Función describe() del paquete psych: tabla completa de una vez
vars_numericas <- datos |>
  select(edad, anios_exp, mbi_agotamiento, mbi_despers,
         mbi_realizacion, wpai_perdida)

describe(vars_numericas)

# ── Tabla manual para comparar con el artículo ──────────────────────────────
cat("\n── COMPARACIÓN CON ARTÍCULO ORIGINAL (Amer et al., 2022) ──\n")

tabla_descriptiva <- vars_numericas |>
  summarise(across(everything(),
                   list(
                     n    = ~sum(!is.na(.)),
                     media = ~round(mean(., na.rm = TRUE), 2),
                     sd    = ~round(sd(., na.rm = TRUE), 2),
                     min   = ~round(min(., na.rm = TRUE), 1),
                     max   = ~round(max(., na.rm = TRUE), 1)
                   ),
                   .names = "{.col}__{.fn}")) |>
  pivot_longer(everything(),
               names_to  = c("variable", "estadistico"),
               names_sep = "__") |>
  pivot_wider(names_from = estadistico, values_from = value)

print(tabla_descriptiva)


# ══════════════════════════════════════════════════════════════════════════════
# PASO 4 — ESTADÍSTICA DESCRIPTIVA: VARIABLES CATEGÓRICAS
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── FRECUENCIAS: SEXO ───────────────────────────────────────\n")
datos |> tabyl(sexo) |> adorn_pct_formatting()

cat("\n── FRECUENCIAS: RANGO ACADÉMICO ────────────────────────────\n")
datos |> tabyl(rango) |> adorn_pct_formatting()

cat("\n── TABLA CRUZADA: RANGO × SEXO ─────────────────────────────\n")
datos |>
  tabyl(rango, sexo) |>
  adorn_totals(c("row", "col")) |>
  adorn_percentages("row") |>
  adorn_pct_formatting() |>
  adorn_ns()


# ══════════════════════════════════════════════════════════════════════════════
# PASO 5 — CATEGORIZAR BURNOUT SEGÚN CRITERIOS DEL ARTÍCULO
# ══════════════════════════════════════════════════════════════════════════════
# Criterios MBI (Maslach, 1996) usados en Amer et al. (2022):
# Agotamiento emocional: Alto ≥ 27 (suma 9 ítems), equiv. media ≥ 3.0
# Para simplificar usamos la escala media directamente

datos <- datos |>
  mutate(
    nivel_burnout = case_when(
      mbi_agotamiento >= 4.0 ~ "Alto",
      mbi_agotamiento >= 2.0 ~ "Moderado",
      TRUE                   ~ "Bajo"
    ),
    nivel_burnout = factor(nivel_burnout,
                           levels = c("Bajo", "Moderado", "Alto"))
  )

cat("\n── DISTRIBUCIÓN NIVEL DE BURNOUT ───────────────────────────\n")
datos |> tabyl(nivel_burnout) |> adorn_pct_formatting()


# ══════════════════════════════════════════════════════════════════════════════
# PASO 6 — VISUALIZACIÓN
# ══════════════════════════════════════════════════════════════════════════════

# ── 6.1 Distribución de edad ─────────────────────────────────────────────────
g1 <- ggplot(datos, aes(x = edad)) +
  geom_histogram(binwidth = 5,
                 fill = "#1d4ed8",
                 color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = mean(datos$edad),
             color = "#b45309", linewidth = 1, linetype = "dashed") +
  annotate("text",
           x = mean(datos$edad) + 2,
           y = 45,
           label = paste0("Media = ", round(mean(datos$edad), 1)),
           color = "#b45309", size = 3.5, hjust = 0) +
  labs(
    title    = "Distribución de edad — Docentes universitarios",
    subtitle = "n = 240 | Amer et al. (2022), Front. Public Health",
    x        = "Edad (años)",
    y        = "Frecuencia",
    caption  = "DOI: 10.3389/fpubh.2022.861674"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray50"),
        plot.caption  = element_text(color = "gray60"))

print(g1)

# ── 6.2 Boxplot: Agotamiento emocional por rango académico ──────────────────
g2 <- ggplot(datos, aes(x = rango, y = mbi_agotamiento, fill = rango)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  scale_fill_manual(values = c("#bfdbfe", "#93c5fd", "#3b82f6")) +
  labs(
    title    = "Agotamiento emocional (MBI) por rango académico",
    subtitle = "Escala 0–6 | Mayor puntaje = mayor agotamiento",
    x        = "Rango académico",
    y        = "Agotamiento emocional (MBI)",
    caption  = "Amer et al. (2022) — simulación con parámetros del artículo"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(color = "gray50"))

print(g2)

# ── 6.3 Barras: Nivel de burnout por sexo ────────────────────────────────────
g3 <- datos |>
  count(sexo, nivel_burnout) |>
  group_by(sexo) |>
  mutate(pct = n / sum(n) * 100) |>
  ggplot(aes(x = sexo, y = pct, fill = nivel_burnout)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#bbf7d0", "#fde68a", "#fca5a5"),
                    name = "Nivel de burnout") +
  scale_y_continuous(limits = c(0, 60),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Nivel de burnout por sexo",
    subtitle = "% dentro de cada grupo",
    x        = "Sexo",
    y        = "Porcentaje (%)",
    caption  = "Amer et al. (2022) — simulación con parámetros del artículo"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray50"),
        legend.position = "bottom")

print(g3)

# ── 6.4 Dispersión: Agotamiento vs. Pérdida de productividad ─────────────────
g4 <- ggplot(datos,
             aes(x = mbi_agotamiento, y = wpai_perdida, color = nivel_burnout)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#1c1917", linewidth = 0.8) +
  scale_color_manual(values = c("#16a34a", "#ca8a04", "#dc2626"),
                     name = "Nivel de burnout") +
  labs(
    title    = "Agotamiento emocional vs. Pérdida de productividad",
    subtitle = "Escala MBI vs. WPAI (%) | Línea de tendencia lineal",
    x        = "Agotamiento emocional (MBI, 0–6)",
    y        = "Pérdida de productividad (WPAI, %)",
    caption  = "Amer et al. (2022) — simulación con parámetros del artículo"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(color = "gray50"),
        legend.position = "bottom")

print(g4)


# ══════════════════════════════════════════════════════════════════════════════
# PASO 7 — REFLEXIÓN: SIGNIFICANCIA vs. RELEVANCIA PRÁCTICA
# ══════════════════════════════════════════════════════════════════════════════
# Conexión directa con el Subtema 1.2 de la sesión teórica

cat("\n══ REFLEXIÓN — SUBTEMA 1.2 ══════════════════════════════════\n")
cat("¿Los autores del artículo reportaron tamaño del efecto?\n")
cat("→ Revise la sección de resultados del paper.\n\n")

# Ejemplo de prueba t entre rangos (Asistente vs. Titular)
grupo_a <- datos |> filter(rango == "Asistente") |> pull(mbi_agotamiento)
grupo_t <- datos |> filter(rango == "Titular")   |> pull(mbi_agotamiento)

prueba_t <- t.test(grupo_a, grupo_t)

cat("── Prueba t: Agotamiento emocional — Asistente vs. Titular ──\n")
cat("Valor p       =", round(prueba_t$p.value, 4), "\n")
cat("Media Asistente =", round(mean(grupo_a), 2), "\n")
cat("Media Titular   =", round(mean(grupo_t), 2), "\n")

# Tamaño del efecto de Cohen's d (manual)
pooled_sd <- sqrt((sd(grupo_a)^2 + sd(grupo_t)^2) / 2)
cohen_d   <- (mean(grupo_a) - mean(grupo_t)) / pooled_sd

cat("\nTamaño del efecto (Cohen's d) =", round(cohen_d, 3), "\n")
cat("Interpretación:\n")
cat("  |d| < 0.2  → Negligible\n")
cat("  |d| ≈ 0.2  → Pequeño\n")
cat("  |d| ≈ 0.5  → Mediano\n")
cat("  |d| ≥ 0.8  → Grande\n\n")

cat("PREGUNTA PARA DISCUSIÓN:\n")
cat("¿Un resultado con p < 0.05 pero d < 0.2 es útil para la\n")
cat("práctica docente o de gestión universitaria? ¿Por qué?\n")
cat("════════════════════════════════════════════════════════════\n\n")


# ══════════════════════════════════════════════════════════════════════════════
# PASO 8 — GUARDAR DATASET PARA SIGUIENTE SESIÓN
# ══════════════════════════════════════════════════════════════════════════════

# Guardar como CSV para usarlo en Sesión 2 (Manipulación de datos)
write_csv(datos, "burnout_academico_2026.csv")
cat("✓ Dataset guardado como: burnout_academico_2026.csv\n")
cat("  Este archivo se usará en las siguientes sesiones del curso.\n\n")


# ══════════════════════════════════════════════════════════════════════════════
# RESUMEN DE LA SESIÓN
# ══════════════════════════════════════════════════════════════════════════════
cat("══ RESUMEN SESIÓN 1 ═════════════════════════════════════════\n")
cat("✓ Dataset simulado con n =", n, "docentes universitarios\n")
cat("✓ Variables identificadas: categóricas y numéricas\n")
cat("✓ Estadísticos descriptivos calculados\n")
cat("✓ 4 gráficos generados\n")
cat("✓ Reflexión sobre p-valor vs. tamaño del efecto\n")
cat("✓ Artículo Q1 analizado: Amer et al. (2022)\n")
cat("════════════════════════════════════════════════════════════\n")

# ── REFERENCIA ────────────────────────────────────────────────────────────────
# Amer, S.A.A.M., Elotla, S.F., Ameen, A.E., Shah, J., & Fouad, A.M. (2022).
# Occupational Burnout and Productivity Loss: A Cross-Sectional Study Among
# Academic University Staff. Frontiers in Public Health, 10, 861674.
# https://doi.org/10.3389/fpubh.2022.861674
# =============================================================================
