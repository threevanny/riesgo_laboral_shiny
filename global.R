# GLOBAL.R
# ---- Librerias --------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(margins)
library(caret)
library(pROC)
library(DT)
library(scales)
library(plotly)
library(sf)
library(leaflet)
library(geodata)

# ---- Colores ----------------------------------------------------

PAL <- list(
  azul_profundo  = "#1B3A4B",
  azul_medio     = "#2E6E8E",
  terracota      = "#C75D3C",
  verde_salvia   = "#5B8C6E",
  arena          = "#F4EFE7",
  gris_texto     = "#33363A",
  gris_claro     = "#8C9094"
)

app_theme <- bs_theme(
  version = 5,
  bg = "#FBF9F5",
  fg = PAL$gris_texto,
  primary = PAL$azul_medio,
  secondary = PAL$terracota,
  success = PAL$verde_salvia,
  base_font = font_google("Source Sans Pro"),
  heading_font = font_google("Lora"),
  font_scale = 0.95
) |>
  bs_add_rules("
    .navbar { border-bottom: 3px solid #1B3A4B; }
    .card { border: none; box-shadow: 0 1px 3px rgba(0,0,0,0.08); border-radius: 10px; }
    .card-header { background-color: #1B3A4B; color: white; font-weight: 600;
                   border-radius: 10px 10px 0 0 !important; }
    .value-box { border-radius: 10px; }
    h2, h3, h4 { color: #1B3A4B; }
    .kpi-num { font-family: 'Lora', serif; font-weight: 700; }
    hr.sep { border-top: 1px solid #d8d2c4; margin: 1.2rem 0; }
  ")

# ---- 1. Carga de datos -------------------------------------------------------
datos_raw <- read.csv("data/base_jovenes_renombrada.csv", stringsAsFactors = FALSE)

# ---- 2. Variables -----------------------------------------
datos_raw <- datos_raw %>%
  mutate(
    SEXO_lbl   = factor(SEXO, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    ZONA_lbl   = factor(ZONA, levels = c(1, 2), labels = c("Cabecera", "Resto (rural)")),
    riesgo_lbl = factor(riesgo_laboral, levels = c(0, 1),
                         labels = c("Ocupado formal", "Desempleo/Informalidad"))
  )

# Base completa (para exploracion descriptiva, incluye NAs en Y)
datos_completos <- datos_raw

# Base limpia para estimacion (replica la logica de na.omit del script original)
vars_modelo <- c("riesgo_laboral", "EDAD", "SEXO", "NIVEL_EDUCATIVO",
                  "EXPERIENCIA_MESES", "ESTRATO", "ZONA")

datos <- datos_raw %>%
  select(all_of(vars_modelo), SEXO_lbl, ZONA_lbl, riesgo_lbl) %>%
  na.omit()

n_total_geih   <- nrow(datos_raw)
n_con_Y        <- sum(!is.na(datos_raw$riesgo_laboral))
n_modelo       <- nrow(datos)

# ---- 3. Particion entrenamiento / prueba (reproducible, semilla fija) -------
set.seed(2026)
indice_entrenamiento <- createDataPartition(datos$riesgo_laboral, p = 0.7, list = FALSE)
datos_train <- datos[indice_entrenamiento, ]
datos_test  <- datos[-indice_entrenamiento, ]

# ---- 4. Estimacion de los tres modelos binarios -----------------------------
form_modelo <- riesgo_laboral ~ EDAD + SEXO + NIVEL_EDUCATIVO +
  EXPERIENCIA_MESES + ESTRATO + ZONA

modelo_lpm    <- lm(form_modelo, data = datos)
modelo_logit  <- glm(form_modelo, family = binomial(link = "logit"),  data = datos)
modelo_probit <- glm(form_modelo, family = binomial(link = "probit"), data = datos)

# Errores robustos HC1 para LPM (heterocedasticidad inherente al modelo lineal
vcov_lpm_hc1 <- vcovHC(modelo_lpm, type = "HC1")
coef_lpm_robusto <- coeftest(modelo_lpm, vcov = vcov_lpm_hc1)

# Efectos marginales promedio (APE) - Logit y Probit
ape_logit  <- summary(margins(modelo_logit))
ape_probit <- summary(margins(modelo_probit))

# ---- 5. Modelo predictivo (entrenado solo con datos_train) ------------------
modelo_predictivo <- glm(form_modelo, family = binomial(link = "logit"), data = datos_train)

prob_pred_test <- predict(modelo_predictivo, newdata = datos_test, type = "response")
curva_roc      <- roc(datos_test$riesgo_laboral, prob_pred_test, quiet = TRUE)
auc_valor      <- as.numeric(auc(curva_roc))

# Predicciones sobre el universo completo de datos limpios (para priorizacion)
datos$prob_riesgo <- predict(modelo_predictivo, newdata = datos, type = "response")

# ---- 6. Funcion para metricas de clasificacion segun umbral -----------------
calcular_metricas <- function(probs, real, umbral) {
  pred <- factor(ifelse(probs >= umbral, 1, 0), levels = c(0, 1))
  real <- factor(real, levels = c(0, 1))
  cm <- confusionMatrix(pred, real, positive = "1")
  list(
    cm = cm,
    accuracy    = unname(cm$overall["Accuracy"]),
    sensibilidad = unname(cm$byClass["Sensitivity"]),
    especificidad = unname(cm$byClass["Specificity"]),
    precision   = unname(cm$byClass["Precision"]),
    f1          = unname(cm$byClass["F1"])
  )
}

# Niveles de educacion
niveles_educativos <- c(
  "Ninguno" = 1, "Preescolar" = 2, "Basica primaria" = 3, "Basica secundaria" = 4,
  "Media academica o tecnica" = 5, "Normalista" = 6, "Tecnico/Tecnologico sin titulo" = 7,
  "Tecnico/Tecnologico titulado" = 8, "Universitario sin titulo" = 9,
  "Universitario titulado" = 10, "Especializacion" = 11, "Maestria" = 12, "Doctorado" = 13
)

# ---- 7. Mapa territorial (tasa de riesgo laboral por departamento) ---------
tabla_dpto <- data.frame(
  REGION = c(5, 8, 11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 50,
             52, 54, 63, 66, 68, 70, 73, 76, 81, 85, 86, 88, 91, 94, 95, 97, 99),
  departamento = c(
    "Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas",
    "Caqueta", "Cauca", "Cesar", "Cordoba", "Cundinamarca", "Choco",
    "Huila", "La Guajira", "Magdalena", "Meta", "Nari\u00f1o",
    "Norte de Santander", "Quindio", "Risaralda", "Santander", "Sucre",
    "Tolima", "Valle del Cauca", "Arauca", "Casanare", "Putumayo",
    "San Andres y Providencia", "Amazonas", "Guainia", "Guaviare",
    "Vaupes", "Vichada"
  ),
  stringsAsFactors = FALSE
)

# Tasa de riesgo laboral observada por departamento
tasa_por_dpto <- datos_raw %>%
  filter(!is.na(riesgo_laboral)) %>%
  group_by(REGION) %>%
  summarise(
    tasa_riesgo = mean(riesgo_laboral, na.rm = TRUE),
    n_jovenes   = n(),
    .groups = "drop"
  ) %>%
  left_join(tabla_dpto, by = "REGION") %>%
  arrange(desc(tasa_riesgo))

# Mapas de departamentos de Colombia (paquete geodata, datos GADM).
directorio_mapas <- file.path(tempdir(), "gadm_colombia")
if (!dir.exists(directorio_mapas)) dir.create(directorio_mapas, recursive = TRUE)

mapa_colombia_terra <- gadm(country = "COL", level = 1, path = directorio_mapas, resolution = 2)
mapa_colombia <- sf::st_as_sf(mapa_colombia_terra)

quitar_tildes <- function(x) {
  pares <- list(
    c("\u00e1", "a"), c("\u00e9", "e"), c("\u00ed", "i"), c("\u00f3", "o"), c("\u00fa", "u"),
    c("\u00c1", "A"), c("\u00c9", "E"), c("\u00cd", "I"), c("\u00d3", "O"), c("\u00da", "U"),
    c("\u00f1", "n"), c("\u00d1", "N")
  )
  for (p in pares) x <- gsub(p[1], p[2], x, fixed = TRUE)
  toupper(trimws(x))
}

# GADM nombra la columna de departamento como NAME_1
mapa_colombia$nombre_dpto_norm <- quitar_tildes(mapa_colombia$NAME_1)
tabla_dpto$nombre_dpto_norm    <- quitar_tildes(tabla_dpto$departamento)

mapa_colombia <- mapa_colombia %>%
  left_join(tabla_dpto %>% select(REGION, nombre_dpto_norm), by = "nombre_dpto_norm")

mapa_riesgo_dpto <- mapa_colombia %>%
  left_join(tasa_por_dpto %>% select(REGION, tasa_riesgo, n_jovenes), by = "REGION")

# Verificacion nombre departamentos
dptos_sin_match <- tabla_dpto$departamento[!tabla_dpto$REGION %in% mapa_colombia$REGION]
if (length(dptos_sin_match) > 0) {
  message(
    "Aviso: los siguientes departamentos no encontraron geometria en GADM ",
    "(revisar nombres en tabla_dpto vs mapa_colombia$NAME_1): ",
    paste(dptos_sin_match, collapse = ", ")
  )
}

