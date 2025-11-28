# ================================================================
# PRONÓSTICOS DE LA TASA DE DESOCUPACIÓN EN MÉXICO
# Usando ARIMA, ARIMAX y VAR/SVAR
# - Desempleo (INEGI, XML ya descargado, indicador 444603)
# - TIIE 28 y FIX (Banxico)
# - INDPRO (EE.UU., FRED)
# ================================================================

# ---------------------------
# 0. Paquetes (instalar si faltan y cargar)
# ---------------------------
required_packages <- c(
  "xml2", "httr", "jsonlite",
  "dplyr", "tidyr", "lubridate",
  "forecast", "vars", "svars",
  "ggplot2", "tibble"
)

missing_pkgs <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---------------------------
# 1. Tokens y parámetros
# ---------------------------

BANXICO_TOKEN <- "81aaa3d53984f47f9fa77269d0a8d2b6963ea2698fdfd00a0bf5fb3475678ad4"
FRED_KEY      <- "77a5ed3808f596778c2456c4f21f3e21"

# Ruta a tu XML de INEGI (AJÚSTALA)
# por ejemplo: "C:/Users/Donovan/Downloads/unemrt.xml"
xml_path <- "unemrt.xml"

start_date <- as.Date("2005-01-01")
end_date   <- as.Date("2025-10-31")

# ================================================================
# 2. FUNCIONES AUXILIARES (Banxico y FRED)
# ================================================================

# 2.1 Banxico: descarga UNA serie y regresa tibble(date, value)
# ------------------------------------------------
# Banxico: descargar UNA serie y regresarla como
# tibble(date, value)
# ------------------------------------------------
get_banxico_series <- function(id_series,
                               start_date,
                               end_date,
                               token = BANXICO_TOKEN) {
  start_str <- format(as.Date(start_date), "%Y-%m-%d")
  end_str   <- format(as.Date(end_date), "%Y-%m-%d")
  
  url <- paste0(
    "https://www.banxico.org.mx/SieAPIRest/service/v1/series/",
    id_series,
    "/datos/",
    start_str, "/",
    end_str,
    "/?token=",
    token
  )
  
  res <- httr::GET(url)
  sc  <- httr::status_code(res)
  
  if (sc == 401) {
    stop(
      "Error 401 (Unauthorized) en Banxico. Revisa tu BANXICO_TOKEN.",
      call. = FALSE
    )
  } else if (sc >= 400) {
    stop(paste0("Error HTTP ", sc, " al consultar Banxico. URL: ", url),
         call. = FALSE)
  }
  
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  
  # 👇 clave: NO simplificar a data.frames, queremos una lista anidada
  j <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  
  # Comprobaciones básicas
  if (is.null(j$bmx) || is.null(j$bmx$series)) {
    stop("La respuesta de Banxico no tiene el nodo 'bmx$series'. Contenido parcial:\n",
         substr(txt, 1, 500),
         call. = FALSE)
  }
  
  # Primera serie (en general solo hay una)
  serie <- j$bmx$series[[1]]
  
  if (is.null(serie$datos)) {
    stop("La serie de Banxico no contiene el nodo 'datos'. Contenido parcial:\n",
         substr(txt, 1, 500),
         call. = FALSE)
  }
  
  datos_list <- serie$datos
  
  # Extraemos fechas y datos de la lista de observaciones
  fechas_chr <- vapply(datos_list, function(x) x$fecha, character(1))
  valores_chr <- vapply(datos_list, function(x) x$dato, character(1))
  
  df <- tibble::tibble(
    date  = as.Date(fechas_chr, format = "%d/%m/%Y"),
    value = as.numeric(valores_chr)
  ) %>%
    dplyr::arrange(date)
  
  return(df)
}

# 2.2 FRED: INDPRO mensual → tibble(date, value)
get_fred_series <- function(series_id,
                            start_date,
                            end_date,
                            fred_key = FRED_KEY) {
  start_str <- format(as.Date(start_date), "%Y-%m-%d")
  end_str   <- format(as.Date(end_date), "%Y-%m-%d")
  
  url <- paste0(
    "https://api.stlouisfed.org/fred/series/observations?",
    "series_id=", series_id,
    "&observation_start=", start_str,
    "&observation_end=", end_str,
    "&frequency=m",
    "&api_key=", fred_key,
    "&file_type=json"
  )
  
  res <- httr::GET(url)
  sc  <- httr::status_code(res)
  
  if (sc == 401) {
    stop(
      "Error 401 (Unauthorized) en FRED. Revisa tu FRED_KEY.",
      call. = FALSE
    )
  } else if (sc >= 400) {
    stop(paste0("Error HTTP ", sc, " al consultar FRED. URL: ", url),
         call. = FALSE)
  }
  
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  j   <- jsonlite::fromJSON(txt)
  
  obs <- j$observations
  
  tibble(
    date  = as.Date(obs$date),
    value = suppressWarnings(as.numeric(obs$value))
  ) %>%
    filter(!is.na(value)) %>%
    arrange(date)
}

# ================================================================
# 3. LEER EL XML DE INEGI (DESOCUPACIÓN, INDICADOR 444603)
# ================================================================

# xml_path debe apuntar al archivo que contiene ese XML enorme que pegaste
xml_doc <- xml2::read_xml(xml_path)

# Quitamos namespaces para hacerlo sencillo: luego podemos usar nombres simples
xml2::xml_ns_strip(xml_doc)

# Obtenemos todos los nodos <Observation>
obs_nodes <- xml2::xml_find_all(xml_doc, ".//Observation")

# De cada Observation sacamos TIME_PERIOD y OBS_VALUE
time_period <- xml2::xml_text(
  xml2::xml_find_all(obs_nodes, "./TIME_PERIOD")
)
values <- xml2::xml_text(
  xml2::xml_find_all(obs_nodes, "./OBS_VALUE")
)

# Convertimos TIME_PERIOD "YYYY/MM" a Date "YYYY-MM-01"
dates <- as.Date(
  paste0(gsub("/", "-", time_period), "-01")
)

unemp_mx <- tibble(
  date      = dates,
  unemp_mx  = as.numeric(values)
) %>%
  arrange(date) %>%
  filter(date >= start_date, date <= end_date)

print(head(unemp_mx))
print(tail(unemp_mx))

# ================================================================
# 4. DESCARGAR TIIE 28, FIX (BANXICO) e INDPRO (FRED)
# ================================================================

# 4.1 TIIE 28 días (SF60648) y FIX (SF43718) diarios
tiie_28_daily <- get_banxico_series(
  id_series  = "SF60648",
  start_date = start_date,
  end_date   = end_date
) %>%
  rename(tiie_28 = value)

fix_daily <- get_banxico_series(
  id_series  = "SF43718",
  start_date = start_date,
  end_date   = end_date
) %>%
  rename(fix_mx = value)

# 4.2 Promedio mensual de TIIE y FIX
tiie_28_m <- tiie_28_daily %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    tiie_28 = mean(tiie_28, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date = month)

fix_mx_m <- fix_daily %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    fix_mx = mean(fix_mx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date = month)

# 4.3 Producción industrial de EE.UU. (INDPRO, FRED)
us_indpro <- get_fred_series(
  series_id  = "INDPRO",
  start_date = start_date,
  end_date   = end_date,
  fred_key   = FRED_KEY
) %>%
  rename(us_indpro = value)

# ================================================================
# 5. INTEGRAR BASE MENSUAL
# ================================================================

data_all <- unemp_mx %>%
  inner_join(tiie_28_m, by = "date") %>%
  inner_join(fix_mx_m, by = "date") %>%
  inner_join(us_indpro, by = "date") %>%
  arrange(date)

# Para modelar, trabajamos con observaciones sin NA
data_model <- data_all %>%
  drop_na()

print(head(data_model))
print(tail(data_model))

# ================================================================
# 6. OBJETOS ts
# ================================================================

start_year  <- lubridate::year(min(data_model$date))
start_month <- lubridate::month(min(data_model$date))

unemp_ts <- ts(
  data_model$unemp_mx,
  start     = c(start_year, start_month),
  frequency = 12
)

xreg_mat <- as.matrix(
  dplyr::select(data_model, tiie_28, fix_mx, us_indpro)
)

var_data <- dplyr::select(
  data_model,
  us_indpro, tiie_28, fix_mx, unemp_mx
)

h <- 12           # horizonte de pronóstico
set.seed(123)

# ================================================================
# 7. MODELO ARIMA (UNIVARIANTE)
# ================================================================

fit_arima <- forecast::auto.arima(
  unemp_ts,
  seasonal      = FALSE,  # asumiendo serie ya desestacionalizada
  stepwise      = FALSE,
  approximation = FALSE
)

print(fit_arima)

fc_arima <- forecast::forecast(fit_arima, h = h)

autoplot(fc_arima) +
  ggplot2::ggtitle("Pronóstico ARIMA - Tasa de desocupación") +
  ggplot2::xlab("Tiempo") +
  ggplot2::ylab("Porcentaje")

# ================================================================
# 8. MODELO ARIMAX (CON TIIE, FIX, INDPRO)
# ================================================================

fit_arimax <- forecast::auto.arima(
  unemp_ts,
  xreg         = xreg_mat,
  seasonal     = FALSE,
  stepwise     = FALSE,
  approximation = FALSE
)

print(fit_arimax)

# Escenario sencillo: mantener regresores constantes en su último valor
last_xreg <- xreg_mat[nrow(xreg_mat), , drop = FALSE]

xreg_future <- matrix(
  rep(as.numeric(last_xreg), each = h),
  nrow = h,
  byrow = TRUE
)
colnames(xreg_future) <- colnames(xreg_mat)

fc_arimax <- forecast::forecast(
  fit_arimax,
  h    = h,
  xreg = xreg_future
)

autoplot(fc_arimax) +
  ggplot2::ggtitle("Pronóstico ARIMAX - Tasa de desocupación\n(Regresores constantes en último valor)") +
  ggplot2::xlab("Tiempo") +
  ggplot2::ylab("Porcentaje")

# ================================================================
# 9. VAR y SVAR
# ================================================================

# Orden económico: us_indpro → tiie_28 → fix_mx → unemp_mx
lag_sel <- vars::VARselect(
  var_data,
  lag.max = 12,
  type    = "const"
)
print(lag_sel$criteria)

p_opt <- as.integer(lag_sel$selection["AIC(n)"])

var_fit <- vars::VAR(
  y    = var_data,
  p    = p_opt,
  type = "const"
)

summary(var_fit)

# SVAR con descomposición de Cholesky (mismo orden que columnas)
svar_fit <- svars::id.chol(var_fit)
svar_fit

# Pronóstico del VAR (la dinámica estructural se basa en éste)
fc_var <- predict(var_fit, n.ahead = h)

# Pronóstico de desempleo (última variable en var_data)
fc_unemp <- fc_var$fcst$unemp_mx[, "fcst"]

# Construimos ts del pronóstico VAR
last_time  <- time(unemp_ts)[length(unemp_ts)]
last_year  <- floor(last_time)
last_month <- round(12 * (last_time - last_year) + 1e-6)

fc_start_year  <- ifelse(last_month == 12, last_year + 1, last_year)
fc_start_month <- ifelse(last_month == 12, 1, last_month + 1)

unemp_var_ts <- ts(
  fc_unemp,
  start     = c(fc_start_year, fc_start_month),
  frequency = 12
)

time_all <- c(time(unemp_ts), time(unemp_var_ts))

plot(
  unemp_ts,
  main = "Tasa de desocupación: observada y pronóstico VAR/SVAR",
  xlab = "Tiempo",
  ylab = "Porcentaje",
  xlim = range(time_all)
)
lines(unemp_var_ts, col = "blue", lty = 2)
abline(v = last_year + (last_month - 1) / 12, col = "red", lty = 2)
legend(
  "topleft",
  legend = c("Observada", "Pronóstico VAR/SVAR", "Inicio del pronóstico"),
  col    = c("black", "blue", "red"),
  lty    = c(1, 2, 2),
  bty    = "n"
)
# ================================================================
# FIN
# ================================================================


# ------------------------------
# PRONÓSTICO OCTUBRE 2025 (ARIMA)
# ------------------------------
oct_2025_arima <- window(
  fc_arima$mean,
  start = c(2025, 10),
  end   = c(2025, 10)
)
oct_2025_arima


# ARIMAX
oct_2025_arimax <- window(
  fc_arimax$mean,
  start = c(2025, 10),
  end   = c(2025, 10)
)
oct_2025_arimax

# VAR/SVAR (usa el objeto unemp_var_ts que construiste)
oct_2025_var <- window(
  unemp_var_ts,
  start = c(2025, 10),
  end   = c(2025, 10)
)
oct_2025_var

# ================================================================
# 10. EVALUACIÓN OOS: RMSE EN LOS ÚLTIMOS 6 MESES
# ================================================================

library(dplyr)

# --- 10.1 Definir ventana de evaluación (últimos 6 meses) ---

h_test   <- 6
n_total  <- nrow(data_model)

test_idx  <- (n_total - h_test + 1):n_total
train_idx <- 1:(n_total - h_test)

data_train <- data_model[train_idx, ]
data_test  <- data_model[test_idx, ]

# Por si quieres ver qué meses entran al test:
data_test$date

# --- 10.2 Serie ts de desempleo: train y test ---

start_year  <- lubridate::year(min(data_model$date))
start_month <- lubridate::month(min(data_model$date))

unemp_ts_train <- ts(
  data_train$unemp_mx,
  start     = c(start_year, start_month),
  frequency = 12
)

# Matrices de regresores (ARIMAX / VAR)
xreg_all   <- as.matrix(dplyr::select(data_model, tiie_28, fix_mx, us_indpro))
xreg_train <- xreg_all[train_idx, , drop = FALSE]
xreg_test  <- xreg_all[test_idx,  , drop = FALSE]

var_data_all   <- dplyr::select(data_model, us_indpro, tiie_28, fix_mx, unemp_mx)
var_data_train <- var_data_all[train_idx, ]
var_data_test  <- var_data_all[test_idx, ]

# --- 10.3 Función de RMSE ---

rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2, na.rm = TRUE))
}

y_test <- data_test$unemp_mx  # valores reales de desempleo en los últimos 6 meses

# --- 10.4 ARIMA (univariante) ---

fit_arima_train <- forecast::auto.arima(
  unemp_ts_train,
  seasonal      = FALSE,
  stepwise      = FALSE,
  approximation = FALSE
)

fc_arima_train <- forecast::forecast(fit_arima_train, h = h_test)
pred_arima     <- as.numeric(fc_arima_train$mean)

rmse_arima <- rmse(y_test, pred_arima)

# --- 10.5 ARIMAX (con TIIE, FIX, INDPRO) ---

fit_arimax_train <- forecast::auto.arima(
  unemp_ts_train,
  xreg         = xreg_train,
  seasonal     = FALSE,
  stepwise     = FALSE,
  approximation = FALSE
)

fc_arimax_train <- forecast::forecast(
  fit_arimax_train,
  h    = h_test,
  xreg = xreg_test
)
pred_arimax <- as.numeric(fc_arimax_train$mean)

rmse_arimax <- rmse(y_test, pred_arimax)

# --- 10.6 VAR (y SVAR) ---

# Seleccionamos rezago óptimo SOLO con datos de entrenamiento
lag_sel_train <- vars::VARselect(
  var_data_train,
  lag.max = 12,
  type    = "const"
)

p_opt_train <- as.integer(lag_sel_train$selection["AIC(n)"])

var_fit_train <- vars::VAR(
  y    = var_data_train,
  p    = p_opt_train,
  type = "const"
)

fc_var_train <- predict(var_fit_train, n.ahead = h_test)

# Pronósticos de desempleo (última variable del sistema)
pred_var <- fc_var_train$fcst$unemp_mx[, "fcst"]

rmse_var <- rmse(y_test, pred_var)

# SVAR con descomposición de Cholesky
svar_fit_train <- svars::id.chol(var_fit_train)

# IMPORTANTE: el pronóstico puntual del SVAR es el mismo que el del VAR
pred_svar <- pred_var
rmse_svar <- rmse(y_test, pred_svar)

# --- 10.7 Tabla resumen de resultados ---

rmse_tbl <- tibble::tibble(
  modelo = c("ARIMA", "ARIMAX", "VAR", "SVAR"),
  RMSE   = c(rmse_arima, rmse_arimax, rmse_var, rmse_svar)
)

print(rmse_tbl)

# (Opcional) ver también pronósticos vs realidad mes a mes
resultados_oos <- tibble::tibble(
  date        = data_test$date,
  unemp_real  = y_test,
  arima_hat   = pred_arima,
  arimax_hat  = pred_arimax,
  var_hat     = pred_var,
  svar_hat    = pred_svar
)

print(resultados_oos)



# Supongo que tu tabla se llama 'resultados_oos', con estas columnas:
# date, unemp_real, arima_hat, arimax_hat, var_hat, svar_hat

library(dplyr)

rmse <- function(y, yhat) sqrt(mean((yhat - y)^2))

tabla_final <- resultados_oos %>%
  # paso a character la fecha para poder agregar una fila "RMSE ..."
  mutate(date = as.character(date)) %>%
  bind_rows(
    tibble(
      date       = "RMSE (2025-03 a 2025-08)",
      unemp_real = NA_real_,
      arima_hat  = rmse(resultados_oos$unemp_real, resultados_oos$arima_hat),
      arimax_hat = rmse(resultados_oos$unemp_real, resultados_oos$arimax_hat),
      var_hat    = rmse(resultados_oos$unemp_real, resultados_oos$var_hat),
      svar_hat   = rmse(resultados_oos$unemp_real, resultados_oos$svar_hat)
    )
  )

tabla_final


library(dplyr)
library(lubridate)
library(tibble)

# ================================================================
# 1. Fila de RMSE de los últimos 6 meses (mar–ago 2025)
# ================================================================
rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2))

rmse_row <- tibble(
  date       = "RMSE (2025-03 a 2025-08)",  # texto -> character
  unemp_real = NA_real_,
  arima_hat  = rmse(resultados_oos$unemp_real, resultados_oos$arima_hat),
  arimax_hat = rmse(resultados_oos$unemp_real, resultados_oos$arimax_hat),
  var_hat    = rmse(resultados_oos$unemp_real, resultados_oos$var_hat),
  svar_hat   = rmse(resultados_oos$unemp_real, resultados_oos$svar_hat)
)

# ================================================================
# 2. Pronóstico para octubre 2025 de cada modelo
#    (ARIMA, ARIMAX, VAR y SVAR)
#    Usa: data_model, fc_arima, fc_arimax, fc_var, h
# ================================================================
target_date <- as.Date("2025-10-01")

last_date <- max(data_model$date)

future_dates <- seq(
  from = last_date %m+% months(1),  # 1 mes después del último dato
  by   = "1 month",
  length.out = h                    # h ya definido (por ejemplo 12)
)

pron_future <- tibble(
  date       = future_dates,
  arima_hat  = as.numeric(fc_arima$mean),
  arimax_hat = as.numeric(fc_arimax$mean),
  var_hat    = as.numeric(fc_var$fcst$unemp_mx[, "fcst"]),
  svar_hat   = var_hat  # mismo pronóstico que VAR
)

pron_oct <- pron_future %>%
  dplyr::filter(date == target_date) %>%
  dplyr::mutate(unemp_real = NA_real_) %>%
  dplyr::select(date, unemp_real, arima_hat, arimax_hat, var_hat, svar_hat)

# ================================================================
# 3. Tabla final: 6 observaciones OOS + pronóstico octubre + RMSE
# ================================================================
tabla_final <- resultados_oos %>%
  dplyr::mutate(date = as.character(date)) %>%                       # Date -> character
  dplyr::bind_rows(pron_oct %>% dplyr::mutate(date = as.character(date))) %>%  # Oct 2025
  dplyr::bind_rows(rmse_row)                                         # fila RMSE

print(tabla_final)


library(dplyr)
library(lubridate)
library(tibble)

# ================================================================
# 1. Función RMSE
# ================================================================
rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2))

# ================================================================
# 2. Ventana de evaluación: ABRIL–SEPTIEMBRE 2025
#    (solo para calcular el RMSE)
# ================================================================
eval_rmse <- resultados_oos %>%
  dplyr::filter(
    date >= as.Date("2025-04-01"),
    date <= as.Date("2025-09-01")
  )

rmse_row <- tibble(
  date       = "RMSE (2025-04 a 2025-09)",  # <- etiqueta corregida
  unemp_real = NA_real_,
  arima_hat  = rmse(eval_rmse$unemp_real, eval_rmse$arima_hat),
  arimax_hat = rmse(eval_rmse$unemp_real, eval_rmse$arimax_hat),
  var_hat    = rmse(eval_rmse$unemp_real, eval_rmse$var_hat),
  svar_hat   = rmse(eval_rmse$unemp_real, eval_rmse$svar_hat)
)

# ================================================================
# 3. Pronóstico para octubre 2025 de cada modelo
#    (ARIMA, ARIMAX, VAR y SVAR)
#    Usa: data_model, fc_arima, fc_arimax, fc_var, h
# ================================================================
target_date <- as.Date("2025-10-01")

last_date <- max(data_model$date)

future_dates <- seq(
  from = last_date %m+% months(1),  # 1 mes después del último dato
  by   = "1 month",
  length.out = h                    # h ya definido (p.ej. 12)
)

pron_future <- tibble(
  date       = future_dates,
  arima_hat  = as.numeric(fc_arima$mean),
  arimax_hat = as.numeric(fc_arimax$mean),
  var_hat    = as.numeric(fc_var$fcst$unemp_mx[, "fcst"]),
  svar_hat   = var_hat  # mismo pronóstico que VAR
)

pron_oct <- pron_future %>%
  dplyr::filter(date == target_date) %>%
  dplyr::mutate(unemp_real = NA_real_) %>%
  dplyr::select(date, unemp_real, arima_hat, arimax_hat, var_hat, svar_hat)

# ================================================================
# 4. Tabla final: OOS + pronóstico octubre + fila de RMSE
#    (si quieres que la tabla también sea solo abril–septiembre,
#     puedes filtrar resultados_oos antes de este paso)
# ================================================================
tabla_final <- resultados_oos %>%
  dplyr::mutate(date = as.character(date)) %>%                       
  dplyr::bind_rows(pron_oct %>% dplyr::mutate(date = as.character(date))) %>%  
  dplyr::bind_rows(rmse_row)                                         

print(tabla_final)



library(dplyr)
library(tibble)

# ================================================================
# 1. Función RMSE
# ================================================================
rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2))

# ================================================================
# 2. Ventana de evaluación: ABRIL–SEPTIEMBRE 2025
#    resultados_oos YA solo tiene abril–septiembre
# ================================================================
eval_rmse <- resultados_oos  # <- sin filtros de fecha

rmse_row <- tibble(
  date       = "RMSE (2025-04 a 2025-09)",  # etiqueta de la fila
  unemp_real = NA_real_,
  arima_hat  = rmse(eval_rmse$unemp_real, eval_rmse$arima_hat),
  arimax_hat = rmse(eval_rmse$unemp_real, eval_rmse$arimax_hat),
  var_hat    = rmse(eval_rmse$unemp_real, eval_rmse$var_hat),
  svar_hat   = rmse(eval_rmse$unemp_real, eval_rmse$svar_hat)
)

library(dplyr)
library(lubridate)
library(tibble)

# ================================================================
# 1. Función RMSE
# ================================================================
rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2))

# ================================================================
# 2. Ventana de evaluación: ABRIL–SEPTIEMBRE 2025
#    (solo para calcular el RMSE)
# ================================================================
eval_rmse <- resultados_oos %>%
  dplyr::filter(
    date >= as.Date("2025-04-01"),
    date <= as.Date("2025-09-01")
  )

rmse_row <- tibble(
  date       = "RMSE (2025-04 a 2025-09)",  # <- etiqueta corregida
  unemp_real = NA_real_,
  arima_hat  = rmse(eval_rmse$unemp_real, eval_rmse$arima_hat),
  arimax_hat = rmse(eval_rmse$unemp_real, eval_rmse$arimax_hat),
  var_hat    = rmse(eval_rmse$unemp_real, eval_rmse$var_hat),
  svar_hat   = rmse(eval_rmse$unemp_real, eval_rmse$svar_hat)
)

# ================================================================
# 3. Pronóstico para octubre 2025 de cada modelo
#    (ARIMA, ARIMAX, VAR y SVAR)
#    Usa: data_model, fc_arima, fc_arimax, fc_var, h
# ================================================================
target_date <- as.Date("2025-10-01")

last_date <- max(data_model$date)

future_dates <- seq(
  from = last_date %m+% months(1),  # 1 mes después del último dato
  by   = "1 month",
  length.out = h                    # h ya definido (p.ej. 12)
)

pron_future <- tibble(
  date       = future_dates,
  arima_hat  = as.numeric(fc_arima$mean),
  arimax_hat = as.numeric(fc_arimax$mean),
  var_hat    = as.numeric(fc_var$fcst$unemp_mx[, "fcst"]),
  svar_hat   = var_hat  # mismo pronóstico que VAR
)

pron_oct <- pron_future %>%
  dplyr::filter(date == target_date) %>%
  dplyr::mutate(unemp_real = NA_real_) %>%
  dplyr::select(date, unemp_real, arima_hat, arimax_hat, var_hat, svar_hat)

# ================================================================
# 4. Tabla final: OOS + pronóstico octubre + fila de RMSE
#    (si quieres que la tabla también sea solo abril–septiembre,
#     puedes filtrar resultados_oos antes de este paso)
# ================================================================
tabla_final <- resultados_oos %>%
  dplyr::mutate(date = as.character(date)) %>%                       
  dplyr::bind_rows(pron_oct %>% dplyr::mutate(date = as.character(date))) %>%  
  dplyr::bind_rows(rmse_row)                                         

print(tabla_final)


# ================================================================
# FIGURA 1: trayectoria de la tasa de desocupación
# ================================================================
p_desemp_ts <- ggplot(data_model, aes(x = date, y = unemp_mx)) +
  geom_line() +
  labs(
    title = "Tasa de desocupación en México",
    x     = "Fecha",
    y     = "Porcentaje de la PEA"
  ) +
  theme_minimal()

ggplot2::ggsave(
  filename = "figs/desocupacion_ts.pdf",
  plot     = p_desemp_ts,
  width    = 7,
  height   = 4
)

# ================================================================
# FIGURA 2: pronósticos recientes por modelo (h pasos adelante)
# ================================================================
# Construimos una base larga con pronósticos para h meses adelante
future_dates <- time(unemp_var_ts)  # mismas fechas que usaste para VAR

df_pron <- tibble::tibble(
  date       = as.Date(as.yearmon(future_dates)),
  arima_hat  = as.numeric(fc_arima$mean),
  arimax_hat = as.numeric(fc_arimax$mean),
  var_hat    = as.numeric(unemp_var_ts)
) %>%
  tidyr::pivot_longer(
    cols      = c(arima_hat, arimax_hat, var_hat),
    names_to  = "modelo",
    values_to = "pronostico"
  ) %>%
  dplyr::mutate(
    modelo = dplyr::recode(
      modelo,
      "arima_hat"  = "ARIMA",
      "arimax_hat" = "ARIMAX",
      "var_hat"    = "VAR/SVAR"
    )
  )

p_pron <- ggplot(df_pron, aes(x = date, y = pronostico, colour = modelo)) +
  geom_line() +
  labs(
    title = "Pronósticos de la tasa de desocupación",
    x     = "Fecha",
    y     = "Porcentaje"
  ) +
  theme_minimal()

ggplot2::ggsave(
  filename = "figs/pronosticos_modelos.pdf",
  plot     = p_pron,
  width    = 7,
  height   = 4
)

# ================================================================
# 10. Rolling OOS: bucle sobre t y horizonte h
# ================================================================

rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2, na.rm = TRUE))

rolling_oos <- function(data, h_max = 6, min_train = 120) {
  # data: debe contener columnas date, unemp_mx, tiie_28, fix_mx, us_indpro
  
  stopifnot(all(c("date", "unemp_mx", "tiie_28", "fix_mx", "us_indpro") %in% names(data)))
  
  n <- nrow(data)
  if (n <= min_train + h_max) {
    stop("La muestra es muy corta para el esquema rolling con esos parámetros.")
  }
  
  res_list <- list()
  
  start_year  <- lubridate::year(min(data$date))
  start_month <- lubridate::month(min(data$date))
  
  # Matriz de regresores fija para todos los t
  xreg_all <- as.matrix(dplyr::select(data, tiie_28, fix_mx, us_indpro))
  
  for (t in seq(min_train, n - h_max)) {
    train_idx <- 1:t
    test_idx  <- (t + 1):(t + h_max)
    
    # Serie ts de desempleo para el periodo de entrenamiento
    unemp_ts_train <- ts(
      data$unemp_mx[train_idx],
      start     = c(start_year, start_month),
      frequency = 12
    )
    
    # Regresores para ARIMAX
    xreg_train <- xreg_all[train_idx, , drop = FALSE]
    xreg_test  <- xreg_all[test_idx,  , drop = FALSE]
    
    # Datos para VAR
    var_data_train <- dplyr::select(
      data[train_idx, ],
      us_indpro, tiie_28, fix_mx, unemp_mx
    )
    
    # --- Estimación de modelos en t ---
    fit_arima_t <- forecast::auto.arima(
      unemp_ts_train,
      seasonal      = FALSE,
      stepwise      = FALSE,
      approximation = FALSE
    )
    
    fit_arimax_t <- forecast::auto.arima(
      unemp_ts_train,
      xreg         = xreg_train,
      seasonal     = FALSE,
      stepwise     = FALSE,
      approximation = FALSE
    )
    
    lag_sel_t <- vars::VARselect(
      var_data_train,
      lag.max = 12,
      type    = "const"
    )
    p_opt_t <- as.integer(lag_sel_t$selection["AIC(n)"])
    
    var_fit_t <- vars::VAR(
      y    = var_data_train,
      p    = p_opt_t,
      type = "const"
    )
    
    # --- Pronósticos h pasos adelante ---
    fc_arima_t  <- forecast::forecast(fit_arima_t,  h = h_max)
    fc_arimax_t <- forecast::forecast(fit_arimax_t, h = h_max, xreg = xreg_test)
    fc_var_t    <- predict(var_fit_t, n.ahead = h_max)
    
    y_test <- data$unemp_mx[test_idx]
    
    for (h in seq_len(h_max)) {
      res_list[[length(res_list) + 1L]] <- tibble::tibble(
        t_origen  = data$date[t],
        horizonte = h,
        date      = data$date[t + h],
        y_real    = y_test[h],
        arima_hat  = as.numeric(fc_arima_t$mean[h]),
        arimax_hat = as.numeric(fc_arimax_t$mean[h]),
        var_hat    = as.numeric(fc_var_t$fcst$unemp_mx[h, "fcst"])
      )
    }
  }
  
  dplyr::bind_rows(res_list)
}

# Ejecutamos el experimento rolling (ejemplo: H = 6, ventana mínima ~10 años)
resultados_rolling <- rolling_oos(
  data    = data_model,
  h_max   = 6,
  min_train = 120   # 120 meses ~ 10 años
)

# ================================================================
# 11. Panel A: RMSE por modelo y horizonte
# ================================================================

panelA_rmse <- resultados_rolling %>%
  tidyr::pivot_longer(
    cols      = c(arima_hat, arimax_hat, var_hat),
    names_to  = "modelo_raw",
    values_to = "y_hat"
  ) %>%
  dplyr::mutate(
    modelo = dplyr::recode(
      modelo_raw,
      "arima_hat"  = "ARIMA",
      "arimax_hat" = "ARIMAX",
      "var_hat"    = "VAR"
    )
  ) %>%
  dplyr::group_by(modelo, horizonte) %>%
  dplyr::summarise(
    RMSE = rmse(y_real, y_hat),
    .groups = "drop"
  ) %>%
  dplyr::arrange(horizonte, modelo)

print(panelA_rmse)

# Exportar a LaTeX (Panel A)
panelA_rmse_tex <- panelA_rmse %>%
  dplyr::mutate(
    RMSE = round(RMSE, 3)
  ) %>%
  tidyr::pivot_wider(
    names_from  = modelo,
    values_from = RMSE
  ) %>%
  knitr::kable(
    format  = "latex",
    booktabs = TRUE,
    caption = "Panel A. RMSE de la tasa de desocupaci\\'on por modelo y horizonte de pron\\'ostico.",
    label   = "panelA_rmse",
    align   = "c"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position   = "center"
  )

kableExtra::save_kable(panelA_rmse_tex, "tex/panelA_rmse.tex")

# ================================================================
# 12. Panel B: OOS reciente + pronóstico octubre 2025 + RMSE
# ================================================================

# Suponemos que ya definiste:
# - h_test (ej. 6)
# - data_train, data_test
# - resultados_oos: tibble con columnas
#   date, unemp_real, arima_hat, arimax_hat, var_hat, svar_hat

# 12.1 RMSE en una ventana reciente (ej. abril–septiembre 2025)
eval_rmse <- resultados_oos %>%
  dplyr::filter(
    date >= as.Date("2025-04-01"),
    date <= as.Date("2025-09-01")
  )

rmse_row <- tibble::tibble(
  date       = "RMSE (2025-04 a 2025-09)",
  unemp_real = NA_real_,
  arima_hat  = rmse(eval_rmse$unemp_real, eval_rmse$arima_hat),
  arimax_hat = rmse(eval_rmse$unemp_real, eval_rmse$arimax_hat),
  var_hat    = rmse(eval_rmse$unemp_real, eval_rmse$var_hat),
  svar_hat   = rmse(eval_rmse$unemp_real, eval_rmse$svar_hat)
)

# 12.2 Pronóstico octubre 2025 por modelo (usando fc_arima, fc_arimax, fc_var)
target_date <- as.Date("2025-10-01")
last_date   <- max(data_model$date)

future_dates <- seq(
  from = last_date %m+% months(1),
  by   = "1 month",
  length.out = h   # h es el horizonte que ya definiste antes (p.ej. 12)
)

pron_future <- tibble::tibble(
  date       = future_dates,
  arima_hat  = as.numeric(fc_arima$mean),
  arimax_hat = as.numeric(fc_arimax$mean),
  var_hat    = as.numeric(fc_var$fcst$unemp_mx[, "fcst"]),
  svar_hat   = var_hat
)

pron_oct <- pron_future %>%
  dplyr::filter(date == target_date) %>%
  dplyr::mutate(unemp_real = NA_real_) %>%
  dplyr::select(date, unemp_real, arima_hat, arimax_hat, var_hat, svar_hat)

# 12.3 Tabla final = OOS reciente + fila de octubre + fila de RMSE
panelB_pronosticos <- resultados_oos %>%
  dplyr::mutate(date = as.character(date)) %>%
  dplyr::bind_rows(pron_oct %>% dplyr::mutate(date = as.character(date))) %>%
  dplyr::bind_rows(rmse_row)

print(panelB_pronosticos)

# 12.4 Exportar Panel B a LaTeX
panelB_tex <- panelB_pronosticos %>%
  dplyr::mutate(
    across(
      .cols = c(unemp_real, arima_hat, arimax_hat, var_hat, svar_hat),
      .fns  = ~ ifelse(is.na(.x), NA, round(.x, 3))
    )
  ) %>%
  knitr::kable(
    format  = "latex",
    booktabs = TRUE,
    caption = "Panel B. Pron\\'osticos de la tasa de desocupaci\\'on (OOS reciente, octubre 2025 y RMSE).",
    label   = "panelB_pronosticos",
    align   = "c"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position   = "center",
    font_size  = 9
  )

kableExtra::save_kable(panelB_tex, "tex/panelB_pronosticos.tex")



# ================================================================
# 13. Trayectorias rolling one-step-ahead (h = 1)
# ================================================================
library(dplyr)
library(ggplot2)

# Nos quedamos con horizonte 1 (one-step-ahead)
traj_h1 <- resultados_rolling %>%
  dplyr::filter(horizonte == 1)

# Si quieres ver solo 2025 completo:
traj_h1_2025 <- traj_h1 %>%
  dplyr::filter(date >= as.Date("2025-01-01"))

gg_traj_h1_2025 <- ggplot(traj_h1_2025, aes(x = date)) +
  geom_line(aes(y = y_real,     colour = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = arima_hat,  colour = "ARIMA"),     linewidth = 0.7) +
  geom_line(aes(y = arimax_hat, colour = "ARIMAX"),    linewidth = 0.7) +
  geom_line(aes(y = var_hat,    colour = "VAR"),       linewidth = 0.7) +
  labs(
    title  = "Pronósticos rolling one-step-ahead de la tasa de desocupación (h = 1)",
    x      = "Fecha",
    y      = "Tasa de desocupación (%)",
    colour = ""
  ) +
  theme_minimal()

# Guardar figura para el reporte
ggplot2::ggsave(
  filename = "figs/pronosticos_rolling_h1_2025.pdf",
  plot     = gg_traj_h1_2025,
  width    = 7,
  height   = 4
)

# (Opcional) solo la ventana abril–septiembre 2025
traj_h1_abr_sep <- traj_h1 %>%
  dplyr::filter(
    date >= as.Date("2025-04-01"),
    date <= as.Date("2025-09-01")
  )

gg_traj_h1_abr_sep <- ggplot(traj_h1_abr_sep, aes(x = date)) +
  geom_line(aes(y = y_real,     colour = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = arima_hat,  colour = "ARIMA"),     linewidth = 0.7) +
  geom_line(aes(y = arimax_hat, colour = "ARIMAX"),    linewidth = 0.7) +
  geom_line(aes(y = var_hat,    colour = "VAR"),       linewidth = 0.7) +
  labs(
    title  = "Pronósticos rolling one-step-ahead (abril–septiembre 2025)",
    x      = "Fecha",
    y      = "Tasa de desocupación (%)",
    colour = ""
  ) +
  theme_minimal()

ggplot2::ggsave(
  filename = "figs/pronosticos_rolling_h1_abr_sep_2025.pdf",
  plot     = gg_traj_h1_abr_sep,
  width    = 7,
  height   = 4
)


# ================================================================
# 13. Trayectorias rolling one-step-ahead (h = 1)
# ================================================================
library(dplyr)
library(ggplot2)

# Nos quedamos con horizonte 1 (one-step-ahead)
traj_h1 <- resultados_rolling %>%
  dplyr::filter(horizonte == 1)

# Si quieres ver solo 2025 completo:
traj_h1_2025 <- traj_h1 %>%
  dplyr::filter(date >= as.Date("2025-01-01"))

gg_traj_h1_2025 <- ggplot(traj_h1_2025, aes(x = date)) +
  geom_line(aes(y = y_real,     colour = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = arima_hat,  colour = "ARIMA"),     linewidth = 0.7) +
  geom_line(aes(y = arimax_hat, colour = "ARIMAX"),    linewidth = 0.7) +
  geom_line(aes(y = var_hat,    colour = "VAR"),       linewidth = 0.7) +
  labs(
    title  = "Pronósticos rolling one-step-ahead de la tasa de desocupación (h = 1)",
    x      = "Fecha",
    y      = "Tasa de desocupación (%)",
    colour = ""
  ) +
  theme_minimal()

# Guardar figura para el reporte
ggplot2::ggsave(
  filename = "figs/pronosticos_rolling_h1_2025.pdf",
  plot     = gg_traj_h1_2025,
  width    = 7,
  height   = 4
)

# (Opcional) solo la ventana abril–septiembre 2025
traj_h1_abr_sep <- traj_h1 %>%
  dplyr::filter(
    date >= as.Date("2025-04-01"),
    date <= as.Date("2025-09-01")
  )

gg_traj_h1_abr_sep <- ggplot(traj_h1_abr_sep, aes(x = date)) +
  geom_line(aes(y = y_real,     colour = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = arima_hat,  colour = "ARIMA"),     linewidth = 0.7) +
  geom_line(aes(y = arimax_hat, colour = "ARIMAX"),    linewidth = 0.7) +
  geom_line(aes(y = var_hat,    colour = "VAR"),       linewidth = 0.7) +
  labs(
    title  = "Pronósticos rolling one-step-ahead (abril–septiembre 2025)",
    x      = "Fecha",
    y      = "Tasa de desocupación (%)",
    colour = ""
  ) +
  theme_minimal()

ggplot2::ggsave(
  filename = "figs/pronosticos_rolling_h1_abr_sep_2025.pdf",
  plot     = gg_traj_h1_abr_sep,
  width    = 7,
  height   = 4
)



# ================================================================
# 14. Tabla tipo Panel B: abril–septiembre 2025 (rolling, h = 1)
# ================================================================
library(tidyr)
library(knitr)
library(kableExtra)

panelB_rolling <- resultados_rolling %>%
  dplyr::filter(
    horizonte == 1,
    date >= as.Date("2025-04-01"),
    date <= as.Date("2025-09-01")
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    date = format(date, "%Y-%m-%d")
  ) %>%
  dplyr::select(
    date,
    unemp_real = y_real,
    arima_hat,
    arimax_hat,
    var_hat
  )

print(panelB_rolling)

panelB_rolling_tex <- panelB_rolling %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(unemp_real, arima_hat, arimax_hat, var_hat),
      .fns  = ~ round(.x, 3)
    )
  ) %>%
  knitr::kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Panel B. Pronósticos rolling one-step-ahead de la tasa de desocupación (abril--septiembre 2025).",
    label     = "panelB_rolling",
    align     = "c",
    col.names = c("Fecha", "Observado", "ARIMA", "ARIMAX", "VAR")
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position   = "center",
    font_size  = 9
  )

kableExtra::save_kable(panelB_rolling_tex, "tex/panelB_rolling.tex")



library(dplyr)

panelB_rolling <- resultados_rolling %>%
  filter(
    horizonte == 1,
    date >= as.Date("2025-04-01"),
    date <= as.Date("2025-09-01")
  ) %>%
  arrange(date) %>%
  transmute(
    date       = format(date, "%Y-%m-%d"),
    unemp_real = y_real,
    arima_hat,
    arimax_hat,
    var_hat
  )

# ← AQUÍ VES EL RESULTADO EN R
print(panelB_rolling, n = Inf)



library(ggplot2)
library(dplyr)

traj_h1_abr_sep <- resultados_rolling %>%
  filter(
    horizonte == 1,
    date >= as.Date("2025-04-01"),
    date <= as.Date("2025-09-01")
  )

gg_traj_h1_abr_sep <- ggplot(traj_h1_abr_sep, aes(x = date)) +
  geom_line(aes(y = y_real,     colour = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = arima_hat,  colour = "ARIMA"),     linewidth = 0.7) +
  geom_line(aes(y = arimax_hat, colour = "ARIMAX"),    linewidth = 0.7) +
  geom_line(aes(y = var_hat,    colour = "VAR"),       linewidth = 0.7) +
  labs(
    title  = "Pronósticos rolling one-step-ahead (abril–septiembre 2025)",
    x      = "Fecha",
    y      = "Tasa de desocupación (%)",
    colour = ""
  ) +
  theme_minimal()

# ← AQUÍ se ve el resultado en la ventana de gráficos de R / RStudio
gg_traj_h1_abr_sep


library(dplyr)

resultados_rolling

tail(resultados_rolling, 2)

library(dplyr)

resultados_rolling %>%
  slice_tail(n = 10)




library(tidyverse)

pron_sep <- resultados_rolling %>%
  filter(
    horizonte == 1,                      # one-step-ahead
    date == as.Date("2025-09-01")       # septiembre 2025
  )

print(pron_sep)



