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
# PRONÓSTICO OCTUBRE 2025, # tabla out of sample, forecast, RMSE
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