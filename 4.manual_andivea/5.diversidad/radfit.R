#' ANÁLISIS COMPLETO DE DISTRIBUCIÓN DE ABUNDANCIA DE TAXAS (RAD)
#' 
#' Este archivo contiene todas las funciones necesarias para:
#' 1. Procesar datos ecológicos
#' 2. Ajustar modelos RAD
#' 3. Seleccionar mejores modelos por AIC
#' 4. Crear visualizaciones profesionales
#' 
#' @version 1.0
#' @author [Tu Nombre]
#' 

# Cargar librerías requeridas
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("vegan", quietly = TRUE)) install.packages("vegan")
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
if (!requireNamespace("cowplot", quietly = TRUE)) install.packages("cowplot")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")

library(tidyverse)
library(vegan)
library(patchwork)
library(cowplot)
library(readxl)
library(kableExtra)

# ==========================================================================
# FUNCIONES PRINCIPALES DE ANÁLISIS
# ==========================================================================

#' Configurar análisis RAD
setup_rad_analysis <- function(data, group_col, station_col, abundance_col, 
                               minlength = 4, station_prefix = "Estación_") {
  
  # [Todo el código de setup_rad_analysis del primer archivo...]
  # Copia aquí TODO el contenido de tu función setup_rad_analysis
  
}

# ==========================================================================
# FUNCIONES DE VISUALIZACIÓN ESPECÍFICA
# ==========================================================================

#' Crear gráficos RAD específicos para cada sitio
create_custom_rad_comparison <- function(rad_analysis, best_models_table) {
  # Versión simple - todos los gráficos en una sola cuadrícula
  create_specific_rad_plots(
    rad_analysis, best_models_table, 
    sites = c("Station_2", "Station_4", "Station_7", 
              "Station_9", "Station_13", "Station_15"),
    ncol = 3
  )
}

# ==========================================================================
# FUNCIONES WRAPPER PARA USUARIOS
# ==========================================================================

#' Ejecutar análisis completo y mostrar resultados tabulares
#'
#' @export
run_complete_analysis <- function(file_path = "plancton.xlsx", 
                                  sheet_name = "Riqueza",
                                  title = "Abundancia por Estación",
                                  group_col = Groups,
                                  station_col = Station,
                                  abundance_col = Abundance,
                                  station_prefix = "Station_") {
  
  # Cargar datos
  biol <- read_xlsx(file_path, sheet = sheet_name)
  
  # Configurar análisis
  rad_analysis <- setup_rad_analysis(
    data = biol,
    group_col = {{ group_col }},
    station_col = {{ station_col }},
    abundance_col = {{ abundance_col }},
    station_prefix = station_prefix
  )
  
  # Obtener resultados
  resultados <- rad_analysis$run_complete_rad_analysis(title)
  
  return(list(
    table = resultados$table,
    combined_plot = resultados$combined_plot,
    rad_analysis = rad_analysis,
    best_models_table = resultados$table
  ))
}

#' Ejecutar análisis y mostrar gráficos específicos
#'
#' @export
#' Ejecutar análisis y mostrar gráficos específicos
run_rad_analysis_and_plot <- function(file_path = "plancton.xlsx", 
                                      sheet_name = "Riqueza",
                                      title = "Abundancia por Estación",
                                      group_col = Groups,
                                      station_col = Station,
                                      abundance_col = Abundance,
                                      station_prefix = "Station_") {
  
  # Cargar datos
  biol <- read_xlsx(file_path, sheet = sheet_name)
  
  # Configurar análisis
  rad_analysis <- setup_rad_analysis(
    data = biol,
    group_col = {{ group_col }},
    station_col = {{ station_col }},
    abundance_col = {{ abundance_col }},
    station_prefix = station_prefix
  )
  
  # Obtener mejores modelos MANUALMENTE (evitando run_complete_rad_analysis)
  best_models_list <- map(rad_analysis$stations, ~{
    site_data <- rad_analysis$data_matrix[, .x]
    rad_analysis$get_best_model(site_data, .x)
  })
  
  best_models_table <- map_dfr(best_models_list, ~.x$table)
  
  # Crear gráficos específicos
  graficos_especificos <- create_custom_rad_comparison(
    rad_analysis, 
    best_models_table
  )
  
  return(graficos_especificos)
}

#' Función para mostrar tabla formateada
#'
#' @export
format_results_table <- function(results_table, caption = "Modelo con mejor ajuste (menor AIC) por Estación") {
  results_table %>%
    mutate(AIC = round(AIC, 1)) %>%
    kbl(caption = caption, align = c("l", "l", "r")) %>%
    kable_classic(full_width = FALSE) %>%
    row_spec(0, bold = TRUE, color = "white", background = "#4C72B0") %>%
    column_spec(1, bold = TRUE)
}

# ==========================================================================
# EJEMPLOS DE USO PARA USUARIOS
# ==========================================================================

#' Ejemplo de uso completo
#'
#' @examples
#' # Análisis completo con resultados tabulares y gráficos
#' # resultados <- run_complete_analysis()
#' # print(format_results_table(resultados$table))
#' # print(resultados$combined_plot)
#' 
#' # Solo gráficos específicos
#' # graficos <- run_rad_analysis_and_plot()
#' # print(graficos)
example_usage <- function() {
  message("Ejemplo de uso:")
  message("1. resultados <- run_complete_analysis()")
  message("2. print(format_results_table(resultados$table))")
  message("3. print(resultados$combined_plot)")
  message("4. graficos <- run_rad_analysis_and_plot()")
  message("5. print(graficos)")
}

# ==========================================================================
# FIN DEL ARCHIVO
# ==========================================================================

# Mensaje al cargar el archivo
cat("¡Análisis RAD cargado exitosamente!\n")
cat("Use example_usage() para ver ejemplos de uso.\n")
cat("Funciones disponibles: run_complete_analysis(), run_rad_analysis_and_plot(), format_results_table()\n")