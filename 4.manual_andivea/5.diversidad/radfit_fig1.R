#' Crear gráficos RAD específicos para cada sitio con su mejor modelo
#'
#' @param rad_analysis Objeto de análisis RAD creado con setup_rad_analysis
#' @param best_models_table Tabla con los mejores modelos por sitio
#' @param sites Vector con los sitios a graficar
#' @param ncol Número de columnas para el arreglo de gráficos
#'
#' @return Objeto ggplot con los gráficos RAD específicos
create_specific_rad_plots <- function(rad_analysis, best_models_table, sites = NULL, ncol = 3) {
  
  # Si no se especifican sitios, usar todos
  if (is.null(sites)) {
    sites <- rad_analysis$stations
  }
  
  # Función para suprimir errores
  suppressAll <- function(expr) {
    suppressMessages(suppressWarnings(try(expr, silent = TRUE)))
  }
  
  # Crear gráficos específicos para cada sitio
  specific_plots <- map(sites, function(site_name) {
    
    # Obtener datos del sitio
    site_data <- rad_analysis$data_matrix[, site_name]
    rad_data <- site_data[site_data > 0]
    
    if (length(rad_data) < 2) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Datos insuficientes") +
               labs(title = paste("RAD -", site_name)) +
               theme_void())
    }
    
    # Obtener el mejor modelo para este sitio
    best_model_info <- best_models_table %>% 
      filter(Estación == site_name)
    
    if (nrow(best_model_info) == 0 || is.na(best_model_info$Modelo)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Modelo no disponible") +
               labs(title = paste("RAD -", site_name)) +
               theme_void())
    }
    
    best_model_name <- best_model_info$Modelo
    
    # Ajustar el modelo RAD SILENCIOSAMENTE (usando la misma estrategia)
    rad_fit <- suppressAll({
      models <- list()
      
      # Ajustar solo el modelo específico que necesitamos
      switch(best_model_name,
             "Null" = try(vegan::rad.null(rad_data), silent = TRUE),
             "Preemption" = try(vegan::rad.preempt(rad_data), silent = TRUE),
             "Lognormal" = try(vegan::rad.lognormal(rad_data), silent = TRUE),
             "Zipf" = try(vegan::rad.zipf(rad_data), silent = TRUE),
             "Mandelbrot" = try(vegan::rad.zipfbrot(rad_data), silent = TRUE),
             NULL
      )
    })
    
    # Si el ajuste falló, usar un enfoque alternativo
    if (inherits(rad_fit, "try-error") || is.null(rad_fit)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Error en el ajuste") +
               labs(title = paste("RAD -", site_name)) +
               theme_void())
    }
    
    # Datos observados
    obs_data <- tibble(
      rank = 1:length(rad_data),
      abundance = sort(rad_data, decreasing = TRUE),
      log_abundance = log10(abundance),
      type = "Observado"
    )
    
    # Extraer predicción del mejor modelo
    if (!is.null(rad_fit$fitted.values)) {
      fitted_values <- rad_fit$fitted.values
      model_data <- tibble(
        rank = 1:length(fitted_values),
        abundance = fitted_values,
        log_abundance = log10(fitted_values),
        model = best_model_name,
        type = "Ajustado"
      )
    } else {
      model_data <- tibble()
    }
    
    # Crear gráfico
    p <- ggplot() +
      geom_point(data = obs_data, aes(x = rank, y = log_abundance), 
                 size = 2, alpha = 0.8, color = "steelblue") +
      labs(
        title = paste(site_name, "-", best_model_name),
        x = "Rango de taxas",
        y = "Log10(Abundancia)"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
      )
    
    # Añadir línea del modelo si está disponible
    if (nrow(model_data) > 0) {
      p <- p + geom_line(
        data = model_data,
        aes(x = rank, y = log_abundance),
        color = "red",
        linewidth = 1.2,
        linetype = "solid"
      )
    }
    
    return(p)
  })
  
  # Combinar gráficos
  wrap_plots(specific_plots, ncol = ncol) +
    plot_annotation(
      title = "Modelos RAD por Sitio - Mejor Ajuste",
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
    )
}


# --------------------------------------------------------------------------
# FUNCIÓN COMPARACIÓN PERSONALIZADA (AGREGAR ESTO)
# --------------------------------------------------------------------------

#' Crear comparación RAD personalizada
#'
#' Función wrapper para crear gráficos RAD específicos
#'
#' @param rad_analysis Objeto de análisis RAD
#' @param best_models_table Tabla con mejores modelos
#' @param sites Sitios a graficar (NULL para todos)
#' @param ncol Número de columnas
#'
#' @return Objeto ggplot con gráficos combinados
create_custom_rad_comparison <- function(rad_analysis, best_models_table, sites = NULL, ncol = 3) {
  # Esta es simplemente un alias para la función principal
  create_specific_rad_plots(rad_analysis, best_models_table, sites, ncol)
}

# --------------------------------------------------------------------------
# FUNCIÓN WRAPPER PARA USUARIOS (AGREGAR AL FINAL DEL ARCHIVO)
# --------------------------------------------------------------------------

#' Ejecutar análisis completo y mostrar gráficos en un solo paso
#' 
#' Función simplificada para usuarios del libro. Ejecuta todo el proceso RAD
#' y devuelve los gráficos de los mejores modelos por estación.
#'
#' @param file_path Ruta al archivo Excel (default: "plancton.xlsx")
#' @param sheet_name Nombre de la hoja (default: "Riqueza")
#' @param title Título para los gráficos (default: "Abundancia por Estación")
#' @param group_col Nombre columna grupos/taxones (default: Groups)
#' @param station_col Nombre columna estaciones (default: Station) 
#' @param abundance_col Nombre columna abundancias (default: Abundance)
#' @param station_prefix Prefijo estaciones (default: "Station_")
#'
#' @return Objeto ggplot con los gráficos combinados
#'
#' @examples
#' # Uso básico:
#' # graficos <- run_rad_analysis_and_plot()
#' # print(graficos)
#' 
#' # Uso personalizado:
#' # run_rad_analysis_and_plot(
#' #   file_path = "mis_datos.xlsx",
#' #   sheet_name = "micapa",
#' #   title = "Mi Análisis RAD"
#' # )
run_rad_analysis_and_plot <- function(file_path = "plancton.xlsx", 
                                      sheet_name = "Riqueza",
                                      title = "Abundancia por Estación",
                                      group_col = Groups,
                                      station_col = Station,
                                      abundance_col = Abundance,
                                      station_prefix = "Station_") {
  
  # Cargar librerías requeridas
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork")
  }
  
  library(readxl)
  library(patchwork)
  
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
  
  # Obtener resultados (silenciosamente)
  suppressMessages({
    resultados <- rad_analysis$run_complete_rad_analysis(title)
  })
  
  # Crear y devolver gráficos
  create_custom_rad_comparison(rad_analysis, resultados$table)
}

# --------------------------------------------------------------------------
# FIN DEL ARCHIVO - No agregar nada después de esta línea
# --------------------------------------------------------------------------