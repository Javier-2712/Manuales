#' Análisis de Distribución de Abundancia de Taxas (RAD)
#' 
#' Convierte datos a formato wide (estaciones en columnas, taxones en filas)
#' y proporciona funciones para análisis RAD.
#' 
#' @param data DataFrame con datos de abundancia
#' @param group_col Columna con los grupos/taxones
#' @param station_col Columna con las estaciones
#' @param abundance_col Columna con las abundancias
#' @param minlength Longitud mínima para abreviar nombres (default: 4)
#' @param station_prefix Prefijo para nombres de estaciones (default: "Estación_")
#' 
#' @return Lista con datos procesados en formato wide y funciones de análisis

setup_rad_analysis <- function(data, group_col, station_col, abundance_col, 
                               minlength = 4, station_prefix = "Estación_") {
  
  # Procesar datos a formato wide
  processed_data <- data %>%
    mutate(
      Abrev = abbreviate({{ group_col }}, minlength = minlength),
      Station = factor({{ station_col }}),
      Abundance = as.numeric({{ abundance_col }})
    ) %>%
    group_by(Abrev, Station) %>%
    summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
    mutate(Station = paste0(station_prefix, Station)) %>%
    pivot_wider(
      names_from = Station,
      values_from = Abundance,
      values_fill = 0
    ) %>%
    arrange(Abrev)
  
  # Crear versión con rownames
  data_with_rownames <- as.data.frame(processed_data)
  rownames(data_with_rownames) <- data_with_rownames$Abrev
  data_with_rownames <- data_with_rownames[, setdiff(names(data_with_rownames), "Abrev"), drop = FALSE]
  
  # Función para obtener el mejor modelo RAD
  get_best_model <- function(site_data, site_name) {
    abundances <- site_data[site_data > 0]
    
    if (length(abundances) < 2) {
      return(list(
        table = tibble(Estación = site_name, Modelo = "Datos insuficientes", AIC = NA),
        best_model = NULL,
        rad_fit = NULL
      ))
    }
    
    models <- list()
    aic_values <- c()
    
    # Función para suprimir errores y warnings
    suppressAll <- function(expr) {
      suppressMessages(suppressWarnings(try(expr, silent = TRUE)))
    }
    
    # Ajustar cada modelo individualmente
    null_fit <- suppressAll(vegan::rad.null(abundances))
    if (!is.null(null_fit) && !inherits(null_fit, "try-error") && 
        !is.null(null_fit$aic) && is.finite(null_fit$aic)) {
      models$Null <- null_fit
      aic_values["Null"] <- null_fit$aic
    }
    
    preemption_fit <- suppressAll(vegan::rad.preempt(abundances))
    if (!is.null(preemption_fit) && !inherits(preemption_fit, "try-error") && 
        !is.null(preemption_fit$aic) && is.finite(preemption_fit$aic)) {
      models$Preemption <- preemption_fit
      aic_values["Preemption"] <- preemption_fit$aic
    }
    
    lognormal_fit <- suppressAll(vegan::rad.lognormal(abundances))
    if (!is.null(lognormal_fit) && !inherits(lognormal_fit, "try-error") && 
        !is.null(lognormal_fit$aic) && is.finite(lognormal_fit$aic)) {
      models$Lognormal <- lognormal_fit
      aic_values["Lognormal"] <- lognormal_fit$aic
    }
    
    zipf_fit <- suppressAll(vegan::rad.zipf(abundances))
    if (!is.null(zipf_fit) && !inherits(zipf_fit, "try-error") && 
        !is.null(zipf_fit$aic) && is.finite(zipf_fit$aic)) {
      models$Zipf <- zipf_fit
      aic_values["Zipf"] <- zipf_fit$aic
    }
    
    mandelbrot_fit <- suppressAll(vegan::rad.zipfbrot(abundances))
    if (!is.null(mandelbrot_fit) && !inherits(mandelbrot_fit, "try-error") && 
        !is.null(mandelbrot_fit$aic) && is.finite(mandelbrot_fit$aic)) {
      models$Mandelbrot <- mandelbrot_fit
      aic_values["Mandelbrot"] <- mandelbrot_fit$aic
    }
    
    if (length(aic_values) == 0) {
      return(list(
        table = tibble(Estación = site_name, Modelo = "Sin modelos válidos", AIC = NA),
        best_model = NULL,
        rad_fit = NULL
      ))
    }
    
    best_model_name <- names(which.min(aic_values))
    best_aic <- min(aic_values)
    
    return(list(
      table = tibble(Estación = site_name, Modelo = best_model_name, AIC = best_aic),
      best_model = best_model_name,
      rad_fit = models[[best_model_name]]
    ))
  }
  
  # Función para crear gráficos RAD
  create_rad_plot <- function(site_data, site_name, best_model_info = NULL) {
    rad_data <- site_data[site_data > 0]
    
    if (length(rad_data) < 2) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Datos insuficientes") +
               labs(title = paste("RAD -", site_name)) +
               theme_void())
    }
    
    # Obtener información del mejor modelo si no se proporciona
    if (is.null(best_model_info)) {
      best_model_info <- get_best_model(site_data, site_name)
    }
    
    best_model_name <- best_model_info$best_model
    best_model_fit <- best_model_info$rad_fit
    
    if (is.null(best_model_fit)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Error en el ajuste del modelo") +
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
    
    # Mapeo directo de modelo a color/tipo de línea
    model_styles <- list(
      "Null" = list(color = "red", linetype = "dashed"),
      "Preemption" = list(color = "green", linetype = "solid"),
      "Lognormal" = list(color = "purple", linetype = "dotdash"),
      "Zipf" = list(color = "orange", linetype = "longdash"),
      "Mandelbrot" = list(color = "brown", linetype = "twodash")
    )
    
    # Obtener valores ajustados del mejor modelo
    p <- ggplot() +
      geom_point(data = obs_data, aes(x = rank, y = log_abundance), 
                 size = 3, alpha = 0.7, color = "steelblue")
    
    if (!is.null(best_model_fit$fitted.values)) {
      fitted_values <- best_model_fit$fitted.values
      model_data <- tibble(
        rank = 1:length(fitted_values),
        log_abundance = log10(fitted_values)
      )
      
      # Añadir línea con estilo específico del modelo
      style <- model_styles[[best_model_name]]
      p <- p + geom_line(
        data = model_data,
        aes(x = rank, y = log_abundance),
        color = style$color,
        linetype = style$linetype,
        linewidth = 2.5
      )
    }
    
    p + labs(
      title = paste("RAD -", site_name),
      x = "Rango de taxas",
      y = "Log10(Abundancia)"
    ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.poEstaciónn = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
      )
  }
  
  # Función para crear leyenda única en dos filas (versión simple)
  create_shared_legend <- function() {
    # Crear un gráfico simple con todos los modelos
    p <- ggplot() +
      geom_line(aes(x = 1, y = 1, color = "Null", linetype = "Null"), linewidth = 1.5) +
      geom_line(aes(x = 1, y = 1, color = "Preemption", linetype = "Preemption"), linewidth = 1.5) +
      geom_line(aes(x = 1, y = 1, color = "Lognormal", linetype = "Lognormal"), linewidth = 1.5) +
      geom_line(aes(x = 1, y = 1, color = "Zipf", linetype = "Zipf"), linewidth = 1.5) +
      geom_line(aes(x = 1, y = 1, color = "Mandelbrot", linetype = "Mandelbrot"), linewidth = 1.5) +
      scale_color_manual(
        name = NULL,
        values = c("Null" = "red", "Preemption" = "green", "Lognormal" = "purple", 
                   "Zipf" = "orange", "Mandelbrot" = "brown"),
        guide = guide_legend(nrow = 2)
      ) +
      scale_linetype_manual(
        name = NULL,
        values = c("Null" = "dashed", "Preemption" = "solid", "Lognormal" = "dotdash",
                   "Zipf" = "longdash", "Mandelbrot" = "twodash"),
        guide = guide_legend(nrow = 2)
      ) +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.text = element_text(size = 10)
      )
    
    # Extraer solo la leyenda
    legend <- cowplot::get_legend(p)
    return(legend)
  }
  
  # --- NUEVAS FUNCIONES SIMPLIFICADAS ---
  
  #' Crear gráficos RAD para todos los Estacións
  create_all_rad_plots <- function(best_models_list) {
    map2(colnames(data_with_rownames), best_models_list, ~{
      site_data <- data_with_rownames[, .x]
      create_rad_plot(site_data, .x, .y)
    })
  }
  
  #' Combinar gráficos RAD en un solo panel
  combine_rad_plots <- function(rad_plots, title = NULL, ncol = 3) {
    combined <- wrap_plots(rad_plots, ncol = ncol)
    
    if (!is.null(title)) {
      combined <- combined + 
        plot_annotation(
          title = title,
          theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
        )
    }
    
    shared_legend <- create_shared_legend()
    plot_grid(combined, shared_legend, ncol = 1, rel_heights = c(1, 0.15))
  }
  
  #' Ejecutar análisis RAD completo
  run_complete_rad_analysis <- function(plot_title = "Distribución de Abundancia de Taxas") {
    
    # Silenciar errores
    con <- file(nullfile(), open = "w")
    sink(con, type = "message")
    best_models_list <- map(colnames(data_with_rownames), ~{
      site_data <- data_with_rownames[, .x]
      get_best_model(site_data, .x)
    })
    sink(type = "message")
    close(con)
    
    rad_plots <- create_all_rad_plots(best_models_list)
    combined_plot <- combine_rad_plots(rad_plots, title = plot_title)
    best_models_table <- map_dfr(best_models_list, ~.x$table)
    
    return(list(
      table = best_models_table,
      plots = rad_plots,
      combined_plot = combined_plot,
      models_list = best_models_list
    ))
  }
  
  # --- FIN DE NUEVAS FUNCIONES ---
  
  # Devolver objeto con datos y funciones
  return(list(
    data_wide = processed_data,
    data_matrix = data_with_rownames,
    get_best_model = get_best_model,
    create_rad_plot = create_rad_plot,
    create_shared_legend = create_shared_legend,
    create_all_rad_plots = create_all_rad_plots,
    combine_rad_plots = combine_rad_plots,
    run_complete_rad_analysis = run_complete_rad_analysis,
    stations = colnames(data_with_rownames)
  ))
}