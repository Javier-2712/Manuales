#' Gráfico RAD profesional para publicación científica
#'
#' @param biol_rad Dataframe con datos de rango-abundancia
#' @param palette_colors Vector de colores personalizado (opcional)
#' @param n_columns Número de columnas para facet_wrap
#' @param point_size Tamaño de los puntos
#' @param text_size Tamaño del texto de las etiquetas
#'
#' @return Objeto ggplot con gráfico RAD profesional
#'
#' @examples
#' # Uso básico:
#' grafico_rad <- create_rad_plot_pro(biol_rad)
#' print(grafico_rad)
create_rad_plot_pro <- function(biol_rad, 
                                palette_colors = NULL,
                                n_columns = NULL,
                                point_size = 3.5,
                                text_size = 3.2) {
  
  # Verificar y cargar ggrepel si es necesario
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    install.packages("ggrepel")
    library(ggrepel)
  }
  
  # Paleta de colores por defecto (mejorada)
  if (is.null(palette_colors)) {
    palette_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                        "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                        "#bcbd22", "#17becf", "#aec7e8", "#ffbb78",
                        "#98df8a", "#ff9896", "#c5b0d5")
  }
  
  # Calcular número óptimo de columnas si no se especifica
  if (is.null(n_columns)) {
    n_stations <- length(unique(biol_rad$Station))
    n_columns <- ifelse(n_stations <= 4, n_stations, 4)
  }
  
  # Crear el gráfico profesional
  rad_plot <- ggplot(biol_rad, 
                     aes(x = Rango, y = Ab_rel, color = Taxas, label = Taxas)) +
    
    # Línea principal (más suave y elegante)
    geom_line(color = "#2E86AB", linewidth = 1.2, alpha = 0.8) +
    
    # Puntos mejorados (más profesionales)
    geom_point(size = point_size, shape = 21, 
               fill = "white", color = "black", stroke = 1.2,
               alpha = 0.9) +
    
    # Etiquetas inteligentes con ggrepel
    ggrepel::geom_text_repel(
      aes(label = Taxas), 
      size = text_size,
      max.overlaps = 20,
      min.segment.length = 0.2,
      box.padding = 0.35,
      point.padding = 0.3,
      segment.color = "gray60", 
      segment.linetype = "dotted",
      segment.size = 0.6,
      force = 1,
      show.legend = FALSE,
      family = "Arial"
    ) +
    
    # Escalas mejoradas
    scale_x_continuous(
      breaks = function(x) seq(0, max(x), by = ceiling(max(x)/6)),
      expand = expansion(mult = 0.05)
    ) + 
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    
    # Paleta de colores profesional
    scale_color_manual(values = palette_colors) +
    
    # Etiquetas profesionales
    labs(
      x = "Rango de taxas",
      y = expression(log[10] ~ "(Abundancia relativa)"),
      title = "Distribución de Abundancia de taxas (RAD)",
      subtitle = "Curvas de Rango-Abundancia por estación de muestreo",
      caption = "Fuente de los datos: Vargas et al. (2025)"
    ) +
    
    # Y añade este tema al final del theme():
    theme(
      # ... [el resto de tu theme] ...
      plot.caption = ggtext::element_markdown(
        size = 9, 
        color = "gray50", 
        hjust = 1,
        margin = margin(t = 10)
      )
    ) +
    
    # Facets con mejor espaciado
    facet_wrap(~Station, ncol = n_columns, scales = "free_x") +
    
    # Tema profesional para publicación
    theme_bw(base_size = 14, base_family = "Arial") +
    theme(
      # Panel principal
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.background = element_rect(fill = "gray97", color = NA),
      panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.5),
      
      # Fondos
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 15, 15),
      
      # Strips de facets
      strip.background = element_rect(fill = "#2E86AB", color = "gray40"),
      strip.text = element_text(face = "bold", size = 11, color = "white",
                                margin = margin(5, 0, 5, 0)),
      
      # Ejes y texto
      axis.text = element_text(color = "gray30", size = 10),
      axis.title = element_text(face = "bold", color = "gray20", size = 12),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      
      # Títulos
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5,
                                color = "gray20", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40",
                                   margin = margin(b = 15)),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 1,
                                  margin = margin(t = 10)),
      
      # Leyenda (oculta pero disponible si se necesita)
      legend.position = "none"
    )
  
  return(rad_plot)
}

# --------------------------------------------------------------------------
# FUNCIÓN PARA EL LIBRO: Versión ultra-simplificada
# --------------------------------------------------------------------------

#' Gráfico RAD para libro (uso simple)
#' 
#' Función simplificada para usuarios del libro. Genera un gráfico profesional
#' de curvas RAD asumiendo que el dataframe 'biol_rad' está cargado en el entorno.
#'
#' @return Gráfico RAD listo para publicación
#' 
#' @section Requisitos del dataframe:
#' El dataframe 'biol_rad' debe contener las siguientes columnas:
#' \describe{
#'   \item{Rango}{Rango de la especie (numérica)}
#'   \item{Ab_rel}{Abundancia relativa (numérica)}
#'   \item{Taxas}{Nombre de la especie/taxa (carácter)}
#'   \item{Station}{Estación de muestreo (factor o carácter)}
#' }
#'
#' @examples
#' # Cargar datos primero:
#' # biol_rad <- preparar_datos_rad(datos_originales)
#' 
#' # Luego simplemente ejecutar:
#' plot_rad_libro()
#' 
#' @export
plot_rad_libro <- function() {
  # Verificar que el dataframe existe
  if (!exists("biol_rad")) {
    stop("ERROR: Dataframe 'biol_rad' no encontrado.\n",
         "Por favor cargue los datos primero con:\n",
         "biol_rad <- preparar_datos_rad(su_dataframe_original)\n",
         "Asegúrese que tenga las columnas: Rango, Ab_rel, Taxas, Station")
  }
  
  # Verificar estructura del dataframe
  required_cols <- c("Rango", "Ab_rel", "Taxas", "Station")
  missing_cols <- setdiff(required_cols, names(biol_rad))
  
  if (length(missing_cols) > 0) {
    stop("ERROR: El dataframe 'biol_rad' no tiene la estructura correcta.\n",
         "Columnas faltantes: ", paste(missing_cols, collapse = ", "), "\n",
         "Columnas requeridas: Rango, Ab_rel, Taxas, Station")
  }
  
  # Verificar tipos de datos
  if (!is.numeric(biol_rad$Rango)) {
    stop("ERROR: La columna 'Rango' debe ser numérica")
  }
  if (!is.numeric(biol_rad$Ab_rel)) {
    stop("ERROR: La columna 'Ab_rel' debe ser numérica")
  }
  
  # Crear y devolver el gráfico
  create_rad_plot_pro(biol_rad)
}

# --------------------------------------------------------------------------
# FUNCIÓN DE PREPARACIÓN DE DATOS (OPCIONAL PERO RECOMENDADA)
# --------------------------------------------------------------------------

#' Preparar datos para gráfico RAD
#'
#' @param datos_originales Dataframe original con datos de abundancia
#' @param columna_estacion Nombre de la columna de estaciones
#' @param columna_taxa Nombre de la columna de taxa
#' @param columna_abundancia Nombre de la columna de abundancia
#'
#' @return Dataframe biol_rad listo para plot_rad_libro()
#'
#' @examples
#' # Preparar datos desde un dataframe original:
#' # biol_rad <- preparar_datos_rad(datos_originales, 
#' #                               "Estacion", "Especie", "Abundancia")
preparar_datos_rad <- function(datos_originales, 
                               columna_estacion = "Station",
                               columna_taxa = "Taxas", 
                               columna_abundancia = "Abundance") {
  
  # Verificar que las columnas existen
  required <- c(columna_estacion, columna_taxa, columna_abundancia)
  missing <- setdiff(required, names(datos_originales))
  
  if (length(missing) > 0) {
    stop("Columnas faltantes en los datos: ", paste(missing, collapse = ", "))
  }
  
  # Procesar datos
  biol_rad <- datos_originales %>%
    rename(Station = !!sym(columna_estacion),
           Taxas = !!sym(columna_taxa),
           Abundance = !!sym(columna_abundancia)) %>%
    group_by(Station) %>%
    mutate(Ab_rel = Abundance / sum(Abundance),
           Rango = rank(-Abundance, ties.method = "first")) %>%
    ungroup() %>%
    select(Station, Taxas, Rango, Ab_rel, Abundance) %>%
    arrange(Station, Rango)
  
  message("Datos preparados exitosamente. ",
          nrow(biol_rad), " filas, ", 
          length(unique(biol_rad$Station)), " estaciones, ",
          length(unique(biol_rad$Taxas)), " taxas únicos")
  
  return(biol_rad)
}