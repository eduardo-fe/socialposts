# ============================================================================
# Análisis de Denuncias y Condenas en España (2006-2025)
# Script de análisis con funciones modulares
# ============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(scales)

# ============================================================================
# FUNCIONES DE CARGA Y PREPARACIÓN DE DATOS
# ============================================================================

#' Cargar y preparar datos de denuncias
#' 
#' @param filepath Ruta al archivo CSV
#' @return Tibble con datos procesados y tasas calculadas
load_and_prepare_data <- function(filepath = "denuncias.csv") {
  
  denuncias <- read_csv(filepath, show_col_types = FALSE)
  
  denuncias_rates <- denuncias %>%
    mutate(
      # Población en número absoluto (estaba en millones)
      POPULATION_ABSOLUTE = POPULATION * 1000000,
      
      # Tasas por 100,000 habitantes
      DENUNCIAS_RATE = (DENUNCIAS / POPULATION_ABSOLUTE) * 100000,
      CONFORMIDAD_RATE = (CONDENAS_CON_CONFORMIDAD / POPULATION_ABSOLUTE) * 100000,
      SIN_CONFORMIDAD_RATE = (CONDENAS_SIN_CONFORMIDAD / POPULATION_ABSOLUTE) * 100000,
      INOCENTES_RATE = (INOCENTES / POPULATION_ABSOLUTE) * 100000,
      
      # Total de condenas
      TOTAL_CONDENAS = CONDENAS_CON_CONFORMIDAD + CONDENAS_SIN_CONFORMIDAD,
      TOTAL_CONDENAS_RATE = (TOTAL_CONDENAS / POPULATION_ABSOLUTE) * 100000,
      
      # Porcentajes del total de denuncias
      PCT_CONFORMIDAD = (CONDENAS_CON_CONFORMIDAD / DENUNCIAS) * 100,
      PCT_SIN_CONFORMIDAD = (CONDENAS_SIN_CONFORMIDAD / DENUNCIAS) * 100,
      PCT_INOCENTES = (INOCENTES / DENUNCIAS) * 100,
      PCT_CONDENAS = ((TOTAL_CONDENAS) / DENUNCIAS) * 100
    )
  
  return(denuncias_rates)
}

# ============================================================================
# FUNCIONES DE ANÁLISIS ESTADÍSTICO
# ============================================================================

#' Calcular estadísticas descriptivas
#' 
#' @param data DataFrame con datos procesados
#' @return Tibble con estadísticas resumen
calculate_summary_stats <- function(data) {
  
  summary_stats <- data %>%
    summarise(
      across(c(DENUNCIAS_RATE, CONFORMIDAD_RATE, SIN_CONFORMIDAD_RATE, 
               INOCENTES_RATE, TOTAL_CONDENAS_RATE),
             list(
               Media = ~mean(.),
               Mediana = ~median(.),
               Min = ~min(.),
               Max = ~max(.),
               SD = ~sd(.)
             ))
    ) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>%
    separate(Variable, into = c("Tipo", "Estadística"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(names_from = Estadística, values_from = Valor)
  
  return(summary_stats)
}

#' Calcular cambios entre años
#' 
#' @param data DataFrame con datos procesados
#' @param year_start Año inicial
#' @param year_end Año final
#' @return Tibble con cambios absolutos y porcentuales
calculate_period_changes <- function(data, year_start = 2006, year_end = 2025) {
  
  cambios <- data %>%
    filter(AÑO %in% c(year_start, year_end)) %>%
    select(AÑO, DENUNCIAS_RATE, CONFORMIDAD_RATE, SIN_CONFORMIDAD_RATE, 
           INOCENTES_RATE, TOTAL_CONDENAS_RATE) %>%
    pivot_longer(-AÑO, names_to = "Variable", values_to = "Valor") %>%
    pivot_wider(names_from = AÑO, names_prefix = "Year_", values_from = Valor) %>%
    mutate(
      Cambio_Absoluto = .data[[paste0("Year_", year_end)]] - .data[[paste0("Year_", year_start)]],
      Cambio_Porcentual = ((Cambio_Absoluto / .data[[paste0("Year_", year_start)]]) * 100)
    )
  
  return(cambios)
}

#' Realizar análisis de regresión lineal
#' 
#' @param data DataFrame con datos procesados
#' @return Tibble con coeficientes de regresión
perform_regression_analysis <- function(data) {
  
  modelo_denuncias <- lm(DENUNCIAS_RATE ~ AÑO, data = data)
  modelo_conformidad <- lm(CONFORMIDAD_RATE ~ AÑO, data = data)
  modelo_sin_conformidad <- lm(SIN_CONFORMIDAD_RATE ~ AÑO, data = data)
  modelo_inocentes <- lm(INOCENTES_RATE ~ AÑO, data = data)
  
  coefs <- tibble(
    Variable = c("Denuncias", "Con conformidad", "Sin conformidad", "Inocentes"),
    Intercepto = c(coef(modelo_denuncias)[1], coef(modelo_conformidad)[1],
                   coef(modelo_sin_conformidad)[1], coef(modelo_inocentes)[1]),
    Pendiente = c(coef(modelo_denuncias)[2], coef(modelo_conformidad)[2],
                  coef(modelo_sin_conformidad)[2], coef(modelo_inocentes)[2]),
    R_cuadrado = c(summary(modelo_denuncias)$r.squared,
                   summary(modelo_conformidad)$r.squared,
                   summary(modelo_sin_conformidad)$r.squared,
                   summary(modelo_inocentes)$r.squared),
    P_valor = c(summary(modelo_denuncias)$coefficients[2,4],
                summary(modelo_conformidad)$coefficients[2,4],
                summary(modelo_sin_conformidad)$coefficients[2,4],
                summary(modelo_inocentes)$coefficients[2,4])
  )
  
  return(coefs)
}

# ============================================================================
# FUNCIONES DE VISUALIZACIÓN
# ============================================================================

#' Graficar denuncias por 100,000 habitantes
#' 
#' @param data DataFrame con datos procesados
#' @return Objeto ggplot
plot_denuncias_trend <- function(data) {
  
  p <- ggplot(data, aes(x = AÑO, y = DENUNCIAS_RATE)) +
    geom_line(color = "#e74c3c", size = 1.2) +
    geom_point(color = "#e74c3c", size = 3) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "#34495e") +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Evolución de Denuncias por 100,000 habitantes",
      subtitle = "España 2006-2025",
      x = "Año",
      y = "Denuncias por 100,000 hab.",
      caption = "Fuente: Datos proporcionados"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray40")
    )
  
  return(p)
}

#' Graficar condenas por 100,000 habitantes
#' 
#' @param data DataFrame con datos procesados
#' @return Objeto ggplot
plot_condenas_trend <- function(data) {
  
  condenas_long <- data %>%
    select(AÑO, CONFORMIDAD_RATE, SIN_CONFORMIDAD_RATE, TOTAL_CONDENAS_RATE) %>%
    pivot_longer(-AÑO, names_to = "Tipo", values_to = "Tasa") %>%
    mutate(
      Tipo = case_when(
        Tipo == "CONFORMIDAD_RATE" ~ "Con conformidad",
        Tipo == "SIN_CONFORMIDAD_RATE" ~ "Sin conformidad",
        Tipo == "TOTAL_CONDENAS_RATE" ~ "Total condenas"
      )
    )
  
  p <- ggplot(condenas_long, aes(x = AÑO, y = Tasa, color = Tipo, linetype = Tipo)) +
    geom_line(size = 1.1) +
    geom_point(size = 2.5) +
    scale_color_manual(values = c(
      "Con conformidad" = "#27ae60",
      "Sin conformidad" = "#f39c12",
      "Total condenas" = "#34495e"
    )) +
    scale_linetype_manual(values = c(
      "Con conformidad" = "solid",
      "Sin conformidad" = "solid",
      "Total condenas" = "dashed"
    )) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Evolución de Condenas por 100,000 habitantes",
      subtitle = "Desglose por tipo de conformidad",
      x = "Año",
      y = "Condenas por 100,000 hab.",
      color = "Tipo de condena",
      linetype = "Tipo de condena",
      caption = "Fuente: Datos proporcionados"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray40"),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Graficar inocentes por 100,000 habitantes
#' 
#' @param data DataFrame con datos procesados
#' @return Objeto ggplot
plot_inocentes_trend <- function(data) {
  
  p <- ggplot(data, aes(x = AÑO, y = INOCENTES_RATE)) +
    geom_line(color = "#3498db", size = 1.2) +
    geom_point(color = "#3498db", size = 3) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "#34495e") +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Evolución de Inocentes por 100,000 habitantes",
      subtitle = "España 2006-2025",
      x = "Año",
      y = "Inocentes por 100,000 hab.",
      caption = "Fuente: Datos proporcionados"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray40")
    )
  
  return(p)
}

#' Graficar todas las categorías juntas
#' 
#' @param data DataFrame con datos procesados
#' @return Objeto ggplot
plot_all_categories <- function(data) {
  
  all_categories <- data %>%
    select(AÑO, DENUNCIAS_RATE, CONFORMIDAD_RATE, SIN_CONFORMIDAD_RATE, 
           INOCENTES_RATE, TOTAL_CONDENAS_RATE) %>%
    pivot_longer(-AÑO, names_to = "Categoria", values_to = "Tasa") %>%
    mutate(
      Categoria = case_when(
        Categoria == "DENUNCIAS_RATE" ~ "Denuncias",
        Categoria == "CONFORMIDAD_RATE" ~ "Condenas con conformidad",
        Categoria == "SIN_CONFORMIDAD_RATE" ~ "Condenas sin conformidad",
        Categoria == "INOCENTES_RATE" ~ "Inocentes",
        Categoria == "TOTAL_CONDENAS_RATE" ~ "Total condenas"
      )
    )
  
  p <- ggplot(all_categories, aes(x = AÑO, y = Tasa, color = Categoria)) +
    geom_line(size = 1.1) +
    geom_point(size = 2) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Comparación de Todas las Categorías por 100,000 habitantes",
      subtitle = "España 2006-2025",
      x = "Año",
      y = "Tasa por 100,000 hab.",
      color = "Categoría",
      caption = "Fuente: Datos proporcionados"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray40"),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(nrow = 2))
  
  return(p)
}

#' Graficar proporciones (área apilada)
#' 
#' @param data DataFrame con datos procesados
#' @return Objeto ggplot
plot_proportions <- function(data) {
  
  proporciones_long <- data %>%
    select(AÑO, PCT_CONFORMIDAD, PCT_SIN_CONFORMIDAD, PCT_INOCENTES) %>%
    pivot_longer(-AÑO, names_to = "Tipo", values_to = "Porcentaje") %>%
    mutate(
      Tipo = case_when(
        Tipo == "PCT_CONFORMIDAD" ~ "Con conformidad",
        Tipo == "PCT_SIN_CONFORMIDAD" ~ "Sin conformidad",
        Tipo == "PCT_INOCENTES" ~ "Inocentes/Archivados"
      )
    )
  
  p <- ggplot(proporciones_long, aes(x = AÑO, y = Porcentaje, fill = Tipo)) +
    geom_area(alpha = 0.7) +
    scale_fill_manual(values = c(
      "Con conformidad" = "#27ae60",
      "Sin conformidad" = "#f39c12",
      "Inocentes/Archivados" = "#3498db"
    )) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Distribución Porcentual de Resultados de Denuncias",
      subtitle = "España 2006-2025",
      x = "Año",
      y = "Porcentaje del total de denuncias",
      fill = "Resultado",
      caption = "Fuente: Datos proporcionados"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray40"),
      legend.position = "bottom"
    )
  
  return(p)
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE ANÁLISIS
# ============================================================================

#' Ejecutar análisis completo
#' 
#' @param filepath Ruta al archivo CSV
#' @param output_dir Directorio para guardar resultados (opcional)
#' @return Lista con todos los resultados del análisis
run_complete_analysis <- function(filepath = "denuncias.csv", output_dir = NULL) {
  
  cat("=== Análisis de Denuncias y Condenas en España ===\n\n")
  
  # 1. Cargar datos
  cat("1. Cargando y preparando datos...\n")
  data <- load_and_prepare_data(filepath)
  cat(sprintf("   Datos cargados: %d filas, %d columnas\n\n", nrow(data), ncol(data)))
  
  # 2. Estadísticas descriptivas
  cat("2. Calculando estadísticas descriptivas...\n")
  stats <- calculate_summary_stats(data)
  print(stats)
  cat("\n")
  
  # 3. Cambios en el período
  cat("3. Calculando cambios 2006-2025...\n")
  cambios <- calculate_period_changes(data)
  print(cambios)
  cat("\n")
  
  # 4. Análisis de regresión
  cat("4. Análisis de regresión lineal...\n")
  regression <- perform_regression_analysis(data)
  print(regression)
  cat("\n")
  
  # 5. Crear visualizaciones
  cat("5. Creando visualizaciones...\n")
  plots <- list(
    denuncias = plot_denuncias_trend(data),
    condenas = plot_condenas_trend(data),
    inocentes = plot_inocentes_trend(data),
    all_categories = plot_all_categories(data),
    proportions = plot_proportions(data)
  )
  
  # 6. Guardar gráficos si se especifica directorio
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    cat(sprintf("   Guardando gráficos en %s...\n", output_dir))
    ggsave(file.path(output_dir, "denuncias_trend.png"), plots$denuncias, 
           width = 10, height = 6, dpi = 300)
    ggsave(file.path(output_dir, "condenas_trend.png"), plots$condenas, 
           width = 10, height = 6, dpi = 300)
    ggsave(file.path(output_dir, "inocentes_trend.png"), plots$inocentes, 
           width = 10, height = 6, dpi = 300)
    ggsave(file.path(output_dir, "all_categories.png"), plots$all_categories, 
           width = 10, height = 6, dpi = 300)
    ggsave(file.path(output_dir, "proportions.png"), plots$proportions, 
           width = 10, height = 6, dpi = 300)
    
    # Guardar datos procesados
    write_csv(data, file.path(output_dir, "denuncias_processed.csv"))
    write_csv(stats, file.path(output_dir, "summary_statistics.csv"))
    write_csv(cambios, file.path(output_dir, "period_changes.csv"))
    write_csv(regression, file.path(output_dir, "regression_results.csv"))
  }
  
  cat("\n=== Análisis completado ===\n")
  
  # Retornar resultados
  return(list(
    data = data,
    statistics = stats,
    changes = cambios,
    regression = regression,
    plots = plots
  ))
}

# ============================================================================
# EJEMPLO DE USO
# ============================================================================

# Ejecutar análisis completo y mostrar gráficos
if (interactive()) {
  # Cargar datos
  results <- run_complete_analysis("denuncias.csv", output_dir = "output_analysis")
  
  # Mostrar gráficos individualmente
  print(results$plots$denuncias)
  print(results$plots$condenas)
  print(results$plots$inocentes)
  print(results$plots$all_categories)
  print(results$plots$proportions)
  
  # Acceder a datos procesados
  # results$data
  # results$statistics
  # results$changes
  # results$regression
}
