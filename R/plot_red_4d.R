#' Plot 4D red-listing classes as a theoretical 4x4 matrix
#'
#' Build a 4x4 matrix plot from [get_red_listing()] output. The matrix projects
#' 4 variables into 2 coordinates:
#' X = `pmax(GDF_num, RCF_scale_num)` and Y = `pmax(OCF_scale_num, ADF_num)`.
#'
#' Tile colors follow the theoretical matrix bands (from `cell_label = 2 * (X + Y)`):
#' 4, 5-7, 8-11, 12-15, and 16.
#'
#' The plot label shows only unique-variety counts per square (`n = ...`).
#'
#' @param results A data frame returned by [get_red_listing()].
#' @param variety_col String. Variety identifier column in `results` used to
#'   count unique varieties (default: `"final_variety_name"`).
#' @param palette Named character vector with colors for `Critically At Risk`,
#'   `At Risk`, `Potentially Vulnerable`, `Stable, Low Concern`, and `Secure`.
#' @param return_tables Logical. If `FALSE` (default), returns only the plot.
#'   If `TRUE`, returns a list with `plot`, `square_summary`,
#'   `square_sum_breakdown`, `square_metric_breakdown`, and
#'   `variety_assignment`.
#'
#' @return A ggplot object, or a list when `return_tables = TRUE`.
#' @export
#'
#' @examples
#' data(Huancavelica_2013)
#' results <- get_red_listing(Huancavelica_2013)
#' p <- plot_red_4d(results)
#' print(p)
plot_red_4d <- function(
    results,
    variety_col = "final_variety_name",
    palette = c(
      "Critically At Risk" = "#FF0000",
      "At Risk" = "#FFA500",
      "Potentially Vulnerable" = "#FFFF00",
      "Stable, Low Concern" = "#92D050",
      "Secure" = "#008000"
    ),
    return_tables = FALSE) {
  required_cols <- c(
    variety_col,
    "OCF_scale_num",
    "RCF_scale_num",
    "GDF_num",
    "ADF_num"
  )
  missing_cols <- setdiff(required_cols, names(results))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in `results`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  risk_levels <- c(
    "Critically At Risk",
    "At Risk",
    "Potentially Vulnerable",
    "Stable, Low Concern",
    "Secure"
  )
  if (!all(risk_levels %in% names(palette))) {
    stop(
      "`palette` must be a named vector with names: ",
      paste(risk_levels, collapse = ", "),
      call. = FALSE
    )
  }

  v <- rlang::sym(variety_col)

  variety_assignment <- results %>%
    dplyr::filter(
      !is.na(!!v),
      !is.na(OCF_scale_num),
      !is.na(RCF_scale_num),
      !is.na(GDF_num),
      !is.na(ADF_num)
    ) %>%
    dplyr::transmute(
      !!v,
      OCF_scale_num = as.integer(OCF_scale_num),
      RCF_scale_num = as.integer(RCF_scale_num),
      GDF_num = as.integer(GDF_num),
      ADF_num = as.integer(ADF_num),
      metrics_sum = OCF_scale_num + RCF_scale_num + GDF_num + ADF_num,
      X = pmax(GDF_num, RCF_scale_num),
      Y = pmax(OCF_scale_num, ADF_num)
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      theoretical_range = dplyr::case_when(
        cell_label == 4 ~ "4",
        cell_label >= 5 & cell_label <= 7 ~ "5-7",
        cell_label >= 8 & cell_label <= 11 ~ "8-11",
        cell_label >= 12 & cell_label <= 15 ~ "12-15",
        cell_label == 16 ~ "16"
      ),
      risk_category = dplyr::case_when(
        theoretical_range == "4" ~ "Critically At Risk",
        theoretical_range == "5-7" ~ "At Risk",
        theoretical_range == "8-11" ~ "Potentially Vulnerable",
        theoretical_range == "12-15" ~ "Stable, Low Concern",
        theoretical_range == "16" ~ "Secure"
      ),
      risk_category = factor(risk_category, levels = risk_levels, ordered = TRUE)
    ) %>%
    dplyr::ungroup()

  square_summary <- variety_assignment %>%
    dplyr::group_by(X, Y) %>%
    dplyr::summarise(
      n = dplyr::n_distinct(!!v),
      observed_min_sum = min(metrics_sum, na.rm = TRUE),
      observed_max_sum = max(metrics_sum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(X = 1:4, Y = 1:4, fill = list(n = 0L)) %>%
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      theoretical_range = dplyr::case_when(
        cell_label == 4 ~ "4",
        cell_label >= 5 & cell_label <= 7 ~ "5-7",
        cell_label >= 8 & cell_label <= 11 ~ "8-11",
        cell_label >= 12 & cell_label <= 15 ~ "12-15",
        cell_label == 16 ~ "16"
      ),
      risk_category = dplyr::case_when(
        theoretical_range == "4" ~ "Critically At Risk",
        theoretical_range == "5-7" ~ "At Risk",
        theoretical_range == "8-11" ~ "Potentially Vulnerable",
        theoretical_range == "12-15" ~ "Stable, Low Concern",
        theoretical_range == "16" ~ "Secure"
      ),
      risk_category = factor(risk_category, levels = risk_levels, ordered = TRUE)
    ) %>%
    dplyr::arrange(Y, X)

  square_sum_breakdown <- variety_assignment %>%
    dplyr::group_by(X, Y, metrics_sum) %>%
    dplyr::summarise(n_varieties = dplyr::n_distinct(!!v), .groups = "drop") %>%
    tidyr::complete(X = 1:4, Y = 1:4, metrics_sum = 4:16, fill = list(n_varieties = 0L)) %>%
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      possible_min_sum = X + Y + 2,
      possible_max_sum = 2 * (X + Y),
      is_possible_for_square = metrics_sum >= possible_min_sum & metrics_sum <= possible_max_sum
    ) %>%
    dplyr::arrange(Y, X, metrics_sum)

  square_metric_breakdown <- variety_assignment %>%
    dplyr::group_by(
      X,
      Y,
      OCF_scale_num,
      RCF_scale_num,
      GDF_num,
      ADF_num,
      metrics_sum
    ) %>%
    dplyr::summarise(n_varieties = dplyr::n_distinct(!!v), .groups = "drop") %>%
    dplyr::mutate(cell_label = 2 * (X + Y)) %>%
    dplyr::arrange(Y, X, metrics_sum, OCF_scale_num, RCF_scale_num, GDF_num, ADF_num)

  p <- ggplot2::ggplot(square_summary, ggplot2::aes(x = X, y = Y)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = risk_category),
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("n = ", n)),
      size = 4.4,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = theoretical_range),
      hjust = 1,
      vjust = 0,
      nudge_x = 0.42,
      nudge_y = -0.42,
      size = 4.2,
      color = "black",
      alpha = 0.55
    ) +
    ggplot2::scale_fill_manual(values = palette, guide = "none", drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = 1:4,
      labels = c("<1%", "1-5%", "5-15%", ">15%"),
      expand = c(0, 0),
      name = "RCF - Frecuencia Relativa del Cultivar",
      sec.axis = ggplot2::sec_axis(
        transform = ~.,
        breaks = 1:4,
        labels = c("<3km", "3-8km", "8-12km", ">12km"),
        name = "GDF - Frecuencia de Dispersion Geografica"
      )
    ) +
    ggplot2::scale_y_continuous(
      breaks = 1:4,
      labels = c("<10%", "10-30%", "30-50%", ">50%"),
      expand = c(0, 0),
      name = "OCF - Frecuencia General del Cultivar",
      sec.axis = ggplot2::sec_axis(
        transform = ~.,
        breaks = 1:4,
        labels = c("<200m", "200-350m", "350-500m", ">500m"),
        name = "ADF - Frecuencia de Dispersion Altitudinal"
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 11, color = "black"),
      axis.text.y = ggplot2::element_text(size = 11, color = "black", angle = 90, hjust = 0.5),
      axis.text.y.right = ggplot2::element_text(size = 11, color = "black", angle = 90, hjust = 0.5),
      axis.ticks = ggplot2::element_line(color = "black"),
      axis.ticks.length = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::coord_equal()

  if (isTRUE(return_tables)) {
    return(list(
      plot = p,
      square_summary = square_summary,
      square_sum_breakdown = square_sum_breakdown,
      square_metric_breakdown = square_metric_breakdown,
      variety_assignment = variety_assignment
    ))
  }

  p
}

# Prevent R CMD check notes for unquoted variables.
X <- Y <- n <- OCF_scale_num <- RCF_scale_num <- GDF_num <- ADF_num <-
  metrics_sum <- cell_label <- theoretical_range <- risk_category <-
  observed_min_sum <- observed_max_sum <- n_varieties <- possible_min_sum <-
  possible_max_sum <- is_possible_for_square <- NULL
