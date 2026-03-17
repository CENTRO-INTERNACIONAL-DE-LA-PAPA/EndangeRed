#' Plot 4D red-listing class combinations as a 2D projected matrix
#'
#' Build a 4x4 matrix plot from the output of [get_red_listing()]. This projects
#' 4 variables onto a 2D grid by taking the maximum of pairs: X-axis represents
#' the maximum of `GDF_num` and `RCF_scale_num`, while the Y-axis represents the
#' maximum of `OCF_scale_num` and `ADF_num`. Tile fill follows the official
#' `metrics_sum` decision bands:
#' 4 (`Critically At Risk`), 5-7 (`At Risk`), 8-11 (`Potentially Vulnerable`),
#' 12-15 (`Stable, Low Concern`), and 16 (`Secure`). Labels show the number of
#' unique varieties and the observed `metrics_sum` range in each cell.
#'
#' @param results A data frame returned by [get_red_listing()].
#' @param variety_col String. Variety identifier column in `results` used to
#'   count unique varieties (default: `"final_variety_name"`).
#' @param palette Named character vector with colors for `Critically At Risk`,
#'   `At Risk`, `Potentially Vulnerable`, `Stable, Low Concern`, and `Secure`.
#' @param return_tables Logical. If `FALSE` (default), returns only the plot.
#'   If `TRUE`, returns a list with:
#'   `plot`, `square_summary`, `limiting_metric_table`, and
#'   `limiting_metric_detail`.
#'
#' @return A ggplot object, or a list (when `return_tables = TRUE`).
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

  grid_matrix <- expand.grid(X = 1:4, Y = 1:4) %>%
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      risk_category = dplyr::case_when(
        cell_label == 4 ~ "Critically At Risk",
        cell_label >= 5 & cell_label <= 7 ~ "At Risk",
        cell_label >= 8 & cell_label <= 11 ~ "Potentially Vulnerable",
        cell_label >= 12 & cell_label <= 15 ~ "Stable, Low Concern",
        cell_label == 16 ~ "Secure"
      ),
      risk_category = factor(risk_category, levels = risk_levels, ordered = TRUE)
    )

  v <- rlang::sym(variety_col)
  metric_levels <- c("OCF", "RCF", "GDF", "ADF")

  variety_square_df <- results %>%
    dplyr::filter(
      !is.na(!!v),
      !is.na(OCF_scale_num),
      !is.na(RCF_scale_num),
      !is.na(GDF_num),
      !is.na(ADF_num)
    ) %>%
    dplyr::mutate(
      metrics_sum = OCF_scale_num + RCF_scale_num + GDF_num + ADF_num,
      X = pmax(GDF_num, RCF_scale_num),
      Y = pmax(OCF_scale_num, ADF_num),
      cell_label = 2 * (X + Y)
    ) %>%
    dplyr::select(
      !!v,
      X,
      Y,
      cell_label,
      metrics_sum,
      OCF_scale_num,
      RCF_scale_num,
      GDF_num,
      ADF_num
    ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  plot_df <- variety_square_df %>%
    dplyr::group_by(X, Y) %>%
    dplyr::summarise(
      n = dplyr::n_distinct(!!v),
      metrics_sum_min = min(metrics_sum, na.rm = TRUE),
      metrics_sum_max = max(metrics_sum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(X = 1:4, Y = 1:4, fill = list(n = 0)) %>%
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      metrics_sum_label = dplyr::case_when(
        n == 0 ~ "m = -",
        metrics_sum_min == metrics_sum_max ~ paste0("m = ", metrics_sum_min),
        TRUE ~ paste0("m = ", metrics_sum_min, "-", metrics_sum_max)
      )
    ) %>%
    dplyr::left_join(
      grid_matrix %>% dplyr::select(X, Y, risk_category),
      by = c("X", "Y")
    )

  limiting_metric_detail <- variety_square_df %>%
    dplyr::select(
      !!v,
      X,
      Y,
      cell_label,
      metrics_sum,
      OCF_metric = OCF_scale_num,
      RCF_metric = RCF_scale_num,
      GDF_metric = GDF_num,
      ADF_metric = ADF_num
    ) %>%
    tidyr::pivot_longer(
      cols = c(OCF_metric, RCF_metric, GDF_metric, ADF_metric),
      names_to = "metric",
      values_to = "metric_value"
    ) %>%
    dplyr::group_by(!!v, X, Y, cell_label, metrics_sum) %>%
    dplyr::mutate(
      min_metric_value = min(metric_value, na.rm = TRUE),
      n_limiting_metrics = sum(metric_value == min_metric_value, na.rm = TRUE),
      is_limiting_metric = metric_value == min_metric_value,
      contribution_weight = dplyr::if_else(
        is_limiting_metric,
        1 / n_limiting_metrics,
        0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is_limiting_metric) %>%
    dplyr::mutate(
      metric = dplyr::recode(
        metric,
        OCF_metric = "OCF",
        RCF_metric = "RCF",
        GDF_metric = "GDF",
        ADF_metric = "ADF"
      ),
      metric = factor(metric, levels = metric_levels, ordered = TRUE),
      risk_category = dplyr::case_when(
        cell_label == 4 ~ "Critically At Risk",
        cell_label >= 5 & cell_label <= 7 ~ "At Risk",
        cell_label >= 8 & cell_label <= 11 ~ "Potentially Vulnerable",
        cell_label >= 12 & cell_label <= 15 ~ "Stable, Low Concern",
        cell_label == 16 ~ "Secure"
      ),
      risk_category = factor(risk_category, levels = risk_levels, ordered = TRUE)
    )

  limiting_metric_table <- limiting_metric_detail %>%
    dplyr::group_by(X, Y, cell_label, risk_category, metric) %>%
    dplyr::summarise(
      n_varieties_raw = dplyr::n(),
      n_varieties_weighted = sum(contribution_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      X = 1:4,
      Y = 1:4,
      metric = factor(metric_levels, levels = metric_levels, ordered = TRUE),
      fill = list(n_varieties_raw = 0, n_varieties_weighted = 0)
    ) %>%
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      risk_category = dplyr::case_when(
        cell_label == 4 ~ "Critically At Risk",
        cell_label >= 5 & cell_label <= 7 ~ "At Risk",
        cell_label >= 8 & cell_label <= 11 ~ "Potentially Vulnerable",
        cell_label >= 12 & cell_label <= 15 ~ "Stable, Low Concern",
        cell_label == 16 ~ "Secure"
      ),
      risk_category = factor(risk_category, levels = risk_levels, ordered = TRUE)
    ) %>%
    dplyr::group_by(X, Y, cell_label, risk_category) %>%
    dplyr::mutate(
      total_weighted = sum(n_varieties_weighted, na.rm = TRUE),
      pct_weighted = dplyr::if_else(
        total_weighted > 0,
        n_varieties_weighted / total_weighted,
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Y, X, metric)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = X, y = Y)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = risk_category),
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = cell_label),
      hjust = 1, vjust = 0, nudge_x = 0.4, nudge_y = -0.4,
      size = 5, color = "black", alpha = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("n = ", n, "\n", metrics_sum_label)),
      size = 4.2, fontface = "bold", lineheight = 1.0
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
    square_summary <- plot_df %>%
      dplyr::select(dplyr::all_of(c(
        "X",
        "Y",
        "cell_label",
        "risk_category",
        "n",
        "metrics_sum_min",
        "metrics_sum_max",
        "metrics_sum_label"
      )))

    return(list(
      plot = p,
      square_summary = square_summary,
      limiting_metric_table = limiting_metric_table,
      limiting_metric_detail = limiting_metric_detail
    ))
  }

  p
}

# Prevent R CMD check notes for unquoted variables.
X <- Y <- cell_label <- risk_category <- n <- GDF_num <- RCF_scale_num <-
  OCF_scale_num <- ADF_num <- metrics_sum <- metrics_sum_min <-
  metrics_sum_max <- metrics_sum_label <- OCF_metric <- RCF_metric <-
  GDF_metric <- ADF_metric <-
  metric <- metric_value <- min_metric_value <- n_limiting_metrics <-
  is_limiting_metric <- contribution_weight <- n_varieties_raw <-
  n_varieties_weighted <- total_weighted <- pct_weighted <- NULL
