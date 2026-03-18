#' Plot 4D red-listing classes as a 4x4 matrix
#'
#' This implementation assigns each variety to one of the 16 theoretical
#' scenarios `(X, Y)` where the ideal profile is:
#' - `RCF_scale_num = X`
#' - `GDF_num = X`
#' - `OCF_scale_num = Y`
#' - `ADF_num = Y`
#'
#' Assignment is constrained by the theoretical range of each cell
#' (`4`, `5-7`, `8-11`, `12-15`, `16`) and then solved by minimum distance to
#' the ideal profile, with deterministic tie-breaking.
#'
#' @param results A data frame returned by [get_red_listing()].
#' @param variety_col String. Variety identifier column in `results` used to
#'   count unique varieties (default: `"final_variety_name"`).
#' @param palette Named character vector with colors for `Critically At Risk`,
#'   `At Risk`, `Potentially Vulnerable`, `Stable, Low Concern`, and `Secure`.
#' @param return_tables Logical. If `FALSE` (default), returns only the plot.
#'   If `TRUE`, returns a list with `plot`, `square_summary`,
#'   `square_sum_breakdown`, `square_metric_breakdown`, and
#'   `variety_assignment`. Backward-compatible aliases
#'   `limiting_metric_table` and `limiting_metric_detail` are also returned.
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

  to_band <- function(x) {
    dplyr::case_when(
      x == 4 ~ "4",
      x >= 5 & x <= 7 ~ "5-7",
      x >= 8 & x <= 11 ~ "8-11",
      x >= 12 & x <= 15 ~ "12-15",
      x == 16 ~ "16"
    )
  }

  band_to_risk <- function(x) {
    dplyr::case_when(
      x == "4" ~ "Critically At Risk",
      x == "5-7" ~ "At Risk",
      x == "8-11" ~ "Potentially Vulnerable",
      x == "12-15" ~ "Stable, Low Concern",
      x == "16" ~ "Secure"
    )
  }

  v <- rlang::sym(variety_col)

  base_df <- results %>%
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
      metrics_sum = OCF_scale_num + RCF_scale_num + GDF_num + ADF_num
    ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  grid <- tidyr::expand_grid(X = 1:4, Y = 1:4) %>%
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      theoretical_range = to_band(cell_label),
      risk_category = band_to_risk(theoretical_range),
      risk_category = factor(risk_category, levels = risk_levels, ordered = TRUE)
    )

  # Keep one profile per variety for matrix assignment.
  if (any(duplicated(base_df[[variety_col]]))) {
    base_df <- base_df %>%
      dplyr::group_by(!!v) %>%
      dplyr::arrange(metrics_sum, OCF_scale_num, RCF_scale_num, GDF_num, ADF_num, .by_group = TRUE) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }

  variety_assignment <- base_df %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    tidyr::crossing(
      grid %>%
        dplyr::select(X, Y, cell_label, theoretical_range, risk_category)
    ) %>%
    dplyr::mutate(
      metrics_band = to_band(metrics_sum),
      is_in_theoretical_band = metrics_band == theoretical_range,
      distance = abs(OCF_scale_num - Y) +
        abs(ADF_num - Y) +
        abs(RCF_scale_num - X) +
        abs(GDF_num - X),
      agreement_count = as.integer(OCF_scale_num == Y) +
        as.integer(ADF_num == Y) +
        as.integer(RCF_scale_num == X) +
        as.integer(GDF_num == X),
      label_gap = abs(metrics_sum - cell_label)
    ) %>%
    dplyr::filter(is_in_theoretical_band) %>%
    dplyr::group_by(.row_id) %>%
    dplyr::arrange(
      distance,
      dplyr::desc(agreement_count),
      label_gap,
      cell_label,
      Y,
      X,
      .by_group = TRUE
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      !!v,
      OCF_scale_num,
      RCF_scale_num,
      GDF_num,
      ADF_num,
      metrics_sum,
      metrics_band,
      X,
      Y,
      cell_label,
      theoretical_range,
      is_in_theoretical_band,
      risk_category = factor(risk_category, levels = risk_levels, ordered = TRUE),
      risk_category_observed = factor(
        band_to_risk(metrics_band),
        levels = risk_levels,
        ordered = TRUE
      ),
      distance,
      agreement_count,
      label_gap
    )

  assigned_counts <- variety_assignment %>%
    dplyr::group_by(X, Y) %>%
    dplyr::summarise(n = dplyr::n_distinct(!!v), .groups = "drop")

  square_summary <- grid %>%
    dplyr::left_join(assigned_counts, by = c("X", "Y")) %>%
    dplyr::mutate(
      n = dplyr::coalesce(n, 0L)
    ) %>%
    dplyr::arrange(Y, X)

  square_sum_breakdown <- variety_assignment %>%
    dplyr::group_by(X, Y, metrics_sum) %>%
    dplyr::summarise(n_varieties = dplyr::n_distinct(!!v), .groups = "drop") %>%
    tidyr::complete(X = 1:4, Y = 1:4, metrics_sum = 4:16, fill = list(n_varieties = 0L)) %>%
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      theoretical_range = to_band(cell_label),
      metrics_band = to_band(metrics_sum),
      is_in_theoretical_band = metrics_band == theoretical_range
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
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      theoretical_range = to_band(cell_label),
      metrics_band = to_band(metrics_sum),
      is_in_theoretical_band = metrics_band == theoretical_range
    ) %>%
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
      variety_assignment = variety_assignment,
      limiting_metric_table = square_metric_breakdown,
      limiting_metric_detail = variety_assignment
    ))
  }

  p
}

# Prevent R CMD check notes for unquoted variables.
X <- Y <- n <- OCF_scale_num <- RCF_scale_num <- NULL
GDF_num <- ADF_num <- metrics_sum <- metrics_band <- risk_category <- risk_category_observed <- NULL
cell_label <- theoretical_range <- n_varieties <- is_in_theoretical_band <- NULL
distance <- agreement_count <- label_gap <- NULL
