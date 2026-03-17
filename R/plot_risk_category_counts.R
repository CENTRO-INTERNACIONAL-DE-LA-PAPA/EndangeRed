#' Plot unique variety counts by risk category
#'
#' Build a bar chart with the number of unique varieties per risk category from
#' [get_red_listing()] output.
#'
#' @param results A data frame returned by [get_red_listing()].
#' @param variety_col String. Variety identifier column in `results` used to
#'   count unique varieties (default: `"final_variety_name"`).
#' @param risk_col String. Risk category column in `results`
#'   (default: `"risk_category"`).
#' @param palette Named character vector with colors for risk categories.
#' @param drop_empty Logical. If `TRUE`, categories with zero counts are omitted.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' data(Huancavelica_2013)
#' results <- get_red_listing(Huancavelica_2013)
#' p <- plot_risk_category_counts(results)
#' print(p)
plot_risk_category_counts <- function(
    results,
    variety_col = "final_variety_name",
    risk_col = "risk_category",
    palette = c(
      "Critically At Risk" = "red",
      "At Risk" = "orange",
      "Potentially Vulnerable" = "yellow",
      "Stable, Low Concern" = "#8CD665",
      "Secure" = "#1E7124"
    ),
    drop_empty = FALSE) {
  required_cols <- c(variety_col, risk_col)
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
  r <- rlang::sym(risk_col)

  plot_df <- results %>%
    dplyr::filter(!is.na(!!v), !is.na(!!r)) %>%
    dplyr::distinct(!!v, !!r) %>%
    dplyr::count(!!r, name = "n_varieties") %>%
    dplyr::rename(risk_category = !!r) %>%
    dplyr::mutate(
      risk_category = factor(
        as.character(risk_category),
        levels = risk_levels,
        ordered = TRUE
      )
    )

  if (!drop_empty) {
    plot_df <- plot_df %>%
      tidyr::complete(
        risk_category = factor(risk_levels, levels = risk_levels, ordered = TRUE),
        fill = list(n_varieties = 0)
      )
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = risk_category, y = n_varieties, fill = risk_category)
  ) +
    ggplot2::geom_col(width = 0.8, color = "black", linewidth = 0.2) +
    ggplot2::geom_text(
      ggplot2::aes(label = n_varieties),
      vjust = -0.35,
      fontface = "bold",
      size = 4
    ) +
    ggplot2::scale_fill_manual(values = palette, drop = drop_empty) +
    ggplot2::labs(
      x = "Risk Category",
      y = "Unique Varieties",
      title = "Unique Varieties per Risk Category"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)
    )
}

n_varieties <- risk_category <- NULL
