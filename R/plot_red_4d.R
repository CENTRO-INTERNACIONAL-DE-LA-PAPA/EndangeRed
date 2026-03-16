#' Plot 4D red-listing class combinations as faceted heatmap
#'
#' Build a 4D matrix plot from the output of [get_red_listing()]. The tile
#' matrix shows `OCF_scale_num` (x) by `RCF_scale_num` (y), and facets by
#' `GDF_num` (rows) and `ADF_num` (columns). Tile fill encodes grouped risk
#' zones (Danger, Transition, Safe), while labels show the number of varieties
#' in each class combination.
#'
#' @param results A data frame returned by [get_red_listing()].
#' @param variety_col String. Variety identifier column in `results` used to
#'   count unique varieties (default: `"final_variety_name"`).
#' @param palette Named character vector with colors for `Danger`,
#'   `Transition`, and `Safe`.
#'
#' @return A ggplot object.
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
      Danger = "#EE7733",
      Transition = "#d8d8d8ff",
      Safe = "#0077BB"
    )) {
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

  expected_palette_names <- c("Danger", "Transition", "Safe")
  if (!all(expected_palette_names %in% names(palette))) {
    stop(
      "`palette` must be a named vector with names: ",
      paste(expected_palette_names, collapse = ", "),
      call. = FALSE
    )
  }

  v <- rlang::sym(variety_col)

  plot_df <- results %>%
    dplyr::distinct(
      !!v,
      OCF_scale_num,
      RCF_scale_num,
      GDF_num,
      ADF_num
    ) %>%
    dplyr::filter(
      !is.na(OCF_scale_num),
      !is.na(RCF_scale_num),
      !is.na(GDF_num),
      !is.na(ADF_num)
    ) %>%
    dplyr::count(
      OCF_scale_num,
      RCF_scale_num,
      GDF_num,
      ADF_num,
      name = "n_varieties"
    ) %>%
    tidyr::complete(
      OCF_scale_num = 1:4,
      RCF_scale_num = 1:4,
      GDF_num = 1:4,
      ADF_num = 1:4,
      fill = list(n_varieties = 0)
    ) %>%
    dplyr::mutate(
      metrics_sum = OCF_scale_num +
        RCF_scale_num +
        GDF_num +
        ADF_num,
      risk_band = dplyr::case_when(
        metrics_sum <= 7 ~ "Danger",
        metrics_sum <= 11 ~ "Transition",
        TRUE ~ "Safe"
      ),
      risk_band = factor(
        risk_band,
        levels = c("Danger", "Transition", "Safe")
      ),
      OCF_axis = factor(
        OCF_scale_num,
        levels = 1:4,
        labels = c(
          "1 Very few households",
          "2 Few households",
          "3 Many households",
          "4 Most households"
        )
      ),
      RCF_axis = factor(
        RCF_scale_num,
        levels = 1:4,
        labels = c(
          "1 Very scarce",
          "2 Scarce",
          "3 Common",
          "4 Abundant"
        )
      ),
      ADF_facet = factor(
        ADF_num,
        levels = 1:4,
        labels = c(
          "ADF 1 Very narrow altitudinal",
          "ADF 2 Narrow altitudinal",
          "ADF 3 Medium altitudinal",
          "ADF 4 Wide altitudinal"
        )
      ),
      GDF_facet = factor(
        GDF_num,
        levels = 4:1,
        labels = c(
          "GDF 4 Wide range",
          "GDF 3 Medium range",
          "GDF 2 Narrow range",
          "GDF 1 Very narrow range"
        )
      ),
      label = ifelse(n_varieties == 0, "", n_varieties),
      txt_col = dplyr::if_else(
        risk_band == "Safe",
        "#FFFFFF",
        "#000000"
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = OCF_axis,
      y = RCF_axis,
      fill = risk_band
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = label,
        color = txt_col
      ),
      size = 2.7,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::facet_grid(
      rows = ggplot2::vars(GDF_facet),
      cols = ggplot2::vars(ADF_facet),
      switch = "both"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_manual(
      values = palette[c("Danger", "Transition", "Safe")],
      name = "Risk zone",
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "4D Red Listing Matrix",
      subtitle = "Tile count = number of varieties in each OCF x RCF x GDF x ADF class combination",
      x = "OCF class",
      y = "RCF class"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.placement = "outside",
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 9)
    )
}

ADF_facet <- GDF_facet <- OCF_axis <- RCF_axis <- label <- metrics_sum <-
  n_varieties <- risk_band <- txt_col <- NULL
