#' Plot 4D red-listing class combinations as a 2D projected matrix
#'
#' Build a 4x4 matrix plot from the output of [get_red_listing()]. This projects 
#' 4 variables onto a 2D grid by taking the maximum of pairs: X-axis represents 
#' the maximum of `GDF_num` and `RCF_scale_num`, while the Y-axis represents the 
#' maximum of `OCF_scale_num` and `ADF_num`. Tile fill encodes the combined risk 
#' category, while labels show the number of unique varieties in each cell.
#'
#' @param results A data frame returned by [get_red_listing()].
#' @param variety_col String. Variety identifier column in `results` used to
#'   count unique varieties (default: `"final_variety_name"`).
#' @param palette Named character vector with colors for `Critically At Risk`,
#'   `Highly At Risk`, `At Risk`, `Secure`, and `Highly Secure`.
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
      "Critically At Risk" = "#d34e47ff", # Muted brick red
      "Highly At Risk"     = "#FC8D59", # Soft orange
      "At Risk"            = "#FEE08B", # Warm, pale gold (instead of blinding yellow)
      "Secure"             = "#91CF60", # Calm green
      "Highly Secure"      = "#1A9850"  # Deep forest green
    )) {
  
  # 1. Validate required columns
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

  # 2. Validate palette
  expected_palette_names <- c(
    "Critically At Risk", "Highly At Risk", "At Risk", 
    "Secure", "Highly Secure"
  )
  if (!all(expected_palette_names %in% names(palette))) {
    stop(
      "`palette` must be a named vector with names: ",
      paste(expected_palette_names, collapse = ", "),
      call. = FALSE
    )
  }

  # 3. Create the fixed 4x4 theoretical coordinate grid
  grid_matrix <- expand.grid(X = 1:4, Y = 1:4) %>%
    dplyr::mutate(
      cell_label = dplyr::case_when(
        X == 1 & Y == 1 ~ 4,
        X == 2 & Y == 1 ~ 6,
        X == 3 & Y == 1 ~ 8,
        X == 4 & Y == 1 ~ 10,
        
        X == 1 & Y == 2 ~ 6,
        X == 2 & Y == 2 ~ 8,
        X == 3 & Y == 2 ~ 10,
        X == 4 & Y == 2 ~ 12,
        
        X == 1 & Y == 3 ~ 8,
        X == 2 & Y == 3 ~ 10,
        X == 3 & Y == 3 ~ 12,
        X == 4 & Y == 3 ~ 14,
        
        X == 1 & Y == 4 ~ 10,
        X == 2 & Y == 4 ~ 12,
        X == 3 & Y == 4 ~ 14,
        X == 4 & Y == 4 ~ 16
      ),
      risk_category = dplyr::case_when(
        cell_label == 4 ~ "Critically At Risk",
        cell_label == 6 ~ "Highly At Risk",
        cell_label %in% c(8, 10) ~ "At Risk",
        cell_label %in% c(12, 14) ~ "Secure",
        cell_label == 16 ~ "Highly Secure"
      ),
      risk_category = factor(risk_category, levels = expected_palette_names)
    )

  # 4. Process data: Calculate coordinates and count unique varieties
  v <- rlang::sym(variety_col)

  plot_df <- results %>%
    dplyr::filter(
      !is.na(OCF_scale_num),
      !is.na(RCF_scale_num),
      !is.na(GDF_num),
      !is.na(ADF_num)
    ) %>%
    dplyr::mutate(
      X = pmax(GDF_num, RCF_scale_num),
      Y = pmax(OCF_scale_num, ADF_num)
    ) %>%
    dplyr::group_by(X, Y) %>%
    dplyr::summarise(n = dplyr::n_distinct(!!v), .groups = "drop") %>%
    tidyr::complete(X = 1:4, Y = 1:4, fill = list(n = 0)) %>%
    dplyr::left_join(grid_matrix, by = c("X", "Y"))

  # 5. Plot the matrix
  ggplot2::ggplot(plot_df, ggplot2::aes(x = X, y = Y)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = risk_category), 
      color = "black", 
      linewidth = 0.5
    ) +
    # Theoretical grid text (4, 6, 8...) in the bottom right corner
    ggplot2::geom_text(
      ggplot2::aes(label = cell_label), 
      hjust = 1, vjust = 0, nudge_x = 0.4, nudge_y = -0.4, 
      size = 5, color = "black", alpha = 0.6
    ) +
    # Actual unique counts ('n') centered
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("n = ", n)), 
      size = 5, fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = palette, 
      guide = "none",
      drop = FALSE
    ) +
    # Dual axes setup
    ggplot2::scale_x_continuous(
      breaks = 1:4, expand = c(0, 0), name = "RCF",
      sec.axis = ggplot2::sec_axis(~., breaks = 1:4, name = "GDF")
    ) +
    ggplot2::scale_y_continuous(
      breaks = 1:4, expand = c(0, 0), name = "OCF",
      sec.axis = ggplot2::sec_axis(~., breaks = 1:4, name = "ADF")
    ) +
    ggplot2::labs(
      title = "4D Red Listing Projected Matrix",
      subtitle = "Tile count = number of unique varieties (n) mapped to maximum paired scores"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.text = ggplot2::element_text(size = 11, color = "black"),
      axis.ticks = ggplot2::element_line(color = "black"),
      axis.ticks.length = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::coord_equal()
}

# Prevent R CMD check notes for unquoted variables in dplyr/ggplot pipelines
X <- Y <- cell_label <- risk_category <- n <- GDF_num <- RCF_scale_num <- 
  OCF_scale_num <- ADF_num <- NULL