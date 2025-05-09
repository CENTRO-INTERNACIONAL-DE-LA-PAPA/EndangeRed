
#' Plot the RCF vs OCF values to identify Red Listing varieties
#'
#' @param OCF_df A data frame generated by the OCF function
#' @param RCF_df A data frame generated by the RCF function
#' @param quantiles Which quantiles you want to break the RCf and OCF distributions
#' @param type Normal scale or Logarithm scale. Default value is log scale.
#'
#' @returns A plot where we can see the RCF vs OCF in normal or log scale.
#' @export
#'
#' @examples
#' data(varieties_data)
#' ocf_data <- OCF(dfr=varieties_data,
#' vname="variety_name",
#' hh="household_code",
#' community="community",
#' location="location")
#' rcf_data <- RCF(dfr=varieties_data,
#' vname="variety_name",
#' hh="household_code",
#' nsvarie="number_of_tubers",
#' community="community",
#' location="location")
#' Plot_Variable_Red_Listing(ocf_data, rcf_data)
Plot_Variable_Red_Listing <- function(OCF_df,
                             RCF_df,
                             quantiles = c(0.25, 0.5, 0.75),
                             type = "log") {


    if(type == "normal"){

        quantiles_OCF <- OCF_df %>%
            dplyr::summarize(
                Q1_ocf = quantile(OCF, probs = quantiles[1], na.rm = TRUE),
                Median_ocf = quantile(OCF, probs = quantiles[2], na.rm = TRUE),
                Q3_ocf = quantile(OCF, probs = quantiles[3], na.rm = TRUE),
                .groups = "drop"
            )

        quantiles_RCF <- RCF_df %>%
            dplyr::summarize(
                Q1_rcf = quantile(RCF, probs = quantiles[1], na.rm = TRUE),
                Median_rcf = quantile(RCF, probs = quantiles[2], na.rm = TRUE),
                Q3_rcf = quantile(RCF, probs = quantiles[3], na.rm = TRUE),
                .groups = "drop"
            )

        q1_ocf    <- quantiles_OCF$Q1_ocf[1]
        median_ocf <- quantiles_OCF$Median_ocf[1]
        q3_ocf <- quantiles_OCF$Q3_ocf[1]
        q1_rcf    <- quantiles_RCF$Q1_rcf[1]
        median_rcf <- quantiles_RCF$Median_rcf[1]
        q3_rcf <- quantiles_RCF$Q3_rcf[1]


        joined_df <- RCF_df %>%
            dplyr::left_join(OCF_df, by = dplyr::join_by(variety_name,
                                           community))

        plt <- ggplot2::ggplot(joined_df, ggplot2::aes(OCF, RCF)) +
            ggplot2::annotate("rect",
                xmin = -Inf, xmax = q1_ocf,
                ymin = -Inf, ymax = q1_rcf,
                fill = "#FF9C4CFF", alpha = 0.7
            ) +
            ggplot2::annotate("rect",
                xmin = q1_ocf, xmax = median_ocf,
                ymin = -Inf, ymax = q1_rcf,
                fill = "#FF9C4CFF", alpha = 0.4
            ) +
            ggplot2::annotate("rect",
                xmin = -Inf, xmax = q1_ocf,
                ymin = q1_rcf, ymax = median_rcf,
                fill = "#FF9C4CFF", alpha = 0.4
            ) +
            ggplot2::annotate("rect",
                              xmin = q3_ocf, xmax = Inf,
                              ymin = q3_rcf, ymax = Inf,
                              fill = "#8BAC54FF", alpha = 0.6
            ) +
            ggplot2::annotate("rect",
                              xmin = median_ocf, xmax = q3_ocf,
                              ymin = q3_rcf, ymax = Inf,
                              fill = "#8BAC54FF", alpha = 0.2
            ) +
            ggplot2::annotate("rect",
                              xmin = q3_ocf, xmax = Inf,
                              ymin = median_rcf, ymax = q3_rcf,
                              fill = "#8BAC54FF", alpha = 0.2
            ) +
            ggplot2::geom_point(colour = "#9386A6FF", size = 2, alpha = 0.5) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Q1_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Median_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Q3_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_hline(yintercept = quantiles_RCF$Q1_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_hline(yintercept = quantiles_RCF$Median_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_hline(yintercept = quantiles_RCF$Q3_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_smooth() +
            ggplot2::labs(title = "Scatter Plot with Quantiles") +
            ggplot2::theme_minimal()

        # Density plot for OCF (top)
        density_x <- ggplot2::ggplot(joined_df, ggplot2::aes(x = OCF)) +
            ggplot2::geom_density(fill = "#C1447EFF", alpha = 0.7) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                axis.title.y = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
            ) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Q1_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Median_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Q3_ocf[1], lty = 2, colour = "red3", linewidth = 1)

        # Density plot for RCF (right)
        density_y <- ggplot2::ggplot(joined_df, ggplot2::aes(x = RCF)) +
            ggplot2::geom_density(fill = "#C1447EFF", alpha = 0.7, bw = 0.1) +
            ggplot2::geom_vline(xintercept = quantiles_RCF$Q1_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_RCF$Median_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_RCF$Q3_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::scale_y_reverse() +
            ggplot2::coord_flip() +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                axis.title.x = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )

        # Combine plots using patchwork
        combined_plot <- (patchwork::plot_spacer() | density_x) /
            (density_y | plt) +
            patchwork::plot_layout(
                widths = c(1, 4),
                heights = c(1, 4)
            ) +
            patchwork::plot_annotation(
                title = "Red Listing of Varieties",
                theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 18, hjust = 0.5, colour = "red4"))
            )

        return(combined_plot)

    } else if(type == "log"){

        quantiles_OCF <- OCF_df %>%
            dplyr::summarize(
                Q1_ocf = quantile(log(OCF), probs = quantiles[1], na.rm = TRUE),
                Median_ocf = quantile(log(OCF), probs = quantiles[2], na.rm = TRUE),
                Q3_ocf = quantile(log(OCF), probs = quantiles[3], na.rm = TRUE),
                .groups = "drop"
            )

        quantiles_RCF <- RCF_df %>%
            dplyr::summarize(
                Q1_rcf = quantile(log(RCF), probs = quantiles[1], na.rm = TRUE),
                Median_rcf = quantile(log(RCF), probs = quantiles[2], na.rm = TRUE),
                Q3_rcf = quantile(log(RCF), probs = quantiles[3], na.rm = TRUE),
                .groups = "drop"
            )

        q1_ocf    <- quantiles_OCF$Q1_ocf[1]
        median_ocf <- quantiles_OCF$Median_ocf[1]
        q3_ocf <- quantiles_OCF$Q3_ocf[1]
        q1_rcf    <- quantiles_RCF$Q1_rcf[1]
        median_rcf <- quantiles_RCF$Median_rcf[1]
        q3_rcf <- quantiles_RCF$Q3_rcf[1]


        joined_df <- RCF_df %>%
            dplyr::left_join(OCF_df, by = dplyr::join_by(variety_name,
                                           community))

        plt <- ggplot2::ggplot(joined_df, ggplot2::aes(log(OCF), log(RCF))) +
            ggplot2::annotate("rect",
                              xmin = -Inf, xmax = q1_ocf,
                              ymin = -Inf, ymax = q1_rcf,
                              fill = "#FF9C4CFF", alpha = 0.7
            ) +
            ggplot2::annotate("rect",
                              xmin = q1_ocf, xmax = median_ocf,
                              ymin = -Inf, ymax = q1_rcf,
                              fill = "#FF9C4CFF", alpha = 0.4
            ) +
            ggplot2::annotate("rect",
                              xmin = -Inf, xmax = q1_ocf,
                              ymin = q1_rcf, ymax = median_rcf,
                              fill = "#FF9C4CFF", alpha = 0.4
            ) +
            ggplot2::annotate("rect",
                              xmin = q3_ocf, xmax = Inf,
                              ymin = q3_rcf, ymax = Inf,
                              fill = "#8BAC54FF", alpha = 0.6
            ) +
            ggplot2::annotate("rect",
                              xmin = median_ocf, xmax = q3_ocf,
                              ymin = q3_rcf, ymax = Inf,
                              fill = "#8BAC54FF", alpha = 0.2
            ) +
            ggplot2::annotate("rect",
                              xmin = q3_ocf, xmax = Inf,
                              ymin = median_rcf, ymax = q3_rcf,
                              fill = "#8BAC54FF", alpha = 0.2
            ) +
            ggplot2::geom_point(colour = "#9386A6FF", size = 2, alpha = 0.5) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Q1_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Median_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Q3_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_hline(yintercept = quantiles_RCF$Q1_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_hline(yintercept = quantiles_RCF$Median_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_hline(yintercept = quantiles_RCF$Q3_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_smooth() +
            ggplot2::labs(title = "Scatter Plot with Quantiles") +
            ggplot2::theme_minimal()

        # Density plot for OCF (top)
        density_x <- ggplot2::ggplot(joined_df, ggplot2::aes(x = log(OCF))) +
            ggplot2::geom_density(fill = "#C1447EFF", alpha = 0.7) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                axis.title.y = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
            ) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Q1_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Median_ocf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_OCF$Q3_ocf[1], lty = 2, colour = "red3", linewidth = 1)

        # Density plot for RCF (right)
        density_y <- ggplot2::ggplot(joined_df, ggplot2::aes(x = log(RCF))) +
            ggplot2::geom_density(fill = "#C1447EFF", alpha = 0.7, bw = 0.1) +
            ggplot2::geom_vline(xintercept = quantiles_RCF$Q1_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_RCF$Median_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::geom_vline(xintercept = quantiles_RCF$Q3_rcf[1], lty = 2, colour = "red3", linewidth = 1) +
            ggplot2::scale_y_reverse() +
            ggplot2::coord_flip() +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                axis.title.x = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )

        # Combine plots using patchwork
        combined_plot <- (patchwork::plot_spacer() | density_x) /
            (density_y | plt) +
            patchwork::plot_layout(
                widths = c(1, 4),
                heights = c(1, 4)
            ) +
            patchwork::plot_annotation(
                title = "Red Listing of Varieties",
                theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 18, hjust = 0.5, colour = "red4"))
            )

        return(combined_plot)
    }

}

community <- NULL
