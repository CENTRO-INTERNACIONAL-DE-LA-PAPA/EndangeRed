#' This function return the categorized varieties by risk
#' 
#' @param data A data frame with the data.
#' @param variety_name_col String. Column name in `data` containing the variety names (default: "final_variety_name").
#' @param community String. Column name in `data` for community identifiers (default: "comunidad").
#' @param household String. Column name in `data` for household identifiers (default: "household").
#' @param location String. Column name in `data` for location/region (default: "region").
#' @param count String. Column name in `data` containing the per-household variety counts (default: "cantidad").
#' @return A tibble with per-record metrics and a `risk_category` factor summarizing vulnerability per variety.
#' @export
#'
#' @examples
#' data(Huancavelica_2013)


get_red_listing <- function(data, variety_name_col="final_variety_name", community="comunidad", household = "household", location="region", count="cantidad"){
    
    OCF_df = OCF(
        dfr = data,
        vname = variety_name_col,
        hh = household,
        community = community,
        location = location
    ) %>%
        dplyr::select(community, variety_name, OCF) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(OCF_scale = dplyr::case_when(
            OCF < 10 ~ "very few households",
            OCF >= 10 & OCF < 30 ~ "few households",
            OCF >= 30 & OCF < 50 ~ "many households",
            OCF >= 50 ~ "most households"
        ))
    
    RCF_df = RCF(
        dfr = data,
        vname = variety_name_col,
        hh = household,
        nsvarie = count,
        community = community,
        location = location
    ) %>%
        dplyr::select(community, variety_name, RCF) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(RCF_scale = dplyr::case_when(
            RCF < 1 ~ "very scarce",
            RCF >= 1 & RCF < 5 ~ "scarce",
            RCF >= 5 & RCF < 15 ~ "common",
            RCF >= 15 ~ "abundant"
        ))
    
    full_df <- data %>%
        dplyr::left_join(
            OCF_df,
            dplyr::join_by(!!rlang::sym(variety_name_col) == variety_name,
                           !!rlang::sym(community)       == community)
        ) %>%
        dplyr::left_join(
            RCF_df,
            dplyr::join_by(!!rlang::sym(variety_name_col) == variety_name,
                           !!rlang::sym(community)       == community)
        )
    
    result_df = full_df %>%
        dplyr::group_by(!!dplyr::sym((variety_name_col))) %>%
        tidyr::nest() %>%
        dplyr::mutate(res = purrr::map(data, calculate_distances)) %>%
        tidyr::unnest(data) %>%
        tidyr::unnest(res) %>%
        dplyr::mutate(
            dist_classes = dplyr::case_when(
                max_dist_m < 3000 ~ "very narrow range",
                max_dist_m >= 3000 & max_dist_m < 8000 ~ "narrow range",
                max_dist_m >= 8000 &
                    max_dist_m < 12000 ~ "medium range",
                max_dist_m >= 12000 ~ "wide range"
            )
        ) %>%
        dplyr::mutate(
            elevation_range_classes = dplyr::case_when(
                elevation_range < 200 ~ "very narrow altitudinal range",
                elevation_range >= 200 &
                    elevation_range < 350 ~ "narrow altitudinal range",
                elevation_range >= 350 &
                    elevation_range < 500 ~ "medium altitudinal range",
                elevation_range >= 500 ~ "wide altitudinal range"
            )
        ) %>%
        dplyr::ungroup()
    
    result_df$OCF_scale = factor(
        result_df$OCF_scale,
        levels = c(
            "very few households",
            "few households",
            "many households",
            "most households"
        )
    )
    result_df$RCF_scale = factor(result_df$RCF_scale,
                                 levels = c("very scarce", "scarce", "common", "abundant"))
    result_df$dist_classes = factor(
        result_df$dist_classes,
        levels = c("very narrow range", "narrow range", "medium range", "wide range")
    )
    result_df$elevation_range_classes = factor(
        result_df$elevation_range_classes,
        levels = c(
            "very narrow altitudinal range",
            "narrow altitudinal range",
            "medium altitudinal range",
            "wide altitudinal range"
        )
    )
    
    categorized_data = result_df %>%
        dplyr::mutate(
            OCF_scale_num = as.integer(OCF_scale),
            RCF_scale_num = as.integer(RCF_scale),
            GDF_num = as.integer(dist_classes),
            ADF_num = as.integer(elevation_range_classes)
        ) %>%
        dplyr::select(
            !!rlang::sym(variety_name_col),
            OCF_scale_num,
            RCF_scale_num,
            GDF_num,
            ADF_num
        ) %>%
        dplyr::distinct() %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(metrics_sum = OCF_scale_num + RCF_scale_num + GDF_num + ADF_num) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
            risk_category = dplyr::case_when(
                metrics_sum <= 4 ~ "Critically At Risk",
                metrics_sum >= 5 & metrics_sum <= 7 ~ "At Risk",
                metrics_sum >= 8 & metrics_sum <= 11 ~ "Potentially Vulnerable",
                metrics_sum >= 12 & metrics_sum <= 15 ~ "Stable, Low Concern",
                metrics_sum >= 16 ~ "Secure"
            )
        )
    
    result_df <- result_df %>%
        dplyr::left_join(categorized_data, by = variety_name_col) %>%
        dplyr::mutate(risk_category = as.factor(risk_category))
    
    return(result_df)
}

ADF_num <- GDF_num <- OCF_scale <- OCF_scale_num <- RCF_scale <- RCF_scale_num <- 
dist_classes <- elevation_range_classes <- final_variety_name <- res <- risk_category <- NULL