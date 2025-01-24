#' Calculate overall cultivar frequency (OCF)
#'
#' @param dfr A data frame
#' @param vname variety names
#' @param hh household names or codes
#' @param community community names
#' @param location location of the varieties
#' @param quantiles quantiles to get
#'
#' @returns A data frame with OCF percentages and metrics to calculate the OCF
#' @export
#'
#' @examples

OCF <- function(dfr, vname, hh, community, location,quantiles=c(0.25,0.5,0.75)){
    if (inherits(dfr, "tibble")) {
        dfr <- as.data.frame(dfr, stringsAsFactors = FALSE)
    }
    index_varname <- which(names(dfr) == vname)
    names(dfr)[index_varname] <- "variety_name"
    index_varname <- which(names(dfr) == hh)
    names(dfr)[index_varname] <- "hh"
    index_varname <- which(names(dfr) == community)
    names(dfr)[index_varname] <- "community"
    index_varname <- which(names(dfr) == location)
    names(dfr)[index_varname] <- "location"
    dfr_ccf <- CCF(dfr, "variety_name", "hh", "community", "location")
    ncom <- length(unique(dfr[, "community"] %>%
                              purrr::as_vector()))
    dfr_ccf <- dfr_ccf %>%
        dplyr::group_by(variety_name) %>%
        dplyr::mutate(sumccf = sum(ccf, na.rm = TRUE)) %>%
        dplyr::ungroup()
    dfr_ocf <- dfr_ccf %>%
        dplyr::mutate(OCF = sumccf / ncom)

    quantiles <- stats::quantile(dfr_ocf$OCF,probs = quantiles)

    dfr_ocf <- dfr_ocf |>
        dplyr::mutate(OCF_scale = dplyr::case_when(
            OCF < quantiles[1] ~ "Very few households",
            OCF >= quantiles[1] & OCF < quantiles[2] ~ "Few households",
            OCF >= quantiles[2] & OCF < quantiles[3] ~ "Many households",
            OCF >= quantiles[3] ~ "most households",
        ))

    return(dfr_ocf)
}


#' Calculate cumulative cutlivar frequency
#'
#' @param dfr A data frame
#' @param vname variety names
#' @param hh household names or codes
#' @param community community
#' @param location location
#'
#' @returns A data frame with the CCf values
#' @export
#'
#' @examples
CCF <- function(dfr,
                 vname,
                 hh,
                 community,
                 location){
    if (inherits(dfr, "tibble")) {
        dfr <- as.data.frame(dfr, stringsAsFactors = FALSE)
    }
    index_varname <- which(names(dfr) == vname)
    names(dfr)[index_varname] <- "variety_name"
    index_varname <- which(names(dfr) == hh)
    names(dfr)[index_varname] <- "hh"
    index_varname <- which(names(dfr) == community)
    names(dfr)[index_varname] <- "community"
    index_varname <- which(names(dfr) == location)
    names(dfr)[index_varname] <- "location"
    dfr_ccf <- CCF(dfr, "variety_name", "hh", "community", "location")
    ncom <- length(unique(dfr[, "community"] %>% purrr::as_vector()))
    dfr_ccf <- dfr_ccf %>%
        dplyr::group_by(variety_name) %>%
        dplyr::mutate(sumccf = sum(ccf, na.rm = TRUE)) %>%
        dplyr::ungroup()
    dfr_ocf <- dfr_ccf %>%
        dplyr::mutate(OCF = sumccf / ncom)
    dfr_ocf <- dfr_ocf %>%
        dplyr::mutate(
        OCF_scale = dplyr::case_when(
            OCF <
                1 ~ "very few households",
            OCF < 5 & OCF >= 1 ~ "few households",
            OCF < 25 &
                OCF >= 5 ~ "many households",
            OCF > 25 ~ "most households",
        )
    )
    return(dfr_ocf)
}

ccf <- sumccf <- variety_name <- quantile <- NULL
