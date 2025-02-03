#' Calculate overall cultivar frequency (OCF)
#'
#' @param dfr A data frame
#' @param vname variety names
#' @param hh household names or codes
#' @param community community names
#' @param location location of the varieties
#'
#' @returns A data frame with OCF percentages and metrics to calculate the OCF
#' @export
#'
#' @examples
#' data(varieties_data)
#' ocf_data <- OCF(dfr=varieties_data,
#' vname="variety_name",
#' hh="household_code",
#' community="community",
#' location="location")

OCF <- function(dfr, vname, hh, community, location){
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

    return(dfr_ocf)
}


#' Calculate cumulative cultivar frequency
#'
#' @param dfr A data frame
#' @param vname variety names
#' @param hh household names or codes
#' @param community community
#' @param location location
#' @param pctn Boolean whether calculate percentages
#'
#' @returns A data frame with the CCf values
#' @export
#'
#' @examples
#' data(varieties_data)
#' ccf_data <- CCF(dfr=varieties_data,
#' vname="variety_name",
#' hh="household_code",
#' community="community",
#' location="location")
CCF <- function(dfr, vname, hh, community, location, pctn = TRUE){
    if (inherits(dfr, "tibble")) {
        dfr <- as.data.frame(dfr, stringsAsFactors = FALSE)
    }
    ncut <- NULL
    index_varname <- which(names(dfr) == vname)
    names(dfr)[index_varname] <- "variety_name"
    index_varname <- which(names(dfr) == hh)
    names(dfr)[index_varname] <- "hh"
    index_varname <- which(names(dfr) == community)
    names(dfr)[index_varname] <- "community"
    index_varname <- which(names(dfr) == location)
    names(dfr)[index_varname] <- "location"
    smry_codefarmer <- dfr %>%
        dplyr::group_by(community) %>%
        dplyr::summarise(nhh = dplyr::n_distinct(hh))
    smry_ntotalhhcomu <- dfr %>%
        dplyr::group_by(community, variety_name) %>%
        dplyr::summarize(nhhxvarie = dplyr::n_distinct(hh))
    out <- dplyr::left_join(smry_codefarmer, smry_ntotalhhcomu, by = c("community"))
    out <- out %>%
        dplyr::mutate(ccf = (nhhxvarie/nhh) * 100) %>%
        dplyr::ungroup()
}

ccf <- sumccf <- variety_name <- quantile <- nhh <- nhhxvarie <- NULL
