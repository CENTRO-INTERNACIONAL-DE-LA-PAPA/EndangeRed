#' Calculate relative cultivar frequency (RCF)
#'
#' @param dfr A data frame
#' @param vname variety names
#' @param hh household names or codes
#' @param nsvarie number of varieties sampled in the field per household
#' @param community community
#' @param location location
#' @param is_grouped Whether we calculate the RCF by location, default=TRUE
#' @returns A data frame with RCF percentages and metrics to calculate the RCF
#' @export
#'
#' @examples
#' data(varieties_data)
#' rcf_data <- RCF(dfr=varieties_data,
#' vname="variety_name",
#' hh="household_code",
#' nsvarie="number_of_tubers",
#' community="community",
#' location="location")
RCF <- function(dfr,
                 vname,
                 hh,
                 nsvarie,
                 community,
                 location,
                 is_grouped = TRUE) {

    if (inherits(dfr, "tibble")) {
        dfr <- as.data.frame(dfr, stringsAsFactors = FALSE)
    }

    names(dfr)[which(names(dfr) == vname)] <- "variety_name"
    names(dfr)[which(names(dfr) == hh)] <- "hh"
    names(dfr)[which(names(dfr) == nsvarie)] <- "nsvarie"
    names(dfr)[which(names(dfr) == location)] <- "location"
    names(dfr)[which(names(dfr) == community)] <- "community"

    dfr_hcf <- HCF(dfr, "hh", "nsvarie")

    if (is_grouped) {
        temp_hcfxvarie <- dfr_hcf %>%
            dplyr::group_by(variety_name, location) %>%
            dplyr::summarise(totalhcfxvarie = sum(HCF, na.rm = TRUE)) %>%
            dplyr::ungroup()
        dfr <- dplyr::left_join(dfr, temp_hcfxvarie, by = c("variety_name", "location"))
    }
    else {
        temp_hcfxvarie <- dfr_hcf %>%
            dplyr::group_by(variety_name) %>%
            dplyr::summarise(totalhcfxvarie = sum(HCF, na.rm = TRUE)) %>%
            dplyr::ungroup()
        dfr <- dplyr::left_join(dfr, temp_hcfxvarie, by = c("variety_name"))
    }
    smry_conteo_hh_location <- dfr %>%
        dplyr::group_by(location) %>%
        dplyr::summarise(total_hh = dplyr::n_distinct(hh, na.rm = TRUE))
    dfr <- dplyr::left_join(dfr, smry_conteo_hh_location, by = c("location"))
    dfr_rcf <- dfr %>%
        dplyr::mutate(RCF = 100 * (totalhcfxvarie / total_hh))

    return(dfr_rcf)
}

#' Calculate household cultivar frequency
#'
#' @param dfr A data frame
#' @param hh household names or codes
#' @param nsvarie number of varieties sampled in the field per household
#' @param pctn If we calculate the RCF as percentage, default=TRUE
#'
#' @returns A data frame with the HCf values
#' @export
#'
#' @examples
#' data(varieties_data)
#' hcf_data <- HCF(dfr=varieties_data,
#' hh="household_code",
#' nsvarie="number_of_tubers")
HCF <- function(dfr, hh, nsvarie, pctn = TRUE){

    if (inherits(dfr, "tibble")) {
        dfr <- as.data.frame(dfr, stringsAsFactors = FALSE)
    }
    index_varname <- which(names(dfr) == hh)
    names(dfr)[index_varname] <- "hh"
    index_varname <- which(names(dfr) == nsvarie)
    names(dfr)[index_varname] <- "nsvarie"
    smry_muestreo_tntuber <- dfr %>%
        dplyr::group_by(hh) %>%
        dplyr::summarize(tn_hh = sum(nsvarie,na.rm = TRUE)) %>%
        dplyr::ungroup()
    dfr_hcf <- dplyr::left_join(dfr, smry_muestreo_tntuber, by = "hh") %>%
        dplyr::mutate(HCF = (nsvarie/tn_hh))
    if (pctn) {
        dfr_hcf <- dfr_hcf %>%
            dplyr::mutate(HCF_percent = 100 * (nsvarie/tn_hh))
    }
}

tn_hh <- totalhcfxvarie <- total_hh <- NULL
