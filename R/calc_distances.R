#' Get the haversine distances and elevation range from coordinates and elevation
#'
#' @param df A data frame
#' @param coord_names column names from the coordinates like: c(long, lat)
#' @param elevation_name column name for the elevation
#' @export
#'
#' @examples
#' data(Huancavelica_2013)


calculate_distances <- function(df, coord_names=c("long","lat"),elevation_name="altitud") {
    
    valid <- !is.na(df[[ coord_names[1] ]]) &
        !is.na(df[[ coord_names[2] ]])
    clean <- df[valid, ]
    coords <- as.matrix(clean[,coord_names])
    
    n <- nrow(coords)
    
    if (n < 2) {
        elev <- max(df[[elevation_name]], na.rm = TRUE) -
            min(df[[elevation_name]], na.rm = TRUE)
        erange <- ifelse(elev < 5, 10, elev)
        
        return(data.frame(
            min_dist_m = 10,
            max_dist_m = 50,
            mean_dist_m = 25,
            n = n,
            elevation_range = erange
        ))
    }
    
    Dmat <- distm(coords, fun = distHaversine)
    d_vec <- Dmat[ lower.tri(Dmat) ]
    
    if (all(is.na(d_vec))) {
        min_d  <- NA_real_
        max_d  <- NA_real_
        mean_d <- NA_real_
    } else {
        min_d  <- min(d_vec, na.rm = TRUE)
        max_d  <- max(d_vec, na.rm = TRUE)
        mean_d <- mean(d_vec, na.rm = TRUE)
    }
    
    elev  <- max(df[[elevation_name]], na.rm = TRUE) -
        min(df[[elevation_name]], na.rm = TRUE)
    erange <- ifelse(elev < 5, 10, elev)
    
    data.frame(
        min_dist_m      = min_d,
        max_dist_m      = max_d,
        mean_dist_m     = mean_d,
        n               = n,
        elevation_range = erange
    )
}
