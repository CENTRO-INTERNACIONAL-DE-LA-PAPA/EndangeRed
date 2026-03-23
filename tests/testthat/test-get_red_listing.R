test_that("get_red_listing errors clearly when required data columns are missing", {
  data("Huancavelica_2013", package = "EndangeRed")

  expect_error(
    get_red_listing(Huancavelica_2013, elevation_name = "Altitude"),
    "Missing required columns in `data`"
  )
})

test_that("calculate_distances handles all-missing elevation without warnings", {
  toy <- data.frame(
    long = c(NA_real_, NA_real_),
    lat = c(NA_real_, NA_real_),
    altitud = c(NA_real_, NA_real_)
  )

  expect_no_warning(
    out <- calculate_distances(
      toy,
      coord_names = c("long", "lat"),
      elevation_name = "altitud"
    )
  )

  expect_equal(out$elevation_range, 10)
})

test_that("calculate_distances errors clearly when coordinate/elevation columns are missing", {
  toy <- data.frame(
    long = c(-71.0, -71.1),
    lat = c(-13.0, -13.1)
  )

  expect_error(
    calculate_distances(
      toy,
      coord_names = c("long", "lat"),
      elevation_name = "altitud"
    ),
    "Missing required columns in `df`"
  )
})

test_that("calculate_distances handles character elevation with invalid tokens", {
  toy <- data.frame(
    long = c(-71.0, -71.1, -71.2),
    lat = c(-13.0, -13.1, -13.2),
    altitud = c("4000", "x|", "4020"),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    out <- calculate_distances(
      toy,
      coord_names = c("long", "lat"),
      elevation_name = "altitud"
    )
  )

  expect_equal(out$elevation_range, 20)
})
