test_that("plot_red_4d keeps one risk and one matrix cell per variety on bundled data", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  risk_per_variety <- results |>
    dplyr::select(final_variety_name, risk_category) |>
    dplyr::distinct() |>
    dplyr::count(final_variety_name)

  expect_equal(max(risk_per_variety$n), 1L)

  cell_per_variety <- results |>
    dplyr::mutate(
      X = pmax(GDF_num, RCF_scale_num),
      Y = pmax(OCF_scale_num, ADF_num)
    ) |>
    dplyr::select(final_variety_name, X, Y) |>
    dplyr::distinct() |>
    dplyr::count(final_variety_name)

  expect_equal(max(cell_per_variety$n), 1L)

  p <- plot_red_4d(results)
  expect_s3_class(p, "ggplot")
  expect_true(all(c("X", "Y", "n") %in% names(p$data)))

  n_unique_varieties <- dplyr::n_distinct(results$final_variety_name)
  expect_equal(sum(p$data$n), n_unique_varieties)
})

test_that("plot_red_4d supports custom variety column names", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013) |>
    dplyr::rename(cu_variety_name = final_variety_name)

  p <- plot_red_4d(results, variety_col = "cu_variety_name")

  expect_s3_class(p, "ggplot")
  expect_equal(sum(p$data$n), dplyr::n_distinct(results$cu_variety_name))
})

test_that("plot_red_4d errors clearly when required columns are missing", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  expect_error(
    plot_red_4d(results, variety_col = "cu_variety_name"),
    "Missing required columns in `results`"
  )
})
