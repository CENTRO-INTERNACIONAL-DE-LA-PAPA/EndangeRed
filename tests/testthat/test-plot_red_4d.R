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

  expected_levels <- c(
    "Critically At Risk",
    "At Risk",
    "Potentially Vulnerable",
    "Stable, Low Concern",
    "Secure"
  )
  expect_equal(levels(p$data$risk_category), expected_levels)
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

test_that("plot_red_4d can return square and limiting-metric tables", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  out <- plot_red_4d(results, return_tables = TRUE)

  expect_type(out, "list")
  expect_named(
    out,
    c("plot", "square_summary", "limiting_metric_table", "limiting_metric_detail")
  )
  expect_s3_class(out$plot, "ggplot")
  expect_equal(nrow(out$square_summary), 16L)
  expect_equal(nrow(out$limiting_metric_table), 64L)

  n_unique_varieties <- dplyr::n_distinct(results$final_variety_name)
  expect_equal(sum(out$square_summary$n), n_unique_varieties)

  weighted_vs_square <- out$limiting_metric_table |>
    dplyr::distinct(X, Y, total_weighted) |>
    dplyr::left_join(
      out$square_summary |>
        dplyr::select(X, Y, n),
      by = c("X", "Y")
    )

  expect_equal(
    weighted_vs_square$total_weighted,
    as.numeric(weighted_vs_square$n)
  )
})

test_that("plot_red_4d validates palette names for official risk bands", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  bad_palette <- c(
    "Critically At Risk" = "red",
    "At Risk" = "orange",
    "Potentially Vulnerable" = "yellow",
    "Stable, Low Concern" = "lightgreen"
  )

  expect_error(
    plot_red_4d(results, palette = bad_palette),
    "`palette` must be a named vector"
  )
})
