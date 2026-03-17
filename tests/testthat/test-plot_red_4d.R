test_that("plot_red_4d plots all unique varieties with strict band assignment", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  out <- plot_red_4d(results, return_tables = TRUE)
  p <- out$plot

  expect_s3_class(p, "ggplot")
  expect_true(all(c("X", "Y", "n", "risk_category", "theoretical_range") %in% names(p$data)))
  expect_equal(nrow(p$data), 16L)

  n_unique_varieties <- dplyr::n_distinct(results$final_variety_name)
  expect_equal(sum(out$square_summary$n), n_unique_varieties)

  # n_projected is diagnostic from raw projection, not assignment.
  expect_equal(sum(out$square_summary$n_projected), n_unique_varieties)
})

test_that("plot_red_4d preserves strict risk totals after cell assignment", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  out <- plot_red_4d(results, return_tables = TRUE)

  observed <- results |>
    dplyr::select(final_variety_name, risk_category) |>
    dplyr::distinct() |>
    dplyr::mutate(risk_category = as.character(risk_category)) |>
    dplyr::count(risk_category, name = "n_observed")

  plotted <- out$square_summary |>
    dplyr::mutate(risk_category = as.character(risk_category)) |>
    dplyr::group_by(risk_category) |>
    dplyr::summarise(n_plotted = sum(n), .groups = "drop")

  compare <- dplyr::full_join(observed, plotted, by = "risk_category") |>
    dplyr::mutate(
      n_observed = dplyr::coalesce(n_observed, 0L),
      n_plotted = dplyr::coalesce(n_plotted, 0L)
    )

  expect_equal(compare$n_plotted, compare$n_observed)
})

test_that("plot_red_4d theoretical matrix ranges match expected 4x4 layout", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)
  out <- plot_red_4d(results, return_tables = TRUE)

  expected_matrix <- data.frame(
    X = rep(1:4, times = 4),
    Y = rep(1:4, each = 4),
    expected_range = c(
      "4", "5-7", "8-11", "8-11",
      "5-7", "8-11", "8-11", "12-15",
      "8-11", "8-11", "12-15", "12-15",
      "8-11", "12-15", "12-15", "16"
    ),
    stringsAsFactors = FALSE
  )

  observed <- out$square_summary |>
    dplyr::arrange(Y, X) |>
    dplyr::select(X, Y, theoretical_range)

  check <- dplyr::left_join(observed, expected_matrix, by = c("X", "Y"))
  expect_equal(check$theoretical_range, check$expected_range)
})

test_that("plot_red_4d supports custom variety column names", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013) |>
    dplyr::rename(cu_variety_name = final_variety_name)

  out <- plot_red_4d(results, variety_col = "cu_variety_name", return_tables = TRUE)
  expect_s3_class(out$plot, "ggplot")
  expect_equal(sum(out$square_summary$n), dplyr::n_distinct(results$cu_variety_name))
})

test_that("plot_red_4d returns expected audit tables and compatibility aliases", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)
  out <- plot_red_4d(results, return_tables = TRUE)

  expect_true(all(c(
    "plot",
    "square_summary",
    "square_sum_breakdown",
    "square_metric_breakdown",
    "variety_assignment",
    "limiting_metric_table",
    "limiting_metric_detail"
  ) %in% names(out)))

  expect_equal(nrow(out$square_summary), 16L)
  expect_equal(nrow(out$square_sum_breakdown), 16L * 13L)
  expect_equal(nrow(out$variety_assignment), dplyr::n_distinct(results$final_variety_name))
  expect_equal(nrow(out$limiting_metric_table), nrow(out$square_metric_breakdown))
  expect_equal(nrow(out$limiting_metric_detail), nrow(out$variety_assignment))

  no_off_band_counts <- out$square_sum_breakdown |>
    dplyr::filter(n_varieties > 0) |>
    dplyr::pull(is_in_theoretical_band)
  expect_true(all(no_off_band_counts))
})

test_that("plot_red_4d counts secure only in 16 cell when present", {
  toy_results <- data.frame(
    cu_variety_name = c("v4", "v6", "v10", "v14", "v16"),
    OCF_scale_num = c(1L, 2L, 3L, 4L, 4L),
    RCF_scale_num = c(1L, 1L, 3L, 4L, 4L),
    GDF_num = c(1L, 2L, 2L, 3L, 4L),
    ADF_num = c(1L, 1L, 2L, 3L, 4L),
    stringsAsFactors = FALSE
  )
  # sums: 4, 6, 10, 14, 16

  out <- plot_red_4d(toy_results, variety_col = "cu_variety_name", return_tables = TRUE)

  secure_in_grid <- out$square_summary |>
    dplyr::filter(X == 4, Y == 4) |>
    dplyr::pull(n)
  expect_equal(secure_in_grid, 1L)
})

test_that("plot_red_4d errors clearly when required columns are missing", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  expect_error(
    plot_red_4d(results, variety_col = "cu_variety_name"),
    "Missing required columns in `results`"
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
