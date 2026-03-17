test_that("plot_red_4d fills matrix with one square per unique variety", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  p <- plot_red_4d(results)
  expect_s3_class(p, "ggplot")
  expect_true(all(c("X", "Y", "n", "risk_category", "theoretical_range") %in% names(p$data)))
  expect_equal(nrow(p$data), 16L)

  n_unique_varieties <- dplyr::n_distinct(results$final_variety_name)
  expect_equal(sum(p$data$n), n_unique_varieties)
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

  check <- dplyr::left_join(
    observed,
    expected_matrix,
    by = c("X", "Y")
  )

  expect_equal(check$theoretical_range, check$expected_range)
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

test_that("plot_red_4d returns audit tables for tracking sums per square", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  out <- plot_red_4d(results, return_tables = TRUE)

  expect_named(
    out,
    c(
      "plot",
      "square_summary",
      "square_sum_breakdown",
      "square_metric_breakdown",
      "variety_assignment"
    )
  )
  expect_s3_class(out$plot, "ggplot")
  expect_equal(nrow(out$square_summary), 16L)
  expect_equal(nrow(out$square_sum_breakdown), 16L * 13L)

  n_unique_varieties <- dplyr::n_distinct(results$final_variety_name)
  expect_equal(sum(out$square_summary$n), n_unique_varieties)
  expect_equal(nrow(out$variety_assignment), n_unique_varieties)

  no_impossible_sums <- out$square_sum_breakdown |>
    dplyr::filter(n_varieties > 0) |>
    dplyr::pull(is_possible_for_square)

  expect_true(all(no_impossible_sums))
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
