test_that("plot_red_4d fills matrix with one square per unique variety", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  p <- plot_red_4d(results)
  expect_s3_class(p, "ggplot")
  expect_true(all(c("X", "Y", "n", "risk_category", "theoretical_range") %in% names(p$data)))
  expect_equal(nrow(p$data), 16L)

  strict_expected <- results |>
    dplyr::transmute(
      final_variety_name,
      metrics_sum = OCF_scale_num + RCF_scale_num + GDF_num + ADF_num,
      X = pmax(GDF_num, RCF_scale_num),
      Y = pmax(OCF_scale_num, ADF_num)
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      theoretical_range = dplyr::case_when(
        cell_label == 4 ~ "4",
        cell_label >= 5 & cell_label <= 7 ~ "5-7",
        cell_label >= 8 & cell_label <= 11 ~ "8-11",
        cell_label >= 12 & cell_label <= 15 ~ "12-15",
        cell_label == 16 ~ "16"
      ),
      metrics_band = dplyr::case_when(
        metrics_sum == 4 ~ "4",
        metrics_sum >= 5 & metrics_sum <= 7 ~ "5-7",
        metrics_sum >= 8 & metrics_sum <= 11 ~ "8-11",
        metrics_sum >= 12 & metrics_sum <= 15 ~ "12-15",
        metrics_sum == 16 ~ "16"
      ),
      is_in_theoretical_band = metrics_band == theoretical_range
    ) |>
    dplyr::summarise(n_strict = dplyr::n_distinct(final_variety_name[is_in_theoretical_band])) |>
    dplyr::pull(n_strict)

  expect_equal(sum(p$data$n), strict_expected)
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
  strict_expected <- results |>
    dplyr::transmute(
      cu_variety_name,
      metrics_sum = OCF_scale_num + RCF_scale_num + GDF_num + ADF_num,
      X = pmax(GDF_num, RCF_scale_num),
      Y = pmax(OCF_scale_num, ADF_num)
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      cell_label = 2 * (X + Y),
      theoretical_range = dplyr::case_when(
        cell_label == 4 ~ "4",
        cell_label >= 5 & cell_label <= 7 ~ "5-7",
        cell_label >= 8 & cell_label <= 11 ~ "8-11",
        cell_label >= 12 & cell_label <= 15 ~ "12-15",
        cell_label == 16 ~ "16"
      ),
      metrics_band = dplyr::case_when(
        metrics_sum == 4 ~ "4",
        metrics_sum >= 5 & metrics_sum <= 7 ~ "5-7",
        metrics_sum >= 8 & metrics_sum <= 11 ~ "8-11",
        metrics_sum >= 12 & metrics_sum <= 15 ~ "12-15",
        metrics_sum == 16 ~ "16"
      ),
      is_in_theoretical_band = metrics_band == theoretical_range
    ) |>
    dplyr::summarise(n_strict = dplyr::n_distinct(cu_variety_name[is_in_theoretical_band])) |>
    dplyr::pull(n_strict)

  expect_equal(sum(p$data$n), strict_expected)
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

  expect_true(all(c(
    "plot",
    "square_summary",
    "square_sum_breakdown",
    "square_metric_breakdown",
    "variety_assignment",
    "limiting_metric_table",
    "limiting_metric_detail"
  ) %in% names(out)))
  expect_s3_class(out$plot, "ggplot")
  expect_equal(nrow(out$square_summary), 16L)
  expect_equal(nrow(out$square_sum_breakdown), 16L * 13L)

  n_unique_varieties <- dplyr::n_distinct(results$final_variety_name)
  strict_expected <- out$variety_assignment |>
    dplyr::summarise(n_strict = dplyr::n_distinct(final_variety_name[is_in_theoretical_band])) |>
    dplyr::pull(n_strict)

  expect_lte(sum(out$square_summary$n), n_unique_varieties)
  expect_equal(sum(out$square_summary$n), strict_expected)
  expect_equal(nrow(out$variety_assignment), n_unique_varieties)

  no_impossible_sums <- out$square_sum_breakdown |>
    dplyr::filter(n_varieties > 0) |>
    dplyr::pull(is_possible_for_square)

  expect_true(all(no_impossible_sums))
})

test_that("plot_red_4d counts only strict secure (metrics_sum == 16) in 16 cell", {
  toy_results <- data.frame(
    cu_variety_name = c("v15", "v16"),
    OCF_scale_num = c(4L, 4L),
    RCF_scale_num = c(4L, 4L),
    GDF_num = c(4L, 4L),
    ADF_num = c(3L, 4L),
    stringsAsFactors = FALSE
  )

  out <- plot_red_4d(toy_results, variety_col = "cu_variety_name", return_tables = TRUE)
  cell_44 <- out$square_summary |>
    dplyr::filter(X == 4, Y == 4)

  expect_equal(cell_44$n_total, 2L)
  expect_equal(cell_44$n, 1L)
  expect_equal(cell_44$theoretical_range, "16")
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
