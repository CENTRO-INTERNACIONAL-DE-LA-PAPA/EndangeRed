test_that("plot_red_4d fills squares using strict equality rule", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  out <- plot_red_4d(results, return_tables = TRUE)
  expect_s3_class(out$plot, "ggplot")
  expect_equal(nrow(out$square_summary), 16L)

  manual_total <- results |>
    dplyr::select(final_variety_name, OCF_scale_num, RCF_scale_num, GDF_num, ADF_num) |>
    dplyr::distinct() |>
    dplyr::filter(
      RCF_scale_num == GDF_num,
      OCF_scale_num == ADF_num
    ) |>
    dplyr::summarise(n = dplyr::n_distinct(final_variety_name)) |>
    dplyr::pull(n)

  expect_equal(sum(out$square_summary$n), manual_total)
})

test_that("plot_red_4d exact cell counts match explicit filters", {
  toy <- data.frame(
    cu_variety_name = c("a", "b", "c", "d"),
    OCF_scale_num = c(1L, 4L, 1L, 4L),
    RCF_scale_num = c(4L, 1L, 4L, 1L),
    GDF_num = c(4L, 1L, 3L, 1L),
    ADF_num = c(1L, 4L, 1L, 4L),
    stringsAsFactors = FALSE
  )
  # Strict matches:
  # a -> (X=4,Y=1)
  # b -> (X=1,Y=4)
  # c not counted (RCF != GDF)
  # d -> (X=1,Y=4)

  out <- plot_red_4d(toy, variety_col = "cu_variety_name", return_tables = TRUE)
  cell_41 <- out$square_summary |>
    dplyr::filter(X == 4, Y == 1) |>
    dplyr::pull(n)
  cell_14 <- out$square_summary |>
    dplyr::filter(X == 1, Y == 4) |>
    dplyr::pull(n)

  expect_equal(cell_41, 1L)
  expect_equal(cell_14, 2L)
  expect_equal(sum(out$square_summary$n), 3L)
})

test_that("plot_red_4d returns expected tables and compatibility aliases", {
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
  expect_equal(nrow(out$square_sum_breakdown), 16L * 13L)
  expect_equal(nrow(out$limiting_metric_table), nrow(out$square_metric_breakdown))
  expect_equal(nrow(out$limiting_metric_detail), nrow(out$variety_assignment))

  expect_true(all(out$variety_assignment$is_in_theoretical_band))
})

test_that("plot_red_4d supports custom variety column names", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013) |>
    dplyr::rename(cu_variety_name = final_variety_name)

  out <- plot_red_4d(results, variety_col = "cu_variety_name", return_tables = TRUE)
  expect_s3_class(out$plot, "ggplot")

  manual_total <- results |>
    dplyr::select(cu_variety_name, OCF_scale_num, RCF_scale_num, GDF_num, ADF_num) |>
    dplyr::distinct() |>
    dplyr::filter(
      RCF_scale_num == GDF_num,
      OCF_scale_num == ADF_num
    ) |>
    dplyr::summarise(n = dplyr::n_distinct(cu_variety_name)) |>
    dplyr::pull(n)

  expect_equal(sum(out$square_summary$n), manual_total)
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
