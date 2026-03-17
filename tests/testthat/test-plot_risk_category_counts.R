test_that("plot_risk_category_counts counts unique varieties per risk category", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  expected_counts <- results %>%
    dplyr::select(final_variety_name, risk_category) %>%
    dplyr::distinct() %>%
    dplyr::count(risk_category, name = "n_expected") %>%
    dplyr::mutate(risk_category = as.character(risk_category))

  p <- plot_risk_category_counts(results)

  expect_s3_class(p, "ggplot")
  expect_true(all(c("risk_category", "n_varieties") %in% names(p$data)))
  expect_equal(sum(p$data$n_varieties), dplyr::n_distinct(results$final_variety_name))

  observed_counts <- p$data %>%
    dplyr::as_tibble() %>%
    dplyr::select(risk_category, n_varieties) %>%
    dplyr::mutate(risk_category = as.character(risk_category)) %>%
    dplyr::rename(n_observed = n_varieties)

  compare <- expected_counts %>%
    dplyr::left_join(observed_counts, by = "risk_category") %>%
    dplyr::mutate(n_observed = dplyr::coalesce(n_observed, 0L))

  expect_equal(compare$n_expected, compare$n_observed)
})

test_that("plot_risk_category_counts supports custom variety and risk columns", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013) %>%
    dplyr::rename(cu_variety_name = final_variety_name, risk_cat = risk_category)

  p <- plot_risk_category_counts(
    results,
    variety_col = "cu_variety_name",
    risk_col = "risk_cat"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(sum(p$data$n_varieties), dplyr::n_distinct(results$cu_variety_name))
})

test_that("plot_risk_category_counts errors clearly when required columns are missing", {
  data("Huancavelica_2013", package = "EndangeRed")
  results <- get_red_listing(Huancavelica_2013)

  expect_error(
    plot_risk_category_counts(results, variety_col = "cu_variety_name"),
    "Missing required columns in `results`"
  )
  expect_error(
    plot_risk_category_counts(results, risk_col = "risk_cat"),
    "Missing required columns in `results`"
  )
})
