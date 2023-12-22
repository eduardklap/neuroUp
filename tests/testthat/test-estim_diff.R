test_that("output class is correct", {
  expected <- estim_diff(feedback,
                         c("mfg_learning", "mfg_application"),
                         20:22, 2)
  expect_s3_class(expected$tbl_select, "data.frame")
  expect_s3_class(expected$fig_diff, "gg")
  expect_s3_class(expected$fig_nozero, "gg")
  expect_s3_class(expected$fig_d_nozero, "gg")
  expect_s3_class(expected$tbl_total, "data.frame")
})

# set seed for creating reproducible plots for plot tests
set.seed(1234)

test_that("check that plots look as expected", {
  expected_fig <- estim_diff(feedback,
                         c("mfg_learning", "mfg_application"),
                         20:30, 11)
    vdiffr::expect_doppelganger(
      title = "create fig diff",
      fig = expected_fig$fig_diff,)
    vdiffr::expect_doppelganger(
      title = "create fig nozero",
      fig = expected_fig$fig_nozero,)
    vdiffr::expect_doppelganger(
      title = "create fig d nozero",
      fig = expected_fig$fig_d_nozero,)
})

