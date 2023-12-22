test_that("output class is correct", {
  expected <- estim_corr(gambling,
                         c("lnacc_self_winvsloss", "age"),
                         20:22, 2)
  expect_s3_class(expected$tbl_select, "data.frame")
  expect_s3_class(expected$fig_corr, "gg")
  expect_s3_class(expected$fig_corr_nozero, "gg")
  expect_s3_class(expected$tbl_total, "data.frame")
})

# set seed for creating reproducible plots for plot tests
set.seed(1234)

test_that("check that plots look as expected", {
  expected_fig <- estim_corr(gambling,
                             c("lnacc_self_winvsloss", "age"),
                             20:30, 11)
  vdiffr::expect_doppelganger(
    title = "create fig corr",
    fig = expected_fig$fig_corr,)
  vdiffr::expect_doppelganger(
    title = "create fig corr nozero",
    fig = expected_fig$fig_corr_nozero,)
})
