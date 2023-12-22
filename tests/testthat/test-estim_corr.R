test_that("output class is correct", {
  expected <- estim_corr(gambling,
                         c("lnacc_self_winvsloss", "age"),
                         20:22, 2)
  expect_s3_class(expected$tbl_select, "data.frame")
  expect_s3_class(expected$fig_corr, "gg")
  expect_s3_class(expected$fig_corr_nozero, "gg")
  expect_s3_class(expected$tbl_total, "data.frame")
})

