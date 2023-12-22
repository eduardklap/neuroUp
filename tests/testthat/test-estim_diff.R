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

