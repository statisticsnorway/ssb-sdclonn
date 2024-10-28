test_that("sdc_lonn m/long", {
  a <- sdclonn_data("syntetisk_5000")
  expect_equal(nrow(a), 5000)
})

