test_that('imprecision', {
  set.seed(1)
  mm <- measurement(data.frame('trueValue' = 3)) %>%
    mm_precCharFunc(.2, 0)
  values <- runSim(mm)
  set.seed(1)
  expect_equal(values$measurement[1],
               rnorm(1, 3, .2))

  set.seed(1)
  mm <- measurement(data.frame('trueValue' = 3)) %>%
    mm_precCharFunc(0, .2)
  values <- runSim(mm)
  set.seed(1)
  expect_equal(values$measurement[1],
               rnorm(1, 3, .2*3))
})


test_that('trueness', {
  mm <- measurement(data.frame('trueValue' = 3)) %>%
    mm_truenessFunc(.2, 0)
  values <- runSim(mm)
  expect_equal(values$measurement[1],
               3 + .2)

  mm <- measurement(data.frame('trueValue' = 3)) %>%
    mm_truenessFunc(0, .2)
  values <- runSim(mm)
  expect_equal(values$measurement[1],
               3*1.2)
})
