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


test_that('change measuredName', {
  mm1 <- measurement(data.frame('trueValue' = 3),
                     params = data.frame("measuredName" = "trueValue",
                                      "resultName" =  "measurement")) %>%
    mm_truenessFunc(.2, 0) %>%
    mm_precCharFunc(.2, .1)

  mm2 <-  measurement(data.frame('xyz' = 3),
                      params = data.frame("measuredName" = "xyz",
                                    "resultName" =  "measurement")) %>%
    mm_truenessFunc(.2, 0) %>%
    mm_precCharFunc(.2, .1)

  set.seed(1)
  values1 <- runSim(mm1)

  set.seed(1)
  values2 <- runSim(mm2)

  idx <- which(colnames(values2) == 'xyz')
  colnames(values2)[idx] <- 'trueValue'

  expect_equal(values1, values2)
})
