test_that('normal distribution', {
  center <- 5
  spread <- 2
  ana <- analyte(data.frame('time' = 1:1000))
  ana <- ana_distrNorm(ana, center = center, spread = spread)
  values <- runSim(ana)
  expect_equal(mean(values$trueValue), center,
               tolerance=.5, scale = 1)
  expect_equal(sd(values$trueValue), spread,
               tolerance=.5, scale = 1)
})

test_that('log normal distribution', {
  center <- 5
  spread <- 2
  ana <- analyte(data.frame('time' = 1:1000))
  ana <- ana_distrLnorm(ana, center = center, spread = spread)
  values <- runSim(ana)
  expect_equal(mean(values$trueValue), center,
               tolerance=.5, scale = 1)
  expect_equal(sd(values$trueValue), spread,
               tolerance=.5, scale = 1)
})

test_that('QC measurements', {
  set.seed(1)
  center <- 5
  spread <- 2
  ana2 <- analyte(data.frame('time' = 1:10, 'type' = 'pat')) %>%
    ana_distrNorm(center = center, spread = spread)

  ana <- ana2 %>%
    ana_addQC(100, 'highQC')
  values <- runSim(ana)
  expect_equal(values[values$type == 'highQC', 'trueValue'],
               rep.int(100, 10))
  set.seed(1)
  values2 <- runSim(ana2)

  expect_equal(values[values$type != 'highQC', 'trueValue'],
               values2[, 'trueValue'])
})
