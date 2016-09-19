test_that('addPraeHook', {
  center <- 5
  spread <- 2
  ana <- analyte(data.frame('time' = 1:10, 'type' = 'pat')) %>%
    ana_distrNorm(center = center, spread = spread) %>%
    ana_addQC(100, 'highQC') %>%
    addPraeHook('center',
                center + 5,
                type != 'highQC')
  values <- runSim(ana)

  expect_equal(values[values$type != 'highQC', 'center'],
               rep.int(center + 5 , 10))
})


test_that('chaining', {
  ana <- analyte(data.frame('time' = 1:10, 'type' = 'pat')) %>%
    ana_distrLnorm(center = 5, spread = 5)

  mm <- measurement(ana) %>%
    mm_precCharFunc(.2, 3)

  values1 <- runSim(mm)
  values2 <- runSim(mm)

  expect_false(isTRUE(all.equal(
    values1$trueValue, values2$trueValue)))
  expect_false(isTRUE(all.equal(
    values1$measurement, values2$measurement)))

  ana <- analyte(data.frame('time' = 1:10, 'type' = 'pat')) %>%
    ana_distrLnorm(center = 5, spread = 5)

  ana_concrete <- runSim(ana)

  mm <- measurement(ana_concrete) %>%
    mm_precCharFunc(.2, 3)

  values3 <- runSim(mm)
  values4 <- runSim(mm)

  expect_equal(values3$trueValues, values4$trueValues)
  expect_false(isTRUE(all.equal(
    values3$measurement, values4$measurement)))

})
