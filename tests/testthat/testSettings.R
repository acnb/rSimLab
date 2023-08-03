test_that('check mean, sd and number of days', {
 nDays <- 1000
 meanNumberPerDay <- 10
 sdPerDay <- 3
 s <- simpleOverTime(nDays, meanNumberPerDay, sdPerDay)


 cs <- s %>% dplyr::group_by(time) %>%
   dplyr::summarise(freq = dplyr::n())

 expect_equal(nrow(cs), nDays)
 expect_equal(mean(cs$freq), meanNumberPerDay,
              tolerance=sdPerDay/(nDays^.5)*2.575, scale = 1)
 # not sure about tolerance
 expect_equal(sd(cs$freq), sdPerDay,
              tolerance=.5, scale = 1)
})
