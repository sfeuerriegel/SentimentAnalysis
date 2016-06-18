library(SentimentAnalysis)
context("Data and dictionary objects")

test_that("load functions access underlying data correctly", {
  d <- loadDictionaryGI()
  expect_is(d, "SentimentDictionaryBinary")
  expect_true("affirm" %in% d$positiveWords)
  expect_true("posit" %in% d$positiveWords)
  
  expect_true("abandon" %in% d$negativeWords)
  expect_true("afraid" %in% d$negativeWords)
  expect_true("negat" %in% d$negativeWords)
  
  expect_equivalent(length(d$positiveWords), 1316)
  expect_equivalent(length(d$negativeWords), 1746)
  
  d <- loadDictionaryHE()
  expect_is(d, "SentimentDictionaryBinary")
  expect_true("accomplish" %in% d$positiveWords)
  expect_true("posit" %in% d$positiveWords)
  
  expect_true("drop" %in% d$negativeWords)
  expect_true("negat" %in% d$negativeWords)
  
  expect_equivalent(length(d$positiveWords), 53)
  expect_equivalent(length(d$negativeWords), 44)
  
  d <- loadDictionaryLM()
  expect_is(d, "SentimentDictionaryBinary")
  expect_true("advanc" %in% d$positiveWords)
  expect_true("posit" %in% d$positiveWords)
  
  expect_true("drop" %in% d$negativeWords)
  expect_true("negat" %in% d$negativeWords)
  
  expect_equivalent(length(d$positiveWords), 145)
  expect_equivalent(length(d$negativeWords), 885)  
  
  d <- loadDictionaryLM_Uncertainty()
  expect_is(d, "SentimentDictionaryWordlist")
  expect_true("uncertain" %in% d$wordlist)
  expect_true("possibl" %in% d$wordlist)

  expect_equivalent(length(d$wordlist), 129)
  
  d <- loadDictionaryQDAP()
  expect_is(d, "SentimentDictionaryBinary")
  expect_true("accomplish" %in% d$positiveWords)
  expect_true("posit" %in% d$positiveWords)
  
  expect_true("fall" %in% d$negativeWords)
  expect_true("negat" %in% d$negativeWords)
  
  expect_equivalent(length(d$positiveWords), 1279)
  expect_equivalent(length(d$negativeWords), 2954)  
})