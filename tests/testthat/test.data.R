library(SentimentAnalysis)
context("Data objects")

library(tm)

test_that("data objects appear correct", { 
  data("DictionaryGI")
  expect_is(DictionaryGI, "list")
  expect_length(DictionaryGI, 2)
  expect_length(DictionaryGI$negative, 2005)
  expect_length(DictionaryGI$positive, 1637)
  expect_true("negative" %in% DictionaryGI$negative)
  expect_true("improve" %in% DictionaryGI$positive)

  data("DictionaryHE")
  expect_is(DictionaryHE, "list")
  expect_length(DictionaryHE, 2)
  expect_length(DictionaryHE$negative, 85)
  expect_length(DictionaryHE$positive, 105)
  expect_true("negative" %in% DictionaryHE$negative)
  expect_true("improve" %in% DictionaryHE$positive)

  data("DictionaryLM")
  expect_is(DictionaryLM, "list")
  expect_length(DictionaryLM, 3)
  expect_length(DictionaryLM$negative, 2355)
  expect_length(DictionaryLM$positive, 354)
  expect_true("negative" %in% DictionaryLM$negative)
  expect_true("improve" %in% DictionaryLM$positive)

  expect_length(DictionaryLM$uncertainty, 297)
  expect_true("uncertain" %in% DictionaryLM$uncertainty)
})