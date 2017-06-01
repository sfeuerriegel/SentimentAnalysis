library(SentimentAnalysis)
context("Dictionary generation")

library(tm)
library(spikeslab)

test_that("dictionary generations works correctly", {
  # Create a vector of strings
  
  documents <- c("This is a good thing!",
                 "This is a very good thing!",
                 "This is okay.",
                 "This is a bad thing.",
                 "This is a very bad thing.")
  response <- c(1, 0.5, 0, -0.5, -1)
  
  # Generate dictionary with LASSO regularization
  
  dictionary <- generateDictionary(documents, response)
  
  expect_is(dictionary, "SentimentDictionaryWeighted")
  expect_equal(dictionary$words, c("good", "bad"))
  expect_equal(dictionary$scores, c(0.5119851, -0.5118801), tolerance=0.001)
  expect_equal(dictionary$idf, c(1, 1))
  expect_equal(dictionary$intercept, 5.55e-05, tolerance=0.001)

  # Dictionary generations with tf weighting works correctly
  
  dictionary <- generateDictionary(documents, response, weighting=weightTf)

  expect_is(dictionary, "SentimentDictionaryWeighted")
  expect_equal(dictionary$words, c("good", "bad"))
  expect_equal(dictionary$scores, c(0.6768076, -0.6766687), tolerance=0.001)
  expect_equal(dictionary$idf, c(1, 1))
  expect_equal(dictionary$intercept, 5.55e-05, tolerance=0.001)

  # Dictionary generations works correctly together with lambda.min
  
  dictionary <- generateDictionary(documents, response, control = list(s="lambda.min"))

  expect_is(dictionary, "SentimentDictionaryWeighted")
  expect_equal(dictionary$words, c("good", "bad"))
  expect_equal(dictionary$scores, c(0.5119851, -0.5118801), tolerance=0.001)
  expect_equal(dictionary$idf, c(1, 1))
  expect_equal(dictionary$intercept, 5.55e-05, tolerance=0.001)

  # Dictionary generations works correctly without LASSO intercept
  
  dictionary <- generateDictionary(documents, response, intercept=FALSE)
  
  expect_is(dictionary, "SentimentDictionaryWeighted")
  expect_equal(dictionary$words, c("good", "bad"))
  expect_equal(dictionary$scores, c(0.4131132, -0.4131132), tolerance=0.001)
  expect_equal(dictionary$idf, c(1, 1))
  expect_equal(dictionary$intercept, 0)
  
  # Dictionary generations works correctly with spike-and-slab regression
  set.seed(0)
  dictionary <- generateDictionary(documents, response, modelType = "spikeslab")

  expect_is(dictionary, "SentimentDictionaryWeighted")
  expect_equal(dictionary$words, c("good", "bad"))
  expect_equal(dictionary$scores, c(0.3955698, -0.4190744), tolerance=0.001)
  expect_equal(dictionary$idf, c(1, 1))
  expect_equal(dictionary$intercept, 0)
})
