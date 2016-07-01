library(SentimentAnalysis)
context("Prediction")

library(tm)

test_that("SentimentDictionaryWeighted is predicted correctly", { 
  documents <- c("This is a good thing!",
                 "This is a very good thing!",
                 "This is okay.",
                 "This is a bad thing.",
                 "This is a very bad thing.")

  dictionary <- SentimentDictionaryWeighted(c("bad", "good"),
                                            c(-2, 5),
                                            c(1, 1),
                                            1)
  
  sentiment <- predict(dictionary, documents)
  
  expect_is(sentiment, "data.frame")
  expect_equal(colnames(sentiment), "Dictionary")
  expect_equal(sentiment$Dictionary, c(6, 6, 1, -1, -1))
  
  dictionary <- SentimentDictionaryWeighted(c("bad", "good"),
                                            c(-2, 5),
                                            c(0.5, 2),
                                            1)
  
  sentiment <- predict(dictionary, documents)
  
  expect_is(sentiment, "data.frame")
  expect_equal(colnames(sentiment), "Dictionary")
  expect_equal(sentiment$Dictionary, c(11, 3.5, 1, -3, 0))
})