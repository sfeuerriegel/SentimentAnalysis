library(SentimentAnalysis)
context("Preprocessing of corpus")

library(tm)

test_that("rules work correctly", {
  dtm <- as.data.frame(matrix(c(0, 0, 0, 1,
                                1, 0, 0, 0,
                                0, 0, 1, 0,
                                1, 1, 1, 1), nrow=4, byrow=TRUE))
  colnames(dtm) <- c("negat", "neutral", "posit", "uncertain")
  
  r <- ruleWordCount(dtm)  
  expect_equal(r, c(1, 1, 1, 4))
    
  d <- SentimentDictionaryWordlist(c("uncertain"))
  r <- ruleRatio(dtm, d)
  expect_equal(r, c(1, 0, 0, 0.25))

  d <- SentimentDictionaryBinary(c("negat", "rise", "more"),
                                 c("posit", "drop"))

  r <- ruleNegativity(dtm, d)
  expect_equal(r, c(0, 0, 1, 0.25))
  r <- rulePositivity(dtm, d)
  expect_equal(r, c(0, 1, 0, 0.25))
  r <- ruleSentiment(dtm, d)
  expect_equal(r, c(0, 1, -1, 0))  
  
  d <- SentimentDictionaryWeighted(c("negat", "posit", "exit"),
                                   c(-0.5, +0.5, -10),
                                   rep(NA, 3),
                                   5)
  r <- ruleLinearModel(dtm, d)
  expect_equal(r, c(5, 4.5, 5.5, 5.0))

})