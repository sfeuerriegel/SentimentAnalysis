library(SentimentAnalysis)
context("Main function for sentiment analysis")

library(tm)

test_that("sentiment analysis returns correct values for Reuters corpus", {
  reut21578 <- system.file("texts", "crude", package="tm")
  reuters <- Corpus(DirSource(reut21578),
                    readerControl=list(reader=readReut21578XML))
  reuters <- tm_map(reuters, PlainTextDocument)
  reuters <- tm_map(reuters, content_transformer(tolower))
  dtm <- DocumentTermMatrix(reuters)
  
  sentiment <- analyzeSentiment(dtm)
  
#  expect_equal(sentiment, c(-0.6000000, -0.3333333, 1.0000000, 1.0000000, NaN, 
#                            -1.0000000, -0.3333333, -0.6000000, -0.7500000, -0.2500000, 
#                            -1.0000000, NaN, 0.3333333, -1.0000000, -1.0000000,
#                            -1.0000000, -1.0000000, 0.0000000, 0.0000000, NaN))
})

test_that("comparison with response variable works correctly", {
  sentiment <- matrix(c(4, 2, 1, 1), 
                     dimnames=list(c("A", "B", "C", "D"), c("Sentiment")))
  # continuous numeric response variable
  response <- c(2, 4, 1, 1)
  cmp <- compareToResponse(sentiment, response)
  
  expect_equal(cmp["cor", "Sentiment"], 1/3)
  expect_equal(cmp["cor.t.statistic", "Sentiment"], 0.5)
  expect_equal(cmp["cor.p.value", "Sentiment"], 0.5)
  expect_equal(cmp["lm.t.value", "Sentiment"], 0.5)
  expect_equal(cmp["r.squared", "Sentiment"], 1/9)
  expect_equal(cmp["RMSE", "Sentiment"], sqrt(2))
  expect_equal(cmp["MAE", "Sentiment"], 1)
  expect_equal(cmp["Accuracy", "Sentiment"], 1)
  
  # binary response variable
  response <- c(TRUE, TRUE, FALSE, FALSE)
  cmp <- compareToResponse(sentiment, response)
  expect_equal(cmp["Accuracy", "Sentiment"], 0.5)
  expect_equal(cmp["Sensitivity", "Sentiment"], 0)
  expect_equal(cmp["Specificity", "Sentiment"], 1)
  expect_equal(cmp["avg.sentiment.pos.response", "Sentiment"], 3)
  expect_equal(cmp["avg.sentiment.neg.response", "Sentiment"], 1)
})

test_that("binary classifier is evaluated correctly", {
  r <- evalBinaryClassifier(factor(c("TRUE", "TRUE", "TRUE", "TRUE"), levels=c("FALSE", "TRUE")),
                            factor(c("FALSE", "FALSE", "FALSE", "FALSE"), levels=c("FALSE", "TRUE")))
  expect_equivalent(as.numeric(r), c(0, NaN, 0, NaN, NaN, NaN))

  r <- evalBinaryClassifier(factor(c("TRUE", "TRUE", "TRUE", "TRUE"), levels=c("FALSE", "TRUE")),
                            factor(c("TRUE", "TRUE", "TRUE", "TRUE"), levels=c("FALSE", "TRUE")))
  expect_equivalent(as.numeric(r), c(1, NaN, NaN, 1, 0, NaN))
  
  r <- evalBinaryClassifier(factor(c("TRUE", "FALSE", "TRUE", "FALSE"), levels=c("FALSE", "TRUE")),
                            factor(c("FALSE", "TRUE", "TRUE", "FALSE"), levels=c("FALSE", "TRUE")))
  expect_equivalent(as.numeric(r), c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5))  
})

test_that("conversion to binary response works correctly for vectors", {
  sentiment <- c(-1, -0.5, +1, 0.6, 0)
  r <- convertToBinaryResponse(sentiment)
  expect_true(is.factor(r))
  expect_equivalent(as.character(r), c("negative", "negative", "positive", "positive", "positive"))
  expect_equivalent(levels(r), c("negative", "positive"))

  sentiment <- c(10)
  r <- convertToBinaryResponse(sentiment)
  expect_true(is.factor(r))
  expect_equivalent(as.character(r), c("positive"))
  expect_equivalent(sort(levels(r)), sort(c("negative", "positive")))
    
  sentiment <- numeric(0)
  r <- convertToBinaryResponse(sentiment)
  expect_true(is.factor(r))
  expect_equivalent(as.character(r), character(0))
  expect_equivalent(sort(levels(r)), sort(c("negative", "positive")))

  expect_error(convertToBinaryResponse(NULL))  
  expect_error(convertToBinaryResponse(factor("positive", "negative")))  
})
  
test_that("conversion to binary response works correctly for data.frame", {
  sentiment <- c(-1, -0.5, +1, 0.6, 0)

  df <- data.frame(No=1:5, Sentiment=sentiment)
  r <- convertToBinaryResponse(df)
  expect_true(is.data.frame(r))
  expect_equivalent(colnames(r), colnames(df))
  expect_equivalent(r$No, df$No)
  expect_true(is.factor(r$Sentiment))
  expect_equivalent(as.character(r$Sentiment), c("negative", "negative", "positive", "positive", "positive"))
  expect_equivalent(sort(levels(r$Sentiment)), sort(c("negative", "positive")))
})

test_that("conversion to binary response works correctly for matrix", {
  sentiment <- c(-1, -0.5, +1, 0.6, 0)
  
  m <- matrix(c(1:5, sentiment), nrow=5, ncol=2, byrow=FALSE)
  colnames(m) <- c("No", "Sentiment")
  r <- convertToBinaryResponse(m)
  expect_true(is.data.frame(r))
  expect_equivalent(colnames(r), colnames(m))
  expect_equivalent(r$No, m[, "No"])
  expect_true(is.factor(r$Sentiment))
  expect_equivalent(as.character(r$Sentiment), c("negative", "negative", "positive", "positive", "positive"))
  expect_equivalent(sort(levels(r$Sentiment)), sort(c("negative", "positive")))
  
  m <- matrix(1:6, nrow=2, ncol=3)
  r <- convertToBinaryResponse(m)
  expect_true(is.data.frame(r))
  expect_equivalent(colnames(r), c("X1", "X2", "X3"))
  expect_equivalent(unname(as.matrix(r)), m)
})

test_that("conversion to direction works correctly for vectors", {
  sentiment <- c(-1, -0.5, +1, 0.6, 0)
  
  r <- convertToDirection(sentiment)
  expect_true(is.factor(r))
  expect_equivalent(as.character(r), c("negative", "negative", "positive", "positive", "neutral"))
  expect_equivalent(sort(levels(r)), sort(c("negative", "neutral", "positive")))

  sentiment <- c(-0)
  r <- convertToDirection(sentiment)
  expect_true(is.factor(r))
  expect_equivalent(as.character(r), c("neutral"))
  expect_equivalent(sort(levels(r)), sort(c("negative", "neutral", "positive")))
    
  sentiment <- numeric(0)
  r <- convertToDirection(sentiment)
  expect_true(is.factor(r))
  expect_equivalent(as.character(r), character(0))
  expect_equivalent(sort(levels(r)), sort(c("negative", "neutral", "positive")))
  
  expect_error(convertToDirection(NULL))  
  expect_error(convertToDirection(factor("positive", "negative")))  
})

test_that("conversion to direction works correctly for data.frame", {
  sentiment <- c(-1, -0.5, +1, 0.6, 0)
  
  df <- data.frame(No=1:5, Sentiment=sentiment)
  r <- convertToDirection(df)
  expect_true(is.data.frame(r))
  expect_equivalent(colnames(r), colnames(df))
  expect_equivalent(r$No, df$No)
  expect_true(is.factor(r$Sentiment))
  expect_equivalent(as.character(r$Sentiment), c("negative", "negative", "positive", "positive", "neutral"))
  expect_equivalent(sort(levels(r$Sentiment)), sort(c("negative", "neutral", "positive")))
})

test_that("conversion to direction works correctly for matrix", {
  sentiment <- c(-1, -0.5, +1, 0.6, 0)
  
  m <- matrix(c(1:5, sentiment), nrow=5, ncol=2, byrow=FALSE)
  colnames(m) <- c("No", "Sentiment")
  r <- convertToDirection(m)
  expect_true(is.data.frame(r))
  expect_equivalent(colnames(r), colnames(m))
  expect_equivalent(r$No, m[, "No"])
  expect_true(is.factor(r$Sentiment))
  expect_equivalent(as.character(r$Sentiment), c("negative", "negative", "positive", "positive", "neutral"))
  expect_equivalent(sort(levels(r$Sentiment)), sort(c("negative", "neutral", "positive")))
  
  m <- matrix(1:6, nrow=2, ncol=3)
  r <- convertToDirection(m)
  expect_true(is.data.frame(r))
  expect_equivalent(colnames(r), c("X1", "X2", "X3"))
  expect_equivalent(unname(as.matrix(r)), m)
})