library(SentimentAnalysis)
context("Main function for sentiment analysis")

library(tm)

test_that("sentiment analysis returns correct values for Reuters corpus", {
  data("crude")
  reuters <- tm_map(crude, PlainTextDocument)
  reuters <- tm_map(reuters, content_transformer(tolower))
  dtm <- DocumentTermMatrix(reuters)
  
  sentiment <- analyzeSentiment(dtm)
  
  expect_is(sentiment, "data.frame")
  expect_equal(nrow(sentiment), 20)
  expect_equal(ncol(sentiment), 14)
  expect_equal(rownames(sentiment), as.character(1:nrow(sentiment)))

  expect_equal(sentiment$WordCount, c(123, 419, 92, 102, 118, 
                                      421, 409, 172, 313, 338, 
                                      371, 136, 141, 130, 136,
                                      167, 210, 117, 287, 96))
})

test_that("sentiment analysis works with custom rules", {
  documents <- c("Alice works much better",
                 "Novel algorithms work absolutely good")
  dictionaryAmplifiers <- SentimentDictionary(c("absolut", "much"))
  sentiment <- analyzeSentiment(documents,
                                rules=list("Amplifiers"=list(ruleRatio,
                                                             dictionaryAmplifiers)))
  expect_is(sentiment, "data.frame")
  expect_equal(colnames(sentiment), "Amplifiers")
  expect_equal(sentiment$Amplifiers, c(0.25, 0.2))

  documents <- c("Das ist ein gutes Resultat",
                 "Das Ergebnis war schlecht")
  dictionaryGerman <- SentimentDictionaryBinary(c("gut"), 
                                                c("schlecht"))
  sentiment <- analyzeSentiment(documents,
                                language="german",
                                rules=list("GermanSentiment"=list(ruleSentiment, dictionaryGerman)))
  expect_is(sentiment, "data.frame")
  expect_equal(colnames(sentiment), "GermanSentiment")
  expect_equal(sentiment$GermanSentiment, c(0.5, -0.5))
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
  response <- convertToBinaryResponse(c(+1, +1, -1, -1))
  cmp <- compareToResponse(sentiment, response)
  expect_equal(cmp["Accuracy", "Sentiment"], 0.5)
  expect_equal(cmp["Sensitivity", "Sentiment"], 0)
  expect_equal(cmp["Specificity", "Sentiment"], 1)
  expect_equal(cmp["avg.sentiment.pos.response", "Sentiment"], 3)
  expect_equal(cmp["avg.sentiment.neg.response", "Sentiment"], 1)
})

test_that("binary classifier is evaluated correctly", {
  r <- evalBinaryClassifier(convertToBinaryResponse(c(+1, +1, +1, +1)),
                            convertToBinaryResponse(c(-1, -1, -1, -1)))
  expect_equivalent(as.numeric(r), c(0, NaN, 0, NaN, NaN, NaN))

  r <- evalBinaryClassifier(convertToBinaryResponse(c(+1, +1, +1, +1)),
                            convertToBinaryResponse(c(+1, +1, +1, +1)))
  expect_equivalent(as.numeric(r), c(1, NaN, NaN, 1, 0, NaN))
  
  r <- evalBinaryClassifier(convertToBinaryResponse(c(+1, -1, +1, -1)),
                            convertToBinaryResponse(c(-1, +1, +1, -1)))
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

test_that("word counting works correctly", {
  documents <- c("This is a test", "an one more")

  # count words (without stopwords)
  wc <- analyzeSentiment(documents, rules=list("WordCount"=list(ruleWordCount)))
  expect_equal(wc$WordCount, c(1, 1))
  
  # count all words (including stopwords)
  wc <- analyzeSentiment(documents, rules=list("WordCount"=list(ruleWordCount)), removeStopwords=FALSE)
  expect_equal(wc$WordCount, c(2, 2))
  # Note: the latter does not overwrite minWordLength (default: 3) because of which "is", "a", etc. is removed
})