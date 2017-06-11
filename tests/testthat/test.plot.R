library(SentimentAnalysis)
context("Plot functions")

library(ggplot2)

test_that("plotting a sentiment curve generates a ggplot object", {
  sentiment <- data.frame(Dictionary=runif(20))
  p <- plotSentiment(sentiment)
  
  expect_is(p, "gg")
  expect_is(p, "ggplot")
  
  p <- plotSentiment(sentiment) + theme_light()
  
  expect_is(p, "gg")
  expect_is(p, "ggplot")
})

test_that("plotting a scatterplot generates a ggplot object", {
  sentiment <- data.frame(Dictionary=runif(10))
  response <- sentiment[[1]] + rnorm(10)
  p <- plotSentimentResponse(sentiment, response)
  
  expect_is(p, "gg")
  expect_is(p, "ggplot")
  
  p <- plotSentimentResponse(sentiment, response) + theme_light()
  
  expect_is(p, "gg")
  expect_is(p, "ggplot")
})

test_that("plotting a weighted dictionary generates a ggplot object", {
  d <- SentimentDictionaryWeighted(paste0(character(100), 1:100), rnorm(100), numeric(100))
  p <- plot(d)
  
  expect_is(p, "gg")
  expect_is(p, "ggplot")
  
  p <- plot(d) + theme_light()
  
  expect_is(p, "gg")
  expect_is(p, "ggplot")
})