library(SentimentAnalysis)
context("Testing n-gram tokenizer")

test_that("character n-grams are generated correctly", {
  en <- c("abcdcd", "cde")
  en.corpus <- VCorpus(VectorSource(en))
  tdm <- TermDocumentMatrix(en.corpus, control=list(wordLengths=c(1,Inf), tokenize=function(x) ngram_tokenize(x, char=TRUE, ngmin=1, ngmax=2)))
  
  expect_equivalent(as.matrix(tdm["a",]), matrix(c(1, 0), nrow=1))
  expect_equivalent(as.matrix(tdm["c",]), matrix(c(2, 1), nrow=1))
  expect_equivalent(as.matrix(tdm["cd",]), matrix(c(2, 1), nrow=1))
  expect_equivalent(as.matrix(tdm["de",]), matrix(c(0, 1), nrow=1))
  expect_equivalent(colSums(as.matrix(tdm)), c(11, 5))
})

test_that("word n-grams are generated correctly", {
  library(tm)
  
  en <- c("Romeo loves Juliet", "Romeo loves a girl")
  en.corpus <- VCorpus(VectorSource(en))
  tdm <- TermDocumentMatrix(en.corpus, control=list(wordLengths=c(1,Inf), tokenize=function(x) ngram_tokenize(x, char=FALSE, ngmin=1, ngmax=3)))
  
  expect_equivalent(as.matrix(tdm["romeo",]), matrix(c(1, 1), nrow=1))
  expect_equivalent(as.matrix(tdm["romeo loves juliet",]), matrix(c(1, 0), nrow=1))
  expect_equivalent(as.matrix(tdm["romeo loves",]), matrix(c(1, 1), nrow=1))
  expect_equivalent(as.matrix(tdm["a",]), matrix(c(0, 1), nrow=1))
  expect_equivalent(colSums(as.matrix(tdm)), c(6, 9))
})
