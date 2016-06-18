library(SentimentAnalysis)
context("Preprocessing of corpus")

test_that("function only accepts valid values as corpus", {
  expect_error(preprocessCorpus())
  expect_error(preprocessCorpus(NULL))
  expect_error(preprocessCorpus(0))
})

test_that("function only accepts valid values as language", {
  dummyCorpus <- transformIntoCorpus(c("a", "b", "c"))
  
  expect_error(preprocessCorpus(dummyCorpus, language=NULL))
  expect_error(preprocessCorpus(dummyCorpus, language=FALSE))
  expect_error(preprocessCorpus(dummyCorpus, language=0))
  expect_error(preprocessCorpus(dummyCorpus, language=min))
  expect_error(preprocessCorpus(dummyCorpus, language=c("english", "spanish")))
  
  expect_silent(preprocessCorpus(dummyCorpus, language="english"))
})

test_that("function only accepts valid values as stemming", {
  dummyCorpus <- transformIntoCorpus(c("a", "b", "c"))
  
  expect_error(preprocessCorpus(dummyCorpus, stemming=NULL))
  expect_error(preprocessCorpus(dummyCorpus, stemming=2))
  expect_error(preprocessCorpus(dummyCorpus, stemming=c(FALSE, FALSE)))
})

test_that("function only accepts valid values as verbose", {
  dummyCorpus <- transformIntoCorpus(c("a", "b", "c"))

  expect_error(preprocessCorpus(dummyCorpus, verbose=NULL))
  expect_error(preprocessCorpus(dummyCorpus, verbose=2))
  expect_error(preprocessCorpus(dummyCorpus, verbose=c(FALSE, FALSE)))
})
