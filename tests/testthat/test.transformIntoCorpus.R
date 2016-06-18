library(SentimentAnalysis)
context("Transforming data into a corpus")

test_that("function only accepts selected class types", {
  expect_error(transformIntoCorpus())
  expect_error(transformIntoCorpus(NULL))
  expect_error(transformIntoCorpus(FALSE))
  expect_error(transformIntoCorpus(0))
  expect_error(transformIntoCorpus(c(1, 2, 3)))
})

test_that("function transforms character into correct corpus", {
  corpus <- transformIntoCorpus(character(0))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 0)
  
  corpus <- transformIntoCorpus("")
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 1)
  expect_equivalent(corpus[[1]]$content, "")
  
  corpus <- transformIntoCorpus(c("Test"))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 1)
  expect_equivalent(corpus[[1]]$content, "Test")
  
  corpus <- transformIntoCorpus(c("Aaa", "Bbb", "Ccc"))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")
})

test_that("function transforms list into correct corpus", {
  corpus <- transformIntoCorpus(list())
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 0)

  corpus <- transformIntoCorpus(list(""))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 1)
  expect_equivalent(corpus[[1]]$content, "")
  
  corpus <- transformIntoCorpus(list("Test"))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 1)
  expect_equivalent(corpus[[1]]$content, "Test")
  
  corpus <- transformIntoCorpus(list("Aaa", "Bbb", "Ccc"))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")
  
  corpus <- transformIntoCorpus(list(x="Aaa", y="Bbb", z="Ccc"))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")  
  
  expect_error(transformIntoCorpus(list("Aaa", "Bbb", 3)))
  expect_error(transformIntoCorpus(list("Aaa", "Bbb", FALSE)))
  expect_error(transformIntoCorpus(list("Aaa", "Bbb", NULL)))
  expect_error(transformIntoCorpus(list("Aaa", "Bbb", c("x", "y"))))
})

test_that("function transforms data.frame (without factors) into correct corpus", {
  corpus <- transformIntoCorpus(data.frame())
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 0)

  corpus <- transformIntoCorpus(data.frame("", stringsAsFactors=FALSE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 1)
  expect_equivalent(corpus[[1]]$content, "")
  
  corpus <- transformIntoCorpus(data.frame("Test", stringsAsFactors=FALSE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 1)
  expect_equivalent(corpus[[1]]$content, "Test")
  
  corpus <- transformIntoCorpus(data.frame("Aaa", "Bbb", "Ccc", stringsAsFactors=FALSE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")
  
  corpus <- transformIntoCorpus(data.frame(x=c("Aaa", "Bbb", "Ccc"), stringsAsFactors=FALSE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")  
  
  corpus <- transformIntoCorpus(data.frame(x=c("Aaa", "Bbb", "Ccc"), stringsAsFactors=FALSE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")
  
  expect_error(transformIntoCorpus(data.frame(Row1=c("a", "a", "a"),
                                              Row2=c("b", "b", "b"),
                                              stringsAsFactors=FALSE)))
})

test_that("function transforms data.frame (with factors) into correct corpus", {
  corpus <- transformIntoCorpus(data.frame())
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 0)
  
  corpus <- transformIntoCorpus(data.frame("", stringsAsFactors=TRUE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 1)
  expect_equivalent(corpus[[1]]$content, "")
  
  corpus <- transformIntoCorpus(data.frame("Test", stringsAsFactors=TRUE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 1)
  expect_equivalent(corpus[[1]]$content, "Test")
  
  corpus <- transformIntoCorpus(data.frame("Aaa", "Bbb", "Ccc", stringsAsFactors=TRUE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")
  
  corpus <- transformIntoCorpus(data.frame(x=c("Aaa", "Bbb", "Ccc"), stringsAsFactors=TRUE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")  
  
  corpus <- transformIntoCorpus(data.frame(x=c("Aaa", "Bbb", "Ccc"), stringsAsFactors=TRUE))
  expect_is(corpus, "Corpus")
  expect_equivalent(length(corpus), 3)
  expect_equivalent(corpus[[1]]$content, "Aaa")
  expect_equivalent(corpus[[2]]$content, "Bbb")
  expect_equivalent(corpus[[3]]$content, "Ccc")
  
  expect_error(transformIntoCorpus(data.frame(Row1=c("a", "a", "a"),
                                              Row2=c("b", "b", "b"),
                                              stringsAsFactors=TRUE)))
})