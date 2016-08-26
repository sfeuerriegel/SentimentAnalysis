test_that("word counting works correctly", {
  documents <- c("This is a test", "an one more")

  # count words (without stopwords)
  wc <- countWords(documents)
  expect_is(wc, "data.frame")
  expect_equal(nrow(wc), 2)
  expect_equal(ncol(wc), 1)
  expect_equal(colnames(wc), "WordCount")
  expect_equal(wc$WordCount, c(1, 1))
  
  # count all words (including stopwords)
  wc <- countWords(documents, removeStopwords=FALSE)
  expect_is(wc, "data.frame")
  expect_equal(nrow(wc), 2)
  expect_equal(ncol(wc), 1)
  expect_equal(colnames(wc), "WordCount")
  expect_equal(wc$WordCount, c(4, 3))
})