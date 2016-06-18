rep_gram <- function(text, n) {
  r <- stringdist::qgrams(text, q=n)
  g <- unlist(sapply(1:length(r), function(x) rep(colnames(r)[x], r[x])))
  return(g)
}

rep_grams <- function(text, ngmin=1, ngmax=2) {
  g <- unlist(sapply(ngmin:ngmax, function(x) rep_gram(text, x)))
  return(g)
}

#' N-gram tokenizer
#' 
#' A tokenizer for use with a document-term matrix from the tm package. Supports 
#' both character and word ngrams, including own wrapper to handle non-Latin 
#' encodings
#' 
#' @param x input string
#' @param char boolean value specifying whether to use character (char = TRUE) 
#' or word n-grams (char = FALSE, default)
#' @param ngmin integer giving the minimum order of n-gram (default: 1)
#' @param ngmax integer giving the maximum order of n-gram (default: 3)
#' @examples
#' library(tm)
#' en <- c("Romeo loves Juliet", "Romeo loves a girl")
#' en.corpus <- Corpus(VectorSource(en))
#' tdm <- TermDocumentMatrix(en.corpus, 
#'                           control=list(wordLengths=c(1,Inf), 
#'                                        tokenize=function(x) ngram_tokenize(x, char=TRUE, 
#'                                                                            ngmin=3, ngmax=3)))
#' inspect(tdm)
#' 
#' ch <- c("abab", "aabb")
#' ch.corpus <- Corpus(VectorSource(ch))
#' tdm <- TermDocumentMatrix(ch.corpus, 
#'                           control=list(wordLengths=c(1,Inf), 
#'                                        tokenize=function(x) ngram_tokenize(x, char=TRUE, 
#'                                                                            ngmin=1, ngmax=2)))
#' inspect(tdm)
#' @export
ngram_tokenize <- function(x, char=FALSE, ngmin=1, ngmax=3) {
  if (ngmin > ngmax) {
    stop("ngmax must be higher than ngmin")
  }
    
  if (!is.logical(char)) {
    stop("Customized routine only supports char grams")
  }
    
  y <- paste(x, collapse=" ") # hint from ngramrr package
  if (char) {
      return(rep_grams(y, ngmin = ngmin, ngmax = ngmax))
  } else {
      return(ngramrr::ngramrr(x, char=char, ngmin=ngmin, ngmax=ngmax))
  }
}
