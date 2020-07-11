### ported from ngramrr

taungram <- function(text, n = 1, tolower = FALSE, split = "[[:space:]]+", ...) {
    r <- tau::textcnt(text, method = 'string', n = n, tolower = tolower, split = split, ...)
    return(Reduce(c, sapply(1:length(r), function(x) rep(names(r[x]), r[x]))))
}

tauchar <- function(text, n = 1, tolower = FALSE, split = "[[:space:]]+", rmEOL = FALSE, ngmin = 1 , ...) {
    r <- tau::textcnt(text, method = 'ngram', n = n, tolower = tolower, split = split, ...)
    g <- unlist(sapply(1:length(r), function(x) rep(names(r[x]), r[x])))
    if (rmEOL) {
        g <- g[grep("_", g, invert = TRUE)]
    }
    if (ngmin > 1 & ngmin <= n) {
        g <- Filter(function(x) nchar(x) >= ngmin, g)
    }
    return(g)
}

# General purpose n-gram tokenizer
#
# A non-Java based n-gram tokenizer to be used with the tm package. Support both character and word n-gram.
# 
# @param x input string.
# @param char logical, using character n-gram. char = FALSE denotes word n-gram.
# @param ngmin integer, minimun order of n-gram
# @param ngmax integer, maximun order of n-gram
# @param rmEOL logical, remove ngrams wih EOL character
# @return vector of n-grams
# @examples
# require(tm)
# 
# nirvana <- c("hello hello hello how low", "hello hello hello how low",
# "hello hello hello how low", "hello hello hello",
# "with the lights out", "it's less dangerous", "here we are now", "entertain us",
# "i feel stupid", "and contagious", "here we are now", "entertain us",
# "a mulatto", "an albino", "a mosquito", "my libido", "yeah", "hey yay")
#
# ngramrr(nirvana[1], ngmax = 3)
# ngramrr(nirvana[1], ngmax = 3, char = TRUE)
# nirvanacor <- Corpus(VectorSource(nirvana))
# TermDocumentMatrix(nirvanacor, control = list(tokenize = function(x) ngramrr(x, ngmax =3)))
#
# # Character ngram
# 
# TermDocumentMatrix(nirvanacor, control = list(tokenize =
# function(x) ngramrr(x, char = TRUE, ngmax =3), wordLengths = c(1, Inf)))
ngramrr <- function(x, char = FALSE, ngmin = 1, ngmax = 2, rmEOL = TRUE) {
    if (ngmin > ngmax) {
        stop("ngmax must be higher than or equal to ngmin")
    }
    y <- paste(x, collapse = " ") # why TDM is so stupid?
    if (char) {
        return(tauchar(y, n = ngmax, rmEOL = rmEOL, ngmin = ngmin))
    }
    sentencelength <- length(unlist(strsplit(y, split = " ")))
    if (sentencelength > ngmax) {
        return(Reduce(c, Map(function(n) taungram(y, n), seq(from = ngmin, to = ngmax))))
    } else {
        return(Reduce(c, Map(function(n) taungram(y, n), seq(from = ngmin, to = sentencelength ))))
    }
}


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
#' en.corpus <- VCorpus(VectorSource(en))
#' tdm <- TermDocumentMatrix(en.corpus, 
#'                           control=list(wordLengths=c(1,Inf), 
#'                                        tokenize=function(x) ngram_tokenize(x, char=TRUE, 
#'                                                                            ngmin=3, ngmax=3)))
#' inspect(tdm)
#' 
#' ch <- c("abab", "aabb")
#' ch.corpus <- VCorpus(VectorSource(ch))
#' tdm <- TermDocumentMatrix(ch.corpus, 
#'                           control=list(wordLengths=c(1,Inf), 
#'                                        tokenize=function(x) ngram_tokenize(x, char=TRUE, 
#'                                                                            ngmin=1, ngmax=2)))
#' inspect(tdm)
#' @keywords preprocessing
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
      return(ngramrr(x, char=char, ngmin=ngmin, ngmax=ngmax))
  }
}
