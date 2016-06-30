#' Counts word frequencies
#' 
#' Counts total word frequencies in each document
#' @param dtm Document-term matrix
#' @return Total number of words
#' @keywords rules
#' @export
ruleWordCount <- function(dtm) {
  return(rowSums(as.matrix(dtm)))
}

ruleDocumentCount <- function(dtm) {
  # TODO
}

#' Ratio of dictionary words
#' 
#' Ratio of words in that dictionary compared to the total number of words in
#' the document
#' @param dtm Document-term matrix
#' @param d Dictionary of type \code{\link{SentimentDictionaryWordlist}} with words 
#' belonging to a single category
#' @return Ratio of dictionary words compared to all
#' @keywords rules
#' @export
ruleRatio <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryWordlist")) {
    stop("Rule does not support dictionary type")
  }
  
  return(rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$wordlist)])) / rowSums(as.matrix(dtm)))
}

#' Ratio of positive words
#' 
#' Ratio of words labeled as positive in that dictionary compared to the total 
#' number of words in the document. Here, it uses the entry \code{positiveWords}
#' of the \code{\link{SentimentDictionaryBinary}}.
#' @param dtm Document-term matrix
#' @param d Dictionary of type \code{\link{SentimentDictionaryBinary}}
#' @return Ratio of positive words compared to all
#' @export
rulePositivity <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryBinary")) {
    stop("Rule does not support dictionary type")
  }
  
  return(rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$positiveWords)])) / rowSums(as.matrix(dtm)))
}

#' Ratio of negative words
#' 
#' Ratio of words labeled as negative in that dictionary compared to the total 
#' number of words in the document. Here, it uses the entry \code{negativeWords}
#' of the \code{\link{SentimentDictionaryBinary}}.
#' @param dtm Document-term matrix
#' @param d Dictionary of type \code{\link{SentimentDictionaryBinary}}
#' @return Ratio of negative words compared to all
#' @keywords rules
#' @export
ruleNegativity <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryBinary")) {
    stop("Rule does not support dictionary type")
  }
  
  return(rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$negativeWords)])) / rowSums(as.matrix(dtm)))
}

#' Sentiment score
#' 
#' Sentiment score defined as the difference between positive and negative
#' word counts divided by the total number of words. 
#' @param dtm Document-term matrix
#' @param d Dictionary of type \code{\link{SentimentDictionaryBinary}}
#' @return Sentiment score in the range of -1 to 1.
#' @details Given the number of positive words \eqn{P} and the number of 
#' negative words \eqn{N}. Further, let \eqn{T} denote the total number of words
#' in that document. Then, the sentiment ratio is defined as 
#' \deqn{\frac{P+N}{T}}. Here, it uses the entries \code{negativeWords} and
#' \code{positiveWords} of the \code{\link{SentimentDictionaryBinary}}.
#' @keywords rules
#' @export
ruleSentiment <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryBinary")) {
    stop("Rule does not support dictionary type")
  }
  
  return((rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$positiveWords)])) 
          - rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$negativeWords)]))) / rowSums(as.matrix(dtm)))
}

#' Sentiment based on linear model
#' 
#' Sentiment score as denoted by a linear model.
#' @param dtm Document-term matrix
#' @param d Dictionary of type \code{\link{SentimentDictionaryWeighted}}
#' @return Continuous sentiment score
#' @keywords rules
#' @export
ruleLinearModel <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryWeighted")) {
    stop("Rule does not support dictionary type")
  }
  
  # TODO check formula -> tdf?
  idx <- intersect(d$words, colnames(dtm))
  return(ifelse(is.null(d$intercept), 0, d$intercept) 
         + rowSums(d$scores[which(d$words %in% idx)] %*% as.matrix(dtm[, which(colnames(dtm) %in% idx)])))
}

defaultSentimentRules <- function() {
  r <- list("WordCount"=list(ruleWordCount))
  
  for (n in names(allDictionaries())) {
    d <- allDictionaries()[[n]][[2]]
    if (inherits(d, "SentimentDictionaryWordlist")) {
      r[[paste0("Ratio", n)]] <- list(ruleRatio, d)
    } else if (inherits(d, "SentimentDictionaryBinary")) {
      r[[paste0("Sentiment", n)]] <- list(ruleSentiment, d)
      r[[paste0("Negativity", n)]] <- list(rulePositivity, d)
      r[[paste0("Positivity", n)]] <- list(ruleNegativity, d)
    } else if (inherits(d, "SentimentDictionaryWeighted")) {
      r[[paste0("lm.", n)]] <- list(ruleLinearModel, d)
    } else {
      stop("Dictionary type not supported")
    }
  }
  
  return(r)
}
