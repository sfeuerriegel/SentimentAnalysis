"ruleWordCount" <- function(dtm) {
  return(rowSums(as.matrix(dtm)))
}

"ruleDocumentCount" <- function() {
  # TODO
}

"ruleRatio" <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryWordlist")) {
    stop("Rule does not support dictionary type")
  }
  
  return(rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$wordlist)])) / rowSums(as.matrix(dtm)))
}

"rulePositivity" <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryBinary")) {
    stop("Rule does not support dictionary type")
  }
  
  return(rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$positiveWords)])) / rowSums(as.matrix(dtm)))
}

"ruleNegativity" <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryBinary")) {
    stop("Rule does not support dictionary type")
  }
  
  return(rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$negativeWords)])) / rowSums(as.matrix(dtm)))
}

"ruleSentiment" <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryBinary")) {
    stop("Rule does not support dictionary type")
  }
  
  return((rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$positiveWords)])) 
          - rowSums(as.matrix(dtm[, which(colnames(dtm) %in% d$negativeWords)]))) / rowSums(as.matrix(dtm)))
}

"ruleLinearModel" <- function(dtm, d) {
  if (!inherits(d, "SentimentDictionaryWeighted")) {
    stop("Rule does not support dictionary type")
  }
  
  # TODO check formula -> tdf?
  idx <- intersect(d$words, colnames(dtm))
  return(ifelse(is.null(d$intercept), 0, d$intercept) 
         + rowSums(d$scores[which(d$words %in% idx)] %*% as.matrix(dtm[, which(colnames(dtm) %in% idx)])))
}

"allSentimentRules" <- function() {
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
