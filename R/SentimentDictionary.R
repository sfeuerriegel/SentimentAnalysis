cat0 <- function(...) cat(..., "\n", sep="")

#' Create new sentiment dictionary based on input
#' 
#' Depending on the input, this function creates a new sentiment dictionary of different type.
#' 
#' @param ... Arguments as passed to one of the three functions 
#' \code{\link{SentimentDictionaryWordlist}}, \code{\link{SentimentDictionaryBinary}} or
#' \code{\link{SentimentDictionaryWeighted}}
#' @keywords dictionary
#' @seealso \code{\link{SentimentDictionaryWordlist}}, 
#' \code{\link{SentimentDictionaryBinary}}, 
#' \code{\link{SentimentDictionaryWeighted}}
#' @export
"SentimentDictionary" <- function(...) {
  if (nargs() == 1) {
    return(SentimentDictionaryWordlist(...))
  } else if (nargs() == 2) {
    return(SentimentDictionaryBinary(...))
  } else if (nargs() >= 3) {
    return(SentimentDictionaryWeighted(...))
  } else {
    stop("Not supported")
  }
}

#' Create a sentiment dictionary consisting of a simple wordlist
#' 
#' This routine creates a new object of type \code{SentimentDictionaryWordlist} 
#' @param wordlist is a vector containing the individual entries as strings
#' @return Returns a new object of type \code{SentimentDictionaryWordlist} 
#' @examples
#' # generate a dictionary with "uncertainty" words
#' d <- SentimentDictionaryWordlist(c("uncertain", "possible", "likely"))
#' summary(d)
#' # alternative call
#' d <- SentimentDictionary(c("uncertain", "possible", "likely"))
#' summary(d)
#' @keywords dictionary
#' @seealso \code{\link{SentimentDictionary}}
#' @export
"SentimentDictionaryWordlist" <-  function(wordlist) {
  if (is.null(wordlist) || !is.character(wordlist)) {
    stop("Input variable wordlist does not match expected format")
  }
  
  d <- structure(list(wordlist=wordlist),
                 class="SentimentDictionaryWordlist")
  return(d)
}

#' Create a sentiment dictionary of positive and negative words
#' 
#' This routines creates a new object of type \code{SentimentDictionaryBinary} that
#' stores two separate vectors of negative and positive words
#' @param positiveWords is a vector containing the entries labeled as positive
#' @param negativeWords is a vector containing the entries labeled as negative
#' @return Returns a new object of type \code{SentimentDictionaryBinary} 
#' @examples
#' # generate a dictionary with positive and negative words
#' d <- SentimentDictionaryBinary(c("increase", "rise", "more"),
#'                                c("fall", "drop"))
#' summary(d)
#' # alternative call
#' d <- SentimentDictionary(c("increase", "rise", "more"),
#'                          c("fall", "drop"))
#' summary(d)
#' @keywords dictionary
#' @seealso \code{\link{SentimentDictionary}}
#' @export
"SentimentDictionaryBinary" <-  function(positiveWords, negativeWords) {
  if (is.null(positiveWords) || !is.character(positiveWords)) {
    stop("Input variable positiveWords does not match expected format")
  }
  if (is.null(negativeWords) || !is.character(negativeWords)) {
    stop("Input variable negativeWords does not match expected format")
  }
  
  d <- structure(list(positiveWords=positiveWords, negativeWords=negativeWords),
                 class="SentimentDictionaryBinary")
  return(d)
}

#' Create a sentiment dictionary of words linked to a score
#' 
#' This routine creates a new object of type \code{SentimentDictionaryWeighted} that
#' contains a number of words, each linked to a continuous score (i.e. weight) for
#' specifying its polarity. The scores can later be interpreted as a linear model
#' @param words is collection (vector) of different words as strings
#' @param scores are the corresponding socres or weights denoting the word's polarity
#' @param idf provide further details on the frequency of words in the corpus as an
#' additional source for normalization
#' @param intercept is an optional parameter for shifting the zero level (default: 0)
#' @return Returns a new object of type \code{SentimentDictionaryWordlist} 
#' @note The intercept is useful when the mean or median of a response variable is 
#' not exactly located at zero. For instance, stock market returns have slight positive
#' bias.
#' @examples
#' # generate dictionary (based on linear model)
#' d <- SentimentDictionaryWeighted(c("increase", "decrease", "exit"),
#'                                  c(+1, -1, -10),
#'                                  rep(NA, 3))
#' summary(d)
#' # alternative call
#' d <- SentimentDictionary(c("increase", "decrease", "exit"),
#'                          c(+1, -1, -10),
#'                          rep(NA, 3))
#' summary(d)                                
#' @references Pr{\"o}llochs and Feuerriegel (2015). Generating Domain-Specific 
#' Dictionaries Using Bayesian Learning. 23rd European Conference on Information 
#' Systems (ECIS 2015).
#' @source \url{http://dx.doi.org/10.2139/ssrn.2522884}
#' @keywords dictionary
#' @seealso \code{\link{SentimentDictionary}}
#' @export
"SentimentDictionaryWeighted" <-  function(words, scores, idf, intercept=0) {
  if (is.null(words) || !is.character(words)) {
    stop("Input variable words does not match expected format")
  }
  if (is.null(scores) || !is.numeric(scores)) {
    stop("Input variable scores does not match expected format")
  }
  if (is.null(idf) || !all(is.numeric(idf) || is.na(idf))) {
    stop("Input variable idf does not match expected format")
  }
  if (is.null(intercept) || !is.numeric(intercept) || length(intercept) > 1) {
    stop("Input variable intercept does not match expected format")
  }
  if (length(words) != length(scores)) {
    stop("Arguments 'words' and 'scores' must be of same length.")
  }
  if (length(words) != length(idf)) {
    stop("Arguments 'words' and 'idf' must be of same length.")
  }

  d <- structure(list(words=words, scores=scores, idf=idf, intercept=intercept),
                 class="SentimentDictionaryWeighted")
  return(d)
}

#' Read dictionary from text file
#' 
#' This routine reads a sentiment dictionary from a text file. Such a text file can
#' be created e.g. via \code{\link{write}}. The dictionary type is recognized 
#' according to the internal format of the file. 
#' @param file File name pointing to text file
#' @return Dictionary of type \code{\link{SentimentDictionaryWordlist}},
#' \code{\link{SentimentDictionaryBinary}} or
#' \code{\link{SentimentDictionaryWeighted}}
#' @examples
#' d.out <- SentimentDictionary(c("uncertain", "possible", "likely"))
#' write(d.out, "example.dict")
#' d.in <- read("example.dict")
#' print(d.in)
#' 
#' d.out <- SentimentDictionary(c("increase", "rise", "more"),
#'                              c("fall", "drop"))
#' write(d.out, "example.dict")
#' d.in <- read("example.dict")
#' print(d.in)
#' 
#' d.out <- SentimentDictionary(c("increase", "decrease", "exit"),
#'                              c(+1, -1, -10),
#'                              rep(NA, 3),
#'                              intercept=5)
#' write(d.out, "example.dict")
#' d.in <- read("example.dict")
#' print(d.in)
#' 
#' unlink("example.dict")
#' @keywords dictionary
#' @seealso \code{\link{write}} for creating such a file
#' @importFrom utils read.table
#' @export
"read" <- function(file) {
  content <- as.matrix(read.table(file, header=FALSE, stringsAsFactors=FALSE, sep=","))
  if (ncol(content) == 3) {
    return(SentimentDictionaryWeighted(content[3:nrow(content),1],
                                       as.numeric(content[3:nrow(content),2]),
                                       idf=as.numeric(content[3:nrow(content),3]),
                                       intercept=as.numeric(content[1,2])))
  } else if (content[1,1] == "POSITIVE_WORDS") {
    return(SentimentDictionaryBinary(content[2:(which(content=="NEGATIVE_WORDS")-1),1], 
                                     content[(which(content=="NEGATIVE_WORDS")+1):length(content),1]))
  } else {
    return(SentimentDictionaryWordlist(content[,1]))
  }
}

#' Write dictionary to text file
#' 
#' This routine exports a sentiment dictionary to a text file which can be the source
#' for additional problems or controlling the output. 
#' @param d Dictionary of type \code{\link{SentimentDictionaryWordlist}},
#' \code{\link{SentimentDictionaryBinary}} or
#' \code{\link{SentimentDictionaryWeighted}}
#' @param file File to which the dictionary should be exported
#' @examples
#' d.out <- SentimentDictionary(c("uncertain", "possible", "likely"))
#' write(d.out, "example.dict")
#' d.in <- read("example.dict")
#' print(d.in)
#' 
#' d.out <- SentimentDictionary(c("increase", "rise", "more"),
#'                              c("fall", "drop"))
#' write(d.out, "example.dict")
#' d.in <- read("example.dict")
#' print(d.in)
#' 
#' d.out <- SentimentDictionary(c("increase", "decrease", "exit"),
#'                              c(+1, -1, -10),
#'                              rep(NA, 3),
#'                              intercept=5)
#' write(d.out, "example.dict")
#' d.in <- read("example.dict")
#' print(d.in)
#' 
#' unlink("example.dict")
#' @keywords dictionary
#' @seealso \code{\link{read}} for later access
#' @rdname write
#' @export
"write" <- function(d, file) {
  if (missing(file)) {
    stop("Argument file is missing")
  }
  if (is.null(d)) {
    stop("Argument d cannot be null")
  }  
  if (is.null(file)) {
    stop("Argument file cannot be null")
  }  
  
  UseMethod("write", d)
}

#' @rdname write
#' @importFrom utils write.table
#' @export
"write.SentimentDictionaryWordlist" <- function(d, file) {
  write.table(d$wordlist, file=file, row.names=FALSE, col.names=FALSE, sep=",")
}

#' @rdname write
#' @importFrom utils write.table
#' @export
"write.SentimentDictionaryBinary" <- function(d, file) {
  write.table(c("POSITIVE_WORDS", d$positiveWords, "NEGATIVE_WORDS", d$negativeWords), file=file, row.names=FALSE, col.names=FALSE, sep=",")
}

#' @rdname write
#' @importFrom utils write.table
#' @export
"write.SentimentDictionaryWeighted" <- function(d, file) {
  tbl <- rbind(c("Intercept", d$intercept, NA),
                 c("Words", "Scores", "Idf"),
                 cbind(d$words, d$scores, d$idf))
  write.table(tbl, file=file, row.names=FALSE, col.names=FALSE, sep=",")
}

#' Number of words in dictionary
#' 
#' Counts total number of entries in dictionary.
#' @param d Dictionary of type \code{\link{SentimentDictionaryWordlist}},
#' \code{\link{SentimentDictionaryBinary}} or
#' \code{\link{SentimentDictionaryWeighted}}
#' @examples 
#' numEntries(SentimentDictionary(c("uncertain", "possible", "likely"))) # returns 3
#' numEntries(SentimentDictionary(c("increase", "rise", "more"),
#'                             c("fall", "drop"))) # returns 5
#' numEntries(SentimentDictionary(c("increase", "decrease", "exit"),
#'                                c(+1, -1, -10),
#'                                rep(NA, 3))) # returns 3
#' @keywords dictionary
#' @seealso \code{\link{numPositiveEntries}} and 
#' \code{\link{numNegativeEntries}} for more option to count the number of entries
#' @export
"numEntries" <- function(d) {
  if (inherits(d, "SentimentDictionaryBinary")) {
    return(length(d$positiveWords) + length(d$negativeWords))
  } else if (inherits(d, "SentimentDictionaryWeighted")) {
    return(length(d$words))
  } else if (inherits(d, "SentimentDictionaryWordlist")) {
    return(length(d$wordlist))
  } else {
    stop("Type not supported")
  }
}

#' Number of positive words in dictionary
#' 
#' Counts total number of positive entries in dictionary.
#' @param d is a dictionary of type \code{\link{SentimentDictionaryBinary}} or
#' \code{\link{SentimentDictionaryWeighted}}
#' @note Entries in \code{\link{SentimentDictionaryWeighted}} with a weight of 0 
#' are not counted here 
#' @examples 
#' numPositiveEntries(SentimentDictionary(c("increase", "rise", "more"),
#'                             c("fall", "drop"))) # returns 3
#' numPositiveEntries(SentimentDictionary(c("increase", "decrease", "exit"),
#'                                c(+1, -1, -10),
#'                                rep(NA, 3))) # returns 1
#' @keywords dictionary
#' @seealso \code{\link{numEntries}} and
#' \code{\link{numNegativeEntries}} for more option to count the number of entries
#' @export
"numPositiveEntries" <- function(d) {
  if (inherits(d, "SentimentDictionaryBinary")) {
    return(length(d$positiveWords))
  } else if (inherits(d, "SentimentDictionaryWeighted")) {
    return(sum(d$scores > 0))
  } else {
    stop("Type not supported")
  }
}

#' Number of negative words in dictionary
#' 
#' Counts total number of negative entries in dictionary.
#' @param d is a dictionary of type \code{\link{SentimentDictionaryBinary}} or
#' \code{\link{SentimentDictionaryWeighted}}
#' @note Entries in \code{\link{SentimentDictionaryWeighted}} with a weight of 0 
#' are not counted here 
#' @examples 
#' numNegativeEntries(SentimentDictionary(c("increase", "rise", "more"),
#'                             c("fall", "drop"))) # returns 2
#' numNegativeEntries(SentimentDictionary(c("increase", "decrease", "exit"),
#'                                c(+1, -1, -10),
#'                                rep(NA, 3))) # returns 2
#' @keywords dictionary
#' @seealso \code{\link{numEntries}} and 
#' \code{\link{numPositiveEntries}} for more option to count the number of entries
#' @export
"numNegativeEntries" <- function(d) {
  if (inherits(d, "SentimentDictionaryBinary")) {
    return(length(d$negativeWords))
  } else if (inherits(d, "SentimentDictionaryWeighted")) {
    return(sum(d$scores < 0))
  } else {
    stop("Type not supported")
  }
}

#' Output content of sentiment dictionary
#' 
#' Prints entries of sentiment dictionary to the secreen
#' @param x Sentiment dictionary of type \code{\link{SentimentDictionaryWordlist}}, 
#' \code{\link{SentimentDictionaryBinary}} or \code{\link{SentimentDictionaryWeighted}}
#' @param ... Additional parameters passed to specific sub-routines
#' @examples
#' print(SentimentDictionary(c("uncertain", "possible", "likely")))
#' print(SentimentDictionary(c("increase", "rise", "more"),
#'                           c("fall", "drop")))
#' print(SentimentDictionary(c("increase", "decrease", "exit"),
#'                           c(+1, -1, -10),
#'                           rep(NA, 3)))
#' @keywords dictionary
#' @seealso \code{\link{summary}} for showing a brief summary
#' @rdname print
#' @export
"print.SentimentDictionaryWordlist" <- function(x, ...) {
  cat("Type: word list (single set)\n")
  for (i in x$wordlist) {
    cat0("* ", i)
  }
}

#' @rdname print
#' @export
"print.SentimentDictionaryBinary" <- function(x, ...) {
  cat("Type: binary (positive / negative)\n")
  for (i in x$positiveWords) {
    cat0("+ ", i)
  }  
  for (i in x$negativeWords) {
    cat0("- ", i)
  }  
}

#' @rdname print
#' @export
"print.SentimentDictionaryWeighted" <- function(x, ...) {
  cat0("Type: weighted (words with individual scores)")
  cat0("Intercept: ", x$intercept)
  for (i in order(x$scores)) {
    cat0(sprintf("%5.2f", x$scores[i]), " ", x$words[i])
  }
}

#' Output summary information on sentiment dictionary
#' 
#' @param object Sentiment dictionary of type \code{\link{SentimentDictionaryWordlist}}, 
#' \code{\link{SentimentDictionaryBinary}} or \code{\link{SentimentDictionaryWeighted}}
#' @param ... Additional parameters passed to specific sub-routines
#' @keywords dictionary
#' @seealso \code{\link{print}} for output the entries of a dictionary
#' @examples
#' summary(SentimentDictionary(c("uncertain", "possible", "likely")))
#' summary(SentimentDictionary(c("increase", "rise", "more"),
#'                             c("fall", "drop")))
#' summary(SentimentDictionary(c("increase", "decrease", "exit"),
#'                             c(+1, -1, -10),
#'                             rep(NA, 3)))
#' @rdname summary
#' @export
"summary.SentimentDictionaryWordlist" <- function(object, ...) {
  summary_SentimentDictionary(object, "word list (single set)", ...) 
}

#' @rdname summary
#' @export
"summary.SentimentDictionaryBinary" <- function(object, ...) {
  summary_SentimentDictionary(object, "binary (positive / negative)", ...) 
}

#' @rdname summary
#' @export
"summary.SentimentDictionaryWeighted" <- function(object, ...) {
  summary_SentimentDictionary(object, "weighted (words with individual scores)", ...) 
}

#' @importFrom stats median sd
"summary_SentimentDictionary" <- function(d, dictionaryType="unknown", ...) {
  cat0("Dictionary type:  ", dictionaryType)
  cat0("Total entries:    ", numEntries(d))
  if (!inherits(d, "SentimentDictionaryWordlist")) {
    cat0("Positive entries: ", numPositiveEntries(d), " (", round(100*numPositiveEntries(d)/numEntries(d), 2), "%)")
    cat0("Negative entries: ", numNegativeEntries(d), " (", round(100*numNegativeEntries(d)/numEntries(d), 2), "%)")
  }
  if (inherits(d, "SentimentDictionaryWeighted")) {
    cat0("Neutral entries:  ", sum(d$scores==0), " (", round(100*sum(d$scores==0)/numEntries(d), 2), "%)")
    
    cat0("\nDetails")
    cat0("Average score:      ", mean(d$scores))
    cat0("Median:             ", median(d$scores))
    cat0("Min:                ", min(d$scores))
    cat0("Max:                ", max(d$scores))
    cat0("Standard deviation: ", sd(d$scores))
    cat0("Skewness:           ", moments::skewness(d$scores))
  }
}

"compareOverlap" <- function(w1, w2) {
  cmp <- list(totalUniqueWords=length(unique(c(w1, w2))), 
              totalSameWords=length(intersect(w1, w2)))
  cmp$ratioSameWords <- cmp$totalSameWords / cmp$totalUniqueWords
  
  cat0("Total unique words: ", cmp$totalUniqueWords)
  cat0("Matching entries: ", cmp$totalSameWords, " (", cmp$ratioSameWords, "%)")
  
  return(cmp)
}

"compareClasses" <- function(cmp, pos1, neg1, pos2, neg2, neutral1=c(), neutral2=c()) {
  cmp$numWordsEqualClass <- length(intersect(unique(pos1), unique(pos2))) + length(intersect(unique(pos1), unique(pos2)))
  cmp$numWordsDifferentClass <- length(intersect(unique(pos1), unique(neg2))) + length(intersect(unique(neg1), unique(pos2)))
  cmp$ratioWordsEqualClass <- cmp$numWordsEqualClass / cmp$totalUniqueWords
  cmp$ratioWordsDifferentClass <- cmp$numWordsDifferentClass / cmp$totalUniqueWords
  
  cat0("Entries with same classification: ", cmp$numWordsEqualClass, " (", cmp$ratioWordsEqualClass, "%)")
  cat0("Entries with different classification: ", cmp$numWordsDifferentClass, " (", cmp$ratioWordsDifferentClass, "%)")
  
  return(cmp)
}

#' Compares two dictionaries
#' 
#' Routine compares two dictionaries in terms of how similarities and differences. Among the 
#' calculated measures are the total of distinct words, the overlap between both 
#' dictionaries, etc. 
#' @param d1 is the first sentiment dictionary of type \code{\link{SentimentDictionaryWordlist}}, 
#' \code{\link{SentimentDictionaryBinary}} or
#' \code{\link{SentimentDictionaryWeighted}}
#' @param d2 is the first sentiment dictionary of type \code{\link{SentimentDictionaryWordlist}}, 
#' \code{\link{SentimentDictionaryBinary}} or
#' \code{\link{SentimentDictionaryWeighted}}
#' @return Returns list with different metrics depending on dictionary type
#' @note Currently, this routine only supports the case where both dictionaries are of the
#' same type
#' @examples 
#' d1 <- SentimentDictionary(c("uncertain", "possible", "likely"))
#' d2 <- SentimentDictionary(c("rather", "intend", "likely"))
#' cmp <- compareDictionaries(d1, d2)
#' 
#' d1 <- SentimentDictionary(c("increase", "rise", "more"),
#'                           c("fall", "drop"))
#' d2 <- SentimentDictionary(c("positive", "rise", "more"),
#'                           c("negative", "drop"))
#' cmp <- compareDictionaries(d1, d2)
#' 
#' d1 <- SentimentDictionary(c("increase", "decrease", "exit"),
#'                           c(+1, -1, -10),
#'                           rep(NA, 3))
#' d2 <- SentimentDictionary(c("increase", "decrease", "drop", "neutral"),
#'                           c(+2, -5, -1, 0),
#'                           rep(NA, 4))
#' cmp <- compareDictionaries(d1, d2)
#' @keywords dictionary
#' @seealso \code{\link{SentimentDictionaryWordlist}}, 
#' \code{\link{SentimentDictionaryBinary}}, 
#' \code{\link{SentimentDictionaryWeighted}} for the specific classes
#' @importFrom stats cor
#' @export
"compareDictionaries" <- function(d1, d2) {
  if (inherits(d1, "SentimentDictionaryWordlist") && inherits(d2, "SentimentDictionaryWordlist")) {
    cat0("Comparing: wordlist vs wordlist\n")
    
    cmp <- compareOverlap(d1$wordlist, d2$wordlist)

    return(cmp)
  } else if (inherits(d1, "SentimentDictionaryBinary") && inherits(d2, "SentimentDictionaryBinary")) {
    cat0("Comparing: binary vs binary\n")

    cmp <- compareOverlap(c(d1$positiveWords, d1$negativeWords), c(d2$positiveWords, d2$negativeWord))
    cmp <- compareClasses(cmp, d1$positiveWords, d1$negativeWords, d2$positiveWords, d2$negativeWords)
    
    return(cmp)
  } else if (inherits(d1, "SentimentDictionaryWeighted") && inherits(d2, "SentimentDictionaryWeighted")) {
    cat0("Comparing: weighted vs weighted\n")
    
    cmp <- compareOverlap(d1$words, d2$words)
    cmp <- compareClasses(cmp,
                          d1$words[d1$scores > 0], d1$words[d1$scores < 0], 
                          d2$words[d2$scores > 0], d2$words[d2$scores < 0],
                          d1$words[d1$scores == 0], d2$words[d2$scores == 0])
    
    is <- intersect(d1$words, d2$words)
    cmp$correlation <- cor(d1$scores[unlist(lapply(is, function(x) which(d1$words == x)))], 
                           d2$scores[unlist(lapply(is, function(x) which(d2$words == x)))])
    cat0("Correlation between scores of matching entries: ", round(cmp$correlation, 2))
    cat0("Average sentiment: ", round(mean(d1$scores), 2), " vs ", round(mean(d2$scores), 2))
    
    return(cmp)
  } else if (inherits(d1, "SentimentDictionaryBinary") && inherits(d2, "SentimentDictionaryWeighted")) {
    cat0("Comparing: wordlist vs weighted\n")
    cmp <- compareOverlap(c(d1$positiveWords, d1$negativeWords), d2$words)
    cmp <- compareClasses(cmp,
                          d1$positiveWords, d1$negativeWords,
                          d2$words[d2$scores < 0], d2$words[d2$scores >= 0])
    is <- intersect(c(d1$positiveWords, d1$negativeWords), d2$words)
    cmp$correlation <- cor(unlist(lapply(is, function(x) ifelse(x %in% d1$positiveWords, +1, -1))), 
                           d2$scores[unlist(lapply(is, function(x) which(d2$words == x)))])
    cat0("Correlation between scores of matching entries: ", round(cmp$correlation, 2))

    return(cmp)
  } else if (inherits(d1, "SentimentDictionaryWeighted") && inherits(d2, "SentimentDictionaryBinary")) {
    compareDictionaries(d2, d1)
  } else {
    stop("Not yet implemented!")
  }
}
