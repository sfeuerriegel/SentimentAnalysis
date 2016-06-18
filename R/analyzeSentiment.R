#' Sentiment analysis
#' 
#' Performs sentiment analysis  of given object (vector of strings, document-term 
#' matrix, corpus). 
#' 
#' @param x A vector of characters, a \code{data.frame}, an object of type 
#' \code{\link[tm]{Corpus}}, \code{\link[tm]{TermDocumentMatrix}} or
#' \code{\link[tm]{DocumentTermMatrix}} 
#' @param language Language used for preprocessing operations (default: 
#' English)
#' @param aggregate A factor variable by which documents can be grouped. 
#' This helpful when joining e.g. news from the same day or move reviews
#' by the same author
#' @param ... Additional parameters passed to function for e.g. 
#' preprocessing 
#' @return Result is a matrix which sentiment values for each document across
#' all defined rules
#' @examples 
#' # via vector of strings
#' corpus <- c("Positive text", "Neutral but uncertain text", "Negative text")
#' sentiment <- analyzeSentiment(corpus)
#' compareToResponse(sentiment, c(+1, 0, -1))
#' 
#' # via Corpus from tm package
#' library(tm)
#' reut21578 <- system.file("texts", "crude", package="tm")
#' reuters <- Corpus(DirSource(reut21578),
#'                   readerControl=list(reader=readReut21578XML))
#'     
#' # via DocumentTermMatrix (with stemmed entries)
#' dtm <- DocumentTermMatrix(Corpus(VectorSource(c("posit posit", "negat neutral")))) 
#' sentiment <- analyzeSentiment(dtm)
#' compareToResponse(sentiment, c(TRUE, FALSE))
#' @details This function returns a data.frame with continuous values. If one desires 
#' other formats, one needs to convert these. Common examples of such formats are
#' binary response values (positive / negative) or tertiary (positive, neutral, 
#' negative). Hence, consider using the functions \code{\link{convertToBinaryResponse}} and
#' \code{\link{convertToDirection}}, which can convert a vector of continuous sentiment
#' scores into a factor object.
#' @seealso \code{\link{compareToResponse}}, \code{\link{convertToBinaryResponse}}, 
#' \code{\link{convertToDirection}} \code{\link{generateDictionary}} 
#' \code{\link{plotSentiment}} \code{\link{plotSentimentResponse}}  
#' @rdname analyzeSentiment
#' @export
"analyzeSentiment" <- function(x, language="english", aggregate=NULL, ...) {
  UseMethod("analyzeSentiment", x)
}
  
#' @rdname analyzeSentiment
#' @export
"analyzeSentiment.Corpus" <- function(x, language="english", aggregate=NULL, ...) {  
  corpus <- preprocessCorpus(x, language)
  dtm <- tm::DocumentTermMatrix(corpus)
  return(analyzeSentiment(dtm, language, aggregate, ...))
}

#' @rdname analyzeSentiment
#' @export
"analyzeSentiment.character" <- function(x, language="english", aggregate=NULL, ...) {
  corpus <- transformIntoCorpus(x)
  return(analyzeSentiment(corpus, language, aggregate, ...))
}

#' @rdname analyzeSentiment
#' @export
"analyzeSentiment.data.frame" <- function(x, language="english", aggregate=NULL, ...) {
  corpus <- transformIntoCorpus(x)
  return(analyzeSentiment(corpus, language, aggregate, ...))  
}

#' @rdname analyzeSentiment
#' @export
"analyzeSentiment.TermDocumentMatrix" <- function(x, language="english", aggregate=NULL, ...) {
  analyzeSentiment(t(x), language, aggregate, ...)
}

#' @rdname analyzeSentiment
#' @export
"analyzeSentiment.DocumentTermMatrix" <- function(x, language="english", aggregate=NULL, ...) {
  sent <- list()
  rules <- allSentimentRules()
  
  for (n in names(rules)) {
    rule <- rules[[n]]
    if (length(rule) == 1) {
      sent[[n]] <- rule[[1]](x)
    } else {
      sent[[n]] <- rule[[1]](x, rule[[2:length(rule)]])
    }
  }
  
  return(as.data.frame(t(do.call(rbind, sent)), 
                       stringsAsFactors=FALSE,
                       row.names=NULL))
}

#' Convert continuous sentiment to direction
#' 
#' This function converts continuous sentiment scores into a their corresponding
#' binary sentiment class. As such, the result is a factor with two levels 
#' indicating positive and negative content. Neutral documents (with a sentiment
#' score of 0) are counted as positive.  
#' 
#' @param sentiment Vector, matrix or data.frame with sentiment scores.
#' @return If a vector is supplied, it returns a factor with two levels representing 
#' positive and negative content. Otherwise, it returns a data.frame with the 
#' corresponding columns being exchanged.
#' @details If a matrix or data.frame is provided, this routine does not touch
#' all columns. In fact, it scans for those where the colum name starts with
#' "Sentiment" and changes these columns only. Hence, colums with pure 
#' negativity, positivity or ratios or word counts are ignored.
#' @examples 
#' sentiment <- c(-1, -0.5, +1, 0.6, 0)
#' convertToBinaryResponse(sentiment)
#' convertToDirection(sentiment)
#' 
#' df <- data.frame(No=1:5, Sentiment=sentiment)
#' df
#' convertToBinaryResponse(df)
#' convertToDirection(df)
#' @seealso \code{\link{convertToDirection}}
#' @export
convertToBinaryResponse <- function(sentiment) {
  if (is.null(sentiment)) {
    stop("Input argument is NULL.")
  }
  
  if (is.matrix(sentiment) || is.data.frame(sentiment)) {
    if (is.matrix(sentiment)) {
      sentiment <- data.frame(sentiment,
                              stringsAsFactors=FALSE)
    }
    
    for (i in grep("^Sentiment", colnames(sentiment))) {
      sentiment[, i] <- convertToBinaryResponse(sentiment[, i])
    }
    
    return(sentiment)
  }

  direction <- sign(sentiment)
  direction[which(direction==0)] <- 1
  
  return(factor(direction, 
                levels=c(-1, +1), 
                labels=c("negative", "positive")))
}

#' Convert continuous sentiment to direction
#' 
#' This function converts continuous sentiment scores into a their corresponding
#' sentiment direction. As such, the result is a factor with three levels 
#' indicating positive, neutral and negative content. In contrast
#' to \code{\link{convertToBinaryResponse}}, neutral documents have their own category.
#' 
#' @param sentiment Vector, matrix or data.frame with sentiment scores.
#' @return If a vector is supplied, it returns a factor with three levels representing 
#' positive, neutral and negative content. Otherwise, it returns a data.frame with the 
#' corresponding columns being exchanged.
#' @details If a matrix or data.frame is provided, this routine does not touch
#' all columns. In fact, it scans for those where the colum name starts with
#' "Sentiment" and changes these columns only. Hence, colums with pure 
#' negativity, positivity or ratios or word counts are ignored. 
#' @examples 
#' sentiment <- c(-1, -0.5, +1, 0.6, 0)
#' convertToBinaryResponse(sentiment)
#' convertToDirection(sentiment)
#' 
#' df <- data.frame(No=1:5, Sentiment=sentiment)
#' df
#' convertToBinaryResponse(df)
#' convertToDirection(df)
#' @seealso \code{\link{convertToBinaryResponse}}
#' @export
convertToDirection <- function(sentiment) {
  if (is.null(sentiment)) {
    stop("Input argument is NULL.")
  }
  
  if (is.matrix(sentiment) || is.data.frame(sentiment)) {
    if (is.matrix(sentiment)) {
      sentiment <- data.frame(sentiment,
                              stringsAsFactors=FALSE)
    }

    for (i in grep("^Sentiment", colnames(sentiment))) {
      sentiment[, i] <- convertToDirection(sentiment[, i])
    }
    
    return(sentiment)
  }
  
  return(factor(sign(sentiment), 
                levels=c(-1, 0, +1), 
                labels=c("negative", "neutral", "positive")))  
}


#' Compare sentiment values to existing response variable
#' 
#' This function compares the calculated sentiment values with an external
#' response variable. Examples of such an exogenous response are stock market
#' movements or IMDb move rating. Both usually reflect a "true" value that 
#' the sentiment should match. 
#' 
#' @param sentiment Matrix with sentiment scores for each document across several 
#' sentiment rules
#' @param response Vector with "true" response. This vector can either be of a 
#' continuous numeric or binary values. In case of the latter, FALSE is matched 
#' to a negative sentiment value, while TRUE is matched to a non-negative one.
#' @return Matrix with different performance metrics for all given sentiment 
#' rules
#' @examples
#' sentiment <- matrix(c(5.5, 2.9, 0.9, -1), 
#'                     dimnames=list(c("A", "B", "C", "D"), c("Sentiment")))
#'
#' # continuous numeric response variable
#' response <- c(5, 3, 1, -1)
#' compareToResponse(sentiment, response)
#' 
#' # binary response variable
#' response <- c(TRUE, TRUE, FALSE, FALSE)
#' compareToResponse(sentiment, response)
#' @rdname compareToResponse
#' @export 
"compareToResponse" <- function(sentiment, response) {
  UseMethod("compareToResponse", response)
} 

#' @rdname compareToResponse
#' @export 
"compareToResponse.logical" <- function(sentiment, response) {
  compareToResponse.factor(sentiment, factor(response, levels=c("FALSE", "TRUE")))
}

#' @rdname compareToResponse
#' @export 
"compareToResponse.factor" <- function(sentiment, response) {
  if (!all(levels(response) == c("FALSE", "TRUE"))) {
    stop("Factor levels do not match expected format")
  }
  
  r <- list()    
  for (n in colnames(sentiment)) {
    r[[n]] <- c(evalBinaryClassifier(factor(sentiment[, n] >= 0, levels=c("FALSE", "TRUE")), response),
                "avg.sentiment.pos.response"=mean(sentiment[response == "TRUE",n]),
                "avg.sentiment.neg.response"=mean(sentiment[response == "FALSE",n]))
  }
  
  return(t(do.call(rbind, r)))
}

#' @rdname compareToResponse
#' @export 
"compareToResponse.integer" <- function(sentiment, response) {
  compareToResponse.numeric(sentiment, response)
}

#' @rdname compareToResponse
#' @export 
"compareToResponse.data.frame" <- function(sentiment, response) {
  compareToResponse.numeric(sentiment, response)
}

#' @rdname compareToResponse
#' @importFrom stats lm cor.test
#' @export 
"compareToResponse.numeric" <- function(sentiment, response) {
  result <- list("cor"=cor(sentiment, response)[,1],
                 "cor.t.statistic"=unlist(lapply(colnames(sentiment), function(x) cor.test(sentiment[, x], response)$statistic)),
                 "cor.p.value"=unlist(lapply(colnames(sentiment), function(x) cor.test(sentiment[, x], response)$statistic)),

                 "lm.t.value"=unlist(lapply(colnames(sentiment), function(x) summary(lm(response ~ sentiment[, x]))$coefficients[2,3])),
                 "r.squared"=(cor(sentiment, response)^2)[,1],
                 
                 "RMSE"=unlist(lapply(colnames(sentiment), function(x) sqrt(mean((sentiment[, x]-response)^2)))),
                 "MAE"=unlist(lapply(colnames(sentiment), function(x) mean(abs(sentiment[, x]-response)))))
    
  return(rbind(do.call(rbind, result),
               compareToResponse(sentiment, response >= 0)))
}

"evalBinaryClassifier" <- function(pred, true) {
  cm <- table(pred, true)
  
  r <- c("Accuracy"=(cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]),
         "Precision"=cm[1,1]/(cm[1,1]+cm[1,2]),
         "Sensitivity"=cm[1,1]/(cm[1,1]+cm[2,1]),
         "Specificity"=cm[2,2]/(cm[1,2]+cm[2,2]),
         "F1"=2*cm[1,1]/(2*cm[1,1]+cm[1,2]+cm[2,2]))
  r["BalancedAccuracy"] <- (r["Sensitivity"]+r["Specificity"])/2
  
  return(r)
}