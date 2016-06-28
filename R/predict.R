#' Prediction for given dictionary
#' 
#' Function takes a dictionary of class \code{\link{SentimentDictionaryWeighted}} with weights 
#' as input. It then applies this dictionary to textual contents in order to calculate
#' a sentiment score.
#' @param d Dictionary of class \code{\link{SentimentDictionaryWeighted}}.
#' @param newdata A vector of characters, a \code{data.frame}, an object of type 
#' \code{\link[tm]{Corpus}}, \code{\link[tm]{TermDocumentMatrix}} or
#' \code{\link[tm]{DocumentTermMatrix}} .
#' @param language Language used for preprocessing operations (default: 
#' English).
#' @param ... Additional parameters passed to function for e.g. 
#' preprocessing.
#' @return \code{data.frame} with predicted sentiment scores.
#' @examples 
#' #' # Create a vector of strings
#' documents <- c("This is a good thing!",
#'                "This is a very good thing!",
#'                "This is okay.",
#'                "This is a bad thing.",
#'                "This is a very bad thing.")
#' response <- c(1, 0.5, 0, -0.5, -1)
#' 
#' # Generate dictionary with LASSO regularization
#' dictionary <- generateDictionary(documents, response)
#' 
#' # Compute in-sample performance
#' sentiment <- predict(dictionary, documents)
#' compareToResponse(sentiment, response)
#' @keywords sentiment evaluation dictionary
#' @seealso \code{\link{SentimentDictionaryWeighted}}, \code{\link{generateDictionary}} and
#' \code{\link{compareToResponse}} for default dictionary generations
#' @export
predict.SentimentDictionaryWeighted <- function(d, newdata=NULL, 
                                                language="english", ...) {
  if (missing(newdata) || is.null(newdata)) {
    stop("Argument 'newdata' is empty.")
  }

  return(predictWeighted(newdata, d, language, ...))
} 

predictWeighted <- function(x, d, language, ...) {
  UseMethod("predictWeighted", x)
}

predictWeighted.Corpus <- function(x, d, language, ...) {
  corpus <- preprocessCorpus(x, language, ...)
  dtm <- tm::DocumentTermMatrix(corpus)
  return(predictWeighted(dtm, d, language, ...))
}

predictWeighted.character <- function(x, d, language, ...) {
  corpus <- transformIntoCorpus(x, ...)
  return(predictWeighted(corpus, d, language, ...))
}

predictWeighted.data.frame <- function(x, d, language, ...) {
  corpus <- transformIntoCorpus(x, ...)
  return(predictWeighted(corpus, d, language, ...))
}

predictWeighted.TermDocumentMatrix <- function(x, d, language, ...) {
  return(predictWeighted(t(x), d, language, ...))
}

predictWeighted.DocumentTermMatrix <- function(x, d, language, ...) {
  out <- rep(d$intercept)
  out <- unname(out + (as.matrix(x[, d$words]) * d$idf) %*% d$scores)
  out <- data.frame(Dictionary=out)
  
  return(out)
}
