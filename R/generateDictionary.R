#' Generates dictionary of decisive terms
#' 
#' Routine applies LASSO regularization to the document-term matrix in order to 
#' extract decisive terms that have a statistically significant impact on the 
#' response variable.
#' @param x A vector of characters, a \code{data.frame}, an object of type 
#' \code{\link[tm]{Corpus}}, \code{\link[tm]{TermDocumentMatrix}} or
#' \code{\link[tm]{DocumentTermMatrix}}.
#' @param response Response variable including the given gold standard. 
#' @param language Language used for preprocessing operations (default: 
#' English).
#' @param alpha Abstraction parameter for switching form LASSO regularization
#' (with default \code{alpha=1}) to ridge regression (\code{alpha=0}). As alternative
#' options, one can also utilize to an elastic net with any continuous value 
#' inbetween.
#' @param s Value of the parameter lambda at which the LASSO is evaluated. Default
#' is \code{s="lambda.1se"} which takes the calculated minimum value for \eqn{\lambda} 
#' and then subtracts one standard error in order to avoid overfitting. This often
#' results in a better performance than using the minimum value itself given by 
#' \code{lambda="lambda.min"}.
#' @param family Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link[glmnet]{glmnet}} for further details. 
#' @param minWordLength Removes words given a specific minimum length (default: 3). This 
#' preprocessing is applied when the input is a character vector or a corpus and the
#' document-term matrix is generated inside the routine. 
#' @param sparsity A numeric for removing sparse terms in the document-term matrix. The
#' argument \code{sparsity} specifies the maximal allowed sparsity. Default is 
#' \code{sparsity=0.9}, however, this is only applied when the document-term matrix
#' is calculated inside the rotuine.
#' @param weighting Weights a document-term matrix by e.g. term frequency - inverse
#' document frequency (default). Other variants can be used from 
#' \code{\link[tm]{DocumentTermMatrix}}.
#' @param ... Additional parameters passed to function for e.g. 
#' preprocessing or \code{\link[glmnet]{glmnet}}.
#' @return Result is a matrix which sentiment values for each document across
#' all defined rules
#' @examples
#' # Create a vector of strings
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
#' # Show dictionary
#' dictionary
#' summary(dictionary)
#' plot(dictionary)
#' 
#' # Compute in-sample performance
#' sentiment <- predict(dictionary, documents)
#' compareToResponse(sentiment, response)
#' plotSentimentResponse(sentiment, response)
#' 
#' # Generate new dictionary with tf weighting innstead of tf-idf
#' 
#' library(tm)
#' dictionary <- generateDictionary(documents, response, weighting=weightTf)
#' sentiment <- predict(dictionary, documents)
#' compareToResponse(sentiment, response)
#'
#' # Use instead lambda.min from the LASSO estimation
#' dictionary <- generateDictionary(documents, response, s="lambda.min")
#' sentiment <- predict(dictionary, documents)
#' compareToResponse(sentiment, response)
#'  
#' \dontrun{
#' imdb <- loadImdb()
#' 
#' # Generate Dictionary
#' dictionary_imdb <- generateDictionary(imdb$Corpus, imdb$Rating, family="poisson")
#' summary(dictionary_imdb)
#' 
#' compareDictionaries(dictionary_imdb,
#'                     loadDictionaryGI())
#'                     
#' # Show estimated coefficients with Kernel Density Estimation (KDE)
#' plot(dictionary_imdb)
#' plot(dictionary_imdb) + xlim(c(-0.1, 0.1))
#' 
#' # Compute in-sample performance
#' pred_sentiment <- predict(dict_imdb, imdb$Corpus)
#' compareToResponse(pred_sentiment, imdb$Rating)
#' 
#' # Test a different sparsity parameter
#' dictionary_imdb <- generateDictionary(imdb$Corpus, imdb$Rating, family="poisson", sparsity=0.99)
#' summary(dictionary_imdb)
#' pred_sentiment <- predict(dict_imdb, imdb$Corpus)
#' compareToResponse(pred_sentiment, imdb$Rating)
#' }
#' @references Proellochs, Feuerriegel and Neumann (2015) \emph{Generating 
#' Domain-Specific Dictionaries Using Bayesian Learning}, Proceedings of the 
#' 23rd European Conference on Information Systems (ECIS 2015), Muenster, 
#' Germany. URL: \url{http://dx.doi.org/10.2139/ssrn.2522884}
#' @seealso \code{\link{analyzeSentiment}} \code{\link{predict.SentimentDictionaryWeighted}}
#' \code{\link{plot.SentimentDictionaryWeighted}}  \code{\link{compareToResponse}} 
#' @rdname generateDictionary
#' @export
generateDictionary <- function(x, response, language="english", 
                               alpha=1, s="lambda.min", family="gaussian",
                               minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  UseMethod("generateDictionary", x)
}

#' @rdname generateDictionary
#' @export
generateDictionary.Corpus <- function(x, response, language="english", 
                                      alpha=1, s="lambda.min", family="gaussian",
                                      minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  corpus <- preprocessCorpus(x, language, ...)
  
  dtm <- tm::DocumentTermMatrix(corpus,
                                control=list(minWordLength=minWordLength,
                                             weighting=weighting))
  dtm <- tm::removeSparseTerms(dtm, sparsity)
  
  return(generateDictionary(dtm, response, language,
                            alpha, s, family,
                            minWordLength, sparsity, weighting, ...)) 
}

#' @rdname generateDictionary
#' @export
generateDictionary.character <- function(x, response, language="english", 
                                         alpha=1, s="lambda.min", family="gaussian",
                                         minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  corpus <- transformIntoCorpus(x, ...)
  return(generateDictionary(corpus, response, language,
                            alpha, s, family,
                            minWordLength, sparsity, weighting, ...))
}

#' @rdname generateDictionary
#' @export
generateDictionary.data.frame <- function(x, response, language="english", 
                                          alpha=1, s="lambda.min", family="gaussian",
                                          minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  corpus <- transformIntoCorpus(x, ...)
  return(generateDictionary(corpus, response, language,
                            alpha, s, family,
                            minWordLength, sparsity, weighting, ...))
}

#' @rdname generateDictionary
#' @export
generateDictionary.TermDocumentMatrix <- function(x, response, language="english", 
                                                  alpha=1, s="lambda.min", family="gaussian",
                                                  minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  return(generateDictionary(t(x), response, language,
                           alpha, s, family,
                           minWordLength, sparsity, weighting, ...))
}

#' @importFrom stats coef
#' @rdname generateDictionary
#' @export
generateDictionary.DocumentTermMatrix <- function(x, response, language="english", 
                                                  alpha=1, s="lambda.min", family="gaussian",
                                                  minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  cv.lasso <- glmnet::cv.glmnet(as.matrix(x), response, alpha=alpha, family=family, ...)
  coefs <- coef(cv.lasso, s=s)
    
  words <- coefs@Dimnames[[1]][setdiff(coefs@i+1, 1)]
  scores <- coefs@x
  if (length(coefs@i) > 0 && coefs@i[1]==0) {
    scores <- scores[-1]
  }
  intercept <- ifelse(length(coefs@i) > 0 && coefs@i[1]==0,
                      coefs@x[1],
                      0)
  
  if (identical(weighting, tm::weightTfIdf)) {
    wordFrequency <- colSums(as.matrix(x[, words]) != 0)
    idf <- log(nrow(x)/wordFrequency)
  } else {
    idf <- rep(1, length(scores))
  }
  
  dict <- SentimentDictionaryWeighted(words,
                                      scores,
                                      idf,
                                      intercept)
  
  return(dict)
}

