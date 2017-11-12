#' Generates dictionary of decisive terms
#' 
#' Routine applies method for dictionary generation (LASSO, ridge regularization, elastic net, 
#' ordinary least squares, generalized linear model or spike-and-slab regression) to 
#' the document-term matrix in order to extract decisive terms that 
#' have a statistically significant impact on the response variable.
#' @param x A vector of characters, a \code{data.frame}, an object of type 
#' \code{\link[tm]{Corpus}}, \code{\link[tm]{TermDocumentMatrix}} or
#' \code{\link[tm]{DocumentTermMatrix}}.
#' @param response Response variable including the given gold standard. 
#' @param language Language used for preprocessing operations (default: 
#' English).
#' @param modelType A string denoting the estimation method. Allowed values are \code{lasso}, \code{ridge}, 
#' \code{enet}, \code{lm} or \code{glm} or \code{spikeslab}.
#' @param filterTerms Optional vector of strings (default: \code{NULL}) to filter terms that are used
#' for dictionary generation. 
#' @param control (optional) A list of parameters defining the model used for dictionary generation.
#' 
#' If \code{modelType=lasso} is selected, individual parameters are as follows:
#' \itemize{
#'  \item{"s"} {Value of the parameter lambda at which the LASSO is evaluated. Default
#' is \code{s="lambda.1se"} which takes the calculated minimum value for \eqn{\lambda} 
#' and then subtracts one standard error in order to avoid overfitting. This often
#' results in a better performance than using the minimum value itself given by 
#' \code{lambda="lambda.min"}.}
#'  \item{"family"} {Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link[glmnet]{glmnet}} for further details.}
#'  \item{"grouped"} {Determines whether grouped LASSO is used (with default \code{FALSE}).}
#' }
#' 
#' If \code{modelType=ridge} is selected, individual parameters are as follows:
#' \itemize{
#'  \item{"s"} {Value of the parameter lambda at which the ridge is evaluated. Default
#' is \code{s="lambda.1se"} which takes the calculated minimum value for \eqn{\lambda} 
#' and then subtracts one standard error in order to avoid overfitting. This often
#' results in a better performance than using the minimum value itself given by 
#' \code{lambda="lambda.min"}.}
#'  \item{"family"} {Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link[glmnet]{glmnet}} for further details.}
#'  \item{"grouped"} {Determines whether grouped function is used (with default \code{FALSE}).}
#' } 
#' 
#' If \code{modelType=enet} is selected, individual parameters are as follows:
#' \itemize{
#'  \item{"alpha"} {Abstraction parameter for switching between LASSO (with \code{alpha=1}) and
#'  ridge regression (\code{alpha=0}). Default is \code{alpha=0.5}. Recommended option is to 
#'  test different values between 0 and 1.}
#'  \item{"s"} {Value of the parameter lambda at which the elastic net is evaluated. Default
#' is \code{s="lambda.1se"} which takes the calculated minimum value for \eqn{\lambda} 
#' and then subtracts one standard error in order to avoid overfitting. This often
#' results in a better performance than using the minimum value itself given by 
#' \code{lambda="lambda.min"}.}
#'  \item{"family"} {Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link[glmnet]{glmnet}} for further details.}
#'  \item{"grouped"} {Determines whether grouped function is used (with default \code{FALSE}).}
#' }
#'  
#' If \code{modelType=lm} is selected, no parameters are passed on. 
#' 
#' If \code{modelType=glm} is selected, individual parameters are as follows:
#' \itemize{
#'  \item{"family"} {Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link{glm}} for further details.}
#' } 
#' 
#' If \code{modelType=spikeslab} is selected, individual parameters are as follows:
#' \itemize{
#'  \item{"n.iter1"} {Number of burn-in Gibbs sampled values (i.e., discarded values). Default is 500.}
#'  \item{"n.iter2"} {Number of Gibbs sampled values, following burn-in. Default is 500.}
#' }
#' @param minWordLength Removes words given a specific minimum length (default: 3). This 
#' preprocessing is applied when the input is a character vector or a corpus and the
#' document-term matrix is generated inside the routine. 
#' @param sparsity A numeric for removing sparse terms in the document-term matrix. The
#' argument \code{sparsity} specifies the maximal allowed sparsity. Default is 
#' \code{sparsity=0.9}, however, this is only applied when the document-term matrix
#' is calculated inside the routine.
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
#' # Generate new dictionary with spike-and-slab regression instead of LASSO regularization
#' library(spikeslab)
#' dictionary <- generateDictionary(documents, response, modelType="spikeslab")
#' 
#' # Generate new dictionary with tf weighting instead of tf-idf
#' 
#' library(tm)
#' dictionary <- generateDictionary(documents, response, weighting=weightTf)
#' sentiment <- predict(dictionary, documents)
#' compareToResponse(sentiment, response)
#'
#' # Use instead lambda.min from the LASSO estimation
#' dictionary <- generateDictionary(documents, response, control=list(s="lambda.min"))
#' sentiment <- predict(dictionary, documents)
#' compareToResponse(sentiment, response)
#' 
#' # Use instead OLS as estimation method
#' dictionary <- generateDictionary(documents, response, modelType="lm")
#' sentiment <- predict(dictionary, documents)
#' sentiment
#' 
#' dictionary <- generateDictionary(documents, response, modelType="lm", 
#'                                  filterTerms = c("good", "bad"))
#' sentiment <- predict(dictionary, documents)
#' sentiment
#' 
#' dictionary <- generateDictionary(documents, response, modelType="lm", 
#'                                  filterTerms = extractWords(loadDictionaryGI()))
#' sentiment <- predict(dictionary, documents)
#' sentiment
#' 
#' # Generate dictionary without LASSO intercept
#' dictionary <- generateDictionary(documents, response, intercept=FALSE)
#' dictionary$intercept
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
#' @references Pr{\"o}llochs and Feuerriegel (2015). Generating Domain-Specific 
#' Dictionaries Using Bayesian Learning. 23rd European Conference on Information 
#' Systems (ECIS 2015).
#' @source \url{https://dx.doi.org/10.2139/ssrn.2522884}
#' @keywords sentiment evaluation dictionary
#' @seealso \code{\link{analyzeSentiment}}, \code{\link{predict.SentimentDictionaryWeighted}}, 
#' \code{\link{plot.SentimentDictionaryWeighted}} and \code{\link{compareToResponse}} for
#' advanced evaluations
#' @rdname generateDictionary
#' @export
generateDictionary <- function(x, response, language="english", modelType="lasso",
                               filterTerms = NULL,
                               control = list(),
                               minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  UseMethod("generateDictionary", x)
}

#' @rdname generateDictionary
#' @export
generateDictionary.Corpus <- function(x, response, language="english", modelType="lasso",
                                      filterTerms = NULL,
                                      control = list(),
                                      minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  dtm <- toDocumentTermMatrix(x, language=language, minWordLength=minWordLength, sparsity=sparsity, weighting=weighting)

  return(generateDictionary(dtm, response, language, modelType, filterTerms,
                            control,
                            minWordLength, sparsity, weighting, ...)) 
}

#' @rdname generateDictionary
#' @export
generateDictionary.character <- function(x, response, language="english", modelType="lasso",
                                         filterTerms = NULL,
                                         control = list(),
                                         minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  corpus <- transformIntoCorpus(x)
  return(generateDictionary(corpus, response, language, modelType, filterTerms,
                            control,
                            minWordLength, sparsity, weighting, ...))
}

#' @rdname generateDictionary
#' @export
generateDictionary.data.frame <- function(x, response, language="english", modelType="lasso",
                                          filterTerms = NULL,
                                          control = list(),
                                          minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  corpus <- transformIntoCorpus(x, ...)
  return(generateDictionary(corpus, response, language, modelType, filterTerms,
                            control,
                            minWordLength, sparsity, weighting, ...))
}

#' @rdname generateDictionary
#' @export
generateDictionary.TermDocumentMatrix <- function(x, response, language="english", modelType="lasso",
                                                  filterTerms = NULL,
                                                  control = list(),
                                                  minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {
  return(generateDictionary(t(x), response, language, modelType, filterTerms,
                            control,
                            minWordLength, sparsity, weighting, ...))
}


#' @rdname generateDictionary
#' @export
generateDictionary.DocumentTermMatrix <- function(x, response, language="english", modelType="lasso",
                                                  filterTerms = NULL,
                                                  control = list(),
                                                  minWordLength=3, sparsity=0.9, weighting=function(x) tm::weightTfIdf(x, normalize=FALSE), ...) {

  estimationMethod <- lookupEstimationMethod(modelType)
  
  if (!is.null(filterTerms)) {
    x <- x[, colnames(x) %in% filterTerms]
  }
  
  model <- estimationMethod(x, response, control, ...)
  
  if (identical(weighting, tm::weightTfIdf)) {
    wordFrequency <- colSums(as.matrix(x[, model$ScoreNames]) != 0)
    idf <- log(nrow(x)/wordFrequency)
  } else {
    idf <- rep(1, length(model$Scores))
  }
  
  dict <- SentimentDictionaryWeighted(model$ScoreNames,
                                      model$Scores,
                                      idf,
                                      model$Intercept)
  
  return(dict)
}
