#' Default preprocessing of corpus
#' 
#' Preprocess existing corpus of type \code{\link[tm]{Corpus}} according to default operations. 
#' This helper function groups all standard preprocessing steps such that the usage of the 
#' package is more convenient. 
#' 
#' @param corpus \code{\link[tm]{Corpus}} object which should be processed
#' @param stemming Perform stemming (default: TRUE)
#' @param language Default language used for preprocessing (i.e. stop word removal and stemming)
#' @param verbose Print preprocessing status information
#' @param removeStopwords Flag indicating whether to remove stopwords or not (default: yes)
#' @return Object of \code{\link[tm]{Corpus}}
#' @keywords corpus preprocessing
#' @export
"preprocessCorpus" <- function(corpus, language="english", stemming=TRUE, verbose=FALSE,
                               removeStopwords=TRUE)  {  
  if (!inherits(corpus, "Corpus")) {
    stop("Input has to be of type tm::Corpus")
  }
  
  if (!is.character(language) || length(language) != 1) {
    stop("Parameter language must be a string")
  }
  
  if (!is.logical(stemming) || length(stemming) != 1) {
    stop("Parameter stemming can only be logical value")
  }   
  
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("Parameter verbose must be a logical value")
  } 
  
  if (verbose) {
    cat("Starting preprocessing\n")
  }
  
  if (verbose) {
    cat("Removing HTML/XML tags\n")
  }
  corpus <- tm::tm_map(corpus, tm::PlainTextDocument)
  corpus <- tm::tm_map(corpus, tm::removeWords, c("nbsp"))
  
  if (verbose) {
    cat("Removing white spaces\n")
  }  
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)
  
  if (verbose) {
    cat("Converting to lower case\n")
  }  
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
  
  if (verbose) {
    cat("Removing punctuation\n")
  }    
  corpus <- tm::tm_map(corpus, tm::removePunctuation)

  if (verbose) {
    cat("Removing numbers\n")
  }    
  corpus <- tm::tm_map(corpus, tm::removeNumbers)
  
  if (removeStopwords) {
    if (verbose) {
      cat("Removing stop words\n")
    }   
    corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords(language))
  }
  
  if (stemming) {
    if (verbose) {
      cat("Perform stemming\n")
    }    
    corpus <- tm::tm_map(corpus, tm::stemDocument, language=language)
  }
      
  if(verbose) {
    cat("Done\n")
  }
  
  return(corpus)
}

#' Default preprocessing of corpus and conversion to document-term matrix
#' 
#' Preprocess existing corpus of type \code{\link[tm]{Corpus}} according to default operations. 
#' This helper function groups all standard preprocessing steps such that the usage of the 
#' package is more convenient. The result is a document-term matrix.
#' 
#' @param x \code{\link[tm]{Corpus}} object which should be processed
#' @param language Default language used for preprocessing (i.e. stop word removal and stemming)
#' @param minWordLength Minimum length of words used for cut-off; i.e. shorter words are 
#' removed. Default is 3.
#' @param removeStopwords Flag indicating whether to remove stopwords or not (default: yes)
#' @param weighting Function used for weighting of words; default is a a link to the tf-idf scheme.
#' @param sparsity A numeric for the maximal allowed sparsity in the range from bigger zero to 
#' smaller one. Default is \code{NULL} in order suppress this functionality.
#' @keywords corpus preprocessing
#' @return Object of \code{\link[tm]{DocumentTermMatrix}}
#' @seealso \code{\link[tm]{DocumentTermMatrix}} for the underlying class
#' @export
toDocumentTermMatrix <- function(x, language="english",
                                 minWordLength=3, sparsity=NULL, 
                                 removeStopwords=TRUE,
                                 weighting=function(x) tm::weightTfIdf(x, normalize=FALSE)) {
    corpus <- preprocessCorpus(x, language=language, removeStopwords=removeStopwords)
    
    # wordLengths is a workaround for minWordLength; see:
    # https://stat.ethz.ch/pipermail/r-help/2012-May/313013.html
    dtm <- tm::DocumentTermMatrix(corpus,
                                  control=list(wordLengths=c(minWordLength, 255),
                                               weighting=weighting))
    if (!is.null(sparsity)) {
      dtm <- tm::removeSparseTerms(dtm, sparsity)
    }
    
    return(dtm)
}