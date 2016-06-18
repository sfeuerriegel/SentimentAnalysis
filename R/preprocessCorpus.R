#' Default preprocessing of corpus to generate a document-term matrix
#' 
#' Preprocess existing corpus of type \code{\link[tm]{Corpus}} according to default operations. 
#' This helper function groups all standard preprocessing steps such that the usage of the 
#' package is more convenient. The result is a document-term matrix.
#' 
#' @param corpus \code{\link[tm]{Corpus}} object which should be processed
#' @param stemming perform stemming (default: TRUE)
#' @param language default language used for preprocessing (i.e. stop word removal and stemming)
#' @param verbose print preprocessing status information
#' @seealso \code{\link[tm]{DocumentTermMatrix}} \code{\link[tm]{TermDocumentMatrix}} 
#' @export
"preprocessCorpus" <- function(corpus, language="english", stemming=TRUE, verbose=FALSE)  {  
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
  
  if (verbose) {
    cat("Removing stop words\n")
  }   
  corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords(language))
  
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