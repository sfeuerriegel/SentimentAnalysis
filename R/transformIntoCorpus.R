#' Transforms the input into a Corpus object
#' 
#' Takes the given input of characters and transforms it into a \code{\link[tm]{Corpus}}. The input is checked to match the expected class and format. 
#' @param x A list, data.frame or vector consisting of characters
#' @return The generated \code{Corpus}
#' @examples
#' transformIntoCorpus(c("Document 1", "Document 2", "Document 3"))
#' transformIntoCorpus(list("Document 1", "Document 2", "Document 3"))
#' transformIntoCorpus(data.frame("Document 1", "Document 2", "Document 3"))
#' @keywords corpus preprocessing
#' @note Factors are automatically casted into characters but with printing a warning
#' @seealso \code{\link{preprocessCorpus}} for further preprocessing, \code{\link{analyzeSentiment}} for subsequent sentiment analysis
#' @export
transformIntoCorpus <- function(x) {
  if (is.null(x)) {
    stop("Parameter cannot be NULL")
  }
  
  if (is.factor(x)) {
    warning("Converted factor to character")
    return(transformIntoCorpus(as.character(x)))
  }
  
  if (!inherits(x, "character") && !inherits(x, "data.frame") && !inherits(x, "list")) {
    stop("Can only handle character, data.frame or list")
  }
  
  if (inherits(x, "list")) {
    if (!all(unlist(lapply(x, function(x) { inherits(x, "character")})))) {
      stop("List can only contain character elements")
    }
    
    if (!all(unlist(lapply(x, function(x) { !is.null(x)})))) {
      stop("List can only contain NULL elements")
    }
    
    if (!all(unlist(lapply(x, function(x) { length(x) == 1})))) {
      stop("List can only contain length 1 elements")
    }
  }
  
  if (inherits(x, "data.frame")) {
    if (nrow(x) != 1 && ncol(x) != 1 && !all.equal(dim(x), c(0, 0))) {
      stop("Cannot handle data.frame that spans in two dimensions")
    }
    if (ncol(x) == 1) {
      return(transformIntoCorpus(x[,1]))
    }
  }
  
  return(tm::VCorpus(tm::VectorSource(x)))
}