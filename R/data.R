#' Dictionary with opinionated words from Loughran-McDonald Financial dictionary
#'
#' Dictionary with a list of positive, negative and uncertainty words according to the 
#' Loughran-McDonald finance-specific dictionary. This dictionary was first presented 
#' in the \emph{Journal of Finance} and has been widely used in the finance domain ever 
#' since. 
#'
#' @docType data
#' @usage data(DictionaryLM)
#' @format A list with different terms according to Loughran-McDonald
#' @note All words are in lower case and non-stemmed
#' @keywords datasets
#' @references Loughran and McDonald (2011) \emph{When is a Liability not a Liability? 
#' Textual Analysis, Dictionaries, and 10-Ks}, Journal of Finance, 66:1, 35-65
#' @source \url{http://www3.nd.edu/~mcdonald/Word_Lists.html}
#' @examples
#' data(DictionaryLM)
#' summary(DictionaryLM)
"DictionaryLM"

#' Dictionary with opinionated words from the Harvard-IV dictionary as used in 
#' the General Inquirer software
#'
#' Dictionary with a list of positive and negative words according to the psychological 
#' Harvard-IV dictionary as used in the General Inquirer software. This is a 
#' general-purpose dictionary developed by the Harvard University.
#'
#' @docType data
#' @usage data(DictionaryGI)
#' @format A list with different terms according to Henry
#' @note All words are in lower case and non-stemmed
#' @keywords datasets
#' @source \url{http://www.wjh.harvard.edu/~inquirer/}
#' @examples
#' data(DictionaryGI)
#' summary(DictionaryGI)
"DictionaryGI"

#' Dictionary with opinionated words from Henry's Financial dictionary
#'
#' Dictionary with a list of positive and negative words according to the Henry's 
#' finance-specific dictionary. This dictionary was first presented in the \emph{Journal 
#' of Business Communication} among one of the early adopters of text analysis in the 
#' finance discipline. 
#'
#' @docType data
#' @usage data(DictionaryHE)
#' @format A list with different wordlists according to Henry
#' @note All words are in lower case and non-stemmed
#' @keywords datasets
#' @references Henry (2008): \emph{Are Investors Influenced By How Earnings Press 
#' Releases Are Written?}, Journal of Business Communication, 45:4, 363-407
#' @examples
#' data(DictionaryHE)
#' summary(DictionaryHE)
"DictionaryHE"

#' Retrieves IMDb dataset
#' 
#' Function downloads IMDb dataset and prepares corresponding user ratings for easy
#' usage.
#' @return Returns a list where entry named \code{Corpus} contains the IMDb reviews,
#' and \code{Rating} is the corresponding scaled rating.
#' @references Pang and Lee (2015) \emph{Seeing Stars: Exploiting Class Relationships 
#' for Sentiment Categorization with Respect to Rating Scales}, Proceeding of the 
#' ACL. See \url{http://www.cs.cornell.edu/people/pabo/movie-review-data/}
#' @keywords datasets
#' @examples
#' \dontrun{
#' imdb <- loadImdb()
#' dictionary <- generateDictionary(imdb$Corpus, imdb$Rating)
#' }
#' @importFrom utils download.file untar
#' @export
loadImdb <- function() {
  cat("Starting download ...")  

  # Download Paths
  imdbScaleDataUrl <- "https://www.cs.cornell.edu/people/pabo/movie-review-data/scale_data.tar.gz"
  imdbReviewUrl <- "https://www.cs.cornell.edu/people/pabo/movie-review-data/scale_whole_review.tar.gz"

  # Download IMDb rating data
  temp <- tempfile()
  tempDir <- tempdir()
  download.file(imdbScaleDataUrl, temp)
  list.files <- untar(temp, list=TRUE)
  untar(temp, exdir=tempDir)
  
  # Load data and extract response
  id <- sapply(list.files[grepl("Schwartz(.*)id",list.files)], 
               function(x) readLines(paste(tempDir, x, sep="/")))
  
  response <- as.numeric(sapply(list.files[grepl("Schwartz(.*)rating",list.files)], 
                                function(x) readLines(paste(tempDir, x, sep="/"))))
  # Normalize response 
  response <- (response - 0.5) * 2
  
  # Download IMDb review documents
  temp <- tempfile()
  tempDir <- tempdir()
  download.file(imdbReviewUrl, temp)
  untar(temp, exdir = tempDir)
  
  # Load data and create corpus
  dirSource <- tm::DirSource(paste(tempDir, "scale_whole_review/Dennis+Schwartz/txt.parag", sep = "/"))
  dirSource$filelist <- sapply(id, function(x) dirSource$filelist[grepl(x, dirSource$filelist)])
  
  corpus <- tm::VCorpus(dirSource, readerControl=list(language="en", reader=tm::readPlain))
  
  return(list(Corpus=corpus, Rating=response))
}