## Case Study: IMDb Movie Reviews
## Authors:  
## - "Stefan Feuerriegel"
## - "Nicolas Proellochs"

library(SentimentAnalysis)

if (file.exists("imdb.Rdata")) {
  load("imdb.Rdata")
} else {
  # Load IMDb dataset from the Internet (this might take a few seconds)
  imdb <- loadImdb()
  
  # Save locally for further usages
  save(imdb, file="imdb.Rdata")  
}

# Analyze sentiment
sentiment <- analyzeSentiment(imdb$Corpus)

# Statistical and visual evaluation
compareToResponse(sentiment, imdb$Rating)
plotSentimentResponse(sentiment$SentimentGI, imdb$Rating)

# Generate Dictionary
dict_imdb <- generateDictionary(imdb$Corpus, imdb$Rating)
summary(dict_imdb)

# Compare entries to built-in, static dictionary
compareDictionaries(dict_imdb,
                    loadDictionaryGI())

# Show estimated coefficients with Kernel Density Estimation (KDE)
plot(dict_imdb)
plot(dict_imdb) + xlim(c(-0.1, 0.1)) # with nicer axis

# Compute in-sample performance
pred_sentiment <- predict(dict_imdb, imdb$Corpus)
plotSentimentResponse(pred_sentiment, imdb$Rating)

perf_dictionary <- compareToResponse(pred_sentiment, imdb$Rating)
perf_sentimentGI <- compareToResponse(sentiment$SentimentGI, imdb$Rating)

# Comparison
perf_dictionary
perf_sentimentGI

