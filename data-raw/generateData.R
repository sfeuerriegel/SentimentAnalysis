library(devtools)
library(readxl)

## LM dictionary

lm <- read_excel("data-raw/LoughranMcDonald_MasterDictionary_2014.xlsx")
lm$Word <- tolower(lm$Word)
dim(lm)

lm.negative <- subset(lm, Negative != 0, select=Word)[[1]]
lm.positive <- subset(lm, Positive != 0, select=Word)[[1]]
lm.uncertainty <- subset(lm, Uncertainty != 0, select=Word)[[1]]

DictionaryLM <- list(negative=lm.negative,
                     positive=lm.positive,
                     uncertainty=lm.uncertainty)
devtools::use_data(DictionaryLM)

## GI dictionary

gi <- read_excel("data-raw/inquirerbasic.xls")
gi$Entry <- tolower(gi$Entry)

gi.words <- sapply(strsplit(gi$Entry, "#"), function(x) x[1])
gi.negative <- unique(gi.words[which(gi$Negativ != "")])
gi.positive <- unique(gi.words[which(gi$Positiv != "")])

DictionaryGI <- list(negative=gi.negative,
                     positive=gi.positive)
devtools::use_data(DictionaryGI)

## HE dictionary

he.negative <- read.table("data-raw/he/negativity.txt", stringsAsFactors=FALSE)[[1]]
head(he.negativity)
he.positive <- read.table("data-raw/he/positivity.txt", stringsAsFactors=FALSE)[[1]]
head(he.positivity)

DictionaryHE <- list(negative=he.negative,
                     positive=he.positive)
devtools::use_data(DictionaryHE)
