# This file generates a word cloud based on Haskell's output

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

setwd("C:/Users/Adolfo/Documents/AdolfoDocs/Computer Science/UU/Master/Thesis")

# A function to plot a single frequency file (generated from the code submissions)

chartFile <- function(fname) {
  # Each line is a number
  textFreqs <- readLines(fname)
  intFreqs <- simplify2array(lapply(textFreqs, type.convert))
  
  # Filter into buckets
  f1 = intFreqs[intFreqs == 1]
  f2 = intFreqs[intFreqs == 2]
  f2_4 = intFreqs[(2 < intFreqs) & (intFreqs <= 4)]
  f4_8 = intFreqs[(4 < intFreqs) & (intFreqs <= 8)]
  f8_16 = intFreqs[(8 < intFreqs) & (intFreqs <= 16)]
  f16_32 = intFreqs[(16 < intFreqs) & (intFreqs <= 32)]
  f32_64 = intFreqs[(32 < intFreqs) & (intFreqs <= 64)]
  f64_inf = intFreqs[64 < intFreqs]
  buckets <- list(f1, f2, f2_4, f4_8, f8_16, f16_32, f32_64, f64_inf)
  
  # Get the frequencies for each bucket
  freqs <- simplify2array(lapply(buckets, length))
  
  # Plot
  png(paste("charts/", basename(fname), ".png", sep=""), width = 4000, height = 4000, res=400)
  
  names <- rev(c("(64, inf)", "(32, 64]", "(16, 32]", "(8, 16]", "(4, 8]", "(2, 4]", "2", "1"))
  barplot(freqs, main="Normalized group distribution", horiz=TRUE, names.arg=names, ylab="Group size", xlab="Frequency", density=16, width=300, las=1)
  
  dev.off()
}

chartAll <- function() {
  paths <- list.files("data/baseline", full.names = TRUE)
  lapply(paths, function(path) {
    if (endsWith(path, ".freqs")) {
      chartFile(path)
    }
  })
}

plotFile <- function(fname) {
  text <- readLines(fname)
  docs <- Corpus(VectorSource(text))
  dtm <- TermDocumentMatrix(docs, control=list())
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  set.seed(1234)
  png(paste("img/", basename(fname), ".png", sep=""), width = 4000, height = 4000, res=400)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  dev.off()
}

plotAll <- function() {
  paths <- list.files("data/preview", full.names = TRUE)
  lapply(paths, plotFile)
}
