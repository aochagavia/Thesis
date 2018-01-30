# This file generates a word cloud based on Haskell's output

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# A function to plot a single frequency file (generated from the code submissions)

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
