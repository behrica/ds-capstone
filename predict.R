library(data.table)

predictNextFromWords <- function(dts,words) {
  by <- column.names[min(maxN,length(words)+1)]
  allMatches <- dts[as.list(words),j=sum(count),by=by]
  allMatches <- allMatches[ allMatches[[by]] !="" ,]
  predictedWord <- as.character(allMatches[order(V1,decreasing = T)][2][,by,with=FALSE][[1]])
  if (is.na(predictedWord)) {
    predictedWord <- predictNextFromWords(dts,words[-1])
  }
  predictedWord
}

predictNext <- function(dts,phrase) {
  words <- tokenize(phrase,simplify=T)
  predictNextFromWords(dts,words)
}
