
createConfidencePlot <- function(predictions) {
  predictions.show <- tail(predictions[! duplicated(predictions),],10)
  predictions.show$word <- make.unique(predictions.show$word)
  predictions.show$word <- factor(predictions.show$word,predictions.show$word,ordered = T)

  ggplot(predictions.show,aes(x = word,y=prob)) +
    geom_bar(aes(fill=prob),stat="identity") +
    scale_fill_gradient2(low = 'red', mid = 'yellow', high = 'green', midpoint = 50) +
    coord_cartesian(xlim=c(0,12),ylim=c(0,100)) +
    ggtitle("Confidence of last 10 predictions") +
    ylab("Confidence") + xlab("predicted word") +
    theme_minimal()
}