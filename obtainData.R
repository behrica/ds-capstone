library(hash)


#download.file("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip")
#unzip("Coursera-SwiftKey.zip")

writeSample <- function() {

    con <- file("final/en_US/en_US.twitter.txt")
    all <- readLines(con)
    s100 <- sample(all,100)
    writeLines(s100,"en_US.twitter.100.txt")
    close(con)
}

tokenizeLine <- function(line) {
    words <- unlist(str_split(line,"[[:blank:]]|[[:punct:]]"))
    words <- tolower(words)
    words <-  Filter(function(word) nchar(word)>0, words)
}

tokenize <- function(file) {
   lapply(readLines(file),tokenizeLine)
}


countWords <- function(tokens) {
    if (exists("wordcount")) rm(wordCounts)
    wordCounts <- hash()
    dummy <-sapply(unlist(linesTokenized),function(word) {
        wordCounts[[word]] <- ifelse(is.null(wordCounts[[word]]),1,wordCounts[[word]] + 1)
    })
    wc <- data.frame(word=names(wordCounts),count=unname(unlist(as.list(wordCounts))))
    rm(wordCounts)
    wc
}





#writeSample()
linesTokenized <-tokenize("en_US.twitter.100.txt")
wc <- countWords(linesTokenized)
