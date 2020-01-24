setwd("~/Documents/presentations/Workshops/drsGHWorkshops/S2020/IntermedTextAnalysis/")

directory <- "texts"

fileNames <- dir(directory, pattern = ".*txt")

# create a list of all of the word frequencies
allFrequencies <- list()
for (i in 1:length(fileNames)) {
  textData <- scan(file.path(directory, fileNames[i]), what = "character", sep = "\n")
  words <- paste(textData, collapse=" ")
  word <- unlist(strsplit(tolower(words), "\\W"))
  noBlanks <- which(word!="")
  word <- word[noBlanks]
  text.freqs <- table(word)
  text.relFreqs <- 100 * (text.freqs/sum(text.freqs))
  allFrequencies[[fileNames[i]]] <- text.relFreqs
}

# create a big list of word frequencies for all the texts, and provide an ID for each text
frequencies <- mapply(data.frame, ID=seq_along(allFrequencies), allFrequencies, SIMPLIFY = FALSE, MoreArgs = list(stringsAsFactors=FALSE))

# create a big data frame (tabular format) of all of the frequencies
# there will be 3 columns: ID, word, Freq
frequencies.df <- do.call(rbind, frequencies)

# create a full word frequency table and then convert it into a matrix
fullFreq.table <- xtabs(Freq ~ ID + word, data = frequencies.df)
fullFreq.matrix <- apply(fullFreq.table, 2, as.numeric)

# reduce the matrix to only include words whose relative frequency is above .25
frequentWords.matrix <- fullFreq.matrix[,apply(fullFreq.matrix,2,mean)>=.25]

# distance between features
featDist <- dist(frequentWords.matrix)
clusterTexts <- hclust(featDist)
clusterTexts$labels <- names(allFrequencies)






