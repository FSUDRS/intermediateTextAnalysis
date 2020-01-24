setwd("~/Documents/presentations/Workshops/drsGHWorkshops/S2020/IntermedTextAnalysis/")

# combine all of Charlotte Smith's Celestina into one R variable
smith1 <- scan("texts/smith.celestina1.txt", what = "character", sep="\n")
smith2 <- scan("texts/smith.celestina2.txt", what = "character", sep="\n")
smith3 <- scan("texts/smith.celestina3.txt", what = "character", sep="\n")
smith4 <- scan("texts/smith.celestina4.txt", what = "character", sep="\n")
smith.all <- c(smith1, smith2, smith3, smith4)
smith <- paste(smith.all, collapse = " ")

# make the text lowercase 
# split the text up on "non-word" characters (outputs as a list)
# convert that list into a character vector
smith.word <- unlist(strsplit(tolower(smith), "\\W"))

# remove blanks
noBlanks <- which(smith.word!="")
smith.word <-smith.word[noBlanks]

# creating a word frequency table in descending order of frequency
smith.freqs <- sort(table(smith.word), decreasing = TRUE)

# plot the top ten words (raw frequency)
plot(smith.freqs[1:10], type = "b", xlab = "Top Ten Words in Smith's Celestina", ylab = "Raw Frequencies of the Top Ten Words")
axis(1,1:10, labels = names(smith.freqs[1:10]))

# create relative frequency table 
smith.relFreqs <- 100 * (smith.freqs/sum(smith.freqs))

# and plot it
plot(smith.relFreqs[1:10], type = "b", xlab = "Top Ten Words in Smith's Celestina", ylab = "Relative Frequencies of the Top Ten Words")
axis(1,1:10, labels = names(smith.relFreqs[1:10]))


