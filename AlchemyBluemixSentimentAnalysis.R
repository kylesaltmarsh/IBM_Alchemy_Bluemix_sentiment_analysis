#-----------------------------------------------------------------------------------------#
# Kyle Saltmarsh
#
# Sentiment Analysis using IBM Alchemy Bluemix call
#
#-----------------------------------------------------------------------------------------#

install.packages("data.table")
install.packages("httr")

library(data.table)
library(httr)

# Set working directory
setwd("Documents/Github/AlchemyBluemix_sentiment_analysis")


# Meeting_Comments_Table is a table of the form
#   V1
# 1 This Is Comment 1
# 2 This Is Comment 2
# 3 This Is Comment 3
# . .

# Read in text csv table
Meeting_Comments_Table <- as.data.table(read.csv("BatmanReviews.csv",header=FALSE,sep=','))

# Perform cleaning and sentiment analysis
Meeting_Comments_Table <- Clean_Meeting_TXT(Meeting_Comments_Table)
Meeting_Comments_Table <- Bluemix_Text_Analytics_Function(Meeting_Comments_Table)

# Save as csv
write.csv(...)

#-----------------------------------------------------------------------------------------#

Clean_Meeting_TXT <- function(Meeting_Comments_Table) {

  # Remove new lines
  Meeting_Comments_Table <- as.data.table(apply(Meeting_Comments_Table, 1, function(y) gsub("\r\n", "", y)))
  # Replace punctuation and spaces with %20 for url format
  Meeting_Comments_Table <- as.data.table(apply(Meeting_Comments_Table, 1, function(y) gsub("[[:punct:]]", "", y)))
  Meeting_Comments_Table <- as.data.table(apply(Meeting_Comments_Table, 1, function(y) gsub(" ", "%20", y)))
  Meeting_Comments_Table <- as.data.table(apply(Meeting_Comments_Table, 1, function(y) gsub("\r\n", "%20", y)))

  return(Meeting_Comments_Table)

}

#-----------------------------------------------------------------------------------------#
# Do Bluemix calls


Bluemix_Text_Analytics_Function <- function(Meeting_Comments_Table) {

  # Unique API key has been replaced with ###
  url <- toString('https://gateway-a.watsonplatform.net/calls/text/TextGetTextSentiment?outputMode=json&apikey=###&text=')

  for(i in 1:dim(Meeting_Comments_Table)[1]) {
    datanewPercent = Meeting_Comments_Table$V1[i]
    urlp2 = toString(datanewPercent)
    senti = paste0(url, urlp2)
    response = POST(senti)
    parsed = content(response, "parsed")

    #Store the value of the post response

    if (is.null(parsed$docSentiment$score) & !is.null(parsed$docSentiment$type)) {
      Meeting_Comments_Table$SentimentScore[i] <- 0
      Meeting_Comments_Table$SentimentType[i] <- parsed$docSentiment$type

    } else if (is.null(parsed$docSentiment$score) & is.null(parsed$docSentiment$type)) {
      Meeting_Comments_Table$SentimentScore[i] <- 0
      Meeting_Comments_Table$SentimentType[i] <- 0

    } else {
      Meeting_Comments_Table$SentimentScore[i] <- parsed$docSentiment$score
      Meeting_Comments_Table$SentimentType[i] <- parsed$docSentiment$type
    }
  }

  return(Meeting_Comments_Table)

}


#-----------------------------------------------------------------------------------------#



#-----------------------------------------------------------------------------------------#
# End of code
#
#-----------------------------------------------------------------------------------------#
