library(ggplot2)
library(readr)
library(plotly)
library(dplyr)
library(lubridate)
library(xts)

### Part 1

### Histograms
infec = read_csv("Covid-Data-Irl.csv")
infec

infec$Date <- sub(" .*", "", infec$Date)
infec

covid <- infec$ConfirmedCovidCases
h_covid_base <- hist(covid,
     main="Recorded Irish Covid Daily Cases 3/3/20 to 21/2/23",
     xlab="Cases",
     breaks=25,
     col="darkgoldenrod"
)

text(h_covid_base$mids,h_covid_base$counts,labels=h_covid_base$counts, adj=c(0.5, 0))

## now use ggplot
ggplot(infec, aes(x=ConfirmedCovidCases))+
  geom_histogram(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(ConfirmedCovidCases)), color="red",
             linetype="dashed") +
  stat_bin(aes(label=..count..), vjust=-0.4, geom = "text") +
  labs(title="Confirmed Daily Total Irish Covid Cases 3/3/20 to 21/2/23",x="Cases", y = "Frequency")

#### Scatterplots
plot(infec$DailyHealthcareWorkersCases, infec$DailyHospitalizedCases, main="Daily Covid Hospital Cases VS Confirmed Healthcare Worker Cases",
     xlab="HealthCare Cases", ylab="Hospitalisations", pch=19)

## ggplot
ggplot(infec, aes(x=DailyHealthcareWorkersCases, y=DailyHospitalizedCases)) + geom_point(size=1, color='slateblue4') +
  geom_smooth(se=FALSE, method=lm, colour = "red1")

## test correlation from scatterplot
test_corr <- cor.test(infec$DailyHealthcareWorkersCases, infec$DailyHospitalizedCases, 
                method = "pearson")
test_corr

### Part 2 - Time Series Analysis
clients = read_csv("client_analysis.csv")
clients
str(clients)

## create time-series friendly dates - taken from here https://stackoverflow.com/questions/39762543/convert-character-month-name-to-date-time-object
myFun <- function(x, dummyDay = "01", dummyYear = "2013"){
  
  x <- ifelse(substr(x, 1, 3) %in% month.abb,
              paste(match(substr(x, 1, 3), month.abb),
                    dummyDay,
                    dummyYear, sep = "/"), x)
  #return date
  mdy(x)
}

res <- data.frame(lapply(clients[1], myFun))
res

### have to use ts function first
clientsdf = clients %>% select_if(is.numeric)
finaldf = cbind(clientsdf, res)
finaldf
plot(finaldf)
## check types
str(finaldf)

df.ts = ts(finaldf[1:ncol(finaldf) - 1], frequency = 12, start=c(2013, 1))
plot.ts(df.ts)

is.ts(df.ts)

ts.plot(df.ts, col = 1:ncol(df.ts) - 1, xlab = "Year/Month", ylab = "Customer Spending Total", main = "Customer Spending for Selected Companies")
legend("topleft", legend = colnames(clientsdf), pch = 1:ncol(df.ts),
       col = 1:ncol(df.ts), cex=0.45, x.intersp=0.25, y.intersp = .2)

### now more advanced libraries using ggplot
## interactive tooltip for values and date
plot_advanced = finaldf %>%
  ggplot(aes(x = Month, Am = Amazon, SP = SAP,
             SB = SmartBear, Cs = Cisco, Del = Deloitte,
             US = USB, Dil = Diligent)) +
  geom_line(aes(y = Amazon, colour="Am")) +
  geom_line(aes(y = USB, colour="US")) +
  geom_line(aes(y = SAP, colour="SP")) +
  geom_line(aes(y = Deloitte, colour="Del")) +
  geom_line(aes(y = Cisco, colour="CS")) +
  geom_line(aes(y = SmartBear, colour="SB")) +
  geom_line(aes(y = Diligent, colour="Dil")) +
  ggtitle("Time Series Plot for Customer Spending 2013") +
  ylab("Sales") + 
  scale_color_manual(values = c(Am = "blue", SP = "red", US = "goldenrod3", Del = "springgreen",
                                CS = "cyan", Dil = "grey22", SB = "mediumvioletred"),
                     labels = c(Am = "Amazon", SP = "SAP", US = "USB", Del = "Deloitte",
                                Cs = "Cisco", Dil = "Diligent", SB = "Smartbear"))

plt22 <- ggplotly(plot_advanced, tooltip = c("x", "SP", "US", "Del", "Am", "SB", "Dil", "Cs"))
plt22

### Part 3 - tweets
install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
library("tm")
library("SnowballC")
#install.packages("tidytext")
library(tidytext)
twitter = read_csv('tweets.csv')
head(twitter, n = 20)

twitter_df <- data_frame(Text = twitter$text)

TextCorp <- Corpus(VectorSource(twitter$text))
TextCorp

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

clean_corp <- function(corr){
  corr <- tm_map(corr, toSpace, "/")
  corr <- tm_map(corr, toSpace, "@")
  corr <- tm_map(corr, toSpace, "\\|")
  ## convert text to lower case
  corr <- tm_map(corr, content_transformer(tolower))
  ## remove numbers
  corr <- tm_map(corr, removeNumbers)
  ## remove stopwords
  corr <- tm_map(corr, removeWords, stopwords("english"))
  ## remove punctuations
  corr <- tm_map(corr, removePunctuation)
  ## remove extra white spaces
  corr <- tm_map(corr, stripWhitespace)
  ## Text stemming - reduces words to their root form
  corr <- tm_map(corr, stemDocument)
  ### return corpus
  return(corr)
}
TextCorp1 = clean_corp(TextCorp)

frequency_m <- function(freq_matrix){
  TextDoc_dtm <- TermDocumentMatrix(freq_matrix)
  dtm_m <- as.matrix(TextDoc_dtm)
  # Sort by descearing value of frequency
  dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
  dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
  return(dtm_d)
}
dtm1 = frequency_m(TextCorp1)

# Display the top 25 most frequent words
head(dtm1, 25)

dtm1[1:25, ] %>%
  mutate(word = reorder(word, freq)) %>% 
  ggplot(aes(word,freq)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Frequent Words In Twitter Data") +
  geom_text(aes(label = freq), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12))

### word cloud
#install.packages("wordcloud")
library(wordcloud)
wordcloud(words = dtm1$word,freq = dtm1$freq, scale=c(2, .5),max.words=40,
          random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

### by hour
library(stringr)
# split date and time into seperate columns
twitter[ , 5:6] <- str_split_fixed(twitter$date, " ", 2)
twitter$Hour = substr(twitter$V2,1,2)
colnames(twitter)[5] <- "Date"
colnames(twitter)[6] <- "Time"
twitter

## now plot by hour
vec <- c("06","07","08","09","10") 
ggplot(twitter, aes(x = Hour, fill = ifelse(Hour %in% vec, "Highlighted", "Normal"))) + 
  ggtitle("Tweets Per Hour") +
  ylab("Tweets") +
  geom_bar() + theme(legend.position = "none", axis.title.y = element_blank())

### usage per month
head(twitter, n = 10)

twitter$Month = str_split_i(twitter$Date, "-", 2)
ggplot(twitter, aes(x = Month)) + 
  ggtitle("Tweets Per Month") +
  ylab("Tweets") +
  geom_bar(colour = "black", fill = "springgreen4")

## TOP 15 WORDS - DIFFERENT SOURCES
twitter %>% count(source, sort = TRUE)
# much more iphone tweets than media studio
#### now seperate based on iphone/media studio
iphone = twitter[twitter$source == 'Twitter for iPhone',]
media_s = twitter[twitter$source == 'Media Studio',]
## create corpus for iphone
TextCorp2 <- Corpus(VectorSource(iphone$text))
TextCorp2
## create corpus for media studio
TextCorp3 <- Corpus(VectorSource(media_s$text))
TextCorp3

TextCorp2_cleaned = clean_corp(TextCorp2)
TextCorp3_cleaned = clean_corp(TextCorp3)
dtm2 = frequency_m(TextCorp2_cleaned)
dtm3 = frequency_m(TextCorp3_cleaned)

### compare
pltiphone = dtm2[1:15, ] %>%
  mutate(word = reorder(word, freq)) %>% 
  ggplot(aes(word, freq)) + 
  geom_col(fill = "springgreen4") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Frequent Words In Twitter Data") +
  geom_text(aes(label = freq), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12))

pltmedia = dtm3[1:15, ] %>%
  mutate(word = reorder(word, freq)) %>% 
  ggplot(aes(word, freq)) + 
  geom_col(fill = "navyblue") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Frequent Words In Twitter Data") +
  geom_text(aes(label = freq), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12))

plt6 <- ggplotly(pltiphone)
plt7 <- ggplotly(pltmedia)

## both graphs on same plot
fig5 <- subplot(plt6, plt7) %>% 
  layout(title = "Comparison of iPhone (green) VS Media Studio (blue) most frequent words")
fig5

### first 6 VS last 6 comparison
twitter$Month = str_split_i(twitter$date, "-", 2)
vec_first <- c("01","02","03","04","05", "06")
first = twitter[twitter$Month %in% vec_first,]
last = twitter[!(twitter$Month %in% vec_first),]

## create corpus for first 6 months data
TextCorp4 <- Corpus(VectorSource(first$text))
TextCorp4
## create corpus for last 6 months data
TextCorp5 <- Corpus(VectorSource(last$text))
TextCorp5
### clean and term frequency matrices
TextCorp4_cleaned = clean_corp(TextCorp4)
TextCorp5_cleaned = clean_corp(TextCorp5)
dtm4 = frequency_m(TextCorp4_cleaned)
dtm5 = frequency_m(TextCorp5_cleaned)

common_firstsix <- c(dtm4[1:1000,]$word)
words_not_used <- c(dtm5$word)

common_firstsix
words_not_used

diff <- setdiff(common_firstsix, words_not_used)
print ("Vec1- Vec2")
print(diff)

dtm4[dtm4$word %in% diff,]

### plot unused words final 6 months
dtm4[dtm4$word %in% diff,] %>%
  mutate(word = reorder(word, freq)) %>% 
  ggplot(aes(word, freq)) + 
  geom_col(fill = "cornflowerblue") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Unused words in last 6 months commonly used in first 6 months") +
  geom_text(aes(label = freq), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12))
