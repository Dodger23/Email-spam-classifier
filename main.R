library(RcppArmadillo)
library(quanteda)
library(RColorBrewer)
library(quanteda.textmodels)

#reading data 
raw.data <- read.csv("data/emails.csv",
                     header=TRUE, sep=",", quote='\"\"', stringsAsFactors=FALSE)

#specifing names for columns 
names(raw.data) <- c("Category", "Message")

#taking a sample of data becaause the full data s too large to model
set.seed(1912)  
raw.data <- raw.data[sample(nrow(raw.data)),]


# making a corups to supset from it later 
sms.corpus <- corpus(raw.data$Message) 
docvars(sms.corpus, "Category" ) <- raw.data$Category

#creating a spam cloud 
spam.plot <- corpus_subset(sms.corpus  ,Category == "spam")  
spam.plot <- dfm(spam.plot, tolower = TRUE, remove_punct = TRUE,
                 remove_twitter = TRUE, remove_numbers = TRUE,
                 remove=stopwords(source = "smart"))
#plot the spam cloud 
spam.col <- brewer.pal(10, "BrBG")  
spam.cloud <- textplot_wordcloud(spam.plot, min_count = 16, color = spam.col)  
title("Spam Wordcloud", col.main = "grey14")


#create a ham cloud 
ham.plot <- corpus_subset(sms.corpus, Category == "ham")  
ham.plot <- dfm(ham.plot, tolower = TRUE, removePunct = TRUE,
                removeTwitter = TRUE, removeNumbers = TRUE,
                remove=c("gt", "lt", stopwords(source= "smart")))  
#plot the ham cloud 
ham.col <- brewer.pal(10, "BrBG")  
ham.cloud = textplot_wordcloud(ham.plot, min_count = 50, color = ham.col )  
title("Ham Wordcloud", col.main = "grey14")



sms.dfm <- dfm(sms.corpus, tolower = TRUE)  
sms.dfm <- dfm_trim(sms.dfm, min_count = 5, min_docfreq = 0.5)  
sms.dfm <- dfm_weight(sms.dfm)  

#splitting the data as a train part and test part 
sms.raw.train <- raw.data[1:4738,]  
sms.raw.test <- raw.data[4739:nrow(raw.data),]

#making a naive bayse model with the train data 
sms.classifier <- textmodel_nb(sms.dfm.train , sms.raw.train$Category)  

#predicting on the test data 
sms.predictions <- predict(sms.classifier, newdata = sms.dfm.test)  
table(sms.predictions, sms.raw.test$Category)














