install.packages("xlsx")
install.packages("plyr")
install.packages("arules")
library(xlsx)
library(plyr)
phone_data <- read.csv(file.choose(),header = FALSE)
# phone_data1 <- readLines(file.choose()) # if we use readLines functions more lines will be reduced
View(phone_data)
str(phone_data)
# converting everything into character format 
phone_data[] <- lapply(phone_data,as.character)
View(phone_data[])
# Creating a custom fucntion to collapse all the items in a transaction into 
# a single sentence 
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
phone_data["new_col"] <- apply(phone_data,1,paste_fun)
View(phone_data)


install.packages("tm")
# tm package is used to do text manipulation and forming DTM and TDM matrices
library(tm)
x <- Corpus(VectorSource(phone_data$new_col)) # Selecting the new column which
# contains all items of a transaction in a single sentence
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(x))
# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

# Association Rules 

library(arules)
library(arulesViz)
library(class)
# Item Frequecy plot
windows()
# count of each item from all the transactions 
barplot(sapply(dtm0_df,sum),col=1:10)
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.5,minlen=2))
inspect(rules)
plot(rules)

# Sorting rules by confidence 

rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

# Visualizing rules in scatter plot
?plot

plot(rules,method = "graph")


########## Groceries Data Set #########
install.packages("arules")
library(arules)
library(readr)
groceries<-read.transactions(file.choose(),format="basket")
View(groceries)
inspect(groceries[1:10])
class(groceries)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=20)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
install.packages("arulesViz")
library(arulesViz)
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
plot(groceries_rules,method = "mosaic")


### On inbuilt Data set #####
library(arules)
data("Groceries")
summary(Groceries)
inspect(Groceries[1:10])
rules <- apriori(Groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=5))
inspect(rules[1:5])

plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

rules <- sort(rules,by="lift")

inspect(rules[1:4])


#####for books data set####

install.packages("mvinfluence")
install.packages("carData")
library(arules)
library(tm)
library(mvinfluence)
library(carData)
library(readr)
Data<-  read.csv(file.choose(), header = FALSE)
colnames(Data)
Data$ChildBks <- factor(Data$ChildBks,levels = c("1","0"),labels = c("ChildBks",""))

Data$YouthBks <- factor(Data$YouthBks,levels = c("1","0"),labels = c("YouthBks",""))

Data$CookBks <- factor(Data$CookBks,levels = c("1","0"),labels = c("CookBks",""))

Data$DoItYBks <- factor(Data$DoItYBks,levels = c("1","0"),labels = c("DoItYBks",""))

Data$RefBks <- factor(Data$RefBks,levels = c("1","0"),labels = c("RefBks",""))

Data$ArtBks <- factor(Data$ArtBks,levels = c("1","0"),labels = c("ArtBks",""))

Data$GeogBks <- factor(Data$GeogBks,levels = c("1","0"),labels = c("GeogBks",""))

Data$ItalCook <- factor(Data$ItalCook,levels = c("1","0"),labels = c("ItalCook",""))

Data$ItalAtlas <- factor(Data$ItalAtlas,levels = c("1","0"),labels = c("ItalAtlas",""))

Data$ItalArt <- factor(Data$ItalArt,levels = c("1","0"),labels = c("ItalArt",""))
Data$Florence <- factor(Data$Florence,levels = c("1","0"),labels = c("Florence",""))
install.packages("car")
library(car)
Data1 <- as(Data,"transactions")
# Item Frequency plot
itemFrequencyPlot(Data1,topN=25)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
Book_apriori <- apriori(Data1, parameter = list(supp=0.005, conf=0.45, minlen=2, maxlen=4))
###Book_apriori<-apriori(Data1,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
install.packages("arulesViz")
library(arules)
library(arulesViz)
plot(Book_apriori,method = "scatterplot")
plot(Book_apriori,method = "grouped")
plot(Book_apriori,method = "graph")
plot(Book_apriori,method = "mosaic")

Book_apriori
inspect(head(sort(Book_apriori),n=20))
inspect(tail(sort(Book_apriori),n=20))

####movies###
movie<-  read.csv(file.choose())
View(movie)
data <- movie[6:15]
class(data)
colnames(data)
data$Sixth.Sense <- factor(data$Sixth.Sense,levels = c("1","0"),labels = c("Sixth.Sense",""))
data$Gladiator <- factor(data$Gladiator,levels = c("1","0"),labels = c("Gladiator",""))
data$LOTR1 <- factor(data$LOTR1,levels = c("1","0"),labels = c("LOTR1",""))
data$Harry.Potter1 <- factor(data$Harry.Potter1,levels = c("1","0"),labels = c("Harry.Potter1",""))
data$Patriot <- factor(data$Patriot,levels = c("1","0"),labels = c("Patriot",""))
data$LOTR2 <- factor(data$LOTR2,levels = c("1","0"),labels = c("LOTR2",""))
data$Harry.Potter2 <- factor(data$Harry.Potter2,levels = c("1","0"),labels = c("Harry.Potter2",""))
data$LOTR <- factor(data$LOTR,levels = c("1","0"),labels = c("LOTR",""))
data$Braveheart <- factor(data$Braveheart,levels = c("1","0"),labels = c("Braveheart",""))
data$Green.Mile <- factor(data$Green.Mile,levels = c("1","0"),labels = c("Green.Mile",""))
data1 <- as(data,"transactions")

itemFrequencyPlot(data1,topN=15)
inspect(head(sort(rules), n = 10))

rules <- apriori(data1, parameter = list(supp = 0.005, confidence = 0.50, minlen = 2, maxlen = 3))

plot(head(sort(rules, by = "lift"), n = 10), method = "graph", control = list(cex = 1.0))
        
plot(rules)

plot(head(sort(rules), n = 10), method = "grouped", control = list(cex = 0.3))


#####retail trasnsactions#####

install.packages("xlsx")
install.packages("dplyr")
library(dplyr)
library(tidytext)
library(readxl)
library('xlsx')
retail <- read.csv(file.choose(),header = FALSE)
colnames(retail)
str(retail)
colSums(is.na(retail))
retail[] <- lapply(retail,as.character)
View(retail)
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
retail["new_col"] <- apply(retail,1,paste_fun)
View(retail)
final_data <- na.exclude(retail)
View(final_data)
install.packages("tm")
# tm package is used to do text manipulation and forming DTM and TDM matrices
library(tm)
y <- Corpus(VectorSource(final_data$new_col)) # Selecting the new column which

# contains all items of a transaction in a single sentence

y <- tm_map(y,stripWhitespace)
# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(y))
# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

library(arules)
install.packages("plyr")
library(plyr)
library(arulesViz)
library(class)
# Item Frequecy plot
# count of each item from all the transactions 
barplot(sapply(dtm0_df,sum),col=1:10)
# Applying apriori algorithm to get relevant rules
rules_retail <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.5,minlen=2))
inspect(rules_retail)
plot(rules_retail)
