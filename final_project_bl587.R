library(tidytext)
library(tidyverse)
library(ggplot2)
library(readxl)
library(tidyr)
library(MASS)
library(openintro)
install.packages("openintro")

data <- read.csv(file="candyhierarchy2017.csv", header=TRUE, sep=",")
income<-read_xlsx("income.xlsx")
income$State = toupper(income$State)

colnames(data)
#Droping redundant variables
data<-dplyr::select(data,c(3,4,5,6,51,87,57,98,35))

colnames(data)
#Normalizing Country
data<- data%>% filter(data$Q4..COUNTRY == "USA" | data$Q4..COUNTRY == "Us" | data$Q4..COUNTRY == "Usa" | data$Q4..COUNTRY == "US" | data$Q4..COUNTRY == "usa"| data$Q4..COUNTRY == "us"| data$Q4..COUNTRY == "United States"|data$Q4..COUNTRY == "United States of America")
data$Q4..COUNTRY = "USA"

#Normalizing State Names
data$Q5..STATE..PROVINCE..COUNTY..ETC =  toupper(data$Q5..STATE..PROVINCE..COUNTY..ETC)
for(x in 1:nrow(data)){
  if (nchar(data$Q5..STATE..PROVINCE..COUNTY..ETC[x]) == 2){
    data$Q5..STATE..PROVINCE..COUNTY..ETC[x] = abbr2state(data$Q5..STATE..PROVINCE..COUNTY..ETC[x])
  }
}
data$Q5..STATE..PROVINCE..COUNTY..ETC =  toupper(data$Q5..STATE..PROVINCE..COUNTY..ETC)
data <- (filter(data, nchar(data$Q5..STATE..PROVINCE..COUNTY..ETC)<= 18))
colnames(data)[4] <- "State"

#Cluster states by income
incomeCluster <- kmeans(income$`2015`, 3)
income$incomeCluster = incomeCluster[[1]]

#Adding Income & Cluster 
data<- inner_join(data, income, by='State')

colnames(data)[1] <- "Gender"
colnames(data)[2] <- "Age"
colnames(data)[3] <- "Country"
colnames(data)[5] <- "KitKAt"
colnames(data)[6] <- "Skittles"
colnames(data)[7] <- "Lollipop"
colnames(data)[8] <- "TicTac"
colnames(data)[9] <- "GumFromBaseballCards"
colnames(data)[10]<- "Income"

#Function for mode calculation
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Mode Imputation for missing vals
for (x in 5:9){
  which.one <- which( levels(data[[x]]) == "" )
  levels(data[[x]])[which.one] <- NA
}
data<- data %>% mutate_if(is.factor, funs(replace(.,is.na(.), Mode(na.omit(.)))))
data$Age = as.numeric(as.character(data$Age))
data<-data%>% drop_na()
data<-filter(data, data$Gender=="Male" | data$Gender=="Female" )

data$ageCluster = 0

#Creating the age cluster
for(x in 1:nrow(data)){
  if(data$Age[x]<=14){
    data$ageCluster[x] = 1
  }else if(data$Age[x] > 14 & data$Age[x]<=30 ){
    data$ageCluster[x] = 2
  }else if(data$Age[x] > 30 & data$Age[x]<=50 ){
    data$ageCluster[x] = 3
  }else {
    data$ageCluster[x] = 4
  }
}


#Saving the data as csv
write.csv(data, "data.csv")


#Get preference-gender distribution on candies
dataKit <- data%>%
  group_by(data$Gender,data$KitKAt)%>%
  summarise(freq=n())
pKit <- ggplot() + geom_bar(aes(y = dataKit$freq, x = dataKit$`data$Gender`, fill = dataKit$`data$KitKAt`), data = dataKit,
                          stat="identity") + labs(title = "Frequency Distribution of Gender by their Happiness factor for Kit")
pKit


dataSkit <- data%>%
  group_by(data$Gender,data$Skittles)%>%
  summarise(freq=n())
pSkit <- ggplot() + geom_bar(aes(y = dataSkit$freq, x = dataSkit$`data$Gender`, fill = dataSkit$`data$Skittles`), data = dataSkit,
                          stat="identity") + labs(title = "Frequency Distribution of Gender by their Happiness factor for Skittles")
pSkit



dataLol <- data%>%
  group_by(data$Gender,data$Lollipop)%>%
  summarise(freq=n())
plol<- ggplot() + geom_bar(aes(y = dataLol$freq, x = dataLol$`data$Gender`, fill = dataLol$`data$Lollipop`), data = dataLol,
                          stat="identity") + labs(title = "Frequency Distribution of Gender by their Happiness factor for Lollipos")
plol


datatic <- data%>%
  group_by(data$Gender,data$TicTac)%>%
  summarise(freq=n())
ptic <- ggplot() + geom_bar(aes(y = datatic$freq, x = datatic$`data$Gender`, fill = datatic$`data$TicTac`), data = dataHershey,
                          stat="identity") + labs(title = "Frequency Distribution of Gender by their Happiness factor for Tic Tacs")
ptic

dataGum<- data%>%
  group_by(data$Gender,data$GumFromBaseballCards)%>%
  summarise(freq=n())
pGum <- ggplot() + geom_bar(aes(y = dataGum$freq, x = dataGum$`data$Gender`, fill = dataGum$`data$GumFromBaseballCards`), data = dataGum,
                                stat="identity") + labs(title = "Frequency Distribution of Gender by their Happiness factor for Gum from baseball cards")
pGum

#Get the distribution of respendents on state and gender
dataStateCensus<- data%>%
  group_by(data$Gender,data$State)%>%
  summarise(freq=n())

pState <- ggplot() + geom_bar(aes(y = dataStateCensus$freq, x = dataStateCensus$`data$State`, fill = dataStateCensus$`data$Gender`), data = dataStateCensus,
                              stat="identity") + labs(title = "Number Of people who participated in the Census grouped by Gender") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pState

#Get the distribution of respendents on age and income
dataStateIncomeAge<- data%>%
  group_by(data$ageCluster,data$incomeCluster)%>%
  summarise(freq=n())

pStateIncomeAge <- ggplot() + geom_bar(aes(y = dataStateIncomeAge$freq, x = dataStateIncomeAge$`data$incomeCluster`, fill = dataStateIncomeAge$`data$ageCluster`), data = dataStateIncomeAge,
                              stat="identity") + labs(title = "Number Of people who participated in the Census grouped by Gender") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pStateIncomeAge

#Preference of lollipop on age
dataIncomeLol<- data%>%
  group_by(data$incomeCluster,data$Lollipop)%>%
  summarise(freq=n())
pIncomeLol <- ggplot() + geom_bar(aes(y = dataIncomeLol$freq, x = dataIncomeLol$`data$incomeCluster`, fill = dataIncomeLol$`data$Lollipop`), data = dataIncomeLol,
                            stat="identity") + labs(title = "Frequency Distribution of Happiness grouped by the income cluster for Lollipops")
pIncomeLol

dataAgeLol<- data%>%
  group_by(data$ageCluster,data$Lollipop)%>%
  summarise(freq=n())
pAgeLol <- ggplot() + geom_bar(aes(y = dataAgeLol$freq, x = dataAgeLol$`data$ageCluster`, fill = dataAgeLol$`data$Lollipop`), data = dataAgeLol,
                                  stat="identity") + labs(title = "Frequency Distribution of Happiness grouped by the age cluster for Lollipops")
pAgeLol

