## read the dataset
df <- read.csv("train.csv")
## dimensions of the dataset
dim(df)
#### first n rows
head(df)
#### last n rows
tail(df)
### number of columns
ncol(df)
## number of rows
nrow(df)
### structure of the dataset 
str(df)
library(dplyr)
### data types
glimpse(df)
## mean, median , mode, quartile
summary(df)
## missing values using is.na function
colSums(is.na(df))
# The province and country names are merged to a new feature named: Area
df$Area <- paste(df$Country_Region, df$Province_State, sep = "-")
## example of Australia
head(df[df$Country_Region == "Australia", ])

########## EDA ############
#Number of confirmed cases and fatalities as per March 28, 2020
colSums(df[as.character(df$Date) == "2020-03-28", c("ConfirmedCases", "Fatalities")])

#Global growth of confirmed cases and fatalities
library(stringr)
#preparation for object
total <- data.frame()

#iterate through month and day
for(month in 1:3) {
  for(day in 1:31) {
    reqDate <- paste("2020-0", month, "-", str_pad(day, 2, pad = "0"), sep = "")
    iter <- as.data.frame(colSums(df[as.character(df$Date) == reqDate,
                                     c("ConfirmedCases", "Fatalities")]))
    iter2 <- data.frame(Num = (month - 1) * 31 + day,
                        Month = month, Day = day, ConfirmedCases = iter[1, ],
                        Fatalities = iter[2, ])
    if(iter[1, ] != 0) total <- rbind(total, iter2)
  }
}

#create plot of cummulative confirmed cases
plot(total$Num, total$ConfirmedCases, 
     type = "l", col = "blue",
     xlab = "Day of (since January 22, 2020)",
     ylab = "Number of Person",
     main = "Cummulative Worldwide Confirmed Cases and Fatalities of Covid-19",
     sub = "arman.yusuf@gmail.com")

par(new = TRUE)

#create plot of cummulative fatalities
plot(total$Num, total$Fatalities, 
     type = "l", lty = 2, col = "red", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(side = 4)

legend("topleft", inset = .05, 
       legend = c("Confirmed Cases [Left axis]", "Fatalities [Right axis]"), 
       col = c("blue", "red"), bg = "gray",
       lty = c(1, 2))

############ Data Modeling #############

#variable initialization
df2 <- list()
confirmed.cases.model <- list()
fatalities.model <- list()

#extract Area information
areadata <- as.data.frame(table(df$Area))
areadata$Num <- row(areadata)

for(area in areadata$Var1) {
  #get per area data
  buffer <- df[df$Area == area, ]
  rownames(buffer) <- NULL
  buffer$Day <- as.numeric(rownames(buffer))
  df2[[area]] <- buffer
  
  #create models
 
  confirmed.cases.model[[area]] <- lm(ConfirmedCases ~ Day + I(Day^2) + I(Day^3) + I(Day^5), df2[[area]])
  fatalities.model[[area]] <- lm(Fatalities ~ Day + I(Day^2) + I(Day^3) + I(Day^5), df2[[area]])
}

########## Model performance #############
#By Visualizing the Cases and Fatalities Actual Data vs Model
#set area
area <- "Indonesia-"

#retrieve the data
data <- as.data.frame(df2[[area]])

#create plot
plot(data$Day, data$ConfirmedCases,
     type = "l", lty = 2,
     col = "blue",
     ylim = c(0, max(data$ConfirmedCases)),
     xlab = "Day of (Since January 22, 2020)", ylab = "Number of People",
     main = paste("Covid-19 Confirmed Cases in", area),
     sub = "arman.yusuf@gmail.com")
par(new = TRUE)
plot(data$Day, fitted(confirmed.cases.model[[area]]),
     type = "l", lty = 3,
     ylim = c(0, max(data$ConfirmedCases)),
     col = "red",
     xlab = "", ylab = "")
par(new = TRUE)

plot(data$Day, data$Fatalities,
     type = "l", lty = 3,
     col = "green",
     ylim = c(0, max(data$Fatalities)),
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
par(new = TRUE)
plot(data$Day, fitted(fatalities.model[[area]]),
     type = "l", lty = 4,
     ylim = c(0, max(data$Fatalities)),
     col = "black",
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
par(new = TRUE)
axis(side = 4)

legend("topleft", inset = .05, 
       legend = c("Confirmed Cases [Left Figure]",
                  "Estimated Cases (based on Model) [Left Figure]", 
                  "Confirmed Fatality [Right Figure]",
                  "Estimated Fatality (based on Model) [Right Figure]"), 
       col = c("blue", "red", "green", "black"), bg = "gray",
       lty = c(2, 3, 4, 5),
       cex = 0.75)

#Measures the model performance by comparing the last day of the (day 67: March 28, 2020)
#accuracy of the prediction for all area:

#set parameter
day <- 67 # March 28, 2020

#prepare the object
accuracy <- data.frame()

#iterate the confirmed vs prediction on each of area
for(area in areadata$Var1) {
  data <- df2[[area]]
  buffer <- data.frame(Area = area, 
                       ConfirmedCases = data$ConfirmedCases[day], 
                       EstimatedCases = round(predict(confirmed.cases.model[[area]], 
                                                      newdata = data.frame(Day = day))), 
                       ConfirmedFatalities = data$Fatalities[day], 
                       EstimatedFatalities = round(predict(fatalities.model[[area]],
                                                           newdata = data.frame(Day = day)))
  )
  accuracy <- rbind(accuracy, buffer)
}
#calculate accuracy for cases and fatality; confirmed vs estimation
accuracy$AccuracyCases <- 1 - (abs(accuracy$ConfirmedCases - accuracy$EstimatedCases) / accuracy$ConfirmedCases)
accuracy$AccuracyFatalities <- 1 - (abs(accuracy$ConfirmedFatalities - accuracy$EstimatedFatalities) / accuracy$ConfirmedFatalities)

#fix the estimated 0 actual 0 calculation
accuracy[is.nan(accuracy$AccuracyFatalities), "AccuracyFatalities"] <- 1

#print the result
accuracy

print(paste("Worldwide Accuracy of Cases: ", mean(accuracy$AccuracyCases)))
print(paste("Worldwide Accuracy of Fatalities: ", mean(accuracy$AccuracyFatalities)))

test <- read.csv("test.csv")
head(test)

## Feature engineering
#fix the data to fit the model
#prepare the Day to fit the regression model
buffer <- data.frame()
for(area2 in 1:294){
  for(date2 in 68:110){
    buffer <- rbind(buffer, data.frame(Day = date2))
  }
}

#join the Day to test data, and fix the columns' names
test <- cbind(test, buffer)
colnames(test) <- c("ForecastId", "Province_State", "Country_Region", "Date", "Day")

#create Area feature
test$Area <- paste(test$Country_Region, test$Province_State, sep = "-")

#list down the test data
head(test)

#Generate the prediction on test data
#prepare the data
final <- data.frame()

#iterate the prediction
for (id in test$ForecastId){
  pred <- test[test$ForecastId == id, ]
  pred.cases <- predict(confirmed.cases.model[[pred$Area]],
                        newdata = data.frame(Day = pred$Day))
  pred.fatality <- predict(fatalities.model[[pred$Area]],
                           newdata = data.frame(Day = pred$Day))
  buffer <- data.frame(ForecastId = id,
                       ConfirmedCases = pred.cases,
                       Fatalities = pred.fatality)
  final <- rbind(final, buffer)
}

#adjusting minus value to 0
final$ConfirmedCases <- ifelse(final$ConfirmedCases < 0, 0, final$ConfirmedCases)
final$Fatalities <- ifelse(final$Fatalities < 0, 0, final$Fatalities)

#print forecast for Indonesia
print(final[final$ForecastId > 5807 & final$ForecastId < 5849, ])




