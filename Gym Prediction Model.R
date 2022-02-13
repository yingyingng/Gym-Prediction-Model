library(dplyr)

data <- read.csv('datatemp (1).csv', stringsAsFactors = FALSE)


data$Date.recode <- as.Date(data$Date.only)

new <- data %>% group_by(Date.recode, day_of_week, hour) %>% summarise(mean(number_people))
new$oneweeklater <- as.Date(new$Date.recode) + 7


new$sem <- 'holiday'
new$sem[new$Date.recode >= "2015-08-19" & new$Date.recode <= "2015-09-22"] <- 'start'
new$sem[new$Date.recode >= "2015-09-23" & new$Date.recode <= "2015-11-04"] <- 'during'
new$sem[new$Date.recode >= "2015-11-05" & new$Date.recode <= "2015-12-18"] <- 'end'
new$sem[new$Date.recode >= "2016-01-12" & new$Date.recode <= "2016-02-28"] <- 'start'
new$sem[new$Date.recode >= "2016-03-01" & new$Date.recode <= "2016-04-07"] <- 'during'
new$sem[new$Date.recode >= "2016-04-08" & new$Date.recode <= "2016-05-15"] <- 'end'
new$sem[new$Date.recode >= "2016-08-19" & new$Date.recode <= "2016-09-22"] <- 'start'
new$sem[new$Date.recode >= "2016-09-23" & new$Date.recode <= "2016-11-04"] <- 'during'
new$sem[new$Date.recode >= "2016-11-05" & new$Date.recode <= "2016-12-18"] <- 'end'
new$sem[new$Date.recode >= "2017-01-12" & new$Date.recode <= "2017-02-28"] <- 'start'
new$sem[new$Date.recode >= "2017-03-01"] <- 'during'

new$time <- 'morning'
new$time[new$hour >= "12" & new$hour <= "16"] <- 'afternoon'
new$time[new$hour >= "17" & new$hour <= "20"] <- 'night'

names(new)[names(new) == "mean(number_people)"] <- "mean.crowd"
#names(new)[names(new) == "mean(temperature)"] <- "mean.temperature"
q <- quantile(new$mean.crowd, probs = c(.75))

new$people <- 'low'
new$people[new$mean.crowd > q] <- 'high'

newtwo <- new[-c(seq(1,88)),]
new$dnhone <- paste(new$oneweeklater, new$hour)
newtwo$datenhour <- paste(newtwo$Date.recode, newtwo$hour)
total <- merge(new,newtwo,by.x=c("dnhone"), by.y = c("datenhour"))

drop <- c("oneweekslater.x", "Date.recode.y", "day_of_week.x", "hour.x", "oneweeklater.y", "time.x", "temp.y", "dnhone", "Date.recode.x", "mean.crowd.x", "mean.temperature.x", "sem.x", "temp.x", )
total <- total[,!(names(total) %in% drop)]

keep <- c("day_of_week.y, hour.y, people.x, sem.y, people.y")
total <- total[c(10,12,17,18,19)]

# total$id <- seq.int(nrow(total))
table(total$people.y)

print(total)

total$day_of_week.y[total$day_of_week.y == '0'] <- '7'

n <- nrow(total)
p <- 0.7

count <- 0
score <- NULL
#rcl <- NULL
#set.seed(count)
while (count < 100)
{ 
idx2 <- sample(n) <= n*p 
train2 <- total[idx2,]
test2 <- total[!idx2,]


S <- table(train2$people.y, train2$sem.y)
S <- S / rowSums(S) #summation of each row

TM <- table(train2$people.y, train2$time.y)
TM <- TM / rowSums(TM)

DW <- table(train2$people.y, train2$day_of_week.y)
DW <- DW / rowSums(DW)

M <- table(train2$people.y, train2$people.x)
M <- M/rowSums(M)


#TE <- table(train2$people.y, train2$temp.x)
#TE <- TE/rowSums(TE)
  
# test2$id <- seq.int(nrow(test2))
machine <- c()
for (i in 1:nrow(test2))
{
  s <- test2$sem.y[i]
  tm <- test2$time.y[i] 
  dw <- test2$day_of_week.y[i]
  m <- test2$people.x[i]
  #te <- test2$temp.x[i]
  #lst <- c(s, tm, dw, m)
  #print(lst)
  
  L <- S[,s] * TM[,tm] * DW[,dw] * M[,m]
  P <- c(.25, .75) #prior probability
  c <- 1/sum(L)
  post <- L* P * c #likelihood x prior x constant
  #print(post)
  pred <- names(post)[which.max(post)]
  #print(pred)
  machine <- c(machine, pred)
  # a <- a + 1
  # print(a)
  # print(pred)
}

#print(machine)
#performancemeasures
Y <- table(machine, test2$people.y) #to find accuracy of machine

precision <- diag(Y)/rowSums(Y)
recall <- diag(Y)/colSums(Y)
#rcl <- rbind(rcl, recall)

f <- 2 * precision * recall /(precision + recall) #calculation of f measure
score <-rbind(score, f) #rbind function to change to matrix
count <- count + 1
}
print(score)
#print(rcl)


colMeans(score)






