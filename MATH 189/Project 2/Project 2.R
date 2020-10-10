#reads in data from local drive
mydata = read.table("/Users/ruyinzhang/Desktop/videodata.txt", header = TRUE)

#highlight function that highlights specific value 
#modified based on reference: 
#https://stackoverflow.com/questions/2127926/how-do-i-highlight-an-observations-bin-in-a-histogram-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

highlight <- function(x, value,val1, subject,col.value, col=NA, ...){
  hst <- hist(x, ...)
  idx <- findInterval(value, hst$breaks)
  idx1 <- findInterval(val1, hst$breaks)
  cols <- rep(col, length(hst$counts))
  cols[idx] <- col.value
  cols[idx1] <- col.value
  hist(x,  main=paste("normal distribution of",subject,sep=" "),col=cols, ...)
}



#creates empty vectors for data of smoke and nonsmoke baby weight
timespent = c()
frequencyspent = c()
x = 1
y = 1

never_played = c()
daily = c()
weekly = c()
monthly = c()
semesterly = c()
not_app=c()

daily = mydata$time[mydata$freq == 1]
weekly = mydata$time[mydata$freq == 2]
monthly =  mydata$time[mydata$freq == 3]
semesterly =  mydata$time[mydata$freq == 4]
not_app = mydata$time[mydata$freq == 99]
timespent = mydata$time
frequencyspent = mydata$freq

count = 0
for(i in not_app){
  if (i>0){
    count = count+1
  }
}

daily_bwt<-data.frame(mass=daily)
weekly_bwt<-data.frame(mass=weekly)
monthly_bwt<-data.frame(mass=monthly)
semesterly_bwt<-data.frame(mass=semesterly)

daily_bwt$freq <- 'daily'
weekly_bwt$freq <- 'weekly'
monthly_bwt$freq <- 'monthly'
semesterly_bwt$freq <- 'semesterly'



times <-rbind(daily_bwt, weekly_bwt, monthly_bwt, semesterly_bwt)
#times = factor(times, levels = levels(mydata$freq)[c("daily","weekly","semesterly","monthly")])
times$freq <- factor(mydata$freq, levels = c("daily","weekly","semesterly","monthly"))
ggplot(mydata, aes(x=freq, y=mass))+geom_boxplot(outline = FALSE, notch=FALSE)+ggtitle("box plot of time spent(hrs) vs.frequency")+labs(y="time spent(hrs)")
#provide summary statisitcs for dataset 
summary(timespent)
summary(frequencyspent)

sample_data=c()
count = 1
for (i in timespent){
  if(i==0){
    sample_data[count] = 0
    count=count+1
  }
  else{
    sample_data[count] =1
    count=count+1
  }
  
}

#CLT Method for Scenario 1
sample_mean = mean(sample_data)
width <- 2*sd(sample_data)/sqrt(length(sample_data))
prop.simple <- c(sample_mean - width, sample_mean+ width)
prop.simple

#Finite Population Correction Method for Scenario 1
width1 <- 1.96 * sqrt(sample_mean*(1-sample_mean)*(314-91)/((91-1)*314))
prop.corrected <- c(sample_mean - width1, sample_mean+ width1)
prop.corrected

#Bootstrap Method for Scenario 1
boot.population <- rep(sample_data, length.out = 314)
length(boot.population)
sample1 <- sample(boot.population, size = 91, replace = FALSE)
#set.seed(189289)
B = 10000 # the number of bootstrap samples we want
boot.sample <- array(dim = c(B, 91))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = 91, replace = FALSE)
}
boot.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)
head(boot.mean)
sum(boot.mean)/10000
int.boot <- c(quantile(boot.mean, 0.025), quantile(boot.mean, 0.975))
int.boot

hist(timespent, density = 20, breaks = 30, freq = TRUE,ylab = "count", xlab = "hrs/week", main="histogram of time spent for video games")
hist(frequencyspent, density = 20, breaks = 15, freq = TRUE,xaxt="n",ylab = "count", xlab = "1=daily, 2=weekly, 3=monthly, 4=semsterly", main="histogram of frequency of video games")
axis(1, at=seq(1.0,4.0,by=1))

sample_mean3 = mean(timespent)
sample_mean3
#Scenario 3 CLT Method
qqnorm(timespent,main = "Normal Q-Q plot for timespent")
qqline(timespent)
width3 <- 2*sd(timespent)/sqrt(length(timespent))
prop.simple3 <- c(sample_mean3 - width3, sample_mean3+ width3)
prop.simple3
#Scenario 3 Finite Population Correction Method
width2 <- 1.96 * (sd(timespent)/sqrt(91))*sqrt((314-91)/314)
prop.simple2 <- c(sample_mean3 - width2, sample_mean3+ width2)
prop.simple2
#Scenario 3 Bootstrap
#timespent = remove_outliers(timespent)
#timespent <- timespent[!is.na(timespent)]
boot.population <- rep(timespent, length.out = 314)
length(boot.population)
sample1 <- sample(boot.population, size = 91, replace = FALSE)
set.seed(189289)
B = 10000 # the number of bootstrap samples we want
boot.sample <- array(dim = c(B, 91))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = 91, replace = FALSE)
}
boot.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)
kurtosis(boot.mean)
skewness(boot.mean)
hist(boot.mean)
head(boot.mean)

#simulation of normal distribution's kurtosis of 484 sample size 
normal_kurtosis=c()
for(i in 1:1000){
  normal_kurtosis[i] = kurtosis(rnorm(484))
}
#simulation of normal distribution's skewness of 484 sample size 
normal_skewness=c()
for(i in 1:1000){
  normal_skewness[i] = skewness(rnorm(484))
}
highlight(normal_kurtosis, 2.83,1000,"kurtosis","red")
highlight(normal_skewness, 0.333,1000,"skewness","red")

sum(boot.mean)/10000
int.boot <- c(quantile(boot.mean, 0.025), quantile(boot.mean, 0.975))
int.boot

#Scenario 4
install.packages("tree")
library(tree)
mydata['dis_like'] <- rep(NA, dim(mydata)[1])
for(i in 1:dim(mydata)[1]){
  like <- mydata[i, 'like']
  if(like==0 || like==4 || like==5){
    mydata[i, 'dis_like'] = 0
  }else{
    mydata[i, 'dis_like'] = 1
  }
}
data.tree <- tree(dis_like~educ+sex+age+home+math+work+own+cdrom+grade, data=data)
plot(data.tree, type="uniform")
text(data.tree)

