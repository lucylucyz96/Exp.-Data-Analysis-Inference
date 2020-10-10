# Group Project 1
# Ruyin Zhang, Jiaci Zhang, Justin Chou, Aaron Liu, June Young Suk, Conner Leonard
# April 16, 2018
# This file takes input from babies' birthweight data and outputs various graphs 
# to assist in prividing evidence for our argument and analysis of the data. 
# Graphs include density graph, histogram, normal curve, qq plot, normal simulation
# of kurtosis and skewness, etc. 

library(ggplot2)
library(moments)
library(outliers)
#reads in data from local drive
mydata = read.table("/Users/ruyinzhang/Desktop/myData.txt", header = TRUE)


#creates empty vectors for data of smoke and nonsmoke baby weight
nonsmoke = c()
smoke = c()
x = 1
y = 1

#for loop to go through all data and extract based on whether mother smoked 
for(i in 1:1236)
{
  if(mydata[i,7] == 0)
  {
    nonsmoke[x] = mydata[i,1]
    x = x + 1
  }
  if(mydata[i,7] == 1)
  {
    smoke[y] = mydata[i,1]
    y = y + 1
  }
}

smoke_bwt<-data.frame(mass=smoke) #creates dataframe based on smoke vector
nonsmoke_bwt<-data.frame(mass=nonsmoke) #creates dataframe based on nonsmoke vector
nonsmoke=rm.outlier(nonsmoke) #removes outlier
#outputs the skewness value of birthweight data of smoke and nonsmoke
skewness(smoke)
skewness(nonsmoke)

#outputs the kurtosis value of birthweight data of smoke and nonsmoke
kurtosis(smoke)
kurtosis(nonsmoke)

#extracts all numerical data from two separate dataframes
smoke_bwt$smoke <- 'smoke'
nonsmoke_bwt$smoke <- 'nonsmoke'

#bind two dataframe together 
smokes<-rbind(smoke_bwt,nonsmoke_bwt) 

#qq plots for nonsmoke and smoke, respectively
qqnorm(smoke,main = "Smoke")
qqline(smoke)
qqnorm(nonsmoke,main = "Non-smoke")
qqline(nonsmoke)

#combined boxplot and density plot 
ggplot(smokes, aes(x=smoke, y=mass))+geom_boxplot(outlier.color = "red",outlier.shape = 8,outlier.size=4,notch=TRUE)+ggtitle("box plot of birth weight related to smoking")+labs(y="birth weight")
ggplot(smokes, aes(mass, fill=smoke))+geom_density(alpha = 0.2)+ggtitle("density plot of birth weight related to smoking")+labs(y="density", x="birth weight")

#density plot with threshold changed to illustrate the significance of changing thresholds for 
#data of smoked and nonsmoked mother's baby birthweight, respectively
d<-density(smoke)
plot(d,main="birth weight(smokers)",xlab="child weight")
x1 <- min(which(d$x <= 88))
x2 <- max(which(d$x <= 88))
x3 <- min(which(d$x <= 96))
x4 <- max(which(d$x <= 96))
x9 <- min(which(d$x <= 80))
x10 <- max(which(d$x <= 80))
with(d,polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="red"))
with(d, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray"))
with(d, polygon(x=c(x[c(x9,x9:x10,x10)]), y= c(0, y[x9:x10], 0), col="blue"))


d1<-density(nonsmoke)
plot(d1,main="birth weight(non-smokers)")
x5 <- min(which(d1$x <= 88))
x6 <- max(which(d1$x <= 88))
x7 <- min(which(d1$x <= 96))
x8 <- max(which(d1$x <= 96))
x11 <- min(which(d1$x <= 80))
x12 <- max(which(d1$x <= 80))
with(d1,polygon(x=c(x[c(x7,x7:x8,x8)]), y= c(0, y[x7:x8], 0), col="red"))
with(d1, polygon(x=c(x[c(x5,x5:x6,x6)]), y= c(0, y[x5:x6], 0), col="gray"))
with(d1, polygon(x=c(x[c(x11,x11:x12,x12)]), y= c(0, y[x11:x12], 0), col="blue"))

#combined histogram of the data
ggplot(smokes, aes(mass,fill = smoke)) + geom_histogram(alpha=0.5,xlab = "birth weight")+ggtitle("histogram of birth weight related to smoking")+labs(y="count",x="birth weight")

#histogram of only smoked data with normal curve for comparison 
m = mean(smoke)
std<-sqrt(var(smoke))
hist(smoke, density = 20, breaks = 20, prob=TRUE, xlab = "count", ylab = "birth weight",main="birth weight(smokers)")
curve(dnorm(x, mean = m, sd=std), col="darkblue",lwd=2,add=TRUE,yaxt="n")


#histogram of only non-smoked data with normal curve for comparison 
m1= mean(nonsmoke)
std1<-sqrt(var(nonsmoke))
hist(nonsmoke, density = 20, breaks = 20, prob=TRUE, xlab = "count", main="birth weight(nonsmokers)")
curve(dnorm(x, mean = m1, sd=std1), col="darkblue",lwd=2,add=TRUE,yaxt="n")

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

#highlight's the data's kurtosis of smoked and nonsmoked, respectively, on the graph
highlight(normal_kurtosis, 2.98,4.04,"kurtosis","red")
highlight(normal_skewness, -0.18,-0.0336,"skewness","red")


