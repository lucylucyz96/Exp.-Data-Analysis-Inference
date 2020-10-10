# Group Project 3
# Ruyin Zhang, Jiaci Zhang, Justin Chou, Aaron Liu, June Young Suk, Conner Leonard
# May 3, 2018
# 
library(ggplot2)
library(moments)

#reads in data from local drive
mydata = read.table("/Users/ruyinzhang/Desktop/mydata3.txt", header = TRUE)

#Part I
#Random Scatter
random_points = c()
random_points <- sample(1:229354, 296, replace=F)
random_points_df = data.frame(mass = random_points)
hist(random_points, breaks = 48, probability = TRUE, col = 3, main= "Uniform distribution samples", xaxt='n')
axis(side=1, at=seq(4000,229354, 5000),labels= seq(4000,229354,by=5000))
title(cex.lab=0.75)
lines(density(random_points, adjust = 2), col = 2)
hist(random_points, breaks = 46, freq = TRUE, col = 3, main= "Locations of 296 random Hits in Intervals of 4778", xaxt='n',xlab = 'Location on hits',ylab = 'Number of hits in the interval',ylim=c(0,15))
axis(side=1, at=seq(4000,229354, 5000),labels= seq(4000,229354,by=5000))
title(cex.lab=0.75)

#Data Plot
data_points = c()
data_points=mydata$location
data_point_df = data.frame(mass = data_points)
data_point_df$from <- "data"
h<-hist(data_points, breaks = 50, freq = TRUE, col = 3, main = "Locations of Palidromes in consecutive Intervals of 5000 base pairs", xlab = "Location on chain of DNA base pairs",ylab = "Number of palindromes in the Interval",ylim = c(0,16))
axis(side=1, at=seq(4000,229354, 5000),labels= seq(4000,229354,by=5000))
title(cex.lab=0.75)
cuts <- cut(h$breaks, c(88000,100000,182000,199000))
plot(h,col=c("green","white","red","white")[cuts],main = "Locations of Palidromes in Intervals of 4778 base pairs", xlab = "Location on chain of DNA base pairs",ylab = "Number of palindromes in the Interval",ylim = c(0,20),xaxt='n')
axis(side=1, at=seq(5000,229354, 4778),labels= seq(5000,229354,by=4778))
title(cex.lab=0.50)

hist(data_points, breaks = 48, probability = TRUE, col = 3, main = "Locations of Palidromes in Intervals of 4778 base pairs", xlab = "Location on chain of DNA base pairs",ylab = "Number of palindromes in the Interval")
lines(density(data_points, adjust = 2),col = 2)

qqplot(x=data_points,y = random_points,main = "Uniform Q-Q plot for palindrome distribution")
#qqline(random_points)
abline(c(0,1))
#legend(x = 229354, y = 20, legend = c("h", "h1"), lty = c(1,1), col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
#chi-square for uniform distribution

tab <- table(cut(data_points, breaks = seq(1, 229354, length.out = 49), include.lowest = TRUE))
head(tab, 10)
counts <- as.vector(tab)
E_i <- 296/48
chi_2 <- sum((counts - E_i)^2/E_i)
chi_2
chi2_compare <- qchisq(p=0.95,df=47)
chi2_compare
p_value <- 1-pchisq(chi_2,df=47)
p_value

#For PART 3
Pois <- rpois(296, lambda = mean(counts))
Pois_frame <- data.frame(mass=Pois)
counts_frame <- data.frame(mass=counts)
Pois_frame$from <- 'Poisson'
counts_frame$from <- 'Data'
points <- rbind(Pois_frame,counts_frame)
Pois <- rpois(296, lambda = mean(counts))
Pois_frame <- data.frame(mass=Pois)
counts_frame <- data.frame(mass=counts)
Pois_frame$from <- 'Poisson'
counts_frame$from <- 'Data'
points <- rbind(Pois_frame,counts_frame)
ggplot(points, aes(mass,fill=from)) + 
  geom_histogram(alpha = 0.5, breaks = seq(0, max(points$mass), by=1),aes(y = ..density..), position = 'identity')+
  stat_density(aes(group=from, color=from), position = "identity", geom="line")+
  xlim(0, max(points$mass))
hist(Pois, breaks = seq(min(Pois), max(Pois), by=1), probability = TRUE, col = 3, main= "Poisson distribution samples")
lines(density(Pois, adjust=2), col = rgb(0,0,1,0.5))
hist(counts,probability=TRUE,main = "Histogram of the number of counts per 48 intervals",xlab = "number of counts inside an interval")
lines(density(Pois, adjust = 2), col = rgb(0,0,1,0.5))
qqplot(x=counts,y = Pois,main = "Poisson Q-Q plot for counts of palindrome distribution")
abline(c(0,1))


bins=c()
bins[1] = 48*dpois(1,6.17)+48*dpois(2,6.17)+48*dpois(3,6.17)

bins[2] = 48*dpois(4,6.58)

bins[3] = 48*dpois(5,6.58)

bins[4] = 48*dpois(6,6.58)

bins[5] = 48*dpois(7,6.58)

bins[6] = 48*dpois(8,6.58)

bins[7] = 48-bin1-bin2-bin3-bin4-bin5-bin6
bins

observed = c()
observed[1]=sum(counts<4)
observed[2]=sum(counts==4)
observed[3]=sum(counts==5)
observed[4]=sum(counts==6)
observed[5]=sum(counts==7)
observed[6]=sum(counts==8)
observed[7]=sum(counts>=9)
sum(counts)

chi_square=sum((observed-bins)^2/bins)
chi_square

chi2_compare <- qchisq(p=0.95,df=5)
chi2_compare

p_value <- 1-pchisq(chi_square,df=5)
p_value

#exponential
data_exp = diff(data_points)
lambda = 1/mean(data_exp)
probs = seq(from = 0, to = 1, by = 1/48)
probs
percentage_quantiles2 = qexp(probs, rate =lambda)
bins2 = cut(data_exp, percentage_quantiles2)
table(bins2)
bins2 = c(15,10,9,9,8,6,10,3,4,7,3,8,4,10,2,7,7,1,4,4,6,4,5,9,3,6,6,4,3,2,6,5,2,6,6,6,10,11,7,3,6,12,5,9,4,3,6,9)


expected_d = rep(295*(1/48), 48)
expected_d
chi_square = sum((bins2-expected_d)^2/expected_d)
chi_square

chi2_compare <- qchisq(p=0.95,df=46)
chi2_compare
p_value <- 1-pchisq(chi_square,df=46)
p_value

