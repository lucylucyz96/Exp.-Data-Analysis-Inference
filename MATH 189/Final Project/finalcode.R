# Final Project
# Ruyin Zhang, Jiaci Zhang, Conner Leonard, Nichole Reyes, Brandon Aquino, Jasmine Barrera
# June 3, 2018

library(ggplot2)
library(dplyr)
library(ggrepel)
library(outliers)
install.packages("devtools")
devtools::install_github("slowkow/ggrepel")


#reads in data from local drive
donors = read.csv("/Users/ruyinzhang/Desktop/donors_california_sample.csv", header = TRUE)
donations = read.csv("/Users/ruyinzhang/Desktop/Donations.csv",header = TRUE)
length(col(donations))
test<-donations[donations$Donor.ID == "863cdfb30cdcc876c10a1f963c496d2b", ]
total<-merge(donors,donations,by = "Donor.ID")
total_single <-  total %>% group_by(Donor.ID) %>% filter(n()==1)
zip_code_single = total_single$Donor.Zip
table(zip_code_single)
zip_code_single = zip_code_single[zip_code_single>=900 & zip_code_single<= 961]
barplot(table(zip_code_single),main = "Single Donor Zip in CA", xlab = "zip code", ylab = "amount of donors")
zip_data_single = as.data.frame(table(zip_code_single))

total <-  total %>% group_by(Donor.ID) %>% filter(n()>1)
write.csv(total, "/Users/ruyinzhang/Desktop/Donors and Donation.csv")
zip_code = total$Donor.Zip
sum(zip_code == '943')
table(zip_code)
zip_code = zip_code[zip_code>=900 & zip_code<= 961]
zip_data = as.data.frame(table(zip_code))
barplot(table(zip_code),main = "Recurring Donor Zip in CA", xlab = "zip code", ylab = "amount of donors")

zip_data$zip_code
income = read.csv("/Users/ruyinzhang/Desktop/CA\ income.csv",header = TRUE)
total<-merge(total,income,by = "Donor.Zip")
total_single<-merge(total_single,income,by = "Donor.Zip")


income = total$median.income
income_df = as.data.frame(table(income))
income_df = income_df[income_df$income != 'N/A', ]
income_df$income = gsub("\\$", '', income_df$income)
income_df$income = gsub("\\,", '', income_df$income)
income_df$income<-as.numeric((income_df$income))
income_df$income = as.numeric(as.character(income_df$income))
income_df = income_df[order((income_df$income)), ]
income_df$recurring = 'Yes'

income_single = total_single$median.income
income_df_s = as.data.frame(table(income_single))
income_df_s = income_df_s[income_df_s$income != 'N/A', ]
colnames(income_df_s)[colnames(income_df_s)=="income_single"]<-"income"
income_df_s$income = gsub("\\$", '', income_df_s$income)
income_df_s$income = gsub("\\,", '', income_df_s$income)
income_df_s$income<-as.numeric((income_df_s$income))
income_df_s$income = as.numeric(as.character(income_df_s$income))
income_df_s = income_df_s[order((income_df_s$income)), ]
income_df_s$recurring = 'No'
income_df = rbind(income_df, income_df_s)
write.csv(income_df, "/Users/ruyinzhang/Desktop/Income Freq Data.csv")
ggplot(income_df, aes(income,Freq,fill = recurring))+geom_bar(stat = "identity",position = "dodge",width = 1000)+scale_fill_brewer(palette="Set1")+ theme(axis.text.x =element_text(size=8.5,angle = 90, hjust = 1),axis.text.y = element_text(size=5))+ggtitle("bar plot of recurring vs.single donor income")+labs(y="Amount of Donors",x="Donor income")





plot(myfreq)
random_points = c()
random_points <- sample(900:961, 42173, replace=T)
table(random_points)
barplot(table(random_points),main = "Uniform distribution samples", xlab = "zip code", ylab = "amount of donors",ylim = c(0,2000))
qqplot(x=zip_code,y = random_points,main = "Uniform Q-Q plot for zip code distribution")
abline(c(0,1))
E_i = 42173/57
chi_2 <- sum((zip_data$Freq - E_i)^2/E_i)
chi_2
chi2_compare <- qchisq(p=0.95,df=56)
chi2_compare
p_value <- 1-pchisq(chi_2,df=56)
p_value

zip_data_single$recurring <-'No'
colnames(zip_data_single)[colnames(zip_data_single)=="zip_code_single"]<-"zip_code"
zip_data$recurring<-'Yes'
zip_data_total = rbind(zip_data_single, zip_data)
ggplot(zip_data_total, aes(zip_code,Freq,fill = recurring))+geom_bar(stat = "identity",position = "dodge")+scale_fill_brewer(palette="Set1")+ theme(axis.text.x =element_text(size=8.5,angle = 90, hjust = 1),axis.text.y = element_text(size=5))+ggtitle("bar plot of recurring vs.single donor zip")+labs(y="Amount of Donors",x="Donor zip")

table(total$Donation.Included.Optional.Donation)
table(total_single$Donation.Included.Optional.Donation)
table(total$Donor.Is.Teacher)
table(total_single$Donor.Is.Teacher)
table(total$Donor.City)
donor.city = as.data.frame(table(total$Donor.City))
donor.city= donor.city[donor.city$Freq != 0, ]
ggplot(data=donor.city, aes(x=Var1, y=Freq)) +geom_bar(stat="identity",position = "dodge")+theme(axis.text.x =element_text(size=5.5,angle = 90, hjust = 1),axis.text.y = element_text(size=5))

zip_code_1 = total[total$Donor.Zip==940, ]
zip_code_1 = zip_code_1$Donation.Amount
zip_code_1<-as.numeric(as.character(zip_code_1))
zip_code_1=rm.outlier(zip_code_1)
zip_code_1
zip_code_1 = round(zip_code_1,digits = 0)
zip_code_1 <- zip_code_1[!is.na(zip_code_1)]
zip_code_1 = as.data.frame(zip_code_1)
zip_code_1$from = '940'
zip_code_2 = total[total$Donor.Zip==900, ]
zip_code_2 = zip_code_2$Donation.Amount
zip_code_2<-as.numeric(as.character(zip_code_2))
zip_code_2
zip_code_2 = round(zip_code_2,digits = 0)
zip_code_2 <- zip_code_2[!is.na(zip_code_2)]
zip_code_2 = as.data.frame(zip_code_2)
zip_code_2$from = '900'
#zip_code_1 = as.data.frame(table(zip_code_1))
zip_code_2$zip_code_2<-as.numeric(as.character(zip_code_2$zip_code_2))
max(zip_code_1)
age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs)
}
zip_code_1 = table(age.cat(zip_code_1$zip_code_1, upper = 8000))
zip_code_1 = as.data.frame(zip_code_1)
zip_code_1$zip_code_1 
write.csv(zip_code_1, "/Users/ruyinzhang/Desktop/zip_code_1.csv")
zip_code_1 = zip_code_1[!(zip_code_1$Freq) ==0,] 
colnames(zip_code_1)[colnames(zip_code_1)=="zip_code_1"]<-"amount"
colnames(zip_code_2)[colnames(zip_code_2)=="zip_code_2"]<-"amount"
zip_codes <-rbind(zip_code_1,zip_code_2)
ggplot(zip_codes, aes(x=from, y=amount))+geom_boxplot(outlier.color = "red",outlier.shape = 8,outlier.size=4,notch=TRUE)+ggtitle("donation amount for 900 and 940")+labs(y="donation amount")+coord_cartesian(ylim = c(0, 500))


                                                                                                       