#Importing the data into R session
library(readxl)
framingham_data <- read_excel("Desktop/framingham_data.xls")
attach(framingham_data)
names(framingham_data)
class(currentSmoker)
class(sysBP)

#Two Sample t-test where equal variance is assumed(pooled)
Y1 = framingham_data[currentSmoker==0,2] #computing systolic blood pressure mean for non smokers

Y2=framingham_data[currentSmoker==1,2] #computing systolic blood pressure mean for smokers

Y1 = unlist(framingham_data[currentSmoker==0,2])
barY1 = mean(Y1)

Y2 = unlist(framingham_data[currentSmoker==1,2])
barY2 = mean(Y2)

S1 = sd(Y1) #computing systolic blood pressure standard deviation for non smokers
S2 = sd(Y2) #computing systolic blood pressure standard deviation for smokers

n1= length(Y1) #number of non smokers
n2= length(Y2) #number of smokers

Sp2 = ((n1-1)*S1^2 + (n2-1)*S2^2)/(n1+n2-2)

T1 = (barY1-barY2)/(sqrt(Sp2)*sqrt(1/n1 + 1/n2))

#p-value
2*(1-pt(3.04,298))
qt(0.025,298)

#Computing confidence interval 
(barY1 - barY2) - (qt(0.025,298))*(sqrt(Sp2)*sqrt(1/n1 + 1/n2))
#0.025 = alpha/2 and df=298
(barY1 - barY2) + (qt(0.025,298))*(sqrt(Sp2)*sqrt(1/n1 + 1/n2))







#Two Sample t-test where unequal variance is assumed
T2 = (barY1-barY2)/(sqrt(S1^2/n1+S2^2/n2)) #test statistic

v = floor((S1^2/n1+S2^2/n2)^2/(((S1^2/n1)^2/(n1-1))+(S2^2/n2)^2/(n2-1))) #computing df (using floor function to round down)

#p-value
2*(1-pt(T2,v))

#Computing confidence interval 
###########
library(ggplot2)
ggplot(framingham_data,mapping = aes(`sysBP`)) +
  geom_histogram(binwidth = 5, fill="#CC0000", color="#000000") + 
  labs(title="Framingham Data: Systolic Blood Pressure Histogram", y="Frequency", x="Systolic Blood Pressure Observations");

#The hypothesis is Ho:u1 = u2 vs Ha:u1 is not equal to u2

#Conducting Welch Two Sample t-test where equal variance is assumed(pooled)
t.test(sysBP[currentSmoker=="0"], sysBP[currentSmoker=="1"], alt="two.sided", var.eq=T, paired=F)

#Conducting Welch Two Sample t-test where unequal variances are assumed(Satterwhite approximation)
t.test(sysBP[currentSmoker=="0"], sysBP[currentSmoker=="1"], alt="two.sided", var.eq=F, paired=F)



#Created boxplot to get a feel for how the values in the data are spread out
boxplot(sysBP~currentSmoker, data=framingham_data) #what is wrong with this visualization

# #Creating qqplot to check normal assumption
# #Look at Week 13 code!
# qqplot(currentSmoker, sysBP)
# qqnorm(currentSmoker)
# qqnorm(sysBP)
# help("qqplot")



#Using f-test to see which test I prefer
var.test(currentSmoker, sysBP, alternative = "two.sided")
#since p is less than 0.05, then reject null. which means i prefer two sample t-test with unequal variances