library(ggplot2)

#What we know
#Two datasets provided--smoker and non-smoker.  Both samples are random, independent and follow a normal distribution N(μ1,σ1^2) and N(μ2,σ2^2).  The variance is unknown but assumed to be equal; thus pooled.  

#Hypothesis Test
#H0: μ1 - μ2 = 0
#H1: μ1 - μ2 ≠ 0

##Data Plots
ggplot(framingham_data_nonsmokers,mapping = aes(`sysBP`)) +
  geom_histogram(binwidth = 1, fill="#CC0000", color="#000000") + 
  labs(title="Framingham Data:  Non-Smoker Systolic Blood Pressure Histogram", y="Frequency", x="Systolic Blood Pressure Observations");

ggplot(framingham_data_smokers,mapping = aes(`sysBP`)) +
  geom_histogram(binwidth = 1, fill="#CC0000", color="#000000") + 
  labs(title="Framingham Data:  Non-Smoker Systolic Blood Pressure Histogram", y="Frequency", x="Systolic Blood Pressure Observations");
#HEX#CC0000 --NCSU Red
#HEX#000000 --black
#HEX#FFFFFF --white


#t-test requires several summary statistics from the samples
#Non-smoker summary statistics
n1 <- nrow(framingham_data_nonsmokers) #Sample size for non-smokers
ns_samplemean <- mean(framingham_data_nonsmokers$sysBP) #Sample mean for non-smokers
ns_variance <- sum((framingham_data_nonsmokers$sysBP-ns_samplemean)^2)*(1/(n1-1)) #Sample variance for non-smokers
ns_stdev <- sqrt(ns_variance)


#Smoker summary statistics
n2 <- nrow(framingham_data_smokers) #Sample size for smokers
s_samplemean <- mean(framingham_data_smokers$sysBP) #Sample mean for smokers
s_variance <- sum((framingham_data_smokers$sysBP-s_samplemean)^2)*(1/(n2-1)) #Sample variance for smokers
#s_stdev <- sqrt(s_variance)

#Common Variance
Sp2 <- ((n1-1)*(ns_variance)+(n2-1)*(s_variance))/(n1+n2-2)

Sp <- sqrt(Sp2)

#Test Statistic
T <- ((ns_samplemean - s_samplemean)/(Sp*sqrt((1/n1)+(1/n2))))

#Critical Value CI
qt(p=0.05/2, df=225+75-2, lower.tail = FALSE)


#2*(1-pt(3.04,298))

###############################
### Built-In R t.test check ###
###############################
t.test(framingham_data_nonsmokers$sysBP, framingham_data_smokers$sysBP, var.equal = TRUE)
