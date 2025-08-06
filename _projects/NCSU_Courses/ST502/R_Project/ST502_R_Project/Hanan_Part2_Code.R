#  Part 2
#135 combinations cause 3(n1)*3(n2)*5(mu2)*3(s1)
library(ggplot2)
library(readr)
library(qqplotr)
library(readr)
library(janitor)

N1 <- c(10,30,70)
N2 <- c(10,30,70)
# MU1 is always same cause Mu1 is 5 so I set different values of Mu2 to get this to work: Ho:u1-u2=0 and Ha:u1-u2 is not equal to 0
MU2 <- c(5, 10, 6, 4, 0)
S1 <- c(1, 4, 9)
B = 100 #replicating 100 times  
alpha = 0.05
result1 = matrix(0, nr=135, nc=B)  #setting matrix to record p-values for equal variance where there are 135 cases so 135 rows and 100 replications
result2 = matrix(0, nr=135, nc=B)  #setting matrix to record p-values for unequal variance where there are 135 cases so 135 rows and 100 replications
#Sp_2 = ((N1-1)*S1^2 + (N2-1)*S2^2)/(N1+N2-2) #equal variance
#T_1 = (mu1-MU2)/(sqrt(Sp_2)*sqrt(1/N1 + 1/N2)) #equal variance test statistic
#T_2 =    #unequal variance test statistic


#Here is I'm working on a way to have the for loop call this function para and compute all the p-values for each combination on it's own
para = matrix(0, nr=135, nc=4)  #4 parameters (N1, N2, Mu2, S1)
# 1 : N1; 2: N2 ; 3: S1; 4: MU2;
para[1:5,1]=10; para[1:5,2] =10; para[1:5,3]=1; para[1:5,4]=MU2  #TA said there are 135 rows but this is just one row. Need to make the rest of rows (3x3x3) which would be how the rest of the combinations look like. For this combination, you are calculating type1error cause u1=u2 so data is generated from Ho  
#output analysis: one you print above, that'll output first 5 rows. 1st row corresponds to type1error, cause u1=5 and u2=5 so u1-u2=0, where n1=10, n2=10, s1=1 and u2=5
#the second row calculates power cause difference of means is -5 and this is the case where again n1=10, n2=10, s1=1 and u2=10. The remaining 3 rows also compute the power when n1=10, n2=10, s1=1
para[6:10,1]=10; para[6:10,2] =30; para[6:10,3]=1; para[6:10,4]=MU2
#output using para[6:10,] #I will write 27-2=25 more lines of para code, cause the fourth row = mu2 so it'll generate 5 rows on its own to calculate error, and these lines of code will cover the combinations of n1, n2 and s1
para[11:15,1]=10; para[11:15,2] =70; para[11:15,3]=1; para[11:15,4]=MU2
para[16:20,1]=10; para[16:20,2] =10; para[16:20,3]=4; para[16:20,4]=MU2
para[21:25,1]=10; para[21:25,2] =30; para[21:25,3]=4; para[21:25,4]=MU2
para[26:30,1]=10; para[26:30,2] =70; para[26:30,3]=4; para[26:30,4]=MU2
para[31:35,1]=10; para[31:35,2] =10; para[31:35,3]=9; para[31:35,4]=MU2
para[36:40,1]=10; para[36:40,2] =30; para[36:40,3]=9; para[36:40,4]=MU2
para[41:45,1]=10; para[41:45,2] =70; para[41:45,3]=9; para[41:45,4]=MU2

para[46:50,1]=30; para[46:50,2] =10; para[46:50,3]=1; para[46:50,4]=MU2  
para[51:55,1]=30; para[51:55,2] =30; para[51:55,3]=1; para[51:55,4]=MU2
para[56:60,1]=30; para[56:60,2] =70; para[56:60,3]=1; para[56:60,4]=MU2
para[61:65,1]=30; para[61:65,2] =10; para[61:65,3]=4; para[61:65,4]=MU2
para[66:70,1]=30; para[66:70,2] =30; para[66:70,3]=4; para[66:70,4]=MU2
para[71:75,1]=30; para[71:75,2] =70; para[71:75,3]=4; para[71:75,4]=MU2
para[76:80,1]=30; para[76:80,2] =10; para[76:80,3]=9; para[76:80,4]=MU2
para[81:85,1]=30; para[81:85,2] =30; para[81:85,3]=9; para[81:85,4]=MU2
para[86:90,1]=30; para[86:90,2] =70; para[86:90,3]=9; para[86:90,4]=MU2

para[91:95,1]=70; para[91:95,2] =10; para[91:95,3]=1; para[91:95,4]=MU2  
para[96:100,1]=70; para[96:100,2] =30; para[96:100,3]=1; para[96:100,4]=MU2
para[101:105,1]=70; para[101:105,2] =70; para[101:105,3]=1; para[101:105,4]=MU2
para[106:110,1]=70; para[106:110,2] =10; para[106:110,3]=4; para[106:110,4]=MU2
para[111:115,1]=70; para[111:115,2] =30; para[111:115,3]=4; para[111:115,4]=MU2
para[116:120,1]=70; para[116:120,2] =70; para[116:120,3]=4; para[116:120,4]=MU2
para[121:125,1]=70; para[121:125,2] =10; para[121:125,3]=9; para[121:125,4]=MU2
para[126:130,1]=70; para[126:130,2] =30; para[126:130,3]=9; para[126:130,4]=MU2
para[131:135,1]=70; para[131:135,2] =70; para[131:135,3]=9; para[131:135,4]=MU2


for(i in 1:135){                            #for loop to go through each case
  sigma1 = para[i,3]; sigma2 = 1            #n1 is 1st column of para matrix, n2 is 2nd column of para matrix, sigma1 is 3rd column of para matrix and mu1 is 4th column of para matrix
  n1 = para[i,1]; n2 = para[i,2]            #sigma2 will always equal 1 and mu1 will always equal 5
  mu1 = 5; mu2 = para[i,4]
  
  for(b in 1:B){
    Y1 = rnorm(n1, mu1, sqrt(sigma1))
    Y2 = rnorm(n2, mu2, sqrt(sigma2))
    meanY1 = mean(Y1)
    meanY2 = mean(Y2)
    Sp_2 = ((n1-1)*sigma1^2 + (n2-1)*sigma2^2)/(n1+n2-2) #equal variance
    T_1 = abs(meanY1-meanY2)/(sqrt(Sp_2)*sqrt(1/n1 + 1/n2)) #equal variance test statistic (mean has to come from true value not sample data)
    df_1 = n1 + n2 - 2
    pvalue1 = 2*(1-pt(T_1, df_1)) #equal variance p-value
    
    Y1mean <- mean(Y1)
    Y1sd <- sd(Y1)
    Y1_lb <- Y1mean - 2*(Y1sd)
    Y1_ub <- Y1mean + 2*(Y1sd)
    
    Y2mean <- mean(Y2)
    Y2sd <- sd(Y2)
    Y2_lb <- Y2mean - 2*(Y2sd)
    Y2_ub <- Y2mean + 2*(Y2sd)
    
    
    T_2 = abs(meanY1-meanY2)/(sqrt((sigma1^2/n1)+(sigma2^2/n2))) #unequal variance t-test statistic
    df_2 = ((sigma1^2/n1)+(sigma2^2/n2)^2)/((((sigma1^2/n1)^2)/(n1-1) + (((sigma2^2/n2)^2)/(n2-1))))         
    pvalue2 = 2*(1-pt(T_2, df_2)) #unequal variance p-value
    
    if(pvalue1 <= alpha){  # checking if p-value for equal variance test is less than or equal to alpha 
      result1[i, b] = 1    # recording number of rejections where ith row corresponds to ith situation/combination/scenario
    } else{                # if p-value is greater than alpha
      result1[i, b] = 0    # recording number of fail to rejections 
    }
    if(pvalue2 <= alpha){  # testing if p-value for unequal variance test is less than or equal to alpha
      result2[i, b] = 1    # recording number of rejections
    } else{                # if p-value is greater than alpha
      result2[i, b] = 0    # recording number of fail to rejections
    }
  }
  
}

100*135
length(which(result1 == 1))/length(result1)


Type1error_1 <- rowMeans(result1)  #this is type1error for the equal variance test and is the mean/proportion of results recorded of 100 samples 
Type1error_2 <- rowMeans(result2)  #this is type1error for the unequal variance test and is the mean/proportion of results recorded of 100 samples 

result1




#when you calculate power, data is generated from alternative hypothesis like if u1=5 and u2=100 then difference is -5, so the means on lines 85 and 86 are power now.
# but when you have for instance, u1=5 and u2==5 then this data is generated from null hypothesis and the means on lines 85 and 86 are calculating type1error

#9 plots (3n1)*(3n2)*(3s1)

data1 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_line(mapping=aes(Type1error_1[1:5]), color="red") + #where n1=10, n2=10, s1=1
  geom_line(mapping=aes(Type1error_1[6:10], color="blue")) + #where n1=10, n2=30, s1=1
  geom_line(mapping=aes(Type1error_1[11:15], color="purple")) + #where n1=10, n2=70, s1=1
  ylim(c(0,1)) + 
  xlim(c(0,1)) +
  xlab(expression(difference_of_means))


data2 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_line(mapping=aes(Type1error_1[16:20]), color="red") + #where n1=10, n2=10, s1=4
  geom_line(mapping=aes(Type1error_1[21:25], color="blue")) + #where n1=10, n2=30, s1=4
  geom_line(mapping=aes(Type1error_1[26:30], color="purple")) + #where n1=10, n2=70, s1=4
  ylim(c(0,1)) + 
  xlim(c(0,1)) +
  xlab(expression(difference_of_means))

data3 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_1[31:35]), color="red") + #where n1=10, n2=10, s1=9
  geom_point(mapping=aes(Type1error_1[36:40], color="blue")) + #where n1=10, n2=30, s1=9
  geom_point(mapping=aes(Type1error_1[41:45], color="purple")) + #where n1=10, n2=70, s1=9
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data4 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_line(mapping=aes(Type1error_1[46:50]), color="red") + #where n1=30, n2=10, s1=1
  geom_line(mapping=aes(Type1error_1[51:55], color="blue")) + #where n1=30, n2=30, s1=1
  geom_line(mapping=aes(Type1error_1[56:60], color="purple")) + #where n1=30, n2=70, s1=1
  ylim(c(0,1)) + 
  xlim(c(0,1)) +
  xlab(expression(difference_of_means))

data5 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_line(mapping=aes(Type1error_1[61:65]), color="red") + #where n1=30, n2=10, s1=4
  geom_line(mapping=aes(Type1error_1[66:70], color="blue")) + #where n1=30, n2=30, s1=4
  geom_line(mapping=aes(Type1error_1[71:75], color="purple")) + #where n1=30, n2=70, s1=4
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data6 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_1[76:80]), color="red") + #where n1=30, n2=10, s1=9
  geom_point(mapping=aes(Type1error_1[81:85], color="blue")) + #where n1=30, n2=30, s1=9
  geom_point(mapping=aes(Type1error_1[86:90], color="purple")) + #where n1=30, n2=70, s1=9
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data7 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_line(mapping=aes(Type1error_1[91:95]), color="red") + #where n1=70, n2=10, s1=1
  geom_line(mapping=aes(Type1error_1[96:100], color="blue")) + #where n1=70, n2=30, s1=1
  geom_line(mapping=aes(Type1error_1[101:105], color="purple")) + #where n1=70, n2=70, s1=1
  ylim(c(0,1)) + 
  xlim(c(0,1)) +
  xlab(expression(difference_of_means))

data8 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_1[106:110]), color="red") + #where n1=70, n2=10, s1=4
  geom_point(mapping=aes(Type1error_1[111:115], color="blue")) + #where n1=70, n2=30, s1=4
  geom_point(mapping=aes(Type1error_1[116:120], color="purple")) + #where n1=70, n2=70, s1=4
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data9 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_1[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_1[121:125]), color="red") + #where n1=70, n2=10, s1=9
  geom_point(mapping=aes(Type1error_1[126:130], color="blue")) + #where n1=70, n2=30, s1=9
  geom_point(mapping=aes(Type1error_1[131:135], color="purple")) + #where n1=70, n2=70, s1=9
  ylim(c(0,1)) + 
  xlim(c(-0.5 ,1)) +
  xlab(expression(difference_of_means))

data1 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[1:5]), color="red") + #where n1=10, n2=10, s1=1
  geom_point(mapping=aes(Type1error_2[6:10], color="blue")) + #where n1=10, n2=30, s1=1
  geom_point(mapping=aes(Type1error_2[11:15], color="purple")) + #where n1=10, n2=70, s1=1
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data2 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[16:20]), color="red") + #where n1=10, n2=10, s1=4
  geom_point(mapping=aes(Type1error_2[21:25], color="blue")) + #where n1=10, n2=30, s1=4
  geom_point(mapping=aes(Type1error_2[26:30], color="purple")) + #where n1=10, n2=70, s1=4
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data3 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[31:35]), color="red") + #where n1=10, n2=10, s1=9
  geom_point(mapping=aes(Type1error_2[36:40], color="blue")) + #where n1=10, n2=30, s1=9
  geom_point(mapping=aes(Type1error_2[41:45], color="purple")) + #where n1=10, n2=70, s1=9
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data4 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[46:50]), color="red") + #where n1=30, n2=10, s1=1
  geom_point(mapping=aes(Type1error_2[51:55], color="blue")) + #where n1=30, n2=30, s1=1
  geom_point(mapping=aes(Type1error_2[56:60], color="purple")) + #where n1=30, n2=70, s1=1
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data5 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[61:65]), color="red") + #where n1=30, n2=10, s1=4
  geom_point(mapping=aes(Type1error_2[66:70], color="blue")) + #where n1=30, n2=30, s1=4
  geom_point(mapping=aes(Type1error_2[71:75], color="purple")) + #where n1=30, n2=70, s1=4
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data6 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[76:80]), color="red") + #where n1=30, n2=10, s1=9
  geom_point(mapping=aes(Type1error_2[81:85], color="blue")) + #where n1=30, n2=30, s1=9
  geom_point(mapping=aes(Type1error_2[86:90], color="purple")) + #where n1=30, n2=70, s1=9
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data7 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[91:95]), color="red") + #where n1=70, n2=10, s1=1
  geom_point(mapping=aes(Type1error_2[96:100], color="blue")) + #where n1=70, n2=30, s1=1
  geom_point(mapping=aes(Type1error_2[101:105], color="purple")) + #where n1=70, n2=70, s1=1
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data8 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[106:110]), color="red") + #where n1=70, n2=10, s1=4
  geom_point(mapping=aes(Type1error_2[111:115], color="blue")) + #where n1=70, n2=30, s1=4
  geom_point(mapping=aes(Type1error_2[116:120], color="purple")) + #where n1=70, n2=70, s1=4
  ylim(c(0,1)) + 
  xlim(c(-1,1)) +
  xlab(expression(difference_of_means))

data9 <- data.frame(difference_of_means=c(0, -5, -1, 1, 5), power=c(Type1error_2[1:5]))
ggplot(data=data1, mapping=aes(x=difference_of_means, y=power)) + 
  geom_point(mapping=aes(Type1error_2[121:125]), color="red") + #where n1=70, n2=10, s1=9
  geom_point(mapping=aes(Type1error_2[126:130], color="blue")) + #where n1=70, n2=30, s1=9
  geom_point(mapping=aes(Type1error_2[131:135], color="purple")) + #where n1=70, n2=70, s1=9
  ylim(c(0,1)) + 
  xlim(c(-0.5 ,1)) +
  xlab(expression(difference_of_means))
