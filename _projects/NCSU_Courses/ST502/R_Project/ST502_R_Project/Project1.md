Hypothesis Testing for the difference in means using the p-value and
confidence interval methods of evaluation of a t-test
================
Brock Akerman and Hanan Ali
ST502 - Spring 2022

A sample of data is extracted from the Framingham Heart Study which
includes the systolic blood pressure of participants who are identified
as either smokers or non-smokers. Smokers in this study are defined as
participants who have smoked cigarettes anytime the year preceding the
physical examination while non-smokers are those who have abstained from
smoking during that same period of time. (SOURCE:
<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6541867/>). The data
contains a column of qualitative binary values characterizing the
subjects smoking habit and a second column pairing it with a single
measurement of systolic blood pressure measured in millimeters of
mercury (mmHg).

We are interested in examining the difference in blood pressure means
between smokers and non-smokers. To test whether a difference between
means exists, we will use the t-test (pooled) the Welch-Satterthwaite
t-test. Random sampling and a normal distribution are assumed for each
of the sample; however let us examine some plots to better understand
the data visually and reinforce the normality assumption.  
  
<img src="Project1_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
From the sample of systolic blood pressures we can see a skew toward the
right. There is concern for outliers influencing this skew. Of
particular concern is the systolic blood pressure for the observations
where the subject had a 200 mmHg reading. A pressure observed above
180mmHg is consider a medical emergency and requires urgent care and
hospitalization. A test for outliers is recommended. Should there be
justification for removing the observations above 180mmHg, this
distribution would appear to closely resemble a normal.  
  
<img src="Project1_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />  
  
A generated boxplot of the sampled data shows six outliers which aligns
with analysis of the histogram and the skew. These data points should be
removed since they deviate from the remaining data points; in reality
these are subjects that either should be hospitalized or the testing
methods should be reevaluated in case this is a user generated error by
the clinician administering the test.  
  
  
<img src="Project1_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />
Our Quantile-Quantile plot illustrates a linear arrangement. Both tails
deviate from that line but not so close to the center of the data that I
would be concerned in rejecting the normality assumption. Potential
outliers are present in the upper blood pressure range. The same six
data points observed in the histogram are causing the upper tail of the
distribution to deviate from this line. Positively identifying and
removing outliers in the upper tail will result in a better normality
shape.  
  

# t-test (pooled)

###### **P-Value method**

Are the mean systolic blood pressures different between smokers and
non-smokers? We will use the t-test in our hypothesis test to answer
that question. First, we will consider the case of equal variance. The
status quo is that the mean systolic blood pressures are equal between
smokers and non-smokers. We would like to test whether there is evidence
to support the claim against the status quo–that there does exist a
difference between means.  
  
  
![H\_{0}: \\mu\_{1}-\\mu\_{2} = 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_%7B0%7D%3A%20%5Cmu_%7B1%7D-%5Cmu_%7B2%7D%20%3D%200 "H_{0}: \mu_{1}-\mu_{2} = 0"),
There is no difference between means of smokers and non-smokers.  
![H\_{0}: \\mu\_{1}-\\mu\_{2} ≠ 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_%7B0%7D%3A%20%5Cmu_%7B1%7D-%5Cmu_%7B2%7D%20%E2%89%A0%200 "H_{0}: \mu_{1}-\mu_{2} ≠ 0"),
There is a difference between means of smokers and non-smokers.  
  
  

``` r
Y1 = framingham_data[currentSmoker==0,2] #Groups non-smokers
Y2 = framingham_data[currentSmoker==1,2] #Groups smokers

Y1 = unlist(framingham_data[currentSmoker==0,2])
barY1 = mean(Y1) #R code for computing systolic blood pressure mean for non-smokers

Y2 = unlist(framingham_data[currentSmoker==1,2])
barY2 = mean(Y2) #R code for computing systolic blood pressure mean for smokers
```

  
  
![\\bar{X}\_{1} = \\frac{1}{225}\\sum(x\_{1},x\_{2},...,x\_{225}) = 137.22444](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbar%7BX%7D_%7B1%7D%20%3D%20%5Cfrac%7B1%7D%7B225%7D%5Csum%28x_%7B1%7D%2Cx_%7B2%7D%2C...%2Cx_%7B225%7D%29%20%3D%20137.22444 "\bar{X}_{1} = \frac{1}{225}\sum(x_{1},x_{2},...,x_{225}) = 137.22444")  
![\\bar{X}\_{2} = \\frac{1}{75}\\sum(x\_{1},x\_{2},...,x\_{75}) = 128.06667](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbar%7BX%7D_%7B2%7D%20%3D%20%5Cfrac%7B1%7D%7B75%7D%5Csum%28x_%7B1%7D%2Cx_%7B2%7D%2C...%2Cx_%7B75%7D%29%20%3D%20128.06667 "\bar{X}_{2} = \frac{1}{75}\sum(x_{1},x_{2},...,x_{75}) = 128.06667")  
  
  

``` r
S1 = sd(Y1) #computing systolic blood pressure standard deviation for non smokers
S2 = sd(Y2) #computing systolic blood pressure standard deviation for smokers

n1= length(Y1) #number of non smokers
n2= length(Y2) #number of smokers
```

  
  
![s^2\_{1} = \\frac{1}{225^2}\\sum^{225}\_{i=1}(x\_{i}-\\bar{x\_{1}}) = 352.2117](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;s%5E2_%7B1%7D%20%3D%20%5Cfrac%7B1%7D%7B225%5E2%7D%5Csum%5E%7B225%7D_%7Bi%3D1%7D%28x_%7Bi%7D-%5Cbar%7Bx_%7B1%7D%7D%29%20%3D%20352.2117 "s^2_{1} = \frac{1}{225^2}\sum^{225}_{i=1}(x_{i}-\bar{x_{1}}) = 352.2117")  
  
![\\sqrt(s^2\_{1}) = 18.76730](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csqrt%28s%5E2_%7B1%7D%29%20%3D%2018.76730 "\sqrt(s^2_{1}) = 18.76730")  
  
![s^2\_{2} = \\frac{1}{75^2}\\sum^{75}\_{j=1}(x\_{j}-\\bar{x\_{2}}) = 562.1447](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;s%5E2_%7B2%7D%20%3D%20%5Cfrac%7B1%7D%7B75%5E2%7D%5Csum%5E%7B75%7D_%7Bj%3D1%7D%28x_%7Bj%7D-%5Cbar%7Bx_%7B2%7D%7D%29%20%3D%20562.1447 "s^2_{2} = \frac{1}{75^2}\sum^{75}_{j=1}(x_{j}-\bar{x_{2}}) = 562.1447")  
  
![\\sqrt(s^2\_{2}) = 23.70959](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csqrt%28s%5E2_%7B2%7D%29%20%3D%2023.70959 "\sqrt(s^2_{2}) = 23.70959")  
  
  

``` r
Sp2 = ((n1-1)*S1^2 + (n2-1)*S2^2)/(n1+n2-2)
```

![S\_{p} = \\frac{(n\_{1}-1)S^2\_{1}+(n\_{2}-1)S^2\_{2}}{n\_{1}+n\_{2}-2} = \\frac{(225-1)352.2117+(75-1)562.1447}{225+75-2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;S_%7Bp%7D%20%3D%20%5Cfrac%7B%28n_%7B1%7D-1%29S%5E2_%7B1%7D%2B%28n_%7B2%7D-1%29S%5E2_%7B2%7D%7D%7Bn_%7B1%7D%2Bn_%7B2%7D-2%7D%20%3D%20%5Cfrac%7B%28225-1%29352.2117%2B%2875-1%29562.1447%7D%7B225%2B75-2%7D "S_{p} = \frac{(n_{1}-1)S^2_{1}+(n_{2}-1)S^2_{2}}{n_{1}+n_{2}-2} = \frac{(225-1)352.2117+(75-1)562.1447}{225+75-2}")

``` r
T1 = (barY1-barY2)/(sqrt(Sp2)*sqrt(1/n1 + 1/n2))
```

\#p-value 2\*(1-pt(3.04,298)) qt(0.025,298)

    We reject H0.  There is sufficient evidence to support the claim that mean systolic blood pressure between smokers and non-smokers differs at a significant level of 0.05.  

    ###### Confidence Interval method

    ```r
    #Computing confidence interval 
    (barY1 - barY2) - (qt(0.025,298))*(sqrt(Sp2)*sqrt(1/n1 + 1/n2))

    ## [1] 15.08355

``` r
#0.025 = alpha/2 and df=298
(barY1 - barY2) + (qt(0.025,298))*(sqrt(Sp2)*sqrt(1/n1 + 1/n2))
```

    ## [1] 3.232003

### t-test (Welch-Satterthwaite)

###### P-Value method

For the Welch-Satterthwaite t-test we will test using the same
hypothesis as in the t-test pooled; however, with the assumption about
the variance removed, the test statistic formula.

H0:μ1-μ2=0, There is no difference between means of smokers and
non-smokers.  
H1:μ1-μ2≠0, There is a difference between means of smokers and
non-smokers.

``` satterthwaite
<INSERT CODE CHUNK OF RESULTS FROM THE SATTERTHWAITE T-TEST>
```

We reject H0. There is sufficient evidence to support the claim that
mean systolic blood pressure between smokers and non-smokers differs at
a significant level of 0.05.

###### Confidence Interval method

### comparison of tests

Both test rejected the null hypothesis.

### concluding remarks
