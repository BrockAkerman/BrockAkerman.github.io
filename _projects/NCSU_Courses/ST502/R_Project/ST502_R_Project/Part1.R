### OPEN WORKSPACE ###
tinytex::install_tinytex()
### Histogram of Smokers
```{r, echo=FALSE, fig.align="center", comment="##Figure 1.1"}
ggplot(framingham_data[framingham_data$currentSmoker == 1,],mapping = aes(`sysBP`)) +
  geom_histogram(binwidth = 2, fill="#CC0000", color="#000000",) + 
  labs(title="Framingham Data:  Smoker Systolic Blood Pressure Histogram", y="Frequency", x="Systolic Blood Pressure Observations")  
```  
### Histogram of Non Smokers
```{r, echo=FALSE, fig.align="center", comment="##Figure 1.2"}
ggplot(framingham_data[framingham_data$currentSmoker == 0,],mapping = aes(`sysBP`)) +
  geom_histogram(binwidth = 2, fill="#CC0000", color="#000000") + 
  labs(title="Framingham Data:  Non-Smoker Systolic Blood Pressure Histogram", y="Frequency", x="Systolic Blood Pressure Observations")
``
### QQ of Smokers
```{r, echo=FALSE, fig.align="center", comment="##Figure 1.3"}
ggplot(framingham_data[framingham_data$currentSmoker == 1,], mapping = aes(sample = framingham_data[framingham_data$currentSmoker == 1,]$sysBP)) +
  geom_qq(size = 2, fill="#CC0000", color="#CC0000") +
  labs(title="QQ-Plot of Systolic Blood Pressure for Smokers", y="Blood Pressure (mmHg)", x="Normal Quantiles")  
```
### QQ of NonSmokers
```{r, echo=FALSE, fig.align="center", comment="##Figure 1.4"}
ggplot(framingham_data[framingham_data$currentSmoker == 0,], mapping = aes(sample = framingham_data[framingham_data$currentSmoker == 0,]$sysBP)) +
  geom_qq(size = 1, fill="#CC0000", color="#CC0000") +
  labs(title="QQ-Plot of Systolic Blood Pressure for Non-Smokers", y="Blood Pressure (mmHg)", x="Normal Quantiles")  
```