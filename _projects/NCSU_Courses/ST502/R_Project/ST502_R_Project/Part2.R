df_nonsmoker = framingham_data[framingham_data$currentSmoker == 0,]

install.packages("tinytex")

ggplot(framingham_data, mapping = aes(x=`sysBP`)) +
  geom_boxplot(fill="#CC0000") +
  labs(title="Framingham Data Box Plot", x="Systolic Blood Pressure (mmHg)") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


var(df_smoker)
var(df_nonsmoker)
sqrt(352.2117)
sqrt(562.1447)


sqrt(((225-1)*(562.1447))+((75-1)*(352.2117))/(298))
2*(1-pt(3.04,298))
qt(p=0.05/2, df=158)