#installing/loading packages
library(readr)
install.packages("ggplot2")
library("ggplot2")
#Getting the data
Indicators<-read.csv(file="C:/Users/ADMIN/Desktop/DSU/Semesters/FALL 2016/INFS 805 Design Research Methods Wang/Final Paper/world-development-indicators/Indicators.csv", header=TRUE, sep=",")

#Exploring data
str(Indicators)
names(Indicators)
head(Indicators)
tail(Indicators)
table(Indicators$Value)

#Creating a dataframe called to store ECOWAS states members data
ECOstates<- c("BEN", "BFA", "CPV", "CIV", "GMB", "GHA", "GIN", "GNB","LBR", "MLI", "NER", "NGA", "SEN", "SLE", "TGO")
View(ECOstates)
ECOIndicators <- subset(Indicators, Indicators$CountryCode %in% ECOstates)
View(ECOIndicators)

#let's now define a data frame for each of our indicator that will be used
#dataframe for GDP per capita

GDP_per_capita <- subset(ECOIndicators, ECOIndicators$IndicatorCode == "NY.GDP.PCAP.CD")
View(GDP_per_capita)
#let's now plot the dataframe for analysis
#plot for GDP_per_capita
ggplot(data = GDP_per_capita, aes(Year, Value)) +
  geom_line(aes(color = CountryCode)) +
  geom_smooth(stat = "summary", fun.y = mean, linetype = 2, size = 2) +
  scale_x_continuous(breaks = seq(1960, 2015, 5)) +
  ggtitle("GDP per capita") +
  ylab("GDP per capita(USD)")

#dataframe for Population density (people per sq. km of land area)

population_density <- subset(ECOIndicators, ECOIndicators$IndicatorCode == "EN.POP.DNST")
head(population_density)
#plot for Population density (people per sq. km of land area)
ggplot(data = population_density, aes(Year, Value)) +
  geom_line(aes(color = CountryCode)) +
  geom_smooth(stat = "summary", fun.y = mean, size = 2) +
  scale_x_continuous(breaks = seq(1960, 2015, 5)) +
  ggtitle("Population density") +
  ylab("Population density (people per sq. km of land area)")

#dataframe for Health expenditure, public (% of government expenditure

health <- subset(ECOIndicators, ECOIndicators$IndicatorCode == "SH.XPD.PCAP") 
View(health)
#plot for Health expenditure, public (% of government expenditure)
ggplot(data =  health, aes(Year, (Value / 1000000))) +
  geom_line(aes(color = CountryCode)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  scale_x_continuous(breaks = seq(1960, 2015, 4)) +
  ggtitle("Health expenditure, public") +
  ylab("Health expenditure, public (% of government expenditure")

#dataframe for Government Expenditure on Education, in % of GDP
education <- subset(ECOIndicators, ECOIndicators$IndicatorCode == "SE.XPD.TOTL.GD.ZS")
View(education)
#plot for Government Expenditure on Education, in % of GDP
ggplot(data =  education, aes(Year, (Value / 1000000))) +
  geom_line(aes(color = CountryCode)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  scale_x_continuous(breaks = seq(1960, 2015, 4)) +
  ggtitle("Government Expenditure on Education, in % of GDP") +
  ylab("Education")
