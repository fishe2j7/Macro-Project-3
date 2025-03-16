install.packages(c("dplyr", "tidyverse", "plm"))
library(dplyr)
library(tidyverse)
library(plm)



library(readr)
FirmData <- read_csv("~/MSAPEC- 1st Semester/ECON 8015/df6v9cruddgldrpr.csv")

FirmDatapd <- pdata.frame(FirmData, index = c("gvkey", "datadate"))

FirmDatapd$Assets = FirmDatapd$atq

#atq = Assets
#dlttq = long term debt
#niq = Net income
#revtq = Revenue
#teqq = equity
#capxy = capital expenditures

FirmDatapd$datadate <- as.Date(FirmDatapd$datadate)
FirmDatapd <- FirmDatapd %>% mutate(year = as.numeric(format(FirmDatapd$datadate, "%Y")))

FirmDatapd$ROA = (FirmDatapd$niq/FirmDatapd$atq) 

data_2007 <- FirmDatapd %>% filter(FirmDatapd$year == 2007)
data_2008 <- FirmDatapd %>% filter(FirmDatapd$year == 2008)
data_2009 <- FirmDatapd %>% filter(FirmDatapd$year == 2009)

median(na.omit(data_2007$revtq))
median(na.omit(data_2008$revtq))
median(na.omit(data_2009$revtq))

median(na.omit(data_2007$capxy))
median(na.omit(data_2008$capxy))
median(na.omit(data_2009$capxy))

data_2007$gvkey <- as.character(data_2007$gvkey)
data_2007 <- data_2007[data_2007$gvkey != "061650", ]
mean(na.omit(data_2007$ROA))
mean(na.omit(data_2008$ROA))
mean(na.omit(data_2009$ROA))

median(na.omit(data_2007$atq))
median(na.omit(data_2008$atq))
median(na.omit(data_2009$atq))

FirmDatapd$gvkey <- as.character(FirmDatapd$gvkey)
FirmDatapd1 = FirmDatapd[FirmDatapd$gvkey == "3",]
FirmDatapd2 = FirmDatapd[FirmDatapd$gvkey == "4",]

ggplot(FirmDatapd1, aes(x = datadate, y = revtq)) +
  geom_line(na.rm = TRUE) + 
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(title = "Kate Spade & Co", x = "Quarter", y = "Revenue")

ggplot(FirmDatapd2, aes(x = datadate, y = revtq)) +
  geom_line(na.rm = TRUE) + 
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(title = "P & F Industries", x = "Quarter", y = "Revenue")




