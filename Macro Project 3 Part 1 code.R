#Reading the data file
data<-read.csv("Project 3 data.csv")
source(file="intord.R")
datac<-na.omit(data)

#Need to make time series data set
dfts = ts(datac,frequency=4,start=c(2008,1))

# running intial stationarity tests
USRGDP<-dfts[,2]
plot(USRGDP)
intord(USRGDP)
dUSRGDP<-diff(USRGDP)
intord(dUSRGDP)
#I(1)

USUN<-dfts[,3]
plot(USUN)
intord(USUN)
adf_test_USUN_numeric <- ur.df(USUN_numeric, type = "trend")  
summary(adf_test_USUN_numeric)
dUSUN<-diff(USUN)
intord(dUSUN)
#I(1) 

US10y<-dfts[,4]
plot(US10y)
intord(US10y)
dUS10y<-diff(US10y)
intord(dUS10y)
#I(1)

USdis<-dfts[,5]
plot(USdis)
intord(USdis)
dUSdis<-diff(USdis)
intord(dUSdis)
ddUSdis<-diff(dUSdis)
intord(ddUSdis)
#I(2)

GerGDP<-dfts[,6]
plot(GerGDP)
intord(GerGDP)
dGerGDP<-diff(GerGDP)
intord(dGerGDP)
#I(1)

GerUN<-dfts[,7]
plot(GerUN)
intord(GerUN)
dGerUN<-diff(GerUN)
intord(dGerUN)
#I(1)

GerIR<-dfts[,8]
plot(GerIR)
intord(GerIR)
dGerIR<-diff(GerIR)
intord(dGerIR)
ddGerIR<-diff(dGerIR)
intord(ddGerIR)
#I(2)

GerDis<-dfts[,9]
plot(GerDis)
intord(GerDis)
adf_test_GerDis_numeric <- ur.df(GerDis_numeric, type = "trend")  
summary(adf_test_GerDis_numeric)
dGerdis<-diff(GerDis)
intord(dGerdis)
ddGerdis<-diff(dGerdis)
intord(ddGerdis)
#I(2) diff dGerdis and will be good

SAGDP<-dfts[,10]
plot(SAGDP)
intord(SAGDP)
dSAGDP<-diff(SAGDP)
intord(dSAGDP)
#I(1)

SAUN<-dfts[,11]
plot(SAUN)
intord(SAUN)
dSAUN<-diff(SAUN)
intord(dSAUN)
ddSAUN<-diff(dSAUN)
intord(ddSAUN)
#I(2)

SA3M<-dfts[,12]
plot(SA3M)
intord(SA3M_numeric)
dSA3M<-diff(SA3M)
intord(dSA3M)
#I(1)

SAret<-dfts[,13]
plot(SAret)
SAret_log <- log(SAret)
intord(SAret_log)
adf_test_SAret_numeric <- ur.df(GerDis_numeric, type = "trend")
summary(adf_test_SAret_numeric)
dSAret_log<-diff(SAret_log)
intord(dSAret_log)
#Trend stationary I(1)


#Plotting correlations 
library(ggplot2)
library(tidyr)

#Correlation
library(corrplot)

# Create correlation matrix
library(corrplot)

# Create correlation matrix
library(corrplot)

# Create correlation matrix
library(ggplot2)
library(reshape2)

# Create correlation matrix
df <- data.frame(dUSUN, dUSRGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between US real GDP and US unemployment rate",
       x = "", y = "")

df <- data.frame(dUS10y, dUSRGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between US real GDP and US 10 year treasury rate",
       x = "", y = "")

dUSRGDP<- dUSRGDP[1:length(ddUSdis)]
df <- data.frame(ddUSdis, dUSRGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between US real GDP and US disposable income",
       x = "", y = "")

df <- data.frame(dGerUN, dGerGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between German real GDP and German Unemployment",
       x = "", y = "")

dGerGDP<- dGerGDP[1:length(ddGerIR)]
df <- data.frame(ddGerIR, dGerGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between German real GDP and German Interest Rate",
       x = "", y = "")

dGerGDP<- dGerGDP[1:length(ddGerdis)]
df <- data.frame(ddGerdis, dGerGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between German real GDP and Household income",
       x = "", y = "")

dSAGDP <- dSAGDP[1:length(ddSAUN)]

df <- data.frame(ddSAUN, dSAGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between South African real GDP and South African Unemployment",
       x = "", y = "")

dSAGDP <- dSAGDP[1:length(dSA3M)]

df <- data.frame(dSA3M, dSAGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between South African real GDP and South African 3 Month Treasury rate",
       x = "", y = "")

dSAGDP <- dSAGDP[1:length(dSA3M)]

df <- data.frame(dSAret_log, dSAGDP)
cor_matrix <- cor(df, use = "complete.obs")

# Convert to long format for ggplot
cor_melt <- melt(cor_matrix)

# Heatmap using ggplot2
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 5) +
  theme_minimal() +
  labs(title = "Correlation between South African real GDP and South African Retail sales",
       x = "", y = "")

#Plotting GDP comparison
plot(dUSRGDP)
library(ggplot2)

USGDP<-dfts[,2]
lUSGDP<-log(USGDP)
USGDP_growth<-diff(lUSGDP)*100
GerGDP<-dfts[,6]
lGerGDP<-log(GerGDP)
GerGDP_growth<-diff(lGerGDP)*100
SAGDP<-dfts[,10]
lSAGDP<-log(SAGDP)
SAGDP_growth<-diff(lSAGDP)*100
library(ggplot2)

library(ggplot2)
library(reshape2)

# Create a proper quarterly time index
Year <- seq(2008, by = 0.25, length.out = length(USGDP_growth))

# Build the data frame with USA, Germany, and South Africa GDP growth
df_US <- data.frame(
  Year = Year, 
  USA = as.numeric(USGDP_growth),
  Germany = as.numeric(GerGDP_growth),
  South_Africa = as.numeric(SAGDP_growth)
)

# Convert to long format for ggplot2
df_long <- melt(df_US, id.vars = "Year")

# Plot the time series
ggplot(df_long, aes(x = Year, y = value, color = variable)) +
  geom_line(size = 1) +  # Line plot for all three variables
  theme_minimal() +
  labs(title = "Comparison of % change in real GDP",
       y = "GDP Growth Rate (%)",
       x = "Year",
       color = "Country") +  # Legend label
  scale_x_continuous(
    breaks = seq(2008, 2023, by = 2),  # Show every 2 years
    labels = seq(2008, 2023, by = 2)   # Ensure proper labeling
  ) +
  scale_y_continuous(labels = scales::comma)  # Remove scientific notation

# Combine results into a data frame
sd_comparison <- data.frame(
  Country = c("USA", "Germany", "South Africa"),
  Standard_Deviation = c(sd_US, sd_Germany, sd_SA)
)

# Print the table
print(sd_comparison)

#Creating new dataset with transformed variables
min_length <- min(
  length(dUSRGDP), length(dGerGDP), length(dSAGDP), length(dUSUN),
  length(dGerUN), length(dSAUN), length(dUS10y), length(dGerIR),
  length(dSA3M), length(dUSdis), length(dGerdis)
)

# Trim all variables to the same length
dUSRGDP <- dUSRGDP[1:min_length]
dGerGDP <- dGerGDP[1:min_length]
dSAGDP <- dSAGDP[1:min_length]
dUSUN <- dUSUN[1:min_length]
dGerUN <- dGerUN[1:min_length]
dSAUN <- dSAUN[1:min_length]
dUS10y <- dUS10y[1:min_length]
dGerIR <- dGerIR[1:min_length]
dSA3M <- dSA3M[1:min_length]
dUSdis <- dUSdis[1:min_length]
dGerdis <- dGerdis[1:min_length]

# Adjust Year to match the new length
Year <- seq(2008, by = 0.25, length.out = min_length)

stationary_data <- data.frame(
  Year = Year,
  dUSRGDP = dUSRGDP,
  dGerGDP = dGerGDP,
  dSAGDP = dSAGDP,
  dUSUN = dUSUN,
  dGerUN = dGerUN,
  dSAUN = dSAUN,
  dUS10y = dUS10y,
  dGerIR = dGerIR,
  dSA3M = dSA3M,
  dUSdis = dUSdis,
  dGerdis = dGerdis
)

# Print first few rows to check
head(stationary_data)

# Compute standard deviations for all stationary variables
sd_values <- sapply(stationary_data[,-1], sd, na.rm = TRUE)  # Exclude 'Year' column

# Convert to a data frame
sd_table <- data.frame(
  Variable = names(sd_values),
  Standard_Deviation = round(sd_values, 3)  # Round for readability
)

# Print the table
print(sd_table)

library(ggplot2)

ggplot(sd_table, aes(x = reorder(Variable, -Standard_Deviation), y = Standard_Deviation, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  labs(title = "Standard Deviations of Stationary Variables",
       y = "Standard Deviation",
       x = "Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate labels for readability
  theme(legend.position = "none")  # Remove redundant legend















