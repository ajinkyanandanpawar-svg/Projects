################################################################################
#                                                                              #
# How concentrated is the banking system in Z ?                                #
#                                                                              #
################################################################################

library(fixest)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)

file = "~/Downloads/bank.csv"
bank = fread(file)
View(bank)

# biggest banks looking at the total assets per year########

bank <- bank %>%
  group_by(Year) %>%
  arrange(desc(TotalAssets), .by_group = TRUE) 

bank <- bank[order(-bank$Year), ]

#sum of total assets in banking sector each year
bank <- bank %>%
  group_by(Year) %>%                    
  mutate(TotalAssetsPerYear = sum(TotalAssets)) %>%  
  ungroup()

#sum total assets of top 5 banks each year
bank <- bank %>%
  group_by(Year) %>%                              
  arrange(desc(TotalAssets), .by_group = TRUE) %>% 
  mutate(Top5TotalAssets = sum(head(TotalAssets, 5))) %>% 
  ungroup()

#c5 ratio each year
bank <- bank %>%
  group_by(Year) %>%                              
  mutate(C5_ratio = (Top5TotalAssets / TotalAssetsPerYear) * 100
  ) 

#graph
ggplot(bank, aes(x = Year, y = C5_ratio)) +
  geom_line(color = "#1B4F72", size = 1.2) +
  scale_y_continuous(limits = c(0, 100)) +     
  labs(
    title = "Concentration in the Banking Sector (C5 ratio)",
    x = "Year",
    y = "C5 ratio (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),              
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

#graph closer look
ggplot(bank, aes(x = Year, y = C5_ratio)) +
  geom_line(color = "#1B4F72", size = 1.2) +   
  labs(
    title = "Concentration in the Banking Sector (C5 ratio)",
    x = "Year",
    y = "C5 ratio (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),             
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

################################################################################
#                                                                              #
# Which Macrofinancial Factors Determine the NIM of Bank 29?                   #
#                                                                              #
################################################################################

library(readxl)
library(dplyr)
library(fixest)

bank  <- 'D:\\fdda-data\\AB25_GroupAssigment_BankData.xlsx'
macro <- 'D:\\fdda-data\\AB25_GroupAssigment_MacroData.xlsx'

bank  <- read_excel(bank)
macro <- read_excel(macro)

bank29 <- bank %>% filter(Bankid == 29)
data   <- merge(bank29, macro, by = "Year")

model <- feols(NIM ~ Growth + Unemployment + Inflation + Spread + 
                 ShortRate + ExchangeRate + StockMarketIndex, data = data)

etable(model, signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.10), se = "hc1")

#Exluding Stock Market Index from the model
model.1 <- feols(NIM ~ Growth + Unemployment + Inflation + Spread + 
                 ShortRate + ExchangeRate, data = data)

etable(model.1, signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.10), se = "hc1")

#Excluding Exchange Rate (from model 1)
model.2 <- feols(NIM ~ Growth + Unemployment + Inflation + Spread + 
                   ShortRate, data = data)

etable(model.2, signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.10), se = "hc1")

#Excluding Unemployment (from model 2)
model.3 <- feols(NIM ~ Growth + Inflation + Spread + ShortRate, data = data)

etable(model.3, signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.10), se = "hc1")

#Excluding Growth and Inflation (from model 3)
model.4 <- feols(NIM ~ Spread + ShortRate, data = data)

etable(model.4, signif.code = c('***' = 0.01, '**' = 0.05, '*' = 0.10), se = "hc1")

# NIM over time
plot(data$Year, data$NIM, type = "b", pch = 16, 
     xlab = "Year", ylab = "NIM", main = "NIM Over Time (Bank 29)")

#ShortRate over time
plot(data$Year, data$ShortRate, type = "b", pch = 16, 
     xlab = "Year", ylab = "Short Rate", 
     main = "Short-Term Interest Rate Over Time")

# NIM & ShortRate
plot(data$ShortRate, data$NIM, pch = 16, 
     xlab = "Short Rate", ylab = "NIM", main = "NIM vs Short Rate")
abline(lm(NIM ~ ShortRate, data = data))

#Spread Over Time
plot(data$Year, data$Spread, type = "b", pch = 16, 
     xlab = "Year", ylab = "Spread", main = "Term Spread Over Time")

#NIM vs Spread
plot(data$Spread, data$NIM, pch = 16, 
     xlab = "Spread", ylab = "NIM", main = "NIM vs Spread")
abline(lm(NIM ~ Spread, data = data))

################################################################################
#                                                                              #
# Stress Test Scenario for Bank 29.                                            #
#                                                                              #
################################################################################


#Stress test
model_full <- lm(
  NIM ~ Growth+ Spread +
    ShortRate,
  data = data
)

# Baseline: last observed year (40)
base40 <- subset(data, Year == 40)

# Predicted NIM in the baseline year from the full model
base40$NIM_pred <- predict(model_full, newdata = base40)
base40[, c("Year", "NIM", "NIM_pred")]

## Year 41 scenario
stress_41 <- base40
stress_41$Year          <- 41
stress_41$Growth        <- -4.0
stress_41$Unemployment  <- base40$Unemployment + 5
stress_41$Inflation     <- 0.0
stress_41$ShortRate     <- 0.0
stress_41$Spread        <- 0.0
stress_41$ExchangeRate  <- base40$ExchangeRate * 1.10   # 10% weaker
stress_41$StockMarketIndex <- base40$StockMarketIndex * 0.60  # 40% drop

## Year 42 scenario
stress_42 <- base40
stress_42$Year          <- 42
stress_42$Growth        <- -1.5
stress_42$Unemployment  <- base40$Unemployment + 6
stress_42$Inflation     <- -0.5
stress_42$ShortRate     <- 0.0
stress_42$Spread        <- 0.0
stress_42$ExchangeRate  <- base40$ExchangeRate * 1.10   # still weak
stress_42$StockMarketIndex <- base40$StockMarketIndex * 0.60  # stays low

# Combine stress years
stress <- rbind(stress_41, stress_42)

# Predicted NIM for stress years using ALL variables
stress$NIM_pred <- predict(model_full, newdata = stress)

# Put baseline and stress together in one table
results <- rbind(
  data.frame(
    Year     = base40$Year,
    Scenario = "Baseline (year 40)",
    NIM_pred = base40$NIM_pred
  ),
  data.frame(
    Year     = stress$Year,
    Scenario = "Severe recession",
    NIM_pred = stress$NIM_pred
  )
)

# Change vs baseline
baseline_nim <- base40$NIM_pred[1]
results$NIM_change_vs_40 <- results$NIM_pred - baseline_nim

results

