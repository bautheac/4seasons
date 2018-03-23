options(error = recover, java.parameters = "-Xmx12g", digits = 5L)
library(readxl); library(plyr); library(dplyr); library(broom); library(magrittr); library(RQuantLib); library(tidyquant); library(lubridate)

source("TDA - Functions.r")

## Ticker data
file.v <- "data/Tickers.xlsx"; tickers.tb <- read_excel(file.v, sheet = "Tickers", skip = 0L)
# saveRDS(tickers.tb, file = "data/Tickers.Rds")


## CFTC data
file.v <- "data/CFTC data.xlsm"; period.v <- "1997/2017"
cftc_data.tb <- cftc_data.fun(file.v, period.v, tickers.tb)


## Market data
file.v <- "data/Market data.xlsm"
dates.v <- (cftc_data.tb %>% filter(Active_ticker %in% (tickers.tb %>% filter(Asset_class == "Commodity", Country == "US", CFTC == TRUE))$Active_ticker))$Date %>% unique %>% sort
futures.tb <- futures_data.fun(file.v, dates.v, tickers.tb)

# saveRDS(futures.tb, file = "data/Futures.Rds")


## Factors - Asset_pool: US commodities

# futures.tb <- readRDS(file = "data/Futures.Rds")
# tickers.tb <- readRDS(tickers.tb, file = "data/Tickers.Rds")
ranking_period.v <- 26L; proportion.v <- 1L/3L
num_leg_positions.v <- ((tickers.tb %>% filter(Market == TRUE, Asset_class == "Commodity", Country == "US"))$Active_ticker %>% length * proportion.v) %>% ceiling
data.tb <- futures.tb %>% filter(Active_ticker %in% (tickers.tb %>% filter(Market == TRUE, Asset_class == "Commodity", Country == "US"))$Active_ticker)

factors.tb <- factors.fun(data.tb, ranking_period.v, num_leg_positions.v) %>% mutate(Asset_pool = "US commodities") %>% select(Asset_pool, everything())

# saveRDS(factors.tb, file = "data/Factors.Rds")






# futures.tb <- readRDS(file = "data/Futures.Rds"); tickers.tb <- readRDS(file = "data/Tickers.Rds")
aggregate_CHP.tb <- aggregate_CHP.fun(futures.tb, tickers.tb) %>% filter(!is.na(CHP)) %>% filter(Asset_pool == "US-Commodity") %>% 
  mutate(`State variable` = CHP) %>% select(Date, `State variable`)



futures_rets.tb <- futures.tb %>% left_join(tickers.tb %>% select(Active_ticker, Market, Asset_class, Country), by = "Active_ticker") %>% 
  filter(Market == TRUE, Asset_class == "Commodity") %>% group_by(Country, Date) %>% summarise(Return = mean(Front_return, na.rm = TRUE)) %>%
  mutate(Name = ifelse(Country == "UK", paste0("EW - ", Country, "-Metals"), paste0("EW - ", Country, "-Commodities"))) %>% ungroup %>% select(-Country) %>% 
  select(Name, everything())

factor_rets.tb <- factors.tb %>% filter(Name %in% c("CHP", "Momentum")) %>% select(Name, Date, Return.long, Return.short) %>% group_by(Name, Date) %>%
  filter(row_number() == 1L) %>% ungroup %>% mutate(Long = Return.long, Short = Return.short) %>% select(Name, Date, Long, Short) %>%
  gather(Leg, Return, -c(Name, Date)) %>% mutate(Name = paste0(Name, " factor - ", Leg)) %>% select(Name, Date, Return)

data.tb <- rbind(futures_rets.tb, factor_rets.tb) %>% spread(key = Name, value = Return) %>% full_join(aggregate_CHP.tb, by = "Date")

SP.tb <- futures.tb %>% filter(Active_ticker == "SPA Comdty") %>% mutate("SP500-Return" = Front_return, "SP500-CHP" = CHP) %>% 
  select(Date, `SP500-Return`, `SP500-CHP`)

data.tb %>% full_join(SP.tb, by = "Date") %>% 
  select(Date, `CHP factor - Long`, `CHP factor - Short`, `Momentum factor - Long`, `Momentum factor - Short`, 
         `EW - UK-Metals`, `EW - US-Commodities`, `State variable`, `SP500-Return`, `SP500-CHP`) %>% 
  write.csv(file = "TDA.csv")

