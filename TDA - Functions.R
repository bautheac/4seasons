

cftc_data.fun <- function(file.v, period.v, tickers.tb){
  
  cftc_data.tb <- lapply((tickers.tb %>% filter(CFTC == TRUE))$Active_ticker, function(x){
    message(paste0("\nLoading ", x, "."))
    data.tb <- read_excel(file.v, sheet = x, skip = 1L) %>% 
      filter(complete.cases(.)) %>% 
      mutate(CHP = PX_LAST / (PX_LAST + PX_LAST__1), Date = Date %>% 
               as.Date %>% 
               advance(dates = ., calendar = (tickers.tb %>% filter(Active_ticker == x))$Calendar, n = 3L, timeUnit = 0L), Active_ticker = x) %>% 
      select(Active_ticker, Date, CHP) %>%
      filter(Date %>% format(., "%Y") >= (strsplit(period.v, split = "/") %>% unlist)[1L], Date %>% format(., "%Y") <= (strsplit(period.v, split = "/") %>% unlist)[2L])
    message(paste0("\nDone loading ", x, "!")); data.tb
  }) %>% do.call(args = ., what = "rbind")
  message(paste0("\nReconciliating gasoline COT time series: switch from unleaded gasoline (HUA Comdty) to RBOB gasoline (XBA Comdty) on 2006-09-01."))
  data.tb <- rbind(cftc_data.tb, rbind(cftc_data.tb %>% filter(Active_ticker == "HUA Comdty", Date < "2006-09-01" %>% as.Date),
                            cftc_data.tb %>% filter(Active_ticker == "XBA Comdty", Date >= "2006-09-01" %>% as.Date)) %>% 
          mutate(Active_ticker = "XBWA Comdty")) %>% filter(!Active_ticker %in% c("HUA Comdty", "XBA Comdty"))
  message(paste0("\nDone !")); data.tb
}


futures_data.fun <- function(file.v, dates.v, tickers.tb, periods.v){
  
  mkt_data.tb <- lapply((tickers.tb %>% filter(Market == TRUE))$Active_ticker, function(x){
    
    message(paste0("\nLoading ", x, "."))
    
    data.tb <- read_excel(file.v, sheet = x, skip = 1L) %>% select(Date, PX_LAST, OPEN_INT, FUT_AGGTE_OPEN_INT, FUT_AGGTE_VOL, Date__1, PX_LAST__1) %>% slice(2L:n()) %>%
      mutate(Date = Date %>% as.Date, PX_LAST = PX_LAST %>% as.numeric, OPEN_INT = OPEN_INT %>% as.numeric, FUT_AGGTE_OPEN_INT = FUT_AGGTE_OPEN_INT %>% as.numeric,
             FUT_AGGTE_VOL = FUT_AGGTE_VOL %>% as.numeric, Date__1 = Date__1 %>% as.Date, PX_LAST__1 = PX_LAST__1 %>% as.numeric)
    
    returns.tb <- data.tb %>% select(Date, PX_LAST) %>% filter(Date %in% dates.v) %>%
      mutate(Front_return = (PX_LAST %>% as.numeric) / lag((PX_LAST %>% as.numeric), n = 1L) - 1L) %>% select(Date, Front_return)
    OI_growth.tb <- data.tb %>% select(Date, FUT_AGGTE_OPEN_INT) %>% filter(Date %in% dates.v) %>%
      mutate(OI_growth = (FUT_AGGTE_OPEN_INT %>% as.numeric) / lag((FUT_AGGTE_OPEN_INT %>% as.numeric), n = 1L) - 1L) %>% select(Date, OI_growth)
    RY.tb <- full_join(data.tb %>% select(Date, PX_LAST), data.tb %>% select(Date__1, PX_LAST__1) %>% mutate(Date = Date__1) %>% select(Date, PX_LAST__1), by = "Date") %>%
      mutate(Roll_yield = (PX_LAST %>% as.numeric) / (PX_LAST__1 %>% as.numeric) - 1L) %>% select(Date, Roll_yield) %>% filter(Date %in% dates.v)
    
    data.tb <- full_join(full_join(returns.tb, OI_growth.tb, by = "Date"), RY.tb, by = "Date") %>% mutate(Active_ticker = x) %>%
      select(Active_ticker, Date, Front_return, OI_growth, Roll_yield) %>%
      filter(Date %>% format(., "%Y") >= (strsplit(period.v, split = "/") %>% unlist)[1L], Date %>% format(., "%Y") <= (strsplit(period.v, split = "/") %>% unlist)[2L])
    
    message(paste0("\nDone loading ", x, "!")); data.tb
    
  }) %>% do.call(args = ., what = "rbind")
  
  data.tb <- mkt_data.tb %>% left_join(cftc_data.tb %>% filter(Date %in% dates.v), by = c("Active_ticker", "Date")) %>% arrange(Active_ticker, Date) %>% ungroup

  message(paste0("\nDone!")); data.tb
}


factors.fun <- function(data.tb, ranking_period.v, num_leg_positions.v){
  options(warn = -1L)
  
  sort_by.v <- "ascend"; name.v <- "Momentum"; message(paste0("\nBuilding ", name.v %>% tolower, " factor."))
  momentum_factor.tb <- factor.fun(data.tb, ranking_variable.v = Front_return, ranking_period.v, num_leg_positions.v, sort_by.v, name.v)
  message(paste0("\nDone!"))
  sort_by.v <- "ascend"; name.v <- "Term structure"; message(paste0("\nBuilding ", name.v %>% tolower, " factor."))
  TS_factor.tb <- factor.fun(data.tb, ranking_variable.v = Roll_yield, ranking_period.v, num_leg_positions.v, sort_by.v, name.v)
  message(paste0("\nDone!"))
  sort_by.v <- "ascend"; name.v <- "Open interest"; message(paste0("\nBuilding ", name.v %>% tolower, " factor."))
  OI_factor.tb <- factor.fun(data.tb, ranking_variable.v = OI_growth, ranking_period.v, num_leg_positions.v, sort_by.v, name.v)
  message(paste0("\nDone!"))
  sort_by.v <- "descend"; name.v <- "CHP"; message(paste0("\nBuilding ", name.v %>% tolower, " factor."))
  CHP_factor.tb <- factor.fun(data.tb, ranking_variable.v = CHP, ranking_period.v, num_leg_positions.v, sort_by.v, name.v)
  message(paste0("\nDone!"))
  name.v <- "Market"; message(paste0("\nBuilding ", name.v %>% tolower, " factor."))
  market_factor.tb <- mrkt_factor.fun(data.tb, name.v)
  message(paste0("\nDone!"))

  factors.tb <- list(momentum_factor.tb, TS_factor.tb, OI_factor.tb, CHP_factor.tb, market_factor.tb) %>% do.call(what = "rbind", args = .)
  options(warn = 0L); factors.tb
}





factor.fun <- function(data.tb, ranking_variable.v, ranking_period.v, num_leg_positions.v, sort_by.v = c("ascend", "descend"), name.v){
  
  Ranking_variable <- enquo(ranking_variable.v)
  
  message("\nPositions.")
  
  positions.tb <- data.tb %>%
    group_by(Active_ticker) %>%
    mutate(Ranking_variable = rollapply(data = !! Ranking_variable, width = ranking_period.v, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")) %>%
    slice(ranking_period.v:n()) %>%
    ungroup %>%
    select(Active_ticker, Date, Ranking_variable) %>%
    group_by(Date) %>%
    do(date_positions.fun(data.tb = ., sort_by.v, num_leg_positions.v)) %>%
    group_by(Date) %>%
    mutate(Count = n()) %>%
    ungroup %>%
    mutate(Date_next = lead(Date, first(Count))) %>%
    group_by(Date) %>%
    mutate(Date_next = first(Date_next)) %>%
    ungroup %>%
    select(-Count) %>%
    select(Date_next, Active_ticker, Positions) %>%
    group_by(Date_next, Positions) %>%
    mutate(ind = row_number(), Date = Date_next) %>%
    ungroup %>%
    filter(!is.na(Date), !is.na(Positions)) %>%
    spread(key = Positions, value = Active_ticker, sep = ".") %>%
    select(-c(ind, Date_next))
  
  message("\nReturns.")
  
  left_join(positions.tb, positions.tb %>% group_by(Date) %>% do(date_returns.fun(positions.tb = ., data.tb)), by = "Date") %>%
    ungroup %>% mutate(Name = name.v) %>% select(Name, Date, everything())
}



date_positions.fun <- function(data.tb, sort_by.v, num_leg_positions.v){
  
  sorted.tb <- tibble(Ranking_variable = data.tb$Ranking_variable, Active_ticker = data.tb$Active_ticker) %>%
    purrr::when(sort_by.v == "ascend" ~ arrange(., Ranking_variable), ~ arrange(., desc(Ranking_variable))) %>%
    filter(complete.cases(.))
  
  if(sorted.tb %>% nrow() < (num_leg_positions.v * 2L)){
    tibble(Active_ticker = rep(NA, num_leg_positions.v * 2L), Positions = rep(NA, num_leg_positions.v * 2L))
  } else {
    slice(sorted.tb, c(1L:num_leg_positions.v, (nrow(sorted.tb) - (num_leg_positions.v - 1L)):nrow(sorted.tb))) %>%
      mutate(Positions = c(rep("short", num_leg_positions.v), rep("long", num_leg_positions.v))) %>% select(Active_ticker, Positions)
  }
}


date_returns.fun <- function(positions.tb, data.tb){
  
  if(any(is.na(c(positions.tb$Positions.long, positions.tb$Positions.short)))){
    
    tibble(Return.long = NA, Return.short = NA, Return.all = NA)
    
  } else {
    
    if(any(is.na(c((data.tb %>% filter(Date == positions.tb$Date, Active_ticker %in% positions.tb$Positions.long))$Front_return, 
                   (data.tb %>% filter(Date == positions.tb$Date, Active_ticker %in% positions.tb$Positions.short))$Front_return)))){
      
      tibble(Return.long = NA, Return.short = NA, Return.all = NA)
      
    } else {
  
      returns.tb <- tibble(Return.long = (data.tb %>% filter(Date == positions.tb$Date, Active_ticker %in% positions.tb$Positions.long) %>% summarise(x = mean(Front_return, na.rm = T)))$x,
                           Return.short = (data.tb %>% filter(Date == positions.tb$Date, Active_ticker %in% positions.tb$Positions.short) %>% summarise(x = -1L * mean(Front_return, na.rm = T)))$x)
      returns.tb %>% mutate(Return.all = mean(c(Return.long, Return.short)))
    }
  }
}




mrkt_factor.fun <- function(data.tb, name.v){
  
  message("\nPositions.")
  
  positions.tb <- data.tb %>% 
    group_by(Date) %>% 
    do(tibble(Positions.long = (select(., Active_ticker, Front_return) %>% filter(complete.cases(.)))$Active_ticker)) %>%
    group_by(Date) %>%
    mutate(Count = n()) %>%
    ungroup %>%
    mutate(Date_next = lead(Date, first(Count))) %>%
    group_by(Date) %>%
    mutate(Date_next = first(Date_next)) %>%
    ungroup %>%
    mutate(Date = Date_next) %>%
    select(-Count, -Date_next) %>%
    filter(!is.na(Date))
  
  message("\nReturns.")
  
  returns.tb <- data.tb %>% group_by(Date) %>% do(data.frame(Return.long = mean(.$Front_return, na.rm = TRUE),
                                                             Return.all = mean(.$Front_return, na.rm = TRUE)))
  
  left_join(positions.tb, returns.tb, by = "Date") %>% ungroup %>% mutate(Name = name.v) %>% select(Name, Date, everything()) %>%
    mutate(Positions.short = NA, Return.short = NA)
}




aggregate_CHP.fun <- function(futures.tb, tickers.tb){
  
  country.tb <- futures.tb %>% left_join(tickers.tb %>% select(Active_ticker, Asset_class, Country), by = "Active_ticker") %>%
    group_by(Asset_class, Country, Date) %>% summarise(CHP = mean(CHP, na.rm = T)) %>% group_by(Asset_class, Country) %>% 
    mutate(CHP_regime = ifelse(CHP < median(CHP, na.rm = T), "Backwardation", "Contango")) %>% ungroup %>% arrange(Asset_class, desc(Country)) %>%
    mutate(Asset_pool = paste(Country, Asset_class, sep = "-")) %>% select(Asset_pool, Date, CHP, CHP_regime)
  
  coutry_sector.tb <- futures.tb %>% left_join(tickers.tb %>% select(Active_ticker, Asset_class, Sector, Country), by = "Active_ticker") %>%
    group_by(Asset_class, Country, Sector, Date) %>% summarise(CHP = mean(CHP, na.rm = T)) %>% group_by(Asset_class, Country, Sector) %>% 
    mutate(CHP_regime = ifelse(CHP < median(CHP, na.rm = T), "Backwardation", "Contango")) %>% ungroup %>% arrange(Asset_class, desc(Country), Sector) %>%
    mutate(Asset_pool = paste(Country, Asset_class, Sector, sep = "-")) %>% select(Asset_pool, Date, CHP, CHP_regime)
  
  coutry_sector_subsector.tb <- futures.tb %>% left_join(tickers.tb %>% select(Active_ticker, Asset_class, Sector, Subsector, Country), by = "Active_ticker") %>%
    group_by(Asset_class, Country, Sector, Subsector, Date) %>% summarise(CHP = mean(CHP, na.rm = T)) %>% group_by(Asset_class, Country, Sector, Subsector) %>% 
    mutate(CHP_regime = ifelse(CHP < median(CHP, na.rm = T), "Backwardation", "Contango")) %>% ungroup %>% arrange(Asset_class, desc(Country), Sector, Subsector) %>%
    mutate(Asset_pool = paste(Country, Asset_class, Sector, Subsector, sep = "-")) %>% select(Asset_pool, Date, CHP, CHP_regime)
  
  list(country.tb, coutry_sector.tb, coutry_sector_subsector.tb) %>% do.call(what = "rbind", args = .)
}
