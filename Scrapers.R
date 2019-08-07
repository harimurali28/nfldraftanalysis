####SCRAPERS  ALL the scrapers used in my draft anaylsis


########## Scraping draft data ------
####https://stmorse.github.io/PFR-scrape.html
##### Much inspiration from the above link
##Basic function to pull one year of draft data 

draftscrape = function(yr) {
  url = paste0("https://www.pro-football-reference.com/years/",yr,"/draft.htm")
  draft_data = getURL(url)
  draft_data = readHTMLTable(draft_data,stringsAsFactors = F)[[1]]
}

####Scrape does not take into account players taken in supplemental draft
###function to pull multiple years of draft data and then combine them 
scrape_mult_draft = function(startyr,endyr){
  master=data.frame()
  for (i in startyr:endyr) {
    draft = draftscrape(i)
    draft$yr = i
    master %<>% 
      bind_rows(draft)
  }
  master <- master[,c(1:22,24)]
  numeric_columns <- 
    master %>%
    select(-c(Tm, Player, Pos, `College/Univ`)) %>% 
    names 
  master[numeric_columns] <-
    sapply(master[numeric_columns],as.numeric)
  master <- master %>% filter(Tm != "Tm")
  master$DrAV <- ifelse(is.na(master$DrAV),0,master$DrAV)
  master$CarAV <- ifelse(is.na(master$CarAV),0,master$CarAV)
  return(master)
}

sd_by_rd <- function(rdst, rdend){
  by_team_rd = data.frame()
  for(i in rdst:rdend){
    by_team_rd_a <- draft_all %>% filter(Rnd == i) %>% group_by(Tm,Rnd) %>% 
      summarize(total_picks = n(),total_non_active = sum(non_activated), pct_non_active = round(sum(non_activated)/total_picks,3)) 
    by_team_rd_a <- by_team_rd_a %>% mutate(sd_away_from_mean = (pct_non_active - mean(pct_non_active))/sd(pct_non_active))
    by_team_rd %<>% bind_rows(by_team_rd_a)
  }
  return(by_team_rd)
}

########### Scraping Spotrac Salary Data -------


salaryscrape = function(urlprefixspot = "https://www.spotrac.com/nfl/draft/",yr) { 
  master =data.frame()
  for(i in 1:7) {
    url = paste0("https://www.spotrac.com/nfl/draft/",yr,"/")
    salary_data = getURL(url)
    salary_data = readHTMLTable(salary_data,stringsAsFactors = F)[[i]]
    master %<>% bind_rows(salary_data)
  }
  return(master)
}
draft2016 <- salaryscrape(yr = 2016)
#### Clears dollar signs away from salary cap values
clean.me.num <- function(data) {
  str_remove_all(data, "[\\$,()\\-]")
}
#### Clears C (compensatory Pick designation) away from pick number 
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

salary_scrapemult = function(urlprefixspot = "https://www.spotrac.com/nfl/draft/",startyr,endyr) { 
  master = data.frame()
  for(i in startyr:endyr){
    salary_year = data.frame()
    url = paste0("https://www.spotrac.com/nfl/draft/",i,"/")
    for(j in 1:7) {
      salary_data = getURL(url)
      salary_data = readHTMLTable(salary_data,stringsAsFactors = F)[[j]]
      salary_data = salary_data[,1:9]
      salary_data$rd = j
      salary_data$year = i
      salary_year %<>% bind_rows(salary_data)
    }
    master %<>% bind_rows(salary_year)
  }
  master$`Signing Bonus` <- clean.me.num(master$`Signing Bonus`)
  master$`Total Value` <- clean.me.num(master$`Total Value`)
  master$Pick <- numextract(master$Pick)
  numeric_columns <- 
    master %>%
    select(-c(Team, Player, Pos, College)) %>% 
    names 
  master[numeric_columns] <-
    sapply(master[numeric_columns],as.numeric)
  master <- master %>% mutate(unique_id = paste0(year,rd,Pick))
  master$`Signing Bonus` <- ifelse(is.na(master$`Signing Bonus`),0,master$`Signing Bonus`)
  master$`Total Value` <- ifelse(is.na(master$`Total Value`),0,master$`Total Value`)
  return(master)
}

############ Scraping Win totals ------
recordscrape = function(urlprefix_win = "https://www.pro-football-reference.com/boxscores/standings.cgi?week=17&year=", urlend_win = "&wk_league=NFL", year_win) {
  url = paste0(urlprefix_win,year_win,urlend_win)
  draft_data = getURL(url)
  draft_data = readHTMLTable(draft_data,stringsAsFactors = F)
  draft_data = draft_data$AFC %>% rbind(draft_data$NFC)
  draft_data = draft_data %>% filter(!is.na(W))
  draft_data$Tm = trim(str_replace_all(draft_data$Tm, "[^[:alnum:]]", " "))
  return(draft_data)
}







###Scraping Multiple Years
scrape_mult_win = function(startyr,endyr){
  master=data.frame()
  for (i in startyr:endyr) {
    win = recordscrape(year_win = i)
    win$yr = i
    master %<>% 
      bind_rows(win)
  }
  master <- master[,1:10]
  numeric_columns <- 
    master %>%
    select(-c(Tm)) %>% 
    names 
  master[numeric_columns] <-
    sapply(master[numeric_columns],as.numeric)
  master$Tm <- ifelse(str_detect(master$Tm, "Rams"),"Los Angeles Rams", ifelse(str_detect(master$Tm, "Chargers"),"Los Angeles Chargers",master$Tm))
  return(master)
}
  
  
#########Scaping Trade Data -------
trade_scrape_pf <- function(startyr,endyr) {
  
  
  ##### Pulling table
  url <- paste0("https://www.pro-football-reference.com/play-index/trade_finder.cgi?request=1&year_min=",startyr,"&year_max=",endyr,"&pos%5B%5D=qb&pos%5B%5D=rb&pos%5B%5D=wr&pos%5B%5D=te&pos%5B%5D=e&pos%5B%5D=t&pos%5B%5D=g&pos%5B%5D=c&pos%5B%5D=ol&pos%5B%5D=dt&pos%5B%5D=de&pos%5B%5D=dl&pos%5B%5D=ilb&pos%5B%5D=olb&pos%5B%5D=lb&pos%5B%5D=cb&pos%5B%5D=s&pos%5B%5D=db&pos%5B%5D=k&pos%5B%5D=p&draft_pick_min=1&draft_pick_max=256&order_by=trans_date")
  trade_pf = getURL(url)
  trade_pf = readHTMLTable(trade_pf,stringsAsFactors = F)[[1]]
  ###Chaning Date into date type to be able to pull different parts
  trade_pf$Date = lubridate::mdy(trade_pf$Date)
  trade_pf$Year = lubridate::year(trade_pf$Date)  
  ###Have to rename to create trade_id
  colnames(trade_pf)[4] <- "AV.From"
  colnames(trade_pf)[7] <- "AV.To"
  trade_pf <- trade_pf %>% mutate(trade_id = paste0(row_number(),trade_pf$Year)) %>% select(trade_id, everything())
  #####Creating two data frames to stack allowing per team rows
  trade_pf_a <- trade_pf %>% select(c(1:2,4:6,9))
  colnames(trade_pf_a)[4] <- "AV"
  colnames(trade_pf_a)[5] <- "Team"
  colnames(trade_pf_a)[3] <- "Received"
  trade_pf_a <- trade_pf_a %>% select(trade_id,Date,Year,Team,Received,AV)
  trade_pf_b <- trade_pf %>% select(c(1:3,7:9))
  colnames(trade_pf_b)[5] <- "AV"
  colnames(trade_pf_b)[3] <- "Team"
  trade_pf_b <- trade_pf_b %>% select(trade_id,Date,Year,Team,Received,AV)
  ####### Stacking rows back into a trade df
  trade_pf <- rbind(trade_pf_a,trade_pf_b) %>% arrange(desc(Date),trade_id)
  ###Using Stringr to extract pick numbers and player names
  trade_pf <- trade_pf %>% mutate(Trade_picks =gsub("[\\(\\)]", "", regmatches(Received, gregexpr("\\(.*?\\)", Received)))) 
  trade_pf$Trade_picks <- ifelse(startsWith(trade_pf$Trade_picks, "$") | startsWith(trade_pf$Trade_picks, "character0"),NA,trade_pf$Trade_picks)
  trade_pf <- trade_pf %>% mutate(Player_Received = ifelse(grepl("^[0-9]", Received),NA,gsub("\\(.*","",Received)),
                                  Picks_Traded = Numextractmult(trade_pf$Trade_picks))
  ##### When extracting the picks, a list is created and in order to unnest it and keep player names we must join the data back on itself
  ##### Therefore Ive created a seperate DF to join back on
  trade_pf_join <- trade_pf %>% unnest(Picks_Traded) %>% select(c("trade_id", "Team", "Picks_Traded"))
  
  trade_pf <-left_join(trade_pf, trade_pf_join, by = c("trade_id","Team"))
  trade_pf <- trade_pf %>% select(c(1:8,10))
  ### Renaming joined column
  colnames(trade_pf)[9] <- "Picks Received"
  
  ##### Extracting conditional string away from player names
  trade_pf$Future_Conditional_Picks <- ifelse(str_detect(trade_pf$Player_Received,"a.+pick") == TRUE, str_extract_all(trade_pf$Player_Received, "a.+pick"), NA)
  trade_pf$Player_Received <- str_remove(trade_pf$Player_Received, "a.+pick")
  trade_pf <- trade_pf %>% unnest(Future_Conditional_Picks)
  return(trade_pf)
}




###Other function used in draft analysis -----

####Leveraged from my data science teacher... TY Professor Belloni

## Selects the Number of Clusters via an Information Criteria
## get AIC (option "A"), BIC (option "B"), HDIC (option "C") for the output of kmeans
kIC <- function(fit, rule=c("A","B","C")){
  df <- length(fit$centers) # K*dim
  n <- sum(fit$size)
  D <- fit$tot.withinss # deviance
  rule=match.arg(rule)
  if(rule=="A")
    #return(D + 2*df*n/max(1,n-df-1))
    return(D + 2*df)
  else if(rule=="B") 
    return(D + log(n)*df)
  else 
    return(D +  sqrt( n * log(df) )*df)
}

