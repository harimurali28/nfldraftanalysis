####The code is very iterative and builds on itself quite a bit. There are a few dataframes that will be created in this code that will
### be repetitive and of no use. However, it was part of my process in getting the data cleaned so I kept it in
## Loading Libraries and Initall Scrape of draft data from Pro Football Reference -------------

########## Scraping draft data
####https://stmorse.github.io/PFR-scrape.html
##### Draft scraper inspirted by link above

library(stringr)
library(XML)
library(magrittr)
library(tidyverse)
library(RCurl)
library(gdata)
library(caret)
library(ggrepel)
library(stargazer)
library(boot)
library(car)
library(rvest)

#### Loading in all the scrapers
####Make Sure you download the CSV from the github to the same working directory
source("Scrapers.R")

###Scraping all the data then combining

urlprefix <- "https://www.pro-football-reference.com/years/"
urlend = "/draft.htm"
yr=2013
url = paste0(urlprefix,yr,urlend)

startyr <- 2005
endyr <- 2016
###Scraping draft data from 2005-2016
draft_all <- scrape_mult_draft(startyr, endyr)

draft_all_ids<- draft_all %>% select(Player)
write.csv(draft_all_ids, "draft_all_ids.csv")
###### Now that the data is scraped, time to begin cleaning a bit more 

#Check how many unique team names are in the data
length(unique(draft_all$Tm))
### There are 33 teams in the data and for sake of analysis I am going to combine STL and LAR into just LAR
### Also after 2017 SDG becomes the LAC and after 2020 OAK becomes LVR.... Only going to write code for LAR and LAC

draft_all$Tm <- ifelse(draft_all$Tm == "SDG","LAC", ifelse(draft_all$Tm == "STL","LAR", draft_all$Tm))
length(unique(draft_all$Tm))

##Should be 32

##Now want to label players that never played in a game or were activated 
summary(draft_all$G)
### See there are NAs so we will add a column that indicates the players

draft_all <- draft_all %>% mutate(non_activated = ifelse(is.na(G),1,0),no_value_for_drafted_team = ifelse(is.na(G),1, ifelse(DrAV < 1,1,0)))
view(draft_all)



##GGPlots for picks by games
##Simple Example of possible exploratory plot with Eagles vs. League and Howie Roseman vs. League
###Can easily do it for your Team, Just filter by correct abbreviation found in Draft_all data frame
########## GG Plots for exploratory analysis --------

# GG plots as part of exploratory Data Analysis using Eagles as example
### Wanted to see Howie Roseman's picks so filtered for years he was "GM"
Eagles_picks <- draft_all %>% filter(Tm == "PHI")
Howie_picks <- Eagles_picks %>% filter(yr > 2009 & yr != 2013 & yr != 2014 )
Howie_Draft <- draft_all %>% filter(yr > 2009 & yr !=2013 & yr !=2014)


G_by_pk <- draft_all %>% ggplot(aes(x=Pick, y=G)) + geom_point(alpha = .05) +
geom_point(data = Eagles_picks, aes(x=Pick, y = G), color = "#004953") + geom_smooth(method = lm) + 
geom_smooth(data = Eagles_picks, method = lm, color = "#004953") + ggtitle("Eagles Picks (Green) vs. League (Blue) 2005 - 2018") + 
ylab("Games")
G_by_pk
G_by_pk_howie <- Howie_Draft %>% ggplot(aes(x=Pick, y=G)) + geom_point(alpha = .05) +
geom_point(data = Howie_picks, aes(x=Pick, y = G), color = "Dark Green") + geom_smooth(method = lm) + 
geom_smooth(data = Howie_picks, method = lm, color = "Green") + ggtitle("Howie Picks (Green) vs. League (Blue) 2009-2012 + 2015-18") + ylab("Games")
G_by_pk_howie



##### Creating non-active player field
### Start of finding NVP (No Value Picks)
############## Non- Active Drafted player --------

by_round <- draft_all %>% group_by(Rnd) %>% summarize(total_picks = n(),total_non_active = sum(non_activated), 
                                                      pct_non_active = round(sum(non_activated)/total_picks,3))







########### Scraping Spotrac Salary Data and combining
##Salary scrapr broke due to spotrac changing their site a bit, will un comment when its fixed

#salary_data_all <-salary_scrapemult(startyr = 2005,endyr= 2016)



###Creating unique id for each draft pick in each database then combining data
draft_all <- draft_all %>% mutate(unique_id = paste0(yr,Rnd,Pick))
draft_comb <- draft_all

###Draft_comb is sort of broken right now because of bug in salary scrape
#### draft_comb  <- draft_all %>% left_join(salary_data_all, by = "unique_id")

summary(draft_comb)
###Creating % of the cap to normalize later analysis
###Not all signing bonuses are created = 

#salary_cap_history <- data.frame()
#cap_year <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
#cap_amount <- c(85500000,102000000,109000000,116000000,123000000,121500000,120000000,120600000,123000000,133000000,143280000,155270000)
#salary_cap_history <- data.frame(cap_year,cap_amount)
##draft_comb <- merge(x = draft_comb, y = salary_cap_history, by.x = "year", by.y = "cap_year")
draft_comb <- draft_comb %>% mutate(gtd_pct_cap = round(`Signing Bonus`/cap_amount,3))


#### Combined Data Analysis
###This function retrives means and std. devs for each rd so have an idea in data frame format where each team stands
sd_by_rd_salary <- function(rdst, rdend){
  by_team_rd = data.frame()
  for(i in rdst:rdend){
    by_team_rd_a <- draft_comb %>% filter(Rnd == i) %>% group_by(Tm,Rnd) %>% 
      summarize(#gtd_money_lost = sum(ifelse(no_value_for_drafted_team == "1", `Signing Bonus`,0)),
        total_picks = n(),total_no_value_players = sum(no_value_for_drafted_team), 
        pct_no_value = round(sum(no_value_for_drafted_team)/total_picks,3)) 
    by_team_rd_a <- by_team_rd_a %>% mutate(sd_away_from_mean = (pct_no_value - mean(by_team_rd_a$pct_no_value))/sd(by_team_rd_a$pct_no_value))
    by_team_rd %<>% bind_rows(by_team_rd_a)
  }
  return(by_team_rd)
}


by_team_comb<- draft_comb %>% group_by(Tm) %>%  summarize(#gtd_money_lost = sum(`Signing Bonus`),
                                                          total_picks = n(),total_no_value_players = sum(no_value_for_drafted_team),
                                                          pct_no_value = round(sum(no_value_for_drafted_team)/total_picks,3)) %>% 
  mutate(sd_away = (pct_no_value - mean(pct_no_value))/sd(pct_no_value))

draft_comb_metrics <- sd_by_rd_salary(rdst,rdend)
summary(draft_comb_metrics)


draft_comb_metrics_graph <-by_team_comb %>% ggplot(aes(x = pct_no_value, y = gtd_money_lost)) + geom_point() + geom_text(aes(label = Tm, vjust = 1)) + geom_smooth(method = lm)
draft_comb_metrics_graph





##########Scraping Team Win Totals ---------------------
# Scraping win totals between 2005-2016

#test_record <- recordscrape(year_win = 2016)



###Scraping Multiple Years
startyr <- 2005
endyr <- 2016

team_rec_05_16 <-scrape_mult_win(startyr = 2005, endyr = 2016)
##Getting abbreviations to be able to join with record data with other data sets
###Found row numbers from records data set and then matched it manually with records from draft pick data set

team_abr <- team_rec_05_16 %>% group_by(Tm) %>% summarize(w = sum(W)) %>% select(Tm) %>% mutate(teamid = row_number())
team_abr_b <- draft_all %>% group_by(Tm) %>% summarize(games = sum(G)) %>% select(Tm) %>% 
  mutate(teamid = ifelse(Tm == "NOR", 22,ifelse(Tm == "NWE", 21, ifelse(Tm == "SEA", 29, ifelse(Tm== "SFO", 28, row_number())))))
colnames(team_abr_b)[1] <- "Team_Abbrv"

team_abr <- team_abr %>% left_join(team_abr_b, by = "teamid")
team_rec_05_16 <- team_rec_05_16 %>% left_join(team_abr, by = "Tm")

team_rec_total <- team_rec_05_16 %>% group_by(Team_Abbrv) %>% 
  summarise(total_wins = sum(W), total_losses = sum(L), total_win_pct = total_wins/(total_wins + total_losses))
mean(team_rec_total$total_win_pct)
sd(team_rec_total$total_win_pct)


############# Scraping Trade Data -------
####Spotrac Data only contains picks traded for a player

trades0616<-trade_scrape(2006,2016)  



trade_value <- read.csv("Draft Value Johnson.csv")
trades <- trades0616 %>% left_join(trade_value, by = c("Picks_Traded" = "Pick"))


#################### Looking at PFR approx value  as well as Cap Hits for a full analysis -----
####AV per round per pick
draft_comb <- draft_comb %>% mutate(section = ifelse(Rnd == 1, "Day One", ifelse(Rnd == 2 | Rnd == 3, "Day Two", ifelse(Rnd == 4 | Rnd == 5, "Early Day Three", "Late Day Three"))),
                                    nonDrAV = CarAV - DrAV, DrAV_diff = nonDrAV-DrAV, AV_diff = predictedAV - DrAV, 
                                    underperform_players = ifelse(AV_diff > 0, 1,0))
draft_comb$section <- as.factor(draft_comb$section)
summary(draft_comb)

full_analysis <- function(rdst, rdend){
  by_team_rd = data.frame()
  for(i in rdst:rdend){
    by_team_rd_a <- draft_comb %>% filter(Rnd == i) %>% group_by(Tm,Rnd) %>% 
      summarize(#gtd_money_lost = sum(ifelse(no_value_for_drafted_team == "1", `Signing Bonus`,0)),
                total_picks = n(),total_no_value_players = sum(no_value_for_drafted_team),
                total_av_for_team = sum(DrAV), AV_per_pick_for_team = sum(DrAV)/total_picks,
                total_AV_per_pick = sum(CarAV)/total_picks, #avg_pct_of_cap_lost = round(mean(ifelse(no_value_for_drafted_team == 1, mean(gtd_pct_cap),0)),3),
                nonDrAV_per_pick = sum(nonDrAV)/total_picks,
                DrAV_pct = sum(DrAV)/sum(CarAV),
                total_under_perform_players = sum(underperform_players),
                pct_underperform = sum(underperform_players)/total_picks,
                pct_no_value = round(sum(no_value_for_drafted_team)/total_picks,3)) 
    by_team_rd_a <- by_team_rd_a %>% mutate(sd_away_from_mean_no_value = (pct_no_value - mean(by_team_rd_a$pct_no_value))/sd(by_team_rd_a$pct_no_value))
    by_team_rd %<>% bind_rows(by_team_rd_a)
  }
  return(by_team_rd)
}


full_table_team<- draft_comb %>% group_by(Tm) %>% 
  summarize(#gtd_money_lost = sum(ifelse(no_value_for_drafted_team == "1", `Signing Bonus`,0)),
            total_picks = n(),total_no_value_players = sum(no_value_for_drafted_team),
            total_av_for_team = sum(DrAV), AV_per_pick_for_team = sum(DrAV)/total_picks,
            total_AV_per_pick = sum(CarAV)/total_picks,
           # avg_pct_of_cap_lost = round(mean(ifelse(no_value_for_drafted_team == 1, mean(gtd_pct_cap),0)),3),
            nonDrAV_per_pick = sum(nonDrAV)/total_picks,
            DrAV_pct = sum(DrAV)/sum(CarAV),
            total_under_perform_players = sum(underperform_players),
            pct_underperform = sum(underperform_players)/total_picks,
            pct_no_value = round(sum(no_value_for_drafted_team)/total_picks,3))  %>% 
  mutate(sd_away = (pct_no_value - mean(pct_no_value))/sd(pct_no_value))


full_table_team_section<- draft_comb %>% group_by(Tm, section) %>% 
  summarize(#gtd_money_lost = sum(ifelse(no_value_for_drafted_team == "1", `Signing Bonus`,0)),
            total_picks = n(),total_no_value_players = sum(no_value_for_drafted_team),
            total_av_for_team = sum(DrAV), AV_per_pick_for_team = sum(DrAV)/total_picks,
            total_AV_per_pick = sum(CarAV)/total_picks, #avg_pct_of_cap_lost = round(mean(ifelse(no_value_for_drafted_team == 1, mean(gtd_pct_cap),0)),3),
            nonDrAV_per_pick = sum(nonDrAV)/total_picks,
            DrAV_pct = sum(DrAV)/sum(CarAV),
            total_under_perform_players = sum(underperform_players),
            pct_underperform = sum(underperform_players)/total_picks,
            pct_no_value = round(sum(no_value_for_drafted_team)/total_picks,3))  %>% 
  mutate(sd_away = (pct_no_value - mean(pct_no_value))/sd(pct_no_value))


full_table_rd <-full_analysis(1,7)

full_table_team <- merge(x = full_table_team , y = team_rec_total, by.x = "Tm", by.y = "Team_Abbrv")
full_table_rd <-  merge(x = full_table_rd, y = team_rec_total, by.x = "Tm", by.y = "Team_Abbrv")
full_table_team_section <-  merge(x = full_table_team_section, y = team_rec_total, by.x = "Tm", by.y = "Team_Abbrv")


################# Analaysis Time -------
##Getting rid of categorical variables and variables that directly correlate or negativley correlate with wins (Losses, winning pct,)
full_table_rd_reg<- full_table_team %>% select(c(2:9,11))

### Normality and plots by section
####looking at by section

wins_no_value <-full_table_team_section %>% ggplot(aes(x = pct_no_value, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) + facet_grid(.~section)
+ geom_label_repel(aes(label = Tm),box.padding   = 0.35,point.padding = 0.5,  segment.color = 'grey50') + 
  ggtitle("Non-Value generating 6-7 rd picks relation to win %") + xlab("Pct Players who have generated <1 Drafted AV") 

wins_no_value
#### Section Day 2 (2-3)
Section2 <- full_table_team_section %>% filter(section == "Day Two")
Pct_no_value_2 <- Section2 %>% 
  ggplot(aes(pct_no_value)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Pct no Value", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(Section2$pct_no_value, na.rm = TRUE), sd = sd(Section2$pct_no_value, na.rm = TRUE)), colour = "black", size = 1)

Pct_no_value_2
stat.desc(Section2$pct_no_value, basic = FALSE, norm = TRUE)
shapiro.test(Section2$pct_no_value)

qqplot.2 <- qplot(sample = Section2$pct_no_value, stat="qq")
qqplot.2


Rd2_wins_no_value <-Section2 %>% ggplot(aes(x = pct_no_value, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) +
  geom_label_repel(aes(label = Tm),box.padding   = 0.35,point.padding = 0.5,  segment.color = 'grey50') + ggtitle("Non-Value generating Day 2 (2-3) picks relation to win %") + xlab("Pct Players who have generated <1 Drafted AV") + labs(subtitle = "Years 2005-2016",caption = "Data source: Pro Football Reference") + 
  theme(plot.subtitle = element_text(color = "black", face = "italic", size = 10))
Rd2_wins_no_value

day2_reg <- lm(data = Section2, total_win_pct ~ pct_no_value)
summary(day2_reg)
cooks.distance(day2_reg)

####Section Late Day 3

SectionL3 <- full_table_team_section %>% filter(section == "Late Day Three")
Pct_no_value_L3 <- SectionL3 %>% 
  ggplot(aes(pct_no_value)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Pct no Value", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(SectionL3$pct_no_value, na.rm = TRUE), sd = sd(SectionL3$pct_no_value, na.rm = TRUE)), colour = "black", size = 1)

Pct_no_value_L3
stat.desc(SectionL3$pct_no_value, basic = FALSE, norm = TRUE)
shapiro.test(SectionL3$pct_no_value)

qqplot.L3 <- qplot(sample = SectionL3$pct_no_value, stat="qq")
qqplot.L3

L3_wins_no_value <-SectionL3 %>% ggplot(aes(x = pct_no_value, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) +
  geom_label_repel(aes(label = Tm),box.padding   = 0.35,point.padding = 0.5,  segment.color = 'grey50') + ggtitle("Non-Value generating 6-7 rd picks relation to win %") + xlab("Pct Players who have generated <1 Drafted AV") 
L3_wins_no_value

l3_reg <- lm(data = SectionL3, total_win_pct ~ pct_no_value)
summary(l3_reg)

SectionE3 <- full_table_team_section %>% filter(section == "Early Day Three")
E3_reg <- lm(data = SectionE3, total_win_pct ~ pct_no_value)
summary(E3_reg)
Section3 <- full_table_team_section %>% filter(section == "Early Day Three" | section == "Late Day Three")
day3_reg <- lm(data = Section3, total_win_pct ~ pct_no_value)
summary(day3_reg)

###GGplopt relation of pct no value to total win pct facet by Section of draft
wins_no_value <-full_table_team_section %>% ggplot(aes(x = pct_no_value, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) + facet_wrap(.~section)+
  geom_label_repel(aes(label = Tm),box.padding   = 0.35,point.padding = 0.5,  segment.color = 'grey50') + 
  ggtitle("Non-Value generating 6-7 rd picks relation to win %") + xlab("Pct Players who have generated <1 Drafted AV") 

wins_no_value

###GGplopt relation of pct no value to total win pct facet by Roubd of draft
wins_no_value_by_round <-full_table_rd %>% ggplot(aes(x = pct_no_value, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) + facet_wrap(.~Rnd)

wins_no_value_by_round

##1 is Day 2 (2-3) -- 2) Early day 3 (4-5) --  3) Late day 3 (6-7)
###whole draft
whole_reg <- lm(data = full_table_team, total_win_pct ~ pct_no_value)
summary(whole_reg)

###Combines section regressions into nice table
stargazer(day2_reg,E3_reg,l3_reg,day3_reg, type="text", column.labels=c("Day 2 (Rd 2-3)", "Early Day 3 (4-5)", "Late Day 3(6-7)", "All Day 3 (4-7)"), out = "sections.htm")

####Creating function to run multiple regressions rapidly for section
Reg_section_table <-function(df){
  lm(data = df, total_win_pct[i] ~ pct_no_value[i])
}

####Boot strapping Day 2 
set.seed(12345) # for reproducibility
system.time(Day_2_boot <- Boot(day2_reg, R=2000))
summary(Day_2_boot, high.moments = TRUE)


bootreg <- function(formula, data, indicies) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

dwt(day2_reg)
rstandard(day2_reg)
qplot(sample = rstudent(day2_reg), stat = "qq")

bootResults <- boot(statistic = bootreg,formula =total_win_pct ~ pct_no_value, data = Section2, R = 2000)

####similar thing by round
Filter_rd_table <-function(Rd){
  full_table_rd %>% filter(Rnd == Rd)
} 
Reg_rd_table <-function(df){
  lm(data = df, total_win_pct ~ pct_no_value)
}


Round1 <- Filter_rd_table(1)
Round2 <- Filter_rd_table(2)
Round3 <- Filter_rd_table(3)
Round4 <- Filter_rd_table(4)
Round5 <- Filter_rd_table(5)
Round6 <- Filter_rd_table(6)
Round7 <- Filter_rd_table(7)

Round1_lm<-Reg_rd_table(Round1)
Round2_lm<-Reg_rd_table(Round2)
Round3_lm<-Reg_rd_table(Round3)
Round4_lm<-Reg_rd_table(Round4)
Round5_lm<-Reg_rd_table(Round5)
Round6_lm<-Reg_rd_table(Round6)
Round7_lm<-Reg_rd_table(Round7)

####Combines all of the regressions into a nice table
stargazer(Round1_lm,Round2_lm,Round3_lm, Round4_lm, Round5_lm, Round6_lm, Round7_lm, type = "text",  column.labels=c("Round"), out = "rounds.htm")


#########Looking at Total AV per pick with winning to see how it correlates with non-value 
Regav_rd_table <-function(df){
  lm(data = df, total_win_pct ~ total_AV_per_pick)
}
draftav_reg <- lm(data = full_table_team, total_win_pct ~ total_AV_per_pick)
day2_av_reg <- lm(data = Section2, total_win_pct ~ total_AV_per_pick)
day3_av_reg <-  lm(data = Section3, total_win_pct ~ total_AV_per_pick)
stargazer(draftav_reg,day2_av_reg,day3_av_reg, column.labels = c("Whole Draft", "Day 2", "Day 3"), type = "text")

Round1av_lm<-Regav_rd_table(Round1)
Round2av_lm<-Regav_rd_table(Round2)
Round3av_lm<-Regav_rd_table(Round3)
Round4av_lm<-Regav_rd_table(Round4)
Round5av_lm<-Regav_rd_table(Round5)
Round6av_lm<-Regav_rd_table(Round6)
Round7av_lm<-Regav_rd_table(Round7)
stargazer(Round1av_lm,Round2av_lm,Round3av_lm, Round4av_lm, Round5av_lm, Round6av_lm, Round7av_lm, type = "text",  column.labels=c("Round"), out = "roundsav.htm")


sec2_wins_av <-Section2 %>% ggplot(aes(x = total_AV_per_pick, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) +
  geom_label_repel(aes(label = Tm),box.padding   = 0.35,point.padding = 0.5,  segment.color = 'grey50') + ggtitle("AV per pick Day 2 (rd2-3) picks relation to win %") + xlab("AV per pick") + labs(subtitle = "Years 2005-2016",caption = "Data source: Pro Football Reference") + 
  theme(plot.subtitle = element_text(color = "black", face = "italic", size = 10))
sec2_wins_av

draft_wins_av <-full_table_team %>% ggplot(aes(x = total_AV_per_pick, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) +
  geom_label_repel(aes(label = Tm),box.padding   = 0.35,point.padding = 0.5,  segment.color = 'grey50') + ggtitle("AV per pick (whole draft) in relation to win %") + xlab("AV per pick") + labs(subtitle = "Years 2005-2016",caption = "Data source: Pro Football Reference") + 
  theme(plot.subtitle = element_text(color = "black", face = "italic", size = 10))
draft_wins_av




simplelog <- lm(data = draft_all, formula = DrAV ~ log(Pick))
summary(simplelog)
simplelog
stargazer(simplelog, type = "text", out = "simplelog.htm")

simple_gg <-draft_all %>% ggplot(aes(x=Pick, y=DrAV)) + geom_point(alpha = .15) +
  geom_smooth(method = lm, formula = y~log(x))

simple_gg







####similar thing by round
#day2sal_reg <- lm(data = Section2, total_win_pct ~ avg_pct_of_cap_lost)
#L3sal_reg <- lm(data = SectionL3, total_win_pct ~ avg_pct_of_cap_lost)
#E3sal_reg <- lm(data = SectionE3, total_win_pct ~ avg_pct_of_cap_lost)
#day3sal_reg <- lm(data = Section3, total_win_pct ~ avg_pct_of_cap_lost)


#Reg_sal_rd_table <-function(df){

#  lm(data = df, total_win_pct ~ avg_pct_of_cap_lost)
#}





#Round1sal_lm<-Reg_sal_rd_table(Round1)
#Round2sal_lm<-Reg_sal_rd_table(Round2)
#Round3sal_lm<-Reg_sal_rd_table(Round3)
#Round4sal_lm<-Reg_sal_rd_table(Round4)
#Round5sal_lm<-Reg_sal_rd_table(Round5)
#Round6sal_lm<-Reg_sal_rd_table(Round6)
#Round7sal_lm<-Reg_sal_rd_table(Round7)

####Combines all of the regressions into a nice table
#stargazer(day2sal_reg,E3sal_reg,L3sal_reg,day3sal_reg, type="text", column.labels=c("Day 2 (Rd 2-3)", "Early Day 3 (4-5)", "Late Day 3(6-7)", "All Day 3 (4-7)"), out = "sections.htm")
#stargazer(Round1sal_lm,Round2sal_lm,Round3sal_lm, Round4sal_lm, Round5sal_lm, Round6sal_lm, Round7sal_lm, type = "text",  column.labels=c("Round"), out = "rounds.htm")


#####Looking at players that underperformed expected value


simplelog <- lm(data = draft_all, formula = DrAV ~ log(Pick))
summary(simplelog)
simplelog

draft_all$predictedAV <- predict(simplelog, newdata = draft_all, type = "response")



sec2_wins_under <-Section2 %>% ggplot(aes(x = pct_underperform, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) +
  geom_label_repel(aes(label = Tm),box.padding   = 0.35,point.padding = 0.5,  segment.color = 'grey50') + ggtitle("Under-performing % of day 2 (rd 2-3) in relation to win %") + xlab("Underperforming pct") + labs(subtitle = "Years 2005-2016      Underperforming = Accruing less DrAV then predicted",caption = "Data source: Pro Football Reference") + 
  theme(plot.subtitle = element_text(color = "black", face = "italic", size = 10))
sec2_wins_under

sec2_wins_under_lm <- lm(data = Section2, formula = total_win_pct ~ pct_underperform)
summary(sec2_wins_under_lm)

stargazer(sec2_wins_under_lm, type = "text", column.labels = c("Day 2"), out = "underperform day 2.htm")

Regunderlm_rd_table <-function(df){
  lm(data = df, total_win_pct ~ pct_underperform)
}
draftav_reg <- lm(data = full_table_team, total_win_pct ~ total_AV_per_pick)
day2_av_reg <- lm(data = Section2, total_win_pct ~ total_AV_per_pick)
day3_av_reg <-  lm(data = Section3, total_win_pct ~ total_AV_per_pick)
stargazer(draftav_reg,day2_av_reg,day3_av_reg, column.labels = c("Whole Draft", "Day 2", "Day 3"), type = "text")

Round1under_lm<-Regunderlm_rd_table(Round1)
Round2under_lm<-Regunderlm_rd_table(Round2)
Round3under_lm<-Regunderlm_rd_table(Round3)
Round4under_lm<-Regunderlm_rd_table(Round4)
Round5under_lm<-Regunderlm_rd_table(Round5)
Round6under_lm<-Regunderlm_rd_table(Round6)
Round7under_lm<-Regunderlm_rd_table(Round7)
stargazer(Round1under_lm,Round2under_lm,Round3under_lm, Round4under_lm, Round5under_lm, Round6under_lm, Round7under_lm, type = "text",  column.labels=c("Round"), out = "roundsunder.htm")

############Starting clustering process


Pct_no_value_whole <- full_table_team %>% 
  ggplot(aes(pct_no_value)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="Pct no Value", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(full_table_team$pct_no_value, na.rm = TRUE), sd = sd(full_table_team$pct_no_value, na.rm = TRUE)), colour = "black", size = 1)
library(pastecs)
Pct_no_value_whole
stat.desc(full_table_team$pct_no_value, basic = FALSE, norm = TRUE)
shapiro.test(full_table_team$pct_no_value)

qqplot.2 <- qplot(sample = full_table_team$pct_no_value, stat="qq")
qqplot.2

wins_no_value <-full_table_team %>% ggplot(aes(x = pct_no_value, y = total_win_pct)) + geom_point()+ geom_smooth(method = lm) +
  geom_label_repel(aes(label = Tm),box.padding   = 0.35,point.padding = 0.5,  segment.color = 'grey50') + ggtitle("Non-Value % of picks from 2006-16 in relation to win %") + xlab("Pct Players who have generated <1 Drafted AV") 
wins_no_value






#####Creating clusters
###Code primarily from Prof. Belloni




Section2_cluster <- Section2 %>% select(pct_no_value,total_win_pct)

Section2_kmeans <- kmeans(Section2_cluster,4,nstart=10)
colorcluster <- 1+Section2_kmeans$cluster
plot(Section2_cluster, col = 1, xlab="% No value Players", ylab="Win %")
plot(Section2_cluster, col = colorcluster, xlab="% No Value Players", ylab="Win %")
points(Section2_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)


### computing # of clusters in our example:
kfit <- lapply(1:20, function(k) kmeans(Section2_cluster,k,nstart=10))
# choose number of clusters based on the fit above


# kmeans for k=1,...50, that was stored in kfit.
# Then "A" for AICc (default) or "B" for BIC
kaic <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
kHDic  <- sapply(kfit,kIC,"C")

par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(kaic, xlab="k (# of clusters)", ylab="IC (Deviance + Penalty)", 
     ylim=range(c(kaic,kbic,kHDic)), # get them on same page
     type="l", lwd=2)

# Vertical line where AIC is minimized
abline(v=which.min(kaic))
# Next we plot BIC
lines(kbic, col=4, lwd=2)
# Vertical line where BIC is minimized
abline(v=which.min(kbic),col=4)
# Next we plot HDIC
lines(kHDic, col=3, lwd=2)
# Vertical line where HDIC is minimized
abline(v=which.min(kHDic),col=3)
# Insert labels
text(c(which.min(kaic),which.min(kbic),which.min(kHDic)),c(mean(kaic),mean(kbic),mean(kHDic)),c("AIC","BIC","HDIC"))


### Using another package to look at proper cluster #

#install.packages("factoextra")
#install.packages("NbClust")
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(Ssimple, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(Ssimple, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(Ssimple, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

res.nbclust <- NbClust(Ssimple, distance = "euclidean",
                       min.nc = 2, max.nc = 15, method = "kmeans")

