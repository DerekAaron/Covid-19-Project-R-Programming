rm(list=ls())
install.packages("lubridate")
library("lubridate")

dates <- seq(mdy("04-12-2020"), Sys.Date()-1, by="days")
dates <- format(dates, "%m-%d-%Y")
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/"
df<- data.frame()

install.packages("plyr")
library("plyr")
install.packages("dplyr")
library("dplyr")

for (date in dates) {
  csv_file = print(paste0(url, date, ".csv"))
  temp =  read.csv(csv_file)
  if (!("People_Tested" %in% colnames(temp))) {
    temp <- temp %>% mutate("People_Tested" = c(NA))
  }
  if (!("Mortality_Rate" %in% colnames(temp))) {
    temp <- temp %>% mutate("Mortality_Rate" = c(NA))
  }   
  temp$Date <- paste0(date)
  df = rbind(df, temp)
}


df$Date <- mdy(df$Date)
df <- df %>%
  mutate(Positivity_Rate = Confirmed / People_Tested * 100)

install.packages("ggplot2")
library("ggplot2")
library("dplyr")

state_deaths <- function(x,y,z) { 
  df %>%
  filter(Province_State %in% c(x)) %>%
  filter(between(Date, ymd(y), ymd(z))) %>%
  ggplot(aes(x=Date, y=Deaths)) +
  geom_point()+
    geom_smooth(se= FALSE)+
    scale_x_date(date_breaks = "1 month")
}
state_deaths("Vermont","2020-04-12","2021-04-12")

state_pos <- function(x,y,z){
  df %>%
    filter(df$Province_State %in% c(x)) %>%
    filter(between(Date, ymd(y), ymd(z))) %>%
    ggplot(aes(x=Date, y=Positivity_Rate)) +
    geom_point()+ geom_smooth(se= FALSE)+
    scale_x_date(date_breaks = "1 month")+
    ylim(0,10)
}
state_pos("Alabama","2020-05-01","2020-09-01")

library("dplyr")

national_deaths <- function(x,y) {
  df %>%
    group_by(Date, Country_Region) %>% 
    summarize(Deaths = sum(Deaths)) %>%
    filter(between(Date, ymd(x), ymd(y))) %>%
    ggplot(aes(x=Date, y=Deaths)) +
    geom_point()+
    geom_smooth(se= FALSE)+
    scale_x_date(date_breaks = "1 month")  
}
national_deaths("2020-04-12","2021-04-12")



