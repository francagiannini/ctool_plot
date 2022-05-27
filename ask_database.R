library(tidyverse)
library(readxl)

# Meteorological data


# Plot scale data SB no CC ----

sb_ask <- read_excel("data/Copy of Yield_parameters_FGK_JLJ.xlsx") |> 
  mutate(year=Year)

table(sb_ask$Year)

sb_ask |> 
  ggplot(aes(year, Grain_DM, 
             group=Sample_ID,
             col=as.factor(Sample_ID)))+
  geom_point()+
  geom_smooth(se=FALSE)+
  theme_bw()

# Meteorological data ----


daily_Ask_2010to2013 <-
  read.csv("data/Askov_Daily_1_1_2010_31_12_2013.csv",
           header = TRUE) |>  mutate(
             date = lubridate::ymd(date),
             prec = as.numeric(recode(daily_Ask_2010to2013$prec08, "null" ='0'))
           )


daily_Ask_from2014 <-read.csv(
  "data/Askov_Daily_1_1_2014_31_12_2021.csv", 
  header = TRUE) |> mutate(date = lubridate::ymd(lubridate::dmy(date)))


monthly_Askov_1940_to_2009 <- 
  read.csv("data/Askov_Monthly_1_1940_12_2009.csv",
           header = TRUE) |>  mutate(date = lubridate::my(date),
                                     makkepot = epot ) 


monthly_Ask_2010to2021 <-
  bind_rows(daily_Ask_2010to2013, daily_Ask_from2014) |>  mutate(
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) |>
  group_by(year, month) |> 
  summarise(temp = mean(temp, na.rm = TRUE),
            maxte= mean(maxte, na.rm = TRUE),
            minte=mean(minte, na.rm = TRUE),
            prec= sum(prec, na.rm = TRUE),
            rh=mean(rh, na.rm = TRUE),
            glorad= mean(glorad, na.rm = TRUE),
            makkepot=mean(makkepot, na.rm = TRUE)) |> 
  mutate(date=lubridate::ym(paste(year,month, sep = "-")))


monthly_ask_1940_2021 <-
  bind_rows(monthly_Askov_1940_to_2009,
            monthly_Ask_2010to2021) |> 
  select(c(temp,maxte,minte,prec,rh,glorad,makkepot,date)) |> 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date))


yearly_ask_temp <- monthly_ask_1940_2021 |> 
  group_by(year) |>  
  summarise(yr.mean=mean(temp, na.rm = TRUE),
            yr.sd=sd(temp, na.rm = TRUE),
            yr.minte=min(minte, na.rm = TRUE),
            yr.maxte=max(maxte, na.rm = TRUE),
            yr.amp=max(temp, na.rm = TRUE)-min(temp, na.rm = TRUE)
  )


yearly_wider <- monthly_ask_1940_2021 |> 
  select(!date) |> 
  pivot_wider(names_from = month,
              values_from = c(temp,maxte,minte,prec,rh,glorad,makkepot)) |> 
  inner_join(yearly_ask_temp, by= 'year')


sb_ask_raw <- inner_join(sb_ask, yearly_wider, by='year')






saveRDS(temp_yr, "temperature/temp_yr25.RDS")

temp30 <- all_temp %>%  filter(
  between(date, as.Date("1966-01-01"), as.Date("2020-12-31")))

temp100 <- all_temp %>%  filter(
  between(date, as.Date("1896-01-01"), as.Date("2020-12-31")))

write.table(
  temp30$tem_ok,
  "temperature\\temp55years.txt",
  col.names = FALSE,
  sep = "\t",
  row.names = FALSE
)

write.table(
  temp100$tem_ok,
  "temperature\\temp125years.txt",
  col.names = FALSE,
  sep = "\t",
  row.names = FALSE
)

# all_temp %>% pivot_longer(cols=c(dk_tem,fou_tem),names_to = "source") %>% 
#   ggplot(aes(date, temp, group=source, fill=source, col=source)) +
#   geom_point() 




  

