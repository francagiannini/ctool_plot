library(tidyverse)
library(readxl)

# Plot scale data SB no CC ----

sb_ask <- read_excel("data/Copy of Yield_parameters_FGK_JLJ.xlsx") |> 
  mutate(year=as.numeric(Year))

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
             prec = as.numeric(recode(prec08, "null" ='0'))
           )

daily_Ask_from2014 <-read.csv(
  "data/Askov_Daily_1_1_2014_31_12_2021.csv", 
  header = TRUE) |> mutate(date = lubridate::ymd(lubridate::dmy(date))
  )


monthly_Askov_1940_to_2009 <- 
  read.csv("data/Askov_Monthly_1_1940_12_2009.csv",
           header = TRUE) |>  mutate(date = lubridate::my(date),
                                     makkepot = epot ,
                                     prec = replace_na(prec,0),
                                     minte = replace_na(minte,0))

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


## Temp file ----

temp_ask <- monthly_ask_1940_2021 %>%  filter(
  between(date, as.Date("1951-01-01"), as.Date("2019-12-31")))

write.table(
  temp_ask$temp,
  "data/temp_ask.txt",
  col.names = FALSE,
  sep = "\t",
  row.names = FALSE
)


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

# Historical_data on yields ----   

FAO_from_1961 <- read_xls("data/FAOSTAT_data_6-5-2022.xls") |> 
  select(Item,Year,Value) |>   
  mutate(Value=as.numeric(Value)*0.0001) |> 
  filter(Item=='Barley'|Item=='Wheat')|> 
  pivot_wider(names_from = Item, values_from = Value)|> 
  mutate(Year=as.numeric(Year)) |> 
  rename("year"=Year) 

dk_1951_to_1960 <- read.csv("data/dk_hist.txt", sep = "\t") |> 
  filter(between(Column1,1920,1960)) |> 
  rename("year"=Column1) |> 
  select(Wheat, Barley, year)

nac_yields <- bind_rows(dk_1951_to_1960,FAO_from_1961) |> 
  rename("Wheat_hist"= Wheat,"Barley_hist"=Barley)|> 
  arrange(year) |> 
  mutate(prev_WW1 = lag(Wheat_hist, 1),
         prev_B1 = lag(Barley_hist , 2),
         prev_WW2 = lag(Wheat_hist, 1),
         prev_B2 = lag(Barley_hist , 2))


yearly_covar <- merge(nac_yields,yearly_wider, by='year') |> 
  select(!starts_with(c("rh_", "glo", "mak")))


# Raw data table to complete ----

sb_ask_raw <- merge(yearly_covar, sb_ask,  by='year') |> 
  select(!Coments) |> 
  mutate_if(is.character,as.factor) |> 
  group_by(Sample_ID) |>
  arrange(Year, .by_group = TRUE) |> 
  mutate(prev_yield1 = lag(Grain_DM, 1),
         prev_yield2 = lag(Grain_DM, 2)) |>
  select(!c(Grain_N, Straw_N, Total_DM, Total_N, H_index, 
            N_Straw, N_Grain,
            Cover_Crop))|> 
  mutate(Sample_ID=as.factor(Sample_ID),
         Block=as.factor(Block),
         Straw_Rate=as.factor(Straw_Rate)
  )

sb_ask_train <- sb_ask_raw |> drop_na(Grain_DM)

library(missForest)

sb_ask_imp <- missForest(as.data.frame(sb_ask_train),
                         maxiter = 50, ntree = 500, mtry=12,
                         verbose = TRUE
)

sb_ask_train <- as.data.frame(sb_ask_imp$ximp) |> select(!Straw_DM)

sb_ask_test <- sb_ask_raw |> filter(is.na(Grain_DM))

# Estimating gaps----
## Grain_DM response variables  -----

library(caret)

gbmgrid <- expand.grid(interaction.depth = seq(2,20,4), 
                       n.trees = c(500,800,1000), 
                       shrinkage = c(0.01),
                       n.minobsinnode = c(2,5))

gbmcontrol <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3
)

gbm_tr <- train(Grain_DM~ .,
                data = sb_ask_train,
                distribution="gaussian",
                tuneGrid = gbmgrid,
                method="gbm",
                trControl = gbmcontrol)

saveRDS(gbm_tr, "gbm_tr_G_DM.RDS")

gbm_tr <- readRDS("gbm_tr_G_DM.RDS")

plot(gbm_tr)

gbm_trres <- as.data.frame(summary(gbm_tr)) %>% 
  slice_max(rel.inf, n=15)

ggplot(gbm_trres, aes(reorder(var,rel.inf) , rel.inf))+
  geom_col() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "", y = "Relative influence predicting Grain DM")

as.data.frame(gbm_tr$results) |> filter(
  n.trees == gbm_tr$bestTune$n.trees &
    interaction.depth == gbm_tr$bestTune$interaction.depth &
    n.minobsinnode == gbm_tr$bestTune$n.minobsinnode
)

sb_ask_pred <- predict(gbm_tr, newdata = sb_ask_test)

sb_ask_test$Grain_DM <- sb_ask_pred

## Straw_DM -----

sb_ask_train_st <- 
  as.data.frame(sb_ask_imp$ximp) |> select(!Grain_DM)

sb_ask_test_st <- 
  sb_ask_raw |> filter(is.na(Straw_DM))

gbm_tr_st <- train(Straw_DM~ .,
                   data = sb_ask_train_st,
                   distribution="gaussian",
                   tuneGrid = gbmgrid,
                   method="gbm",
                   trControl = gbmcontrol)

saveRDS(gbm_tr_st, "gbm_tr_S_DM.RDS")

gbm_tr_st <- readRDS("gbm_tr_S_DM.RDS")

#plot(gbm_tr)

gbm_trres <- as.data.frame(summary(gbm_tr_st))  |>  
  slice_max(rel.inf, n=15)

ggplot(gbm_trres, aes(reorder(var,rel.inf) , rel.inf))+
  geom_col() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="", y="Relative influence predicting Straw DM")

gbm_tr_st$results |> filter(
  n.trees == gbm_tr_st$bestTune$n.trees &
    interaction.depth == gbm_tr_st$bestTune$interaction.depth &
    n.minobsinnode == gbm_tr_st$bestTune$n.minobsinnode
)

sb_ask_pred_st <- predict(gbm_tr_st, newdata = sb_ask_test_st)

sb_ask_test$Straw_DM <- sb_ask_pred_st

sb_ask_complete <- 
  sb_ask_raw |> drop_na(Grain_DM) |> bind_rows(sb_ask_test)

write.table(sb_ask_complete,"sb_ask_complete.txt", sep = "\t")


# Explore pred ----

sb_ask_complete <- read.table("sb_ask_complete.txt", header = T, sep = "\t")

sb_ask_complete |> pivot_longer(cols = c(Straw_DM,Grain_DM)) |> 
  ggplot(aes(y=value, x=year, col=name)) +
  geom_point() +
  geom_smooth()+
  theme_bw()
# scale_y_continuous(sec.axis =sec_axis(~.*1))

sb_ask_complete |> 
  pivot_longer(cols = c(Straw_DM,Grain_DM), names_to = "Variable") |> 
  group_by(year,Variable, Crop) |> 
  summarise(meanDM = mean(value),
            sdDM=sd(value)#/sqrt(n())
  ) |> 
  ggplot(aes(y=meanDM, x=year, col=interaction(Variable,Crop))) +
  scale_x_continuous(breaks = seq(1981,2019,1), minor_breaks = NULL)+
  geom_point() +
  geom_line()+
  geom_errorbar(
    aes(ymin=meanDM-sdDM, ymax=meanDM+sdDM), width=.2,
    position=position_dodge(0.05))+
  geom_vline(xintercept = c(1987,2003,2013), linetype="dashed") +
  theme_bw()+
  labs(y="DM (Mg/ha)")+
theme(axis.text.x = element_text(angle = 90))


y <- sb_ask_complete |>  group_by(year, Sample_ID) |>
  summarise(meanGDM = mean(Grain_DM),
            meanSDM = mean(Straw_DM)) |>
  pivot_wider(names_from = Sample_ID,
              values_from = c(meanGDM, meanSDM)#,
              #values_fn = ~mean(.x, na.rm = TRUE)
  ) |>
  ts(start = 1981, frequency = 1)

plot(y[,'meanGDM_201'])
y_d <- decompose(y)
plot(y_d)

# Initialization ----

## Grain_DM ----

# yearly_covar <- yearly_covar |> 
#   arrange(year) |> 
#   mutate(prev_WW1 = lag(Wheat_hist, 1),
#          prev_B1 = lag(Barley_hist , 2),
#          prev_WW2 = lag(Wheat_hist, 1),
#          prev_B2 = lag(Barley_hist , 2))

init_data_ww <- sb_ask_complete  #|> 
  # group_by(Sample_ID) |>
  # arrange(Year, .by_group = TRUE) |> 
  # mutate(prev_WW1 = lag(Wheat_hist, 1),
  #        prev_B1 = lag(Barley_hist , 2),
  #        prev_WW2 = lag(Wheat_hist, 1),
  #        prev_B2 = lag(Barley_hist , 2))  |>   
#filter(Crop == 'WinterWheat')

plots <-  init_data_ww |> filter(year == 2000) |> select(Sample_ID, Block)


init_test <- 
  expand.grid('year' = seq(1951,1980,1), 
              'Sample_ID'=unique(init_data_ww$Sample_ID)) |> 
  inner_join(yearly_covar, by="year") |> 
  inner_join(plots, by="Sample_ID") |> 
  mutate(Crop="WinterWheat")


init_train <- init_data_ww |> select(colnames(init_test),Grain_DM)

gbmgrid <- expand.grid(interaction.depth = seq(2,ncol(init_train)/2,4), 
                       n.trees = c(200,300,400,500), 
                       shrinkage = c(0.01),
                       n.minobsinnode = c(2,3,4))

gbmcontrol <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3
)

gbm_init_GM <- train(Grain_DM~ .,
                     data = init_train,
                     distribution="gaussian",
                     tuneGrid = gbmgrid,
                     method="gbm",
                     trControl = gbmcontrol)

as.data.frame(gbm_init_GM$results) |> filter(
  n.trees == gbm_init_GM$bestTune$n.trees &
    interaction.depth == gbm_init_GM$bestTune$interaction.depth &
    n.minobsinnode == gbm_init_GM$bestTune$n.minobsinnode
)#/mean(init_train$Grain_DM)*100


## Straw_DM ----

init_train_st <- init_data_ww |> select(colnames(init_test),Straw_DM)

gbm_init_GM_st <- train(Straw_DM~ .,
                     data = init_train_st,
                     distribution="gaussian",
                     tuneGrid = gbmgrid,
                     method="gbm",
                     trControl = gbmcontrol)

as.data.frame(gbm_init_GM_st$results) |> filter(
  n.trees == gbm_init_GM_st$bestTune$n.trees &
    interaction.depth == gbm_init_GM_st$bestTune$interaction.depth &
    n.minobsinnode == gbm_init_GM_st$bestTune$n.minobsinnode
)#/mean(init_train_st$Straw_DM)*100


init_data <- cbind(init_test,
                  "Straw_DM"=predict(gbm_init_GM_st, init_test),
                  "Grain_DM"=predict(gbm_init_GM, init_test))


write.table(init_data, "init_data.txt", sep = "\t")


plot_est <- init_data |> select(year,Sample_ID, Block,Straw_DM,Grain_DM) |> 
  bind_rows(
    sb_ask_complete |> select(year,Sample_ID, 
                              Block,
                              Crop,
                              Straw_DM,
                              Grain_DM, 
                              Straw_Rate,
                              Straw_C,
                              Slurry_C)
  ) |> mutate(
    Straw_Rate = ifelse(is.na(Straw_Rate), Straw_DM, Straw_Rate),
    Straw_C = ifelse(is.na(Straw_C), Straw_DM*0.423, Straw_C),
    Slurry_C = ifelse(is.na(Slurry_C), 0, Slurry_C),
    Crop = ifelse(is.na(Crop),"WinterWheat", paste(Crop))
    )

write.table(plot_est, "plot_est.txt", sep = "\t")

# End -------------
# 
# saveRDS(temp_yr, "temperature/temp_yr25.RDS")
# 
# temp30 <- all_temp %>%  filter(
#   between(date, as.Date("1966-01-01"), as.Date("2020-12-31")))
# 
# temp100 <- all_temp %>%  filter(
#   between(date, as.Date("1896-01-01"), as.Date("2020-12-31")))
# 
# write.table(
#   temp30$tem_ok,
#   "temperature\\temp55years.txt",
#   col.names = FALSE,
#   sep = "\t",
#   row.names = FALSE
# )
# 
# write.table(
#   temp100$tem_ok,
#   "temperature\\temp125years.txt",
#   col.names = FALSE,
#   sep = "\t",
#   row.names = FALSE
# )
# 
# # all_temp %>% pivot_longer(cols=c(dk_tem,fou_tem),names_to = "source") %>% 
# #   ggplot(aes(date, temp, group=source, fill=source, col=source)) +
# #   geom_point() 
# 
# 
# 
# 
#   
# 
