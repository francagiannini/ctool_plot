library(tidyverse)
library(lme4)
library(nlme)
library(agricolae)
library(ggpubr)

#### output ----
out_tbl <- readRDS("data/ctool_plot_mon.RDS") |> separate(
  ID, 
  c('id', 'InitSoilC','Allo','Initialization', 'HI', 'Ccont', 'Eps', 'Plot'),
  sep = "_")  |>  rename(
    totalC_topsoil='total(1,1)',
    totalC_subsoil='total(2,1)'
  ) |> mutate(
    SoilC_tot=totalC_topsoil+totalC_subsoil,
    CO2_tot= Foml1+Foml2+Huml1+Huml2+Roml1+Roml2,
    transport_tot=Fom+Hum+Rom
  )

# Validation data ----
info.plot <- plot_est |> filter(year==2010)|> select(Sample_ID , Straw_Rate, Block) 

out_tbl <- merge(out_tbl, info.plot, 
                 by.x = 'Plot', by.y = 'Sample_ID'#, 
                 #incomparables = NULL
                 )

obs <- readxl::read_excel("data/Soil_parameters_16052022.xlsx", 
                          sheet = 'Soil_C_N_take') |> 
  pivot_longer(cols=c(starts_with("C_"),starts_with("N_")),
               names_sep =  "_",
               names_to = c('variable','year')#,
              #values_to =c('C','N')
              ) |> 
  pivot_wider(names_from = 'variable',
              values_from = 'value') |> 
  mutate(Topsoil_C_obs=((0.20*10000)*Bulk_Density_2020)*(C/100),
         CN=C/N)

obs |>  group_by(year,Straw_Rate) |> 
  summarise(mean = mean(Topsoil_C_obs),
            sd=sd(Topsoil_C_obs)#/sqrt(n())
  ) |> 
  ggplot(aes(y=mean, x=as.numeric(year), col=interaction(Straw_Rate))) +
  scale_x_continuous(breaks = as.vector(as.numeric(unique(obs$year))) ,minor_breaks = NULL)+
  geom_point() +
  geom_line()+
  geom_errorbar(
    aes(ymin=mean-sd, ymax=mean+sd), width=.2,
    position=position_dodge(0.05))+
  #geom_vline(xintercept = c(1987,2003,2013), linetype="dashed") +
  theme_bw()+
  labs(y="Topsoil C Observed (Mg/ha)", x="year")+
  theme(axis.text.x = element_text(angle = 90))


obs_sum <- obs |> group_by(Block) |> summarise_if(is.numeric, mean, na.rm = TRUE) 

obs <- obs|> select(year, Sample_ID ,Topsoil_C_obs)

# Comparison ----

out_comp <- merge(out_tbl,obs, 
                      by.x=c('Plot', 'year'), by.y=c('Sample_ID','year'),
                      all.x = TRUE
                      )


#### Long term scatter -----

out_comp|> 
  ggplot(aes(y=totalC_topsoil, x=year, col=as.factor(Straw_Rate)))+
  geom_point()+
  scale_x_continuous(breaks=seq(1951,2019,14))+
  geom_smooth() +
  theme_bw()+
  facet_grid(InitSoilC~.)+
  ylab("Topsoil soil C [ Mg/ha m] (C topsoil + C subsoil)")
  

# out_complete|> ggplot(aes(y=Topsoil_C_obs, x=as.factor(year), 
#                           col=as.factor(Straw_Rate)))+
#   geom_boxplot()+
#   scale_x_continuous(breaks=seq(1951,2019,14)
#                      )+
#   #geom_smooth()+
#   theme_bw()+ ylab("Topsoil soil C [ Mg/ha m] (C topsoil + C subsoil)")

out_comp |> ggplot(aes(y=totalC_topsoil, x=year, col=as.factor(Straw_Rate)))+
  geom_point()+
  scale_x_continuous(breaks=seq(1866,2020,14))+
  geom_point(aes(y=Topsoil_C_obs, shape=as.factor(Straw_Rate)), col="black")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "Observed"
                                         #breaks =seq(40,180,20),
  ))+
  #facet_grid(Initialization + HI ~ Ccont)+
  #geom_smooth()+
  #geom_smooth((aes(y=Topsoil_C_obs)))+
  theme_bw()+ylab("Topsoil Total C [Mg/ha m]")

library(ggpubr)
library(ggpmisc)

out_comp |> 
  ggplot(aes(x=totalC_topsoil, y=Topsoil_C_obs,
                           group=interaction(Allo,Straw_Rate),
             col=as.factor(Straw_Rate)
                           ))+
 #stat_poly_line() +
 #stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
 geom_point()+
 geom_abline(intercept = 0, slope= 1)+
 theme_bw()#+
 #facet_grid(Allo+Eps+HI~InitSoilC+Block) 
 #facet_grid(Allo+Eps~InitSoilC) 
 #facet_grid(Allo+Eps+HI~InitSoilC+Ccont+Initialization) #

out_comp|> dplyr::filter(Allo == 'NotFixed' | InitSoilC=='opt') |> 
  ggplot(aes(x=totalC_topsoil, y=Topsoil_C_obs#,
             #col=interaction(Straw_Rate)
  ))+
  stat_poly_line() +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
  geom_point()+
  geom_abline(intercept = 0, slope= 1)+
  theme_bw()+
  scale_x_continuous(limits = c(30,60))+
  scale_y_continuous(limits = c(30,60))+
  facet_grid(Eps~Block+HI#.#InitSoilC#+
             )


saveRDS(out_complete,"out_complete.RDS")

write.table(out_complete,"out_complete.txt", sep = "\t", dec=".")

summ <- out_comp |> drop_na(Topsoil_C_obs) |> 
  mutate(residual=totalC_topsoil-Topsoil_C_obs
         ) |>
  
  group_by(InitSoilC,Allo,Initialization, HI, Ccont, Eps
           ,Straw_Rate
           , Block
           ) |>
  
  dplyr::summarise(
    
    mean_obs=mean(Topsoil_C_obs),
    mean_est=mean(totalC_topsoil),
    
    RMSE=sqrt(mean(residual^2)),
    
    RMSE_rel=(sqrt(mean(residual^2)))/mean(Topsoil_C_obs)*100,
    
    #AVE=sum
      #1-(sum(residual^2)/sum((totalC_topsoil-mean_obs)^2)),
    
    cor=cor(totalC_topsoil,Topsoil_C_obs),
    
    n=n()
    
    ) |> unique()

summ |> ggplot(aes(RMSE_rel, fill=interaction(InitSoilC)))+
  geom_density(alpha=0.5)+
  theme_bw()+
  facet_grid(Eps~Block)

summ |> filter(cor>0) |> #filter(!Block==2) |>  
  ggplot(aes(RMSE_rel, cor, col=interaction(InitSoilC)))+
  geom_point()+
  geom_smooth()

write.table(summ, "summary_outputs.txt", sep="\t")
# End ----

# ##### VCA ----
# # Total amount of C (Mg ha-1m-1) in topsoil out_tbl$`total(1,1)`
# 
# SOC_vca<-lme4::lmer(
#   totalC_subsoil~1+year+
#     (1|soilCinit)+(1|soiltype)+(1|croprot_man)+(1|inittime)+(1|initC)
#   ,na.action=na.omit
#   ,REML=T
#   ,control=lmerControl(optimizer="bobyqa")
#   ,data=out_a)
# 
# summary(SOC_vca)
# 
# vca <- as.data.frame(VarCorr(SOC_vca)) 
# 
# vca |> group_by(grp) |> summarise(
#   varprop = vcov / sum(vca$vcov) * 100) |> arrange(
#     varprop, grp)
# 
# options(contrasts=c("contr.sum", "contr.poly"))
# 
# 
# ##### Linear model SOC----
# 
# SoilC_reg <- lm(
#   totalC_topsoil~year+soiltype+soilCinit+croprot_man+inittime+initC+
#     year*initC+year*soilCinit+year*croprot_man+year*croprot_man, #+year*inittime+year*soiltype,
#   data = out_a)
# 
# anova(SoilC_reg)
# summary(SoilC_reg)
# 
# LSD.test(SoilC_reg, "croprot",console = TRUE)
# 
# # SoilC_reg <- lme4::lmer(
# #   SoilC_tot~soiltype+croprot_man+inittime+initC+
# #     soilCinit*croprot_man+soiltype*soilCinit+year*croprot_man+(1|soilCinit),
# #   data = out_a)
# # 
# # anova(SoilC_reg)
# # summary(SoilC_reg)
# 
# 
# ##### time serie decompose ----
# 
# ts_SOC <- out_a |> filter(croprot_man=="Organic")
#   #group_by(id)
# 
# y <- ts_SOC |> filter(id==" 37") 
# 
# ts_soc <-ts(y$SoilC_tot,  frequency = 5)
# 
# plot(ts_soc)
# 
# ts_soc_d <- decompose(ts_soc)
# 
# plot(ts_soc_d)
# 
# 
# ##### Difference final and initial situation ----
# 
# #obs_val <- out_a |> filter(id=="  1")
# 
# dif <- out_tbl_a |>  filter(
#   year == 1995 | year== 1996 |year == 2020) |> pivot_wider(
#     names_from = year, 
#     values_from = c(totalC_topsoil,CO2_tot,transport_tot)) |> 
#   group_by(id) |> 
#   mutate(dif_topC1995 =
#           mean(`totalC_topsoil_2020`,na.rm = T)-mean(`totalC_topsoil_1995`,na.rm = T),
#         dif_top1996=
#           mean(`totalC_topsoil_2020`,na.rm = T)-mean(`totalC_topsoil_1996`,na.rm = T),
#         change_SoilC=
#           (mean(`totalC_topsoil_2020`,na.rm = T)-mean(`totalC_topsoil_1996`,na.rm = T))/(1996-2020),
#         dif_CO2 =
#           mean(`CO2_tot_2020`,na.rm = T)-mean(`CO2_tot_1995`,na.rm =T ),
#         dif_trans =
#           mean(transport_tot_2020,na.rm = T)-mean(transport_tot_1995,na.rm = T)
#       ) |> filter(!is.na(totalC_topsoil_2020)
#       )
# 
# write.table(dif,"dif.txt", sep="\t", dec=".")
# 
# 
# 
# dif |> group_by(soiltype,soilCinit,croprot_man,initC) |> 
#   summarize(dif_C=mean(dif_topC1995),
#             sd=sd(dif_topC1995)) |> 
#   ggplot(aes(y = dif_C, x = soiltype ,
#               colour = soilCinit,
#               fill = soilCinit
#             )) +
#   geom_bar(stat = "identity", position = "dodge", alpha=0.5) +
#   facet_grid(croprot_man ~ initC) +
#   geom_errorbar(aes(ymin=dif_C-sd, ymax=dif_C+sd), 
#                 position = position_dodge(0.9), width = 0.25)+
#   theme_bw()+
#   theme(panel.grid = element_blank())
# 
# 
# dif |> ggplot(aes(y=dif_topC1995, x=initC, 
#                    fill=croprot_man)) +
#   geom_boxplot()+
#   scale_fill_manual(name="Crop rotation", 
#      values=c("#15863E","#55E88B", "#EF8D64" ))+
#   theme_bw()
# 
# difC_vca<-lme4::lmer(
#   dif_top1995~1+(1|soilCinit)+(1|soiltype)+(1|croprot)+(1|inittime)+(1|initC)
#   ,na.action=na.omit
#   ,REML=T
#   ,control=lmerControl(optimizer="bobyqa")
#   ,data=dif)
# 
# summary(difC_vca)
# 
# vca <- as.data.frame(VarCorr(difC_vca)) 
# 
# vca |> group_by(grp) |> summarise(
#   varprop = vcov / sum(vca$vcov)*100) |> arrange(
#     varprop, grp)
# 
# 
# # dif |> ggplot(aes(dif_CO2, fill=croprot)) +geom_histogram()+theme_bw()
# # dif |> ggplot(aes(dif_trans, fill=soiltype)) +geom_histogram()+theme_bw()
# 
# 
# 
# 
# # stl(ts_soc, s.window ="periodic", t.window=5)
# # 
# # ts_SOC |> filter(id=="  8") |> select(year,SoilC_tot)|> ets() 
# #   forecast() |>
# #   autoplot()
# # 
# # 
# # ts_SOC |> group_by(id) |>  time_decompose(SoilC_tot, method = "stl")|> autoplot()
# # 
# # autoplot(decompose(ts_SOC, type = "additive"))+
# #   labs(y=expression(Chl[a]~(µg/L)), x="Year") + 
# #   ggtitle(expression(Decomposed~Chl[a]~Time~Series~BB1)) +
# #   theme(
# #     plot.title=element_text(hjust=0.5),
# #     text = element_text(family = "Times New Roman", size = 15)
# #   )
# # 
# 
# 
# #LSD.test(SoilC_reg, "croprot_man",console = TRUE)
# 
# 
# out_a |> dplyr::filter(soilCinit=="Mediuminit") |> ggplot(
#   aes(y=SoilC_tot, x=croprot_man, fill= soiltype))+
#   geom_boxplot()+theme_bw()
# 
# 
# #### CO2 -----
# temp <- read.table("temperature/temp55years.txt")
# colnames(temp) <- "temp"
# out_a <- cbind(out_a,"temp"=rep(tail(temp,25)$temp, times=486))
# 
# out_a |> ggplot(aes(y=CO2_tot, x=year, 
#                      group=croprot_man, soilCinit,
#                      col=croprot_man))+
#   geom_point(size=0.5)+
#   geom_smooth()+
#   geom_line(aes(y=(temp/100)+0.2), col="black")+
#   scale_color_manual(name="Crop rotation", 
#                      values=c("#15863E","#55E88B", "#EF8D64" ))+
#   scale_x_continuous(breaks=seq(1996,2020,4))+
#   scale_y_continuous(breaks =seq(0,0.5,0.06)#,
#                      #sec.axis = sec_axis(~(.*20)-0.2, name="Temp")
#                      )+
#   facet_grid(initC~soilCinit)+
#   theme_bw()+ylab("CO_2")
# 
# 
# CO2_vca<-lme4::lmer(
#   CO2_tot~1+year+(1|temp)+(1|soilCinit)+(1|soiltype)+(1|croprot_man)+(1|inittime)+(1|initC)
#   ,na.action=na.omit
#   ,REML=T
#   ,control=lmerControl(optimizer="bobyqa")
#   ,data=out_a)
# 
# summary(CO2_vca)
# 
# vca <- as.data.frame(VarCorr(CO2_vca)) 
# 
# vca |> group_by(grp) |> summarise(
#   varprop = vcov / sum(vca$vcov)*100) |> arrange(
#     varprop, grp)
# 
# 
# #### Transport ----
# 
# 
# 
# 
# out_a |> ggplot(aes(y=transport_tot, x=year, 
#                      group=croprot_man, soilCinit,
#                      col=croprot_man))+
#   geom_point(size=0.5)+
#   geom_smooth()+
#   geom_line(aes(y=(temp/1000)+0.02), col="black")+
#   scale_color_manual(name="Crop rotation", 
#                      values=c("#15863E","#55E88B", "#EF8D64" ))+
#   scale_x_continuous(breaks=seq(1996,2020,4))+
#   #scale_y_continuous(sec.axis = sec_axis(~(.*1000)-0.02, name="Temp"))+
#   facet_grid(initC~soilCinit)+
#   theme_bw()+ylab("Transport C")
# 
# 
# 
# trans_vca<-lme4::lmer(
#   transport_tot~1+year+
#     +(1|temp)+(1|soilCinit)+(1|soiltype)+(1|croprot)+(1|inittime)+(1|initC)
#   ,na.action=na.omit
#   ,REML=T
#   ,control=lmerControl(optimizer="bobyqa")
#   ,data=out_a)
# 
# summary(trans_vca)
# 
# vca <- as.data.frame(VarCorr(trans_vca)) 
# 
# vca |> group_by(grp) |> summarise(
#   varprop = vcov / sum(vca$vcov)*100) |> arrange(
#     varprop, grp)
# 
# out_a |> ggplot(
#   aes(y=transport_tot, x=croprot_man, fill= initC))+
#   geom_boxplot() + theme_bw()
# 
# 
# 
# 
# ##### Relations between outputs
# 
# out_a |> 
#   select(totalC_topsoil,totalC_subsoil,transport_tot,CO2_tot, 
#          `Carbon deposited in the topsoil (t/ha)`) |> 
#   pairs()
# 
# #### input----
# 
# tbl_fill <- readRDS("tbl_fill.RDS")
# 
# out_a |> group_by(croprot) |> summarise(
#   Ctopsum=sum(`Carbon deposited in the topsoil (t/ha)`),
#   Ctopmean=mean(`Carbon deposited in the topsoil (t/ha)`),
#   Ctopsd=sd(`Carbon deposited in the topsoil (t/ha)`),
#   
#   Csubsum=sum(`C deposited in the subsoil (t/ha)`),
#   Csubmean=mean(`C deposited in the subsoil (t/ha)`),
#   Csubsd=sd(`C deposited in the subsoil (t/ha)`),
#   
#   Cmansum=sum(`C deposited in the topsoil from manure (tC ha-1)`),
#   Cmanmean=mean(`C deposited in the topsoil from manure (tC ha-1)`),
#   Cmansd=sd(`C deposited in the topsoil from manure (tC ha-1)`)
# )
