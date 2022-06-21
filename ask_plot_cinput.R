library(tidyverse)

plot_est <- read.table("plot_est.txt", sep = "\t", header= T) 

scn_plot <- expand.grid(
  "InitSoilC"= c('ave','st0','st4','opt'),
  "Allo"= c("Fixed", "NotFixed"),
  "t.Init" =c("Fixed", "estimated"),
  "t.HI"=c("HI.calc", "HI.st"),
  "t.Ccont"=c("ccont.st", "ccont.bent"),
  "t.Eps" =c("Eps.st", "Eps.are"),
  "plot"=unique(plot_est$Sample_ID)#, 
                        #"year"=unique(plot_est$year)#,
                        #"Allo"= c("Allo.yes","Allo.est")
) |> filter(Allo=='NotFixed' | 
           Allo=='Fixed' & t.Init=='Fixed' & t.HI=='HI.st'& t.Eps=='Eps.are'& t.Ccont=='ccont.st')|> 
  mutate(id = row_number(),
            scn_name = 
              paste(InitSoilC, Allo,t.Init,t.HI,t.Ccont,t.Eps,as.factor(plot), sep = "_"))


plot_param <- merge(x=scn_plot, y=plot_est, by.x = 'plot', by.y = 'Sample_ID')|>
  #Inital scenarios that modifies Straw and Grain Yield 
  mutate(
    Straw_DM=as.numeric(ifelse(t.Init=='Fixed',
                               ifelse(between(year, 1951,1980),
                                      3.242505,
                                      paste(Straw_DM)),
                               paste(Straw_DM))),
    
    Grain_DM=as.numeric(ifelse(t.Init=='Fixed',
                               ifelse(between(year, 1951,1980),
                                      3.361815,
                                      paste(Grain_DM)),
                               paste(Grain_DM))))|> 
  #Allometrics definition
  mutate(
    HI =ifelse(t.HI=='HI.calc', 
               Grain_DM / (Straw_DM + Grain_DM), 
               0.45),
    Eps = ifelse(t.Eps=='Eps.st',
                 as.numeric(recode(Crop,
                                   "SpringBarley" = '0.8', 
                                   'SpringWheat' = '0.7', 
                                   'WinterWheat' = '0.7')), 
                 0.91),
    ccont =ifelse(t.Ccont=='ccony.st', 
                  0.45,
                  as.numeric(recode(Crop,
                                    "SpringBarley" = '0.423',
                                    'SpringWheat' = '0.424',
                                    'WinterWheat' = '0.424'))),
    Fre = as.numeric(
      recode(Crop,
             "SpringBarley" = '0.17',
             'SpringWheat' = '0.25',
             'WinterWheat' = '0.25')),
    
    'Initial C(t/ha)' = as.numeric(as.character(
      recode(InitSoilC,
        'ave' = '112.5',
        'st0' = '102.7',
        'st4' = '106.1',
        'opt'='93')))
    ,
      #       "201"="86.2",	"206"="83.4",	"208"="85.1",
      #       "301"="83.5",	"306"="77.7",	"308"="80.9",
      #       "601"="83.0",	"606"="85.6",	"608"="83.5",
      #       "701"="79.8",	"706"="81.4",	"708"="80.3"
       
    "C/N" = 11.19048
      # as.numeric(recode(plot,
      #     "201"="11.34572034",	"206"="11.0388871",	"208"="11.25273805",	
      #     "301"="11.67764513",	"306"="11.41472226",	"308"="10.94947204",	
      #     "601"="11.2681818",	"606"="11.36176364",	"608"="11.4498992",	
      #     "701"="11.24119399",	"706"="11.11457574",	"708"="10.97455757"
      #                         ))
    ,
    "clayfraction" = as.numeric(recode(plot,
                                       "201"="0.117",	"206"="0.115",	"208"="0.125",	
                                       "301"="0.110",	"306"="0.120",	"308"="0.125",	
                                       "601"="0.121",	"606"="0.109",	"608"="0.120",	
                                       "701"="0.117",	"706"="0.109",	"708"="0.119"
    ))
    
  )


plot_Cinp <- plot_param |> mutate(
  Ctop =ifelse(Allo=='NotFixed',
               Straw_C + Eps * (Fre / ((1 - Fre) * HI) * ((Straw_DM + Grain_DM) * ccont)),
               2.28),# Cres + Cbelow * ep
  
  Csub = ifelse(Allo=='NotFixed',
    (1 - Eps) * (Fre /(( 1 - Fre ) * HI) * ((Straw_DM + Grain_DM) * ccont)),
    0.12), 
  Cman = Slurry_C
  
)

summary(plot_Cinp)
#check
table(plot_Cinp$year)==nrow(scn_plot)
table(plot_Cinp$id)==length(unique(plot_Cinp$year))

plot_Cinp |> ggplot(aes(x=year,y=Ctop, col=interaction(t.Init,Allo)))+
  geom_point()+
  #geom_smooth()+
  theme_bw()

plot_Cinp |> 
  pivot_longer(cols = c(Ctop,Csub), names_to = "Variable") |> 
  group_by(year,Variable,t.Init,t.HI,t.Ccont,t.Eps,plot,Allo, InitSoilC) |> 
  summarise(mean = mean(value),
            sdDM=sd(value)#/sqrt(n())
  ) |> 
  ggplot(aes(y=mean, x=year, col=interaction(Variable,t.Eps))) +
  scale_x_continuous(breaks = seq(1951,2019,1), minor_breaks = NULL)+
  geom_point() +
  #geom_line()+
  # geom_errorbar(
  #   aes(ymin=meanDM-sdDM, ymax=meanDM+sdDM), width=.2,
  #   position=position_dodge(0.05))+
  # geom_vline(xintercept = c(1987,2003,2013), linetype="dashed") +
 labs(y="C Input (Mg/ha)")+
  theme(axis.text.x = element_text(angle = 90), 
        panel.background = NULL)

write.table(plot_Cinp, "plot_Cinp.txt", sep = "\t")

write.csv(plot_Cinp, "plot_Cinp.csv")

Cinp_spl <- plot_Cinp |>
  group_split(scn_name) #|>
  #setNames(unique(plot_Cinp$scn_name))

saveRDS(Cinp_spl, "Cinp_spl.RDS")


