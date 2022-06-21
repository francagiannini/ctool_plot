
Cinp_spl <- readRDS("data/Cinp_spl.RDS")

#i=Cinp_spl[[1]]

# Data and input ----

data_build_plot <- function(i){
  #browser()
  #ID
  scn_name <- paste(unique(i$id), unique(i$scn_name), sep = "_")
  #data
  data <- as.data.frame(i)
  
  data_tbl <- cbind("Year"=data$year,
                    "Carbon deposited in the topsoil (t/ha)"=round(data$Ctop,9),
                    "C deposited in the subsoil (t/ha)"=round(data$Csub,9),
                    "C deposited in the topsoil from manure (tC ha-1)"=round(data$Cman,9),
                    "Cor-Atmospheric 14C  pM Plant"=0,
                    "Cor-Atmospheric 14C  pM Manure"=0)
  ########## data writing input------  
  my_input <- c(
    c(  "[Parameters]	
        PLoweLayer",	paste0(0.312)),
    "
      offset",	paste(0),
    "
      depth", paste(100),
    "
      PupperLayer",	paste(0.48),
    "
      Initial pMC(%)", paste(0),
    "
      Initial C(t/ha)", paste(unique(i$'Initial C(t/ha)')),
    "
      C/N", 	paste(unique(i$'C/N')),
    "
      Amended C", paste(0),
    "
      Crop
      [HUM]
      HUMdecompositionrate", 	paste(0.0028),
    "
      [FOM]
      FOMdecompositionrate", paste(0.12),
    "
      clayfraction", paste(unique(i$clayfraction)),
    "
      tF", paste(0.003),
    "
      [ROM]
      ROMfraction", paste(0.012),
    "
      ROMdecompositionrate", 	paste(3.858e-05),
    "
      Manure
      [HUM]	
      HUMdecompositionrate", 	paste(0.0028),
    "
      HumFraction", paste(0.192),
    "
      [FOM]
      FOMdecompositionrate", paste(0.12),
    "
      clayfraction", paste(unique(i$clayfraction)),
    "
      tF", paste(0),
    "
      [ROM]
      ROMfraction", paste(0),
    "
      ROMdecompositionrate", 	paste(0),
    "
      CropC14
      [HUM]	
      HUMdecompositionrate", 	paste(0.0028),
    "
      [FOM]
      FOMdecompositionrate", paste(0),
    "
      clayfraction", paste(unique(i$clayfraction)),
    "
      tF", paste(0),
    "
      [ROM]
      ROMfraction", paste(0),
    "
      ROMdecompositionrate", 	paste(0),
    "
      decay rate", 	paste(0),
    "
      ManureC14
      [HUM]	
      HUMdecompositionrate", 	paste(0.0028),
    "
      HumFraction", paste(0.192),
    "
      [FOM]
      FOMdecompositionrate", paste(0),
    "
      clayfraction", paste(unique(i$clayfraction)),
    "
      tF", paste(0),
    "
      [ROM]
      ROMfraction", paste(0),
    "
      ROMdecompositionrate", 	paste(0),
    "
      decay rate", 	paste(0),
    "
      [FOM]
      FOMfractionPlantTopLayer",paste(0.032)	,
    "
      FOMfractionPlantLowerLayer",	paste(0.003),
    "
      FOMfractionPlantTopLayerC14",	paste(0.032),
    "
      FOMfractionPlantLowerLayerC14",	paste(0.003),
    "
      [end] ")
  
  ############ func output----  
  
  data <- as.data.frame(data_tbl)
  
  input <- my_input
  #writeLines(my_input, sep = "\t")
  
  return(list(scn_name,data,input))
  
}

# Apply func to the list in this case

aver_plot <- lapply(Cinp_spl,data_build_plot)


saveRDS(aver_plot, "aver_plot.RDS")



