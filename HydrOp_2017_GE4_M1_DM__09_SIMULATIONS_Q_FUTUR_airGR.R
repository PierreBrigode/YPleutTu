################################################################################################################
# PIERRE BRIGODE - JANVIER 2018
#  DM MALPASSET
################################################################################################################


##################################################################################
# CHEMINS, FONCTIONS
##################################################################################

# Librairies
DIR_ROOT      <- "D:\\DATA\\"
library(sp)
library(rgdal)
library(readxl)
library(raster)
library(maptools)
library(deldir)
library(dismo)
library(rgeos)
library(RColorBrewer)
library(DtgImportExport)

# Chemins
DIR_PROJ      <- paste(DIR_ROOT, "14_ENSEIGNEMENTS\\2017-2018\\GE4-M1\\HYDROP\\DM\\", sep="")
DIR_DATA      <- paste(DIR_PROJ, "DATA\\", sep="")
DIR_OUT       <- paste(DIR_PROJ, "CALCULS\\", sep="")

# Fonctions
source(paste(DIR_ROOT, "03_FONCTIONS\\PB_00_FUNCTIONS.R", sep=""))


##################################################################################
# LECTURE DES DONNEES
##################################################################################

# Données calculées par étudiants
data_PET   <- import(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__P_ETP_FUTUR.res", sep=""), quiet=TRUE)$data

# Paramètres du modèle
tab_CAL   <- import(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__RES_CALAGE_GR2M_NSE.res", sep=""), quiet=TRUE)$data
Param     <- c(tab_CAL$Param_X1, tab_CAL$Param_X2)


###########################################################################################
# OPTIONS DE MODELISATION (aiGR)
###########################################################################################

# Paramètres de modélisation
nom_model         <- "GR2M"
nom_crit          <- "NSE"
date_deb_INI_SIM  <- as.Date("1992-01-01")
date_fin_INI_SIM  <- as.Date("1993-12-01")
date_deb_RUN_SIM  <- as.Date("1994-01-01")
date_fin_RUN_SIM  <- as.Date("2007-12-01")

# Paramètres airGR
FUN_CALIB  <- Calibration_Michel
runmod     <- get(paste("RunModel_", nom_model, sep=""))


###########################################################################################
# SIMULATION
###########################################################################################

# Périodes étudiées
ind_ini_SIM  <- which(data_PET$Date >= date_deb_INI_SIM & data_PET$Date <= date_fin_INI_SIM)
ind_run_SIM  <- which(data_PET$Date >= date_deb_RUN_SIM & data_PET$Date <= date_fin_RUN_SIM)

# Préparation des données d'entrées
InputsModel          <- CreateInputsModel(FUN_MOD   = runmod,
                                          DatesR    = as.POSIXct(data_PET$Date),
                                          Precip    = data_PET$P_BV,
                                          PotEvap   = data_PET$ETP_O,
                                          verbose   = TRUE)

# RunOptions
RunOptions       <- CreateRunOptions(FUN_MOD           = runmod,
                                     InputsModel       = InputsModel,
                                     IndPeriod_WarmUp  = ind_ini_SIM,
                                     IndPeriod_Run     = ind_run_SIM,
                                     verbose           = FALSE)

# Run du modèle
OutputsModel_SIM <- RunModel(InputsModel  = InputsModel,
                             RunOptions   = RunOptions,
                             Param        = Param,
                             FUN_MOD      = runmod)

# Export du tableau créé
tmp_OUT   <- data.frame(Date   = as.Date(OutputsModel_SIM$DatesR),
                        Qsim   = OutputsModel_SIM$Qsim,
                        P_BV   = OutputsModel_SIM$Precip,
                        ETP_O  = OutputsModel_SIM$PotEvap)
suppressWarnings(
  export(file    = paste(DIR_OUT, "Y5325010_Reyran@Fréjus__SIM_QFUTUR_", nom_model, "_", nom_crit, ".res", sep=""),
         data    = tmp_OUT,
         quiet   = TRUE,
         digits  = 10,
         dec     = 4,
         comment = c(paste("Date de calcul : ", Sys.Date(), sep="")
         )))


