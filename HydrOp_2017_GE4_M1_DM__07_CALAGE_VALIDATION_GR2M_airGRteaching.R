################################################################################################################
# PIERRE BRIGODE - JANVIER 2018
#  DM MALPASSET
################################################################################################################


##################################################################################
# CHEMINS, FONCTIONS
##################################################################################

# Librairies
DIR_ROOT      <- "D:\\DATA\\"
library(airGR)
library(airGRteaching)
library(DtgImportExport)

# Chemins
DIR_PROJ      <- paste(DIR_ROOT, "14_ENSEIGNEMENTS\\2017-2018\\GE4-M1\\HYDROP\\DM\\", sep="")
DIR_DATA      <- paste(DIR_PROJ, "DATA\\", sep="")
DIR_OUT       <- paste(DIR_PROJ, "CALCULS\\", sep="")


##################################################################################
# LECTURE DES DONNEES
##################################################################################

# Données SAFRAN + BANQUE HYDRO
data_QPT   <- import(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__DATA_QPT.res", sep=""),quiet=TRUE)$data

# Données calculées par étudiants
data_PET   <- import(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__DATA_PET_BV.res", sep=""),quiet=TRUE)$data

# Fusion des tableaux
data_BVd   <- merge(x=data_PET, y=data_QPT[,c("Date", "E")], by.x="Date")


###########################################################################################
# AGGREGATION MENSUELLE
###########################################################################################

# Conversion from daily to monthly time-step
TimeFormat      <- "daily"
NewTimeFormat   <- "monthly"
ConvertFun      <- c("sum","sum", "mean", "sum")
TabSeries       <- data.frame(DateR   = as.POSIXlt(data_BVd$Date),
                              E       = data_BVd$E,
                              P       = data_BVd$P_BV,
                              T       = data_BVd$Tmean,
                              ETP     = data_BVd$ETP_O)
data_BVm        <- SeriesAggreg(TabSeries     = TabSeries,
                                TimeFormat    = TimeFormat,
                                NewTimeFormat = NewTimeFormat,
                                ConvertFun    = ConvertFun)
data_BVm$Date   <- as.Date(data_BVm$DateR)


###########################################################################################
# OPTIONS DE MODELISATION (aiGR)
###########################################################################################

# Paramètres de modélisation
nom_model     <- "GR2M"
nom_crit      <- "NSE"
date_deb_INI_CAL  <- as.Date("1992-01-01")
date_fin_INI_CAL  <- as.Date("1993-12-01")
date_deb_RUN_CAL  <- as.Date("1994-01-01")
date_fin_RUN_CAL  <- as.Date("2007-12-01")
date_deb_INI_VAL  <- as.Date("1980-01-01")
date_fin_INI_VAL  <- as.Date("1981-12-01")
date_deb_RUN_VAL  <- as.Date("1982-01-01")
date_fin_RUN_VAL  <- as.Date("1990-12-01")

# Préparation des données d'entrées
InputsModel  <- ObsGR(ObsBV=data_BVm[,c("DateR", "P", "ETP", "E")], HydroModel=nom_model, CemaNeige=FALSE)


###########################################################################################
# CALAGE
###########################################################################################

# Calage !
OutputsCalib <- CalGR(ObsGR=InputsModel, CalCrit=nom_crit, 
                      WupPer=c(date_deb_INI_CAL, date_fin_INI_CAL), 
                      CalPer=c(date_deb_RUN_CAL, date_fin_RUN_CAL), verbose=TRUE)
cat(paste("  Critère de performance en calage : ", round(OutputsCalib$OutputsCalib$CritFinal, 3), "\n", sep=""))
cat("\n")
Param           <- OutputsCalib$OutputsCalib$ParamFinalR

# Run du modèle (sur la période de calage)
OutputsModel_CAL  <- SimGR(ObsGR=InputsModel, Param=Param, EffCrit=nom_crit,
                           WupPer=c(date_deb_INI_CAL, date_fin_INI_CAL), 
                           SimPer=c(date_deb_RUN_CAL, date_fin_RUN_CAL),verbose=FALSE)


###########################################################################################
# VALIDATION
###########################################################################################

# Run du modèle (sur la période de validation)
OutputsModel_VAL  <- SimGR(ObsGR=InputsModel, Param=Param, EffCrit=nom_crit,
                           WupPer=c(date_deb_INI_VAL, date_fin_INI_VAL), 
                           SimPer=c(date_deb_RUN_VAL, date_fin_RUN_VAL),verbose=FALSE)

# Affichage des performances en validation
cat(paste("  Critère de performance en validation : ", round(OutputsModel_VAL$EffCrit$CritValue, 3), "\n", sep=""))
cat("\n")
