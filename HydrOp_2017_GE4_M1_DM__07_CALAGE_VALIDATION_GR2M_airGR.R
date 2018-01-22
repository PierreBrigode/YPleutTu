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

# Paramètres airGR
FUN_CALIB  <- Calibration_Michel
runmod     <- get(paste("RunModel_", nom_model, sep=""))
funcrit    <- get(paste("ErrorCrit_", nom_crit, sep=""))


###########################################################################################
# CALAGE
###########################################################################################

# Périodes étudiées
ind_ini_CAL   <- which(data_BVm$Date >= date_deb_INI_CAL & data_BVm$Date <= date_fin_INI_CAL)
ind_run_CAL   <- which(data_BVm$Date >= date_deb_RUN_CAL & data_BVm$Date <= date_fin_RUN_CAL)
ind_lac       <- !is.na(data_BVm$E[ind_run_CAL])
cat(paste("Nombre de lacunes sur la période de calage : ", length(which(!ind_lac)), "\n", sep=""))

# Préparation des données d'entrées
InputsModel          <- CreateInputsModel(FUN_MOD   = runmod,
                                          DatesR    = data_BVm$DateR,
                                          Precip    = data_BVm$P,
                                          PotEvap   = data_BVm$ETP,
                                          verbose   = TRUE)

# RunOptions (spécification des périodes d'initialisation et de calage)
RunOptions       <- CreateRunOptions(FUN_MOD           = runmod,
                                     InputsModel       = InputsModel,
                                     IndPeriod_WarmUp  = ind_ini_CAL,
                                     IndPeriod_Run     = ind_run_CAL,
                                     verbose           = FALSE)

# Critères de calage
InputsCrit       <- CreateInputsCrit(FUN_CRIT          = funcrit, 
                                     InputsModel       = InputsModel,
                                     RunOptions        = RunOptions,
                                     Qobs              = data_BVm$E[ind_run_CAL],
                                     BoolCrit          = ind_lac)
CalibOptions     <- CreateCalibOptions(FUN_MOD         = runmod,
                                       FUN_CALIB       = FUN_CALIB)

# Calage !
OutputsCalib    <- Calibration(InputsModel   = InputsModel,
                               RunOptions    = RunOptions,
                               InputsCrit    = InputsCrit,
                               CalibOptions  = CalibOptions,
                               FUN_MOD       = runmod,
                               FUN_CRIT      = funcrit,
                               FUN_CALIB     = FUN_CALIB,
                               verbose       = TRUE)
cat(paste("  Critère de performance en calage : ", round(OutputsCalib$CritFinal, 3), "\n", sep=""))
cat("\n")
Param      <- OutputsCalib$ParamFinalR
crit_CAL   <- OutputsCalib$CritFinal

# Export du jeu de paramètres obtenu
tab_RES    <- data.frame(Date_deb_cal = date_deb_RUN_CAL, 
                         Date_fin_cal = date_fin_RUN_CAL,
                         Model        = nom_model,
                         Nom_crit_cal = nom_crit,
                         Crit_valeur  = crit_CAL,
                         Param_X1     = Param[1],
                         Param_X2     = Param[2])
suppressWarnings(
  export(file    = paste(DIR_OUT, "Y5325010_Reyran@Fréjus__RES_CALAGE_", nom_model, "_", nom_crit, ".res", sep=""),
         data    = tab_RES,
         quiet   = TRUE,
         digits  = 10,
         dec     = 4,
         comment = c(paste("Date de calcul : ", Sys.Date(), sep="")
         )))

# Run du modèle (sur la période de calage)
OutputsModel_CAL <- RunModel(InputsModel  = InputsModel,
                             RunOptions   = RunOptions,
                             Param        = Param,
                             FUN_MOD      = runmod)
# plot(OutputsModel_CAL, Qobs=data_BVm$E[ind_run_CAL])


###########################################################################################
# VALIDATION
###########################################################################################

# Périodes étudiées
ind_ini_VAL   <- which(data_BVm$Date >= date_deb_INI_VAL & data_BVm$Date <= date_fin_INI_VAL)
ind_run_VAL   <- which(data_BVm$Date >= date_deb_RUN_VAL & data_BVm$Date <= date_fin_RUN_VAL)
ind_lac       <- !is.na(data_BVm$E[ind_run_VAL])
cat(paste("Nombre de lacunes sur la période de validation : ", length(which(!ind_lac)), "\n", sep=""))

# RunOptions (spécification des périodes d'initialisation et de validation)
RunOptions       <- CreateRunOptions(FUN_MOD           = runmod,
                                     InputsModel       = InputsModel,
                                     IndPeriod_WarmUp  = ind_ini_VAL,
                                     IndPeriod_Run     = ind_run_VAL,
                                     verbose           = FALSE)

# Run du modèle (sur la période de validation)
OutputsModel_VAL <- RunModel(InputsModel  = InputsModel,
                             RunOptions   = RunOptions,
                             Param        = Param,
                             FUN_MOD      = runmod)
# plot(OutputsModel_VAL, Qobs=data_BVm$E[ind_run_VAL])

# Calcul des performances en validation
InputsCrit       <- CreateInputsCrit(FUN_CRIT          = funcrit, 
                                     InputsModel       = InputsModel,
                                     RunOptions        = RunOptions,
                                     Qobs              = data_BVm$E[ind_run_VAL],
                                     BoolCrit          = ind_lac)
crit_VAL  <- ErrorCrit(InputsCrit=InputsCrit, OutputsModel=OutputsModel_VAL, FUN_CRIT=funcrit, verbose=FALSE)
cat(paste("  Critère de performance en validation : ", round(crit_VAL$CritValue, 3), "\n", sep=""))
cat("\n")


###########################################################################################
# EXPORT DE LA SIMULATION SUR L'ENSEMBLE DE LA PERIODE
###########################################################################################

# Périodes étudiées
ind_ini_SIM   <- 1:12
ind_run_SIM   <- 13:length(data_BVm$Date)

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
  export(file    = paste(DIR_OUT, "Y5325010_Reyran@Fréjus__SIM_Q_", nom_model, "_", nom_crit, ".res", sep=""),
         data    = tmp_OUT,
         quiet   = TRUE,
         digits  = 10,
         dec     = 4,
         comment = c(paste("Date de calcul : ", Sys.Date(), sep=""),
                     paste(" Période de calage : ", date_deb_RUN_CAL, " - ", date_fin_RUN_CAL, sep="")
         )))


###########################################################################################
# FIGURE
###########################################################################################

# Paramètres graphiques
col_OBS   <- "black"
col_SIM   <- "cornflowerblue"
ylim      <- c(0,220)
ind_date  <- which(as.integer(format(data_BVm$Date, "%m")) == 1) 

# PNG : Performances en calage
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG11_PERFORMANCES_CALAGE.png", sep=""), 
    width=24, height=12, units="cm", res=600)
xlim      <- range(c(date_deb_RUN_CAL, date_fin_RUN_CAL))
par(mar=c(3,4,2,1), mgp=c(3,0.5,0))
plot(x=0, type="n", xlab="", ylab="", axes=FALSE, xlim=xlim, ylim=ylim, xaxs="i")
grid(nx=NA, ny=NULL)
abline(v=data_BVm$Date[ind_date], col="lightgray", lty=3)
axis(1, at=data_BVm$Date[ind_date], labels=format(data_BVm$Date[ind_date], "%Y"), cex.axis=1)
axis(2, cex.axis=1)
mtext(side=2, "E [mm/m]", line=2, cex=1.2)
mtext(side=3, paste(nom_crit, " (calage) = ", round(crit_CAL, 3), sep=""), line=0, cex=1.5, font=2)
lines(x=as.Date(data_BVm$DateR), y=data_BVm$E, col=col_OBS, lwd=2)
points(x=as.Date(data_BVm$DateR), y=data_BVm$E, col=col_OBS, pch=16, cex=1.2)
lines(x=as.Date(OutputsModel_CAL$DatesR), y=OutputsModel_CAL$Qsim, col=col_SIM, lwd=1)
box()
graphics.off()


# PNG : Performances en validation
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG12_PERFORMANCES_VALIDATION.png", sep=""), 
    width=24, height=12, units="cm", res=600)
xlim      <- range(c(date_deb_RUN_VAL, date_fin_RUN_VAL))
par(mar=c(3,4,2,1), mgp=c(3,0.5,0))
plot(x=0, type="n", xlab="", ylab="", axes=FALSE, xlim=xlim, ylim=ylim, xaxs="i")
grid(nx=NA, ny=NULL)
abline(v=data_BVm$Date[ind_date], col="lightgray", lty=3)
axis(1, at=data_BVm$Date[ind_date], labels=format(data_BVm$Date[ind_date], "%Y"), cex.axis=1)
axis(2, cex.axis=1)
mtext(side=2, "E [mm/m]", line=2, cex=1.2)
mtext(side=3, paste(nom_crit, " (validation) = ", round(crit_VAL$CritValue, 3), sep=""), line=0, cex=1.5, font=2)
lines(x=as.Date(data_BVm$DateR), y=data_BVm$E, col=col_OBS, lwd=2)
points(x=as.Date(data_BVm$DateR), y=data_BVm$E, col=col_OBS, pch=16, cex=1.2)
lines(x=as.Date(OutputsModel_VAL$DatesR), y=OutputsModel_VAL$Qsim, col=col_SIM, lwd=1)
box()
graphics.off()

