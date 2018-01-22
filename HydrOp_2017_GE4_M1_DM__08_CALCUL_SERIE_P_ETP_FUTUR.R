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
data_PET   <- import(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__SIM_Q_GR2M_NSE.res", sep=""), quiet=TRUE)$data

# Tableau de deltas climatiques
tab_deltas  <- read_excel(paste(DIR_DATA, "_DELTAS_CLIMATIQUES_1992-2007_2027-2033.xlsx", sep=""))
tab_deltas  <- as.data.frame(tab_deltas)
delta_m_ETP <- as.numeric(tab_deltas[1,-1])
delta_m_P   <- as.numeric(tab_deltas[2,-1])


##################################################################################
# CREATION DES SERIES DU FUTUR
##################################################################################

# Découpage sur la période de référence
data_PET$An  <- as.integer(format(data_PET$Date, "%Y"))
ind          <- which(data_PET$An >= 1992 & data_PET$An <= 2007)
data_PET     <- data_PET[ind, c("Date", "P_BV", "ETP_O")]

# Applications des deltas
data_PET_FUTUR         <- data_PET
data_PET_FUTUR$Mois_i  <- as.integer(format(data_PET_FUTUR$Date, "%m"))
data_PET_FUTUR$ETP_O   <- data_PET_FUTUR$ETP_O + delta_m_ETP[data_PET_FUTUR$Mois_i]
data_PET_FUTUR$P_BV    <- data_PET_FUTUR$P_BV + delta_m_P[data_PET_FUTUR$Mois_i]

# Mise à 0 des pluies mensuelles inférieures à 0
ind                       <- which(data_PET_FUTUR$P_BV < 0)
data_PET_FUTUR$P_BV[ind]  = 0

# Découpage du tableau
data_PET_FUTUR            <- data_PET_FUTUR[,c("Date", "P_BV", "ETP_O")]

# Export du tableau créé
suppressWarnings(
  export(file    = paste(DIR_OUT, "Y5325010_Reyran@Fréjus__P_ETP_FUTUR.res", sep=""),
         data    = data_PET_FUTUR,
         quiet   = TRUE,
         digits  = 10,
         dec     = 3,
         comment = c(paste("Date de mise en forme : ", Sys.Date(), sep="")
         )))


##################################################################################
# FIGURE ILLUSTRATIVE : REGIME
##################################################################################

# Calculs
reg_TP  <- aggregate(data_PET[,-1], by=list(format(data_PET$Date, "%m")), FUN=mean)
reg_TF  <- aggregate(data_PET_FUTUR[,-1], by=list(format(data_PET_FUTUR$Date, "%m")), FUN=mean)

# Paramètres graphiques
col_OBS   <- "black"
col_SIM   <- "cornflowerblue"

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG13_CHANGEMENTS_CLIMATIQUES.png", sep=""),
    width=30, height=15, units="cm", res=300)
mat     <- matrix(c(01,02), ncol=2, byrow=TRUE)
layout(mat)

# P
ylim   <- range(c(reg_TP$P_BV, reg_TF$P_BV))
par(mar=c(3,4,2,1), mgp=c(3,0.5,0))
plot(x=0, type="n", xlim=c(1,12), ylim=ylim, axes=FALSE, xlab="", ylab="")
grid()
axis(1, at=1:12, labels=substr(month.abb,1,1))
axis(2)
mtext(side=1, "Mois", line=2, cex=1.2)
mtext(side=2, "Pm [mm]", line=2, cex=1.2)
mtext(side=3, "Précipitations mensuelles", line=0, cex=1.5, font=2)
points(x=1:12, reg_TP$P_BV, col=col_OBS, pch=16, cex=1.2)
points(x=1:12, reg_TF$P_BV, col=col_SIM, pch=15, cex=1.2)
lines(x=1:12, reg_TP$P_BV, col=col_OBS, lwd=2)
lines(x=1:12, reg_TF$P_BV, col=col_SIM, lwd=2, lty=2)
legend("top", legend=c("1992-2007", "2027-2033"), col=c(col_OBS, col_SIM), lwd=2, lty=c(1,2), pch=c(16,15),
       pt.cex=1.2)
box()

# ETP
ylim   <- range(c(reg_TP$ETP_O, reg_TF$ETP_O))
par(mar=c(3,4,2,1), mgp=c(3,0.5,0))
plot(x=0, type="n", xlim=c(1,12), ylim=ylim, axes=FALSE, xlab="", ylab="")
grid()
axis(1, at=1:12, labels=substr(month.abb,1,1))
axis(2)
mtext(side=1, "Mois", line=2, cex=1.2)
mtext(side=2, "ETPm [mm]", line=2, cex=1.2)
mtext(side=3, "ETP (Oudin) mensuelles", line=0, cex=1.5, font=2)
points(x=1:12, reg_TP$ETP_O, col=col_OBS, pch=16, cex=1.2)
points(x=1:12, reg_TF$ETP_O, col=col_SIM, pch=15, cex=1.2)
lines(x=1:12, reg_TP$ETP_O, col=col_OBS, lwd=2)
lines(x=1:12, reg_TF$ETP_O, col=col_SIM, lwd=2, lty=2)
legend("bottom", legend=c("1992-2007", "2027-2033"), col=c(col_OBS, col_SIM), lwd=2, lty=c(1,2), pch=c(16,15),
       pt.cex=1.2)
box()
graphics.off()



