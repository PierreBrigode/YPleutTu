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
data_QSIM_TP   <- import(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__SIM_Q_GR2M_NSE.res", sep=""), quiet=TRUE)$data
data_QSIM_TF   <- import(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__SIM_QFUTUR_GR2M_NSE.res", sep=""), quiet=TRUE)$data

# Découpage des données TP
ind           <- which(data_QSIM_TP$Date >= as.Date("1994-01-01") & data_QSIM_TP$Date <= as.Date("2007-12-01"))
data_QSIM_TP  <- data_QSIM_TP[ind,]

# Métadonnées
S_BV      <- 73
vol_MALP  <- 48
S_MALP    <- 47
Slac_MALP <- 3.7
QMNA5     <- 2  # A Affiner !

# Premiers calculs
ratio_S         <- S_MALP/S_BV
EA_mm           <- 1350
Qls_Evap        <- 1350* Slac_MALP / (3600*24*365.24/10^3) *1000
prop_perteA_E   <- 4.5/60


##################################################################################
# CUMULS 
##################################################################################

# Conversion des lames d'eau simulées en l/s : TEMPS PRESENT (TP)
tmp_dates         <- seq(from=head(data_QSIM_TP$Date,1), by="1 month", length.out=length(data_QSIM_TP$Date)+1)
n_jours           <- as.integer(diff(tmp_dates))
data_QSIM_TP$Qls  <- data_QSIM_TP$Qsim / n_jours * S_BV / 86.4 * 1000

# Conversion des lames d'eau simulées en l/s : TEMPS FUTUR (TF)
data_QSIM_TF$Qls  <- data_QSIM_TF$Qsim / n_jours * S_BV / 86.4 * 1000

# Fusion des tableaux
tab_DATA   <- data.frame(Date    = data_QSIM_TP$Date,
                         Qls_TP  = data_QSIM_TP$Qls,
                         Qls_TF  = data_QSIM_TF$Qls)

# Calcul du pas de temps (en s)
pdt_s      <- as.numeric(diff(head(tab_DATA$Date,2)), units="secs")

# Volume écoulé par pas de temps pour assurer le débit réservé
vol_QMNA5  <- QMNA5/1000 *pdt_s *ratio_S/10^6

# Volume évaporé par pas de temps
Vol_Evap   <- Qls_Evap/1000 *pdt_s /10^6


# Fonction de calculs d'apport
calcul_apport_Mm3  <- function(Q_ls, pdt_s, rapport_S=1, VolEvap=0, VolrQres=0) {
  
  # Conversions : l/s --> Mm3/mois
  volumes   <- Q_ls/1000 *pdt_s /10^6

  # Cumuls
  cumuls    <- cumsum(volumes)
  
  # Mise à l'échelle du bassin étudé
  cumuls    <- cumuls*rapport_S
  
  # Prise en compte du débit réservé
  cumuls    <- cumuls - VolrQres
  
  # Prise en compte de l'évaporation sur la retenue
  cumuls   <- cumuls - VolEvap

  # Sortie de la fonction
  return(cumuls)
  
}

# Calculs des apports
tab_DATA$APP_TP  <- calcul_apport_Mm3(Q_ls=tab_DATA$Qls_TP, pdt_s=pdt_s, rapport_S=ratio_S, VolEvap=Vol_Evap, VolrQres=vol_QMNA5)
tab_DATA$APP_TF  <- calcul_apport_Mm3(Q_ls=tab_DATA$Qls_TF, pdt_s=pdt_s, rapport_S=ratio_S, VolEvap=Vol_Evap, VolrQres=vol_QMNA5)


##################################################################################
# FIGURE 
##################################################################################

# Paramètres
n_mois  <- length(tab_DATA$APP_TP)
xlim    <- c(1, n_mois)
ylim    <- c(0,50)

# Figure
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG14_REMPLISSAGE_GR2M.png", sep=""), width=24, height=12, units="cm", res=600)
par(mar=c(4,4,1,0.5), mgp=c(3,0.5,0))
plot(x=0, type="n", xlim=xlim, ylim=ylim, xlab="", ylab="", axes=FALSE, xaxs="i")
abline(v=seq(from=0, by=12, length.out=20), col="lightgray", lty=3)
axis(1, at=seq(from=0, by=12, length.out=20), cex.axis=1.0)
axis(2, cex.axis=1.0)
mtext(side=1, "Nombre de mois", line=2, cex=1.2)
mtext(side=2, expression(paste("V [M", m^3, "]", sep="")), line=2, cex=1.2)
abline(h=vol_MALP, col="red", lwd=5)
lines(x=1:n_mois, y=tab_DATA$APP_TP, col="blue", lwd=1.2)
lines(x=1:n_mois, y=tab_DATA$APP_TF, col="red", lwd=1.2)
legend("bottomright", legend=c("Temps présent", "Temps futur", "Volume retenue Malpasset"), 
       col=c("blue", "red", "red"), text.col="black", 
       pch=NA, lwd=c(1.2,1.2,5), box.col="white")
box()
graphics.off()

