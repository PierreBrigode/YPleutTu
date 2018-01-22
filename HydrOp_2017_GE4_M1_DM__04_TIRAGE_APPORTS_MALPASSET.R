################################################################################################################
# PIERRE BRIGODE - JANVIER 2018
#  DM MALPASSET
################################################################################################################


##################################################################################
# CHEMINS, FONCTIONS
##################################################################################

# Librairies
DIR_ROOT      <- "D:\\DATA\\"
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

# Métadonnées
S_BV      <- 73
vol_MALP  <- 48
S_MALP    <- 47

# Lecture des données
tab_DATA  <- import(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__DATA_QPT.res", sep=""), quiet=TRUE)$data

# tab_QA et tab_f_QA
tab_QA    <- import(paste(DIR_OUT, "TAB_QA_Y5325010_Reyran@Fréjus.res", sep=""), quiet=TRUE)$data
tab_f_QA  <- import(paste(DIR_OUT, "TAB_FREQ_QA_Y5325010_Reyran@Fréjus.res", sep=""), quiet=TRUE)$data

# Périodes
per_ALL    <- c(min(tab_f_QA$An), max(tab_f_QA$An))
per_RECENT <- c(1994, max(tab_f_QA$An))

# Débits réservés : 1/10 du QA ou QMNA5
Qres_PER1   <- mean(tab_f_QA$QA, na.rm=TRUE)/10
ind         <- which(tab_f_QA$An >= per_RECENT[1] & tab_f_QA$An <= per_RECENT[2])
Qres_PER2   <- mean(tab_f_QA$QA[ind], na.rm=TRUE)/10
QMNA5       <- 2  # A Affiner !


##################################################################################
# TIRAGES
##################################################################################

# Paramètres
n_tir   <- 1000
n_an    <- 12

# Loi normale : période ALL (PER1)
moy_QA         <- mean(tab_f_QA$logQA)
sd_QA          <- sd(tab_f_QA$logQA)
tir_norm_PER1  <- apply(matrix(1:n_tir, ncol=1), 1, FUN=function(x) rnorm(n=n_an, mean=moy_QA, sd=sd_QA))
tir_norm_PER1  <- exp(tir_norm_PER1)

# Loi normale : période RECENT (PER2)
ind            <- which(tab_f_QA$An >= per_RECENT[1] & tab_f_QA$An <= per_RECENT[2])
moy_QA         <- mean(tab_f_QA$logQA[ind])
sd_QA          <- sd(tab_f_QA$logQA[ind])
tir_norm_PER2  <- apply(matrix(1:n_tir, ncol=1), 1, FUN=function(x) rnorm(n=n_an, mean=moy_QA, sd=sd_QA))
tir_norm_PER2  <- exp(tir_norm_PER2)

# Empirique : période ALL (PER1)
tir_empi_PER1  <- data.frame(matrix(NA, ncol=n_tir, nrow=n_an))
for(i in 1:n_tir) {
  tir_empi_PER1[,i]  <- sample(x=tab_f_QA$logQA, size=n_an, replace=TRUE)
}
tir_empi_PER1  <- exp(tir_empi_PER1)

# Empirique : période RECENT (PER2)
ind            <- which(tab_f_QA$An >= per_RECENT[1] & tab_f_QA$An <= per_RECENT[2])
tir_empi_PER2  <- data.frame(matrix(NA, ncol=n_tir, nrow=n_an))
for(i in 1:n_tir) {
  tir_empi_PER2[,i]  <- sample(x=tab_f_QA$logQA[ind], size=n_an, replace=TRUE)
}
tir_empi_PER2  <- exp(tir_empi_PER2)


##################################################################################
# CONVERSION EN VOLUMES
##################################################################################

# Conversions : l/s --> Mm3
vol_norm_PER1   <- tir_norm_PER1/1000*3600*24*365.25 /10^6
vol_norm_PER2   <- tir_norm_PER2/1000*3600*24*365.25 /10^6
vol_empi_PER1   <- tir_empi_PER1/1000*3600*24*365.25 /10^6
vol_empi_PER2   <- tir_empi_PER2/1000*3600*24*365.25 /10^6

# Cumuls sur les n_an années
cum_norm_PER1   <- apply(vol_norm_PER1, 2, cumsum)
cum_norm_PER2   <- apply(vol_norm_PER2, 2, cumsum)
cum_empi_PER1   <- apply(vol_empi_PER1, 2, cumsum)
cum_empi_PER2   <- apply(vol_empi_PER2, 2, cumsum)


##################################################################################
# FIGURE TYPE
##################################################################################

# Cadre de figure type
plot_FUN  <- function(x) {
  par(mar=c(4,4,1,0.5), mgp=c(3,0.5,0))
  plot(x=0, type="n", xlab="", ylab="", axes=FALSE, xlim=xlim, ylim=ylim)
  grid(nx=NA, ny=NULL)
  abline(v=1:n_an, col="lightgray", lty=3)
  axis(1, cex.axis=1.0)
  axis(2, cex.axis=1.0)
  mtext(side=1, "Années", line=2, cex=1.2)
  mtext(side=2, expression(paste("V [M", m^3, "]", sep="")), line=2, cex=1.2)
  abline(h=vol_MALP, col="red", lwd=5)
}


##################################################################################
# FIGURE (VOLUME CUMULES @ FREJUS)
##################################################################################

# Paramètres communs
xlim   <- c(1,10)
ylim   <- c(0,150)

# Calculs de quantiles
tmp_box_T1  <- PB_boxplot(t(cum_norm_PER1), quant=c(0.1,0.25,0.5,0.75,0.9))
tmp_box_T2  <- PB_boxplot(t(cum_norm_PER2), quant=c(0.1,0.25,0.5,0.75,0.9))

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG05_VOL_CUM_FREJUS.png", sep=""), width=24, height=12, units="cm", res=600)
plot_FUN()
bxp(tmp_box_T1, add=TRUE, boxfill="purple", border="black", horizontal=FALSE, axes=FALSE,
    boxwex=0.2, boxlwd=1, whisklty=1, whisklwd=1, at=(1:n_an)+0.12, show.names=NULL)
bxp(tmp_box_T2, add=TRUE, boxfill="darkgreen", border="black", horizontal=FALSE, axes=FALSE,
    boxwex=0.2, boxlwd=1, whisklty=1, whisklwd=1, at=(1:n_an)-0.12, show.names=NULL)
legend("bottomright", legend=c(paste(per_ALL[1], per_ALL[2], sep="-"), 
                               paste(per_RECENT[1], per_RECENT[2], sep="-"),
                               "Volume retenue Malpasset"), col=c("black", "black", "red"),
       pt.bg=c("purple", "darkgreen",NA), text.col="black", 
       pch=c(22,22,NA), lwd=c(NA,NA,5), box.col="white")
box()
graphics.off()


##################################################################################
# MISE A L'ECHELLE DE MALPASSET
##################################################################################

# Mise à l'échelle du bassin de Malpasset
ratio_S         <- S_MALP/S_BV
cum_norm_PER1   <- cum_norm_PER1*ratio_S
cum_norm_PER2   <- cum_norm_PER2*ratio_S

# Calculs de quantiles
tmp_box_T1  <- PB_boxplot(t(cum_norm_PER1), quant=c(0.1,0.25,0.5,0.75,0.9))
tmp_box_T2  <- PB_boxplot(t(cum_norm_PER2), quant=c(0.1,0.25,0.5,0.75,0.9))

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG06_VOL_CUM_MALPASSET.png", sep=""), width=24, height=12, units="cm", res=600)
plot_FUN()
bxp(tmp_box_T1, add=TRUE, boxfill="purple", border="black", horizontal=FALSE, axes=FALSE,
    boxwex=0.2, boxlwd=1, whisklty=1, whisklwd=1, at=(1:n_an)+0.12, show.names=NULL)
bxp(tmp_box_T2, add=TRUE, boxfill="darkgreen", border="black", horizontal=FALSE, axes=FALSE,
    boxwex=0.2, boxlwd=1, whisklty=1, whisklwd=1, at=(1:n_an)-0.12, show.names=NULL)
legend("bottomright", legend=c(paste(per_ALL[1], per_ALL[2], sep="-"), 
                               paste(per_RECENT[1], per_RECENT[2], sep="-"),
                               "Volume retenue Malpasset"), col=c("black", "black", "red"),
       pt.bg=c("purple", "darkgreen",NA), text.col="black", 
       pch=c(22,22,NA), lwd=c(NA,NA,5), box.col="white")
box()
graphics.off()


##################################################################################
# PRISE EN COMPTE DU DEBIT RESERVE
##################################################################################

# Prise en compte du débit réservé
# Vres           <- Qres/1000*3600*24*365.25*ratio_S/10^6
vol_QMNA5          <- QMNA5/1000*3600*24*365.25*ratio_S/10^6
cum_norm_PER1      <- cum_norm_PER1 - vol_QMNA5
cum_norm_PER2      <- cum_norm_PER2 - vol_QMNA5

# Calculs de quantiles
tmp_box_T1  <- PB_boxplot(t(cum_norm_PER1), quant=c(0.1,0.25,0.5,0.75,0.9))
tmp_box_T2  <- PB_boxplot(t(cum_norm_PER2), quant=c(0.1,0.25,0.5,0.75,0.9))

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG07_VOL_CUM_MALPASSET_-QRESERVE.png", sep=""), width=24, height=12, units="cm", res=600)
plot_FUN()
bxp(tmp_box_T1, add=TRUE, boxfill="purple", border="black", horizontal=FALSE, axes=FALSE,
    boxwex=0.2, boxlwd=1, whisklty=1, whisklwd=1, at=(1:n_an)+0.12, show.names=NULL)
bxp(tmp_box_T2, add=TRUE, boxfill="darkgreen", border="black", horizontal=FALSE, axes=FALSE,
    boxwex=0.2, boxlwd=1, whisklty=1, whisklwd=1, at=(1:n_an)-0.12, show.names=NULL)
legend("bottomright", legend=c(paste(per_ALL[1], per_ALL[2], sep="-"), 
                               paste(per_RECENT[1], per_RECENT[2], sep="-"),
                               "Volume retenue Malpasset"), col=c("black", "black", "red"),
       pt.bg=c("purple", "darkgreen",NA), text.col="black", 
       pch=c(22,22,NA), lwd=c(NA,NA,5), box.col="white")
box()
graphics.off()


##################################################################################
# PRISE EN COMPTE DE l'EVAPORATION
##################################################################################

# SOURCE : Annexe 1 et 2 (pages 43 et 44) du rapport de M2 de Vachala (2008), intitulé :
# Évaporation sur les retenues EDF du Sud de la France.
# DATA : 
#   Volume évaporé moyen annuel (hm3) de la retenue de Saint-Cassien : 4.5 hm3
#   Volume de la retenue RN de Saint-Cassien : 60.00 hm3
perte           <- 4.5/60 
cum_norm_PER1   <- cum_norm_PER1 - (cum_norm_PER1*perte)
cum_norm_PER2   <- cum_norm_PER2 - (cum_norm_PER2*perte)

# Calculs de quantiles
tmp_box_T1  <- PB_boxplot(t(cum_norm_PER1), quant=c(0.1,0.25,0.5,0.75,0.9))
tmp_box_T2  <- PB_boxplot(t(cum_norm_PER2), quant=c(0.1,0.25,0.5,0.75,0.9))

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG08_VOL_CUM_MALPASSET_-QRESERVE-E.png", sep=""), width=24, height=12, units="cm", res=600)
plot_FUN()
bxp(tmp_box_T1, add=TRUE, boxfill="purple", border="black", horizontal=FALSE, axes=FALSE,
    boxwex=0.2, boxlwd=1, whisklty=1, whisklwd=1, at=(1:n_an)+0.12, show.names=NULL)
bxp(tmp_box_T2, add=TRUE, boxfill="darkgreen", border="black", horizontal=FALSE, axes=FALSE,
    boxwex=0.2, boxlwd=1, whisklty=1, whisklwd=1, at=(1:n_an)-0.12, show.names=NULL)
legend("bottomright", legend=c(paste(per_ALL[1], per_ALL[2], sep="-"), 
                               paste(per_RECENT[1], per_RECENT[2], sep="-"),
                               "Volume retenue Malpasset"), col=c("black", "black", "red"),
       pt.bg=c("purple", "darkgreen",NA), text.col="black", 
       pch=c(22,22,NA), lwd=c(NA,NA,5), box.col="white")
box()
graphics.off()


##################################################################################
# TEMPS DE REMPLISSAGE
##################################################################################

# Calculs de temps de remplissage du volume de la retenue 
tps  <- data.frame(Sce=1:n_tir, Tps_PER1=NA, Tps_PER2=NA)
for(i in 1:n_tir) {
  tmp              <- approx(x=1:n_an, y=cum_norm_PER1[,i])
  tps$Tps_PER1[i]  <- tmp$x[which.min(abs(tmp$y-vol_MALP))]
  tmp              <- approx(x=1:n_an, y=cum_norm_PER2[,i])
  tps$Tps_PER2[i]  <- tmp$x[which.min(abs(tmp$y-vol_MALP))]
}

# Boxplots de temps de remplissage
tmp_box  <- PB_boxplot(tps[,-1], quant=c(0.1,0.25,0.5,0.75,0.9))

# Figure
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG09_TEMPS_REMPLISSAGE.png", sep=""), width=12, height=12, units="cm", res=600)
par(mar=c(4,4,1,0.5), mgp=c(3,0.5,0))
bxp(tmp_box, boxfill=c("purple", "darkgreen"), xlab="Scénarios", ylab="Temps [an]", show.names=FALSE)
axis(1, at=1:2, labels=c(paste(per_ALL[1], per_ALL[2], sep="-"), 
                         paste(per_RECENT[1], per_RECENT[2], sep="-")))
graphics.off()


