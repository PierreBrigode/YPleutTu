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


##################################################################################
# LECTURE DES DONNEES
##################################################################################

# Métadonnées
S_BV    <- 73

# Lecture des données
tab_DATA  <- import(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__DATA_QPT.res", sep=""), quiet=TRUE)$data

# tab_QA et tab_f_QA
tab_QA    <- import(paste(DIR_OUT, "TAB_QA_Y5325010_Reyran@Fréjus.res", sep=""), quiet=TRUE)$data
tab_f_QA  <- import(paste(DIR_OUT, "TAB_FREQ_QA_Y5325010_Reyran@Fréjus.res", sep=""), quiet=TRUE)$data

# Périodes
per_ALL    <- c(min(tab_f_QA$An), max(tab_f_QA$An))
per_RECENT <- c(1994, max(tab_f_QA$An))


##################################################################################
# FIGURE (VARIABILITE INTERANNUELLE ET FREQUENCE)
##################################################################################

# Paramètres communs
moy_QA   <- mean(tab_QA$QA_ls, na.rm=TRUE)
sd_QA    <- sd(tab_QA$QA_ls, na.rm=TRUE)

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG02_FREQUENCES_QA.png", sep=""), 
    width=20, height=20, units="cm", res=600)
layout(matrix(c(01,01,
                02,03), nrow=2, ncol=2, byrow=TRUE))

# Chronique temporelle
par(mar=c(4,4,1,0.5), mgp=c(3,0.5,0))
xlim   <- c(min(tab_f_QA$An, na.rm=TRUE)-0.5, max(tab_f_QA$An, na.rm=TRUE)+0.5)
ylim   <- range(tab_QA$QA_ls, na.rm=TRUE)
plot(x=0, type="n", col="royalblue", xlab="", ylab="", axes=FALSE, xlim=xlim, ylim=ylim, xaxs="i")
grid(nx=NA, ny=NULL)
abline(v=seq(per_ALL[1],per_ALL[2],2), col="lightgray", lty=3)
axis(1, at=seq(per_ALL[1],per_ALL[2],2), cex.axis=1.0)
axis(2, cex.axis=1.0)
mtext(side=1, "Années", line=2, cex=1.2)
mtext(side=2, expression(paste(Q[A], " [l/s]", sep="")), line=2, cex=1.2)
points(x=tab_QA$An, y=tab_QA$QA_ls, col="royalblue", pch=16, cex=1.2)
lines(x=tab_QA$An, y=tab_QA$QA_ls, col="royalblue")
abline(h=c(moy_QA-sd_QA, moy_QA, moy_QA+sd_QA), col="red", lty=c(3,2,3), lwd=2)
box()

# Paramètres graphiques
xQ     <- seq(-1000, max(tab_f_QA$QA)*2, le=100)
xlim   <- c(0,1300)
ylim   <- c(0,5)

# Histogramme (ALL puis RECENT)
for(i in 1:2) {
  if(i == 1) {
    tmp_QA   <- tab_f_QA$QA
    titre    <- paste(per_ALL[1], per_ALL[2], sep="-")
  }
  if(i == 2) {
    ind      <- which(tab_f_QA$An >= per_RECENT[1] & tab_f_QA$An <= per_RECENT[2])
    tmp_QA   <- tab_f_QA$QA[ind]
    titre    <- paste(per_RECENT[1], per_RECENT[2], sep="-")
  }
  
  # Graphique
  par(mar=c(4,4,1,0.5), mgp=c(3,0.5,0))
  hist(tmp_QA, nclass=10, xlab="", ylab="", freq=TRUE, col="royalblue", axes=FALSE, xlim=xlim, ylim=ylim, main=titre)
  grid()
  axis(1)
  axis(2, las=1)
  mtext(side=1, expression(paste(Q[A], " [l/s]", sep="")), line=2, cex=1.2)
  mtext(side=2, "Fréquence [-]", line=2, cex=1.2)
  hist(tmp_QA, nclass=10, freq=TRUE, col="royalblue", add=TRUE)
  box()
}
graphics.off()



##################################################################################
# FIGURE (DENSITE)
##################################################################################

# Paramètres graphiques
xQ     <- seq(-1000, max(tab_f_QA$QA)*2, le=100)
xlim   <- c(0,1300)
cols   <- c("red", "darkred")

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG03_DENSITES_PROB_QA.png", sep=""), width=24, height=12, units="cm", res=600)
layout(matrix(c(01,02), nrow=1, ncol=2, byrow=TRUE))

# Histogramme (ALL puis RECENT)
for(i in 1:2) {
  if(i == 1) {
    tmp_QA   <- tab_f_QA$QA
    titre    <- paste(per_ALL[1], per_ALL[2], sep="-")
  }
  if(i == 2) {
    ind      <- which(tab_f_QA$An >= per_RECENT[1] & tab_f_QA$An <= per_RECENT[2])
    tmp_QA   <- tab_f_QA$QA[ind]
    titre    <- paste(per_RECENT[1], per_RECENT[2], sep="-")
  }
  
  # Graphique
  par(mar=c(4,4,1,0.5), mgp=c(3,0.5,0))
  hist(tmp_QA, nclass=10, xlab="", ylab="", main=titre, freq=FALSE, col="royalblue", axes=FALSE, xlim=xlim)
  grid()
  axis(1)
  axis(2)
  mtext(side=1, expression(paste(Q[A], " [l/s]", sep="")), line=2, cex=1.2)
  mtext(side=2, "Densité [-]", line=2, cex=1.2)
  hist(tmp_QA, nclass=10, freq=FALSE, col="royalblue", add=TRUE)
  lines(density(tmp_QA), lwd=2)
  lines(xQ, dnorm(xQ, mean(tmp_QA), sd(tmp_QA)), lwd=4, col=cols[i])
  box()
  cat(paste(titre, " : moy=", round(mean(tmp_QA),0), " l/s ; sd=", round(sd(tmp_QA)), " l/s\n", sep=""))
}
graphics.off()


##################################################################################
# FIGURE (DENSITE LOG)
##################################################################################

# Paramètres graphiques
xQ     <- seq(-1, max(tab_f_QA$logQA)*2, le=1000)
xlim   <- c(3,9)
cols   <- c("red", "darkred")

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG04_DENSITES_PROB_logQA.png", sep=""), width=24, height=12, units="cm", res=600)
layout(matrix(c(01,02), nrow=1, ncol=2, byrow=TRUE))

# Histogramme (ALL puis RECENT)
for(i in 1:2) {
  if(i == 1) {
    tmp_QA   <- tab_f_QA$logQA
    titre    <- paste(per_ALL[1], per_ALL[2], sep="-")
  }
  if(i == 2) {
    ind      <- which(tab_f_QA$An >= per_RECENT[1] & tab_f_QA$An <= per_RECENT[2])
    tmp_QA   <- tab_f_QA$logQA[ind]
    titre    <- paste(per_RECENT[1], per_RECENT[2], sep="-")
  }
  
  # Graphique
  par(mar=c(4,4,1,0.5), mgp=c(3,0.5,0))
  hist(tmp_QA, nclass=10, xlab="", ylab="", main=titre, freq=FALSE, col="royalblue", axes=FALSE, xlim=xlim)
  grid()
  axis(1)
  axis(2)
  mtext(side=1, expression(paste(Q[A], " [l/s]", sep="")), line=2, cex=1.2)
  mtext(side=2, "Densité [-]", line=2, cex=1.2)
  hist(tmp_QA, nclass=10, freq=FALSE, col="royalblue", add=TRUE)
  lines(density(tmp_QA), lwd=2)
  lines(xQ, dnorm(xQ, mean(tmp_QA), sd(tmp_QA)), lwd=4, col=cols[i])
  box()
  cat(paste(titre, " : moy=", round(mean(tmp_QA),3), " log(l/s) ; sd=", round(sd(tmp_QA),3), " log(l/s)\n", sep=""))
}
graphics.off()


