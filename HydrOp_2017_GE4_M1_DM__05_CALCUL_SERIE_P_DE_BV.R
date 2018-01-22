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

# Contour du bassin
BV_L2E   <- readShapePoly(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__CONTOUR_BV__L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))
ext_BV   <- extent(BV_L2E)

# ShapePoints pluviomètres Météo-France
PLUVIO_L2E  <- readShapePoints(paste(DIR_DATA, "Pluviomètres_Météo-France_L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))
ext_P       <- extent(PLUVIO_L2E)

# Autres informations (pour figure)
STA_L2E  <- readShapePoints(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__STATION__L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))
LACS_L2E <- readShapePoly(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__LACS__L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))
MNT_L2E  <- raster(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__MNT_100m__L2E.tif", sep=""))
CUM_L2E  <- raster(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__CUM_100m__L2E.tif", sep=""))
MALP_L2E <- readShapePoints(paste(DIR_DATA, "BARRAGE_MALPASSET_L2E.shp", sep=""),  proj4string=CRS("+init=EPSG:27572"))


##################################################################################
# POLYGONES DE THIESSEN
##################################################################################

# Limites spatiales du domaine étudié
lim          <- list(x=c(min(c(ext_P@xmin, ext_BV@xmin)), max(c(ext_P@xmax, ext_BV@xmax))), 
                     y=c(min(c(ext_P@ymin, ext_BV@ymin)), max(c(ext_P@ymax, ext_BV@ymax))))
ext_dom      <- extent(lim)
ext_dom@xmin <- ext_dom@xmin*0.995
ext_dom@xmax <- ext_dom@xmax*1.005
ext_dom@ymin <- ext_dom@ymin*0.995
ext_dom@ymax <- ext_dom@ymax*1.005

# Polygones
poly_Thiessen   <- voronoi(xy=PLUVIO_L2E, ext=ext_dom)

# Découpe des contours des polygones par le contour du bassin
poly_BV         <- crop(x=poly_Thiessen, y=BV_L2E)

# Calculs des aires et écriture dans la table attributaire
poly_BV@data$Aire_km2  <- area(poly_BV) / 10^6
poly_BV@data$Ratio     <- poly_BV@data$Aire_km2 / area(BV_L2E) * 10^6


##################################################################################
# CALCUL DE LA SERIE DE PLUIE DE BASSIN
##################################################################################

# Création d'une data.frame des pluies à remplir (! ici toutes les séries sont de 1980 à 2010)
noms_pluvio  <- as.character(poly_BV@data$Nom)
codes_pluvio <- as.character(poly_BV@data$Code)
n_pluvio     <- length(codes_pluvio)
noms_col     <- c("Dates", noms_pluvio)
n_col        <- length(noms_col)
Dates        <- seq(from=as.Date("1980-01-01"), to=as.Date("2010-12-31"), "1 day")
n_jours      <- length(Dates)
tab_P        <- data.frame(matrix(NA, nrow=n_jours, ncol=n_col, dimnames=list(NULL, noms_col)))
tab_P$Dates  <- Dates

# Boucle pour ouverture des fichiers de données et stockage dans la matrice
for(i in 1:n_pluvio) {
  tmp_P        <- import(paste(DIR_DATA, codes_pluvio[i], "_", noms_pluvio[i], "_MF__PTd.txt", sep=""), quiet=TRUE)$data
  tab_P[,i+1]  <- tmp_P$Ptot 
}

# Calcul de la pluie de bassin
tab_P$P_BV     <- round(apply(tab_P[,-1], 1, function(x) sum(x*poly_BV@data$Ratio)) , digits=2)

# Export du tableau créé
suppressWarnings(
  export(file    = paste(DIR_DATA, "Y5325010_Reyran@Fréjus__DATA_P_BV.res", sep=""),
         data    = tab_P,
         quiet   = TRUE,
         digits  = 7,
         dec     = 2,
         comment = c(paste("Date de mise en forme : ", Sys.Date(), sep="")
         )))


##################################################################################
# FIGURE ILLUSTRATIVE
##################################################################################

# Mise à NA des pixels drainant moins de 5 km² (pour afficher des rivières)
ind               <- which(values(CUM_L2E) <= 5)
CUM_L2E[ind]      <- NA

# Mise à NA des pixels avec moins de 0 m d'altitude (pour visualiser la mer)
ind               <- which(values(MNT_L2E) <= 0)
MNT_L2E[ind]      <- NA

# Couleurs du MNT
levels_alt  <- c(seq(0, 800, 10), 2000)
n_lev       <- length(levels_alt)
cols_alt    <- terrain.colors(n_lev-1)

# Couleurs des pluvios
cols_pluvio <-brewer.pal(n=n_pluvio, name="Set3")

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG10_CARTE_POLYGONES_THIESSEN.png", sep=""), width=30, height=34, units="cm", res=300)
mat     <- matrix(c(01,02), nrow=2, byrow=TRUE)
layout(mat, height=c(30,4))

# Cadre
par(mar=c(4,4,1,1), mgp=c(3,0.5,0))
image(MNT_L2E, breaks=levels_alt, col=cols_alt, xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="")
axis(1)
axis(2)
mtext(side=1, "X (L2E) [m]", line=2.5, cex=1.2)
mtext(side=2, "Y (L2E) [m]", line=2.5, cex=1.2)

# Surfaces cumulées et lacs
image(CUM_L2E, add=TRUE, col="royalblue", maxpixels=ncell(CUM_L2E))
plot(LACS_L2E, add=TRUE, col="royalblue", border=NA)

# Pluviomètres
points(PLUVIO_L2E, cex=3, pch=23, col="red", bg=cols_pluvio, lwd=4)
text(PLUVIO_L2E, labels=as.character(PLUVIO_L2E$Nom), pos=3, col="black", cex=1.2, font=2, offset=1.2)

# Polygones de Thiessen
plot(poly_BV, add=TRUE, col=add.alpha(cols_pluvio, 0.75), border="red", lwd=4)
text(poly_BV, labels=paste(round(poly_BV$Ratio*100,1), "%", sep=""), col="black", cex=1.5, font=2)

# Contour du bassin versant et des stations étudiées
plot(BV_L2E, add=TRUE, col=NA, border="black", lwd=4)
points(STA_L2E, cex=3, pch=16, col="black")
points(MALP_L2E, cex=3, pch=17, col="red")

# Légende
legend("topright", legend=c("Pluviomètres Météo-France",
                            "Reyran@Fréjus (Y5325010, 73 km²)", 
                            "Barrage de Malpasset (47 km²)",
                            "Surfaces cumulées > 5 km² et lacs"),
       pch=c(23,16,17,15), col=c("red","black", "red", "royalblue"), 
       pt.lwd=c(4,NA,NA,NA), pt.cex=c(3,3,3,1), pt.bg=c("white", NA,NA,NA),
       cex=1.5, box.col="black")
box()

# Légende du MNT
par(mar=c(4,4,2,4), mgp=c(3,0.5,0))
plot_legend(bornes=levels_alt, col=cols_alt, fleche_inf=FALSE, fleche_sup=TRUE, horiz=TRUE)
axis(1)
mtext("Altitude (SRTM) [m]", side=1, line=2, cex=1.2)
graphics.off()



