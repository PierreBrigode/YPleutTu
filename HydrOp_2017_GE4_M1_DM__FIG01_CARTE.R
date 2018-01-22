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
library(RColorBrewer)
library(DtgImportExport)

# Chemins
DIR_PROJ      <- paste(DIR_ROOT, "14_ENSEIGNEMENTS\\2017-2018\\GE4-M1\\HYDROP\\DM\\", sep="")
DIR_SIG       <- paste(DIR_ROOT, "02_DATA\\SIG\\", sep="")
DIR_DATA_BV   <- paste(DIR_ROOT, "02_DATA\\BVs\\FR_IRSTEA\\", sep="")
DIR_DATA_SIG  <- paste(DIR_ROOT, "02_DATA\\SIG\\BVs\\FR_BANQUE_HYDRO\\", sep="")
DIR_DATA_MNT  <- paste(DIR_ROOT, "02_DATA\\SIG\\MNTs\\MNT_100m_FRANCE\\", sep="")
DIR_OUT       <- paste(DIR_PROJ, "DATA\\", sep="")

# Fonctions
source(paste(DIR_ROOT, "03_FONCTIONS\\PB_00_FUNCTIONS.R", sep=""))


##################################################################################
# CHARGEMENT DONNEES
##################################################################################

# INFOS SIG BV
BV_L2E   <- readShapePoly(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__CONTOUR_BV__L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))
STA_L2E  <- readShapePoints(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__STATION__L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))
RIV_L2E  <- readShapePoints(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__RIVIERES__L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))
LACS_L2E <- readShapePoly(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__LACS__L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))
MNT_L2E  <- raster(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__MNT_100m__L2E.tif", sep=""))
CUM_L2E  <- raster(paste(DIR_OUT, "Y5325010_Reyran@Fréjus__CUM_100m__L2E.tif", sep=""))

# Pluviomètres Météo-France
PLUVIO_L2E  <- readShapePoints(paste(DIR_OUT, "Pluviomètres_Météo-France_L2E.shp", sep=""),  proj4string=CRS("+init=EPSG:27572"))

# Malpasset
MALP_L2E    <- readShapePoints(paste(DIR_OUT, "BARRAGE_MALPASSET_L2E.shp", sep=""),  proj4string=CRS("+init=EPSG:27572"))


##################################################################################
# TRAITEMENT RASTER
##################################################################################

# Découpe
tmp_ext      <- extent(BV_L2E)
tmp_ext@xmin <- tmp_ext@xmin*0.995
tmp_ext@xmax <- tmp_ext@xmax*1.005
tmp_ext@ymin <- tmp_ext@ymin*0.995
tmp_ext@ymax <- tmp_ext@ymax*1.005

# Traitements raster
ind               <- which(values(MNT_L2E) <= 0)
MNT_L2E[ind]      <- NA
ind               <- which(values(CUM_L2E) <= 5)
CUM_L2E[ind]      <- NA
hillShade         <- terrain(MNT_L2E, opt='aspect')


##################################################################################
# OPTIONS GRAPHIQUES
##################################################################################

# Palette MNT
TabRGB               <- read.csv(file=paste(DIR_ROOT, "02_DATA\\SIG\\_PALETTES\\Pal_WIKI_test_MS_bathy_profonde.txt", sep=""),
                                 skip=2, header=FALSE, stringsAsFactors=FALSE) 
TabCol               <- rgb(TabRGB[,2]/255,TabRGB[,3]/255,TabRGB[,4]/255)
TabColBathy          <- TabCol
TabColBathy[11]      <- TabColBathy[10]
TabLevels            <- c(-100000, TabRGB[,1])
ind                  <- which(TabLevels < 0)
TabCol[ind]          <- NA
ColMNT               <- TabCol[-ind]

# Bornes altitude
levels_alt     <- c(seq(0, 800, 10), 2000)
n_lev          <- length(levels_alt)
myPal          <- colorRampPalette(ColMNT)(n_lev-1)	
myPal_alpha    <- add.alpha(COLORS=myPal, ALPHA=0.9)


##################################################################################
# FIGURE
##################################################################################

# PNG
png(paste(DIR_PROJ, "HydrOp_2017_GE4_M1_DM__FIG01_CARTE.png", sep=""), width=30, height=34, units="cm", res=300)
mat     <- matrix(c(01,02), nrow=2, byrow=TRUE)
layout(mat, height=c(30,4))

# Contour
par(mar=c(4,4,1,1), mgp=c(3,0.5,0))
image(hillShade, col=grey(0:100/100), xaxs="i", yaxs="i", axes=FALSE, xlab="", ylab="", maxpixels=ncell(hillShade))
axis(1)
axis(2)
mtext(side=1, "X (L2E) [m]", line=2.5, cex=1.2)
mtext(side=2, "Y (L2E) [m]", line=2.5, cex=1.2)

# MNT, surfaces cumulées et lacs
image(MNT_L2E, add=TRUE, col=myPal_alpha, breaks=levels_alt, maxpixels=ncell(hillShade))
image(CUM_L2E, add=TRUE, col="royalblue", maxpixels=ncell(CUM_L2E))
plot(LACS_L2E, add=TRUE, col="royalblue", border=NA)

# Contour du bassin versant et des stations étudiées
plot(BV_L2E, add=TRUE, col=NA, border="black", lwd=4)
points(STA_L2E, cex=3, pch=16, col="black")

# Barrage
points(MALP_L2E, cex=3, pch=17, col="red")

# Pluviomètres
points(PLUVIO_L2E, cex=3, pch=18, col="purple")
text(PLUVIO_L2E, labels=as.character(PLUVIO_L2E$Nom), pos=3, col="purple")

# Légende
legend("topright", legend=c("Pluviomètres Météo-France",
                            "Reyran@Fréjus (Y5325010, 73 km²)", 
                            "Barrage de Malpasset (47 km²)",
                            "Surfaces cumulées > 5 km² et lacs"),
       pch=c(18,16,17,15), col=c("purple","black", "red", "royalblue"), 
       pt.bg=NA, pt.cex=c(3,3,3,1),
       cex=1.5, box.col="black")
box()

# Légende du MNT
par(mar=c(4,4,2,4), mgp=c(3,0.5,0))
plot_legend(bornes=levels_alt, col=myPal, fleche_inf=FALSE, fleche_sup=TRUE, horiz=TRUE)
axis(1)
mtext("Altitude (SRTM) [m]", side=1, line=2, cex=1.2)
graphics.off()



