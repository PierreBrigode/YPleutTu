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


##################################################################################
# COORDONNEES DE MALPASSET
##################################################################################

# Coordonnées de Malpasset
MALP_WGS84               <- data.frame(X=6+45/60+25.23/3600, Y=43+30/60+43.83/3600)
coordinates(MALP_WGS84)  <- ~ X + Y
proj4string(MALP_WGS84)  <- CRS("+init=EPSG:4326")

# Export d'un SHP en L2E
MALP_L2E      <- spTransform(MALP_WGS84, CRS("+init=EPSG:27572"))
MALP_L2E      <- SpatialPointsDataFrame(coords=MALP_L2E, data=data.frame(Nom="Malpasset"))
TMP_DIR       <- substr(DIR_OUT, 1, nchar(DIR_OUT)-1)
writeOGR(MALP_L2E, TMP_DIR, "BARRAGE_MALPASSET_L2E", driver="ESRI Shapefile", overwrite_layer=TRUE)


##################################################################################
# COORDONNEES DES PLUVIOMETRES MF
##################################################################################

# Lectures
tab_PLUVIO            <- read.csv(paste(DIR_OUT, "Pluviomètres_Météo-France.csv", sep=""), 
                                   header=TRUE, sep=";", stringsAsFactors=FALSE)
PLUVIO_MF              <- tab_PLUVIO
coordinates(PLUVIO_MF) <- ~ X_L2E + Y_L2E
proj4string(PLUVIO_MF) <- CRS("+init=EPSG:27572")
PLUVIO_MF              <- SpatialPointsDataFrame(coords=PLUVIO_MF, data=tab_PLUVIO)

# Export
writeOGR(PLUVIO_MF, TMP_DIR, "Pluviomètres_Météo-France_L2E", driver="ESRI Shapefile", overwrite_layer=TRUE)


##################################################################################
# CONTOUR DU BASSIN ETUDIE
##################################################################################

# Bassin étudié
code_BV   <- "Y5325010"
nom_BV    <- "Reyran@Fréjus"

# Liste (et caractéristiques principales) des stations Banque HYDRO
liste_BV      <- import(paste(DIR_DATA_SIG, "3725exus_FRANCE_CORSE_L93.txt", sep=""), quiet=TRUE)$data

# Import ShapeFile des bassins versants Banque HYDRO
BV_L93     <- readShapePoly(paste(DIR_DATA_SIG, "3725BVs_FRANCE_CORSE_L93.shp", sep=""), proj4string=CRS("+init=EPSG:2154"))
STA_L93    <- readShapePoints(paste(DIR_DATA_SIG, "3725exus_FRANCE_CORSE_L93.shp", sep=""), proj4string=CRS("+init=EPSG:2154"))
BV_L2E     <- spTransform(BV_L93, CRS("+init=EPSG:27572"))
STA_L2E    <- spTransform(STA_L93, CRS("+init=EPSG:27572"))

# Selection des données du bassin étudié
ind      <- which(BV_L2E@data$Code == code_BV)
BV_L2E   <- BV_L2E[ind,]
ind      <- which(STA_L2E@data$Code == code_BV)
STA_L2E  <- STA_L2E[ind,]

# Ecriture
writeOGR(obj=BV_L2E, dsn=TMP_DIR, layer=paste(code_BV, "_", nom_BV, "__CONTOUR_BV__L2E", sep=""),
         driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(obj=STA_L2E, dsn=TMP_DIR, layer=paste(code_BV, "_", nom_BV, "__STATION__L2E", sep=""),
         driver="ESRI Shapefile", overwrite_layer=TRUE)


##################################################################################
# AUTRES DONNEES SIG (POUR AFFICHAGE)
##################################################################################

# Rivières
load(paste(DIR_SIG, "Rivieres\\_MAT_XYS_RESEAU_HYDROGRAPHIQUE_100m.RDATA", sep=""))
PTS_RIV              <- mat_reseau_hydro
coordinates(PTS_RIV) <- ~ X_L2E + Y_L2E
proj4string(PTS_RIV) <- CRS("+init=EPSG:27572")

# Lacs
LACS_L93  <- readShapePoly(paste(DIR_SIG, "CARTHAGE\\MasseDEauPlanDEau_FXX-shp\\MasseDEauPlanDEau_FXX.shp", sep=""),
                           proj4string=CRS("+init=EPSG:2154"))
LACS_L2E  <- spTransform(LACS_L93, CRS("+init=EPSG:27572"))

# Import rasters
MNT_L2E   <- raster(paste(DIR_SIG, "MNTs\\MNT_100m_FRANCE\\FRANCE_100m_L2E_MNT.tif", sep=""))
CUM_L2E   <- raster(paste(DIR_SIG, "MNTs\\MNT_100m_FRANCE\\FRANCE_100m_L2E_CUMULS.tif", sep=""))

# Limite de découpe
tmp_ext      <- extent(BV_L2E)
tmp_ext@xmin <- tmp_ext@xmin*0.995
tmp_ext@xmax <- tmp_ext@xmax*1.005
tmp_ext@ymin <- tmp_ext@ymin*0.995
tmp_ext@ymax <- tmp_ext@ymax*1.005

# Découpes
MNT_L2E       <- crop(x=MNT_L2E, y=tmp_ext, snap="out")
CUM_L2E       <- crop(x=CUM_L2E, y=tmp_ext, snap="out")
RIV_L2E       <- crop(x=PTS_RIV, y=tmp_ext, snap="out")
LACS_L2E      <- crop(x=LACS_L2E, y=tmp_ext, snap="out")

# EXPORTS
writeRaster(x=MNT_L2E, filename=paste(DIR_OUT, code_BV, "_", nom_BV, "__MNT_100m__L2E", sep=""), format="GTiff")
writeRaster(x=CUM_L2E, filename=paste(DIR_OUT, code_BV, "_", nom_BV, "__CUM_100m__L2E", sep=""), format="GTiff")
writeOGR(obj=RIV_L2E, dsn=TMP_DIR, layer=paste(code_BV, "_", nom_BV, "__RIVIERES__L2E", sep=""),
         driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(obj=LACS_L2E, dsn=TMP_DIR, layer=paste(code_BV, "_", nom_BV, "__LACS__L2E", sep=""),
         driver="ESRI Shapefile", overwrite_layer=TRUE)

