################################################################################################################
# PIERRE BRIGODE - JANVIER 2018
#  DM MALPASSET
################################################################################################################


##################################################################################
# CHEMINS, FONCTIONS
##################################################################################

# Librairies
DIR_ROOT      <- "D:\\DATA\\"
library(maptools)
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

# Température à Fréjus (plus longue série, peu de lacunes)
tmp_T  <- import(paste(DIR_DATA, "83061001_FREJUS_MF__PTd.txt", sep=""), quiet=TRUE)$data
tmp_T  <- tmp_T[,c("Date", "Tmean")]

# Pluie spatiale
data_P  <- import(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__DATA_P_BV.res", sep=""), quiet=TRUE)$data

# Contour du bassin
BV_L2E   <- readShapePoly(paste(DIR_DATA, "Y5325010_Reyran@Fréjus__CONTOUR_BV__L2E.shp", sep=""), proj4string=CRS("+init=EPSG:27572"))


##################################################################################
# CALCUL DE SERIES CONTINUES DE T ET D'ETP D'OUDIN
##################################################################################

# Data.frame à remplir
n_jours        <- length(data_P$Dates)
noms_col       <- c("Date", "P_BV", "Tmean", "ETP_O")
n_col          <- length(noms_col)
data_BV        <- data.frame(matrix(NA, nrow=n_jours, ncol=n_col, dimnames=list(NULL, noms_col)))
data_BV$Date   <- data_P$Dates
data_BV$P_BV   <- data_P$P_BV

# Approximation linéaire (!) des températures journalières
approx_T      <- approx(x=tmp_T$Date, y=tmp_T$Tmean, xout=data_BV$Date)
data_BV$Tmean <- approx_T$y

# Calcul de la latitude moyenne du bassin (en radian)
BV_WGS84       <- spTransform(BV_L2E, CRSobj=CRS("+init=EPSG:4326"))
LatBV          <- mean(BV_WGS84@polygons[[1]]@Polygons[[1]]@coords[,2])
LatBV_rad      <- LatBV*pi/180

# Calcul ETP d'Oudin avec fonction airGR
data_BV$ETP_O  <- PEdaily_Oudin(JD      = as.integer(format(data_BV$Date, "%j")),
                                Temp    = data_BV$Tmean, 
                                LatRad  = LatBV_rad)
data_BV$ETP_O   <- round(data_BV$ETP_O, 3)


##################################################################################
# EXPORT
##################################################################################

# Export du tableau créé
suppressWarnings(
  export(file    = paste(DIR_DATA, "Y5325010_Reyran@Fréjus__DATA_PET_BV.res", sep=""),
         data    = data_BV,
         quiet   = TRUE,
         digits  = 7,
         dec     = 3,
         comment = c(paste("Date de mise en forme : ", Sys.Date(), sep="")
         )))



