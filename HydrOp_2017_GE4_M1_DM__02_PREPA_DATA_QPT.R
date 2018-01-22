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


##################################################################################
# MISE EN FORME DES DONNEES QPT (BANQUE-HYDRO / SAFRAN, IRSTEA)
##################################################################################

# Métadonnées
S_BV    <- 73

# Lecture des débits du Reyran@Fréjus (export BANQUE HYDRO le plus récent)
tab_Qj            <- read.table(paste(DIR_DATA, "Y5325010_HYDRO.txt", sep=""), 
                                comment.char="#", sep=";", header=TRUE, stringsAsFactors=FALSE)
tab_Qj$Date       <- as.Date(as.character(tab_Qj$Date), format="%Y%m%d")
ind               <- which(tab_Qj$Qls < 0)
tab_Qj$Qls[ind]   <- NA
tab_Qj            <- tab_Qj[,c("Date", "Qls", "val_H", "val_I")]

# Lecture des données de bassin d'Irstea
tab_QPT           <- read.table(paste(DIR_DATA, "Y5325010_BV.txt", sep=""), 
                                comment.char="#", sep=";", header=TRUE, stringsAsFactors=FALSE)
tab_QPT$Date      <- as.Date(as.character(tab_QPT$Date), format="%Y%m%d")
ind               <- which(tab_QPT$Q < 0)
tab_QPT$Q[ind]    <- NA

tab_QPT           <- tab_QPT[,c("Date", "Ptot", "Temp", "ETP_O")]

# Fusion des deux tableaux
tab_DATA      <- merge(x=tab_Qj, y=tab_QPT, by="Date", all.y=TRUE)
tab_DATA$E    <- tab_DATA$Qls/1000*86.4/S_BV
tab_DATA$Qls  <- round(tab_DATA$Qls, 1)
tab_DATA      <- tab_DATA[, c("Date", "Qls", "E", "Ptot", "Temp", "ETP_O", "val_H", "val_I")]

# EXPORT
suppressWarnings(export(data     = tab_DATA,
                        file     = paste(DIR_OUT, "Y5325010_Reyran@Fréjus__DATA_QPT.res", sep=""),
                        digits   = 8,
                        dec      = 5,
                        comment  = c(paste("Date d'export : ", Sys.Date(), sep="")),
                        quiet=TRUE))
