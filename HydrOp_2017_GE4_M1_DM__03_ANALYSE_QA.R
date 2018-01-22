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


##################################################################################
# ANALYSE DES QA
##################################################################################

# Aggrégation annuelle
tab_DATA$An    <- as.integer(format(tab_DATA$Date, "%Y"))
tab_QA         <- aggregate(x=tab_DATA$Qls, by=list(An=tab_DATA$An), FUN=mean)
names(tab_QA)  <- c("An", "QA_ls")

# Nombre de lacunes par années
tmp_FUN     <- function(x) {
  length(which(is.na(x)))
}
n_lac       <- aggregate(x=tab_DATA$Qls, by=list(An=tab_DATA$An), FUN=tmp_FUN)
tab_QA$nlac <- n_lac$x

# Année complète ?
tmp_FUN     <- function(x) {
  length(seq(from = as.Date(paste(x, "-01-01", sep="")),
             to   = as.Date(paste(x, "-12-31", sep="")),
             by   = "1 day"))
}
n_j_theo       <- apply(matrix(tab_QA[,c("An")], ncol=1), 1, tmp_FUN)
tab_QA$nj      <- aggregate(x=tab_DATA$Qls, by=list(An=tab_DATA$An), FUN=length)[,2]
tab_QA$njtheo  <- n_j_theo

# On enlève les années avec des lacunes et/ou incomplètes
ind                 <- which(tab_QA$nlac == 0 & tab_QA$nj == tab_QA$njtheo)
tab_QA$QA_ls[-ind]  <- NA
tab_QA_LAC          <- tab_QA[which(!is.na(tab_QA$QA_ls)),]
tab_QA_LAC$logQA    <- log(tab_QA_LAC$QA_ls)

# Analyse fréquentielle
tri               <- sort(tab_QA_LAC$QA_ls, decreasing=TRUE, index.return=TRUE)
tab_f_QA          <- data.frame(An=tab_QA_LAC$An[tri$ix], QA=tab_QA_LAC$QA_ls[tri$ix])
n_QA              <- length(tab_f_QA$QA)
tab_f_QA$Rang     <- 1:n_QA
tab_f_QA$F_Hazen  <- (tab_f_QA$Rang - 0.5)/n_QA
tab_f_QA$T_ans    <- 1 / (1-tab_f_QA$F_Hazen)
tab_f_QA$U        <- qnorm(p=tab_f_QA$F_Hazen) 
tab_f_QA$logQA    <- log(tab_f_QA$QA)
moy_QA            <- mean(tab_f_QA$QA)
sd_QA             <- sd(tab_f_QA$QA)

# Définitions des périodes d'analyse
per_ALL    <- c(min(tab_f_QA$An), max(tab_f_QA$An))
per_RECENT <- c(1994, max(tab_f_QA$An))

# Export des tableaux
ind          <- which(sapply(tab_QA, class) == "integer")
tab_QA[,ind] <- apply(tab_QA[,ind], 2, as.numeric)
suppressWarnings(export(data     = tab_QA,
                        file     = paste(DIR_OUT, "TAB_QA_Y5325010_Reyran@Fréjus.res", sep=""),
                        digits   = 8,
                        dec      = 3,
                        comment  = c(paste("Date de calcul : ", Sys.Date(), sep="")),
                        quiet=TRUE))
ind            <- which(sapply(tab_f_QA, class) == "integer")
tab_f_QA[,ind] <- apply(tab_f_QA[,ind], 2, as.numeric)
suppressWarnings(export(data     = tab_f_QA,
                        file     = paste(DIR_OUT, "TAB_FREQ_QA_Y5325010_Reyran@Fréjus.res", sep=""),
                        digits   = 8,
                        dec      = 3,
                        comment  = c(paste("Date de calcul : ", Sys.Date(), sep="")),
                        quiet=TRUE))
