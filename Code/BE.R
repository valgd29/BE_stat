

couples <- read.csv("C:/Users/Mehdi/Desktop/M2 2017-2018/S2/BE - Statistique et santé/couples.csv", sep = ',')
attach(couples)

#### Mise en forme des données
couples$ddn = as.Date(couples$ddn, "%d/%m/%Y")
couples$dconsultation = as.Date(couples$dconsultation, "%d/%m/%Y")
couples$dconception = as.Date(couples$dconception, "%d/%m/%Y")
couples$enfant = as.factor(couples$enfant)
couples$fecondite = as.factor(ifelse(couples$fecondite == 'primaire', 0, 1))
couples$cryptorchidie = as.factor(ifelse(couples$cryptorchidie == 'Oui', 1, 0))
couples$age_f = as.numeric(as.character(couples$age_f))

couples$trt_aucun = as.factor(ifelse(couples$traitement == 'Aucun', 1, 0))
couples$trt_fiv = as.factor(ifelse(couples$traitement == 'FIV', 1, 0))
couples$trt_iac = as.factor(ifelse(couples$traitement == 'IAC', 1, 0))
couples$trt_iad = as.factor(ifelse(couples$traitement == 'IAD', 1, 0))
couples$trt_icsi = as.factor(ifelse(couples$traitement == 'ICSI', 1, 0))
couples$trt_med = as.factor(ifelse(couples$traitement == 'Médical', 1, 0))
couples$patho_f[couples$patho_f == ""] <- NA
couples$patho_f[couples$patho_f == "."] <- NA
couples <- subset(couples, select = -traitement) #remove the column 'traitement'

couples$spermo_anormal = as.factor(ifelse(couples$spermo == 'anormal', 1, 0))
couples$spermo_azoo = as.factor(ifelse(couples$spermo == 'azoo', 1, 0))
couples$spermo_normal = as.factor(ifelse(couples$spermo == 'normal', 1, 0))
couples <- subset(couples, select = -spermo) #remove the column 'spermo'

couples$diplomeF_bac = as.factor(ifelse(couples$diplome_f == 'Bac', 1, 0))
couples$diplomeF_bacP = as.factor(ifelse(couples$diplome_f == 'Bac-', 1, 0))
couples$diplomeF_bacM = as.factor(ifelse(couples$diplome_f == 'Bac+', 1, 0))

couples$diplomeH_bac = as.factor(ifelse(couples$diplome_h == 'Bac', 1, 0))
couples$diplomeH_bacP = as.factor(ifelse(couples$diplome_h == 'Bac-', 1, 0))
couples$diplomeH_bacM = as.factor(ifelse(couples$diplome_h == 'Bac+', 1, 0))

pm <- c('neurologique','sinusites chroniques , pathologies respiratoire chroniques','pathologies respiratoire chroniques'
        ,'diabète','sinusites chroniques')
couples$patH_maladie = as.factor(ifelse(couples$patho_h %in% pm, 1, 0))
ps <- c('cancer testis , chimiothérapie','sarcome , chimiothérapie','hodgkin , chimiothérapie , radiothérapie',
        'chimiothérapie','chimiothérapie , radiothérapie')
couples$patH_soin = as.factor(ifelse(couples$patho_h %in% ps, 1, 0))
pc <- c('cancer testis , chimiothérapie','hodgkin , chimiothérapie , radiothérapie','sarcome , chimiothérapie')
couples$patH_cancer = as.factor(ifelse(couples$patho_h %in% pc, 1, 0))
couples$patH_autre = as.factor(ifelse(couples$patho_h == 'autre', 1, 0))
couples$patH_non = as.factor(ifelse(couples$patho_h == 'non', 1, 0))

pm <- c('endométriose','hydrosalpinx')
couples$patF_maladie = as.factor(ifelse(couples$patho_f %in% pm, 1, 0))
pt <- c('pb tubaire bilatéral','pb tubaire unilatéral')
couples$patF_tubaire = as.factor(ifelse(couples$patho_f %in% pt, 1, 0))

couples$patF_na = as.factor(ifelse(is.na(couples$patho_f), 1, 0))
couples$patF_autre = as.factor(ifelse(couples$patho_h == 'autre', 1, 0))
couples$patF_non = as.factor(ifelse(couples$patho_h == 'non', 1, 0))

summary(couples)


## faire classe d'age
hist(couples$age_h)
hist(couples$age_f)

