
couples <- read.table(file = "C:/Users/valentin/Documents/BE_stat/couples.csv", sep = ",", header= TRUE)

couples$enfant = as.factor(couples$enfant)

library("ggplot2")
library("reshape2")


pairs(couples[, c(2, 7, 8, 9, 10)])

#pairs(couples)
couples[couples == ""] <- NA
couples[couples == "."] <- NA
couples = droplevels(couples)
couples$ID <- seq.int(nrow(couples))
library("VIM")
#graph valeurs manquantes par variable
pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(apply(couples,2,pMiss))

#df <- data.frame(table(couples$patho_f, exclude = NULL))
df <- data.frame(table(couples$patho_f))
df$Perc <- df$Freq / sum(df$Freq) * 100
df = df[order((df$Freq), decreasing = TRUE),]

#df <- data.frame(table(couples$patho_f, exclude = NULL))
df_bh_f <- data.frame(table(couples$bh_f))
df_bh_f$Perc <- df_bh_f$Freq / sum(df_bh_f$Freq) * 100
df_bh_f = df_bh_f[order((df_bh_f$Perc), decreasing = TRUE),]

#df <- data.frame(table(couples$ct_f, exclude = NULL))
df_ct_f <- data.frame(table(couples$ct_f))
df_ct_f$Perc <- df_ct_f$Freq / sum(df_ct_f$Freq) * 100
df_ct_f = df_ct_f[order((df_ct_f$Perc), decreasing = TRUE),]

#Date format
couples$dconsultation = as.Date(couples$dconsultation ,format='%d/%m/%Y')
couples$dconception= as.Date(couples$dconception ,format='%d/%m/%Y')
couples$ddn = as.Date(couples$ddn ,format='%d/%m/%Y')

#couples ecart ddn, dconception et dconsultation
#var ddn et dconsultation
couples$ddnconcept <- ifelse(!is.na(couples$ddn), couples$ddn, couples$dconception)
couples$ddnconcept <- as.Date(couples$ddnconcept, origin = "1970-01-01")

couples$diff_consult_ddnconcept <-  couples$ddnconcept - couples$dconsultation 
couples <-couples[,!(colnames(couples) %in% c( "ddn","dconception", "ddnconcept"))]


X <- couples[,(colnames(couples) %in% c( "patho_f", "bh_f", "ct_f", "bmi_h", "diplome_h", "age_f", "diplome_f"))]
aggr_plot <- aggr(X, col=c('navyblue','red'), numbers=T, sortVars=T, labels=names(X), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

pMissbyrow <- function(x){sum(is.na(x))}
couples$na_count = apply(couples,1,pMissbyrow)

#suppression des lignes à 4 valeurs NA
couples <- couples[couples$na_count != "4",]

#suppression valeurs na pour diplome_f
couples <- subset(couples, !is.na(couples$diplome_f))
#suppression lignes pour bmi_h = NA
couples <- subset(couples, !is.na(couples$bmi_h))

#supp diplom_h na
couples <- subset(couples, !is.na(couples$diplome_h))
#supp bim_h na
couples <- subset(couples, !is.na(couples$bmi_h))

library("questionr")
#convert var age_f to numeric
couples$age_f = as.numeric(as.character(couples$age_f))

couples$age_f[is.na(couples$age_f)] <- round(mean(couples$age_f, na.rm = TRUE))
couples$age_F = quant.cut(couples$age_f, 6)
couples$age_F_16_27 <- as.factor(ifelse(couples$age_F == "[16,27)", 1, 0))
couples$age_F_27_29 <- as.factor(ifelse(couples$age_F == "[27,29)", 1, 0))
couples$age_F_29_31 <- as.factor(ifelse(couples$age_F == "[29,31)", 1, 0))
couples$age_F_31_33 <- as.factor(ifelse(couples$age_F == "[31,33)", 1, 0))
couples$age_F_33_35 <- as.factor(ifelse(couples$age_F == "[33,35)", 1, 0))
couples$age_F_35_47 <- as.factor(ifelse(couples$age_F == "[35,47]", 1, 0))


#convert var age_M to numeric
couples$age_h = as.numeric(as.character(couples$age_h))
couples$age_M = quant.cut(couples$age_h, 7)
summary(couples)


#bmi and class
couples$bmi_h = as.numeric(as.character(couples$bmi_h))
#mean inputation
couples$bmi_h[is.na(couples$bmi_h)] <- round(mean(couples$bmi_h, na.rm = TRUE))

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
couples <- subset(couples, select = -traitement) #remove the column 'traitement'

couples$spermo_anormal = as.factor(ifelse(couples$spermo == 'anormal', 1, 0))
couples$spermo_azoo = as.factor(ifelse(couples$spermo == 'azoo', 1, 0))
couples$spermo_normal = as.factor(ifelse(couples$spermo == 'normal', 1, 0))
couples <- subset(couples, select = -spermo) #remove the column 'spermo'

couples$diplomeF_bac = as.factor(ifelse(couples$diplome_f == 'Bac' & !is.na(couples$diplome_f), 1, 0))
couples$diplomeF_bacP = as.factor(ifelse(couples$diplome_f == 'Bac-' & !is.na(couples$diplome_f), 1, 0))
couples$diplomeF_bacM = as.factor(ifelse(couples$diplome_f == 'Bac+' & !is.na(couples$diplome_f), 1, 0))

couples$diplomeH_bac = as.factor(ifelse(couples$diplome_h == 'Bac' & !is.na(couples$diplome_h), 1, 0))
couples$diplomeH_bacP = as.factor(ifelse(couples$diplome_h == 'Bac-' & !is.na(couples$diplome_h), 1, 0))
couples$diplomeH_bacM = as.factor(ifelse(couples$diplome_h == 'Bac+' & !is.na(couples$diplome_h), 1, 0))

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

couples$bmi_h_class <- cut(couples$bmi_h, breaks = c(0, 18.5, 25, 30, 35, 40, 55), na.rm = FALSE)
couples$bmi_h_0_18_5 <- as.factor(ifelse(couples$bmi_h_class == "(0,18.5]", 1, 0))
couples$bmi_h_18_5_25 <- as.factor(ifelse(couples$bmi_h_class == "(18.5,25]", 1, 0))
couples$bmi_h_25_30 <- as.factor(ifelse(couples$bmi_h_class == "(25,30]", 1, 0))
couples$bmi_h_35_40 <- as.factor(ifelse(couples$bmi_h_class == "(35,40]", 1, 0))
couples$bmi_h_40_55 <- as.factor(ifelse(couples$bmi_h_class == "(40,55]", 1, 0))


#supp unique NA if nb NA by row > 3


df <- data.frame(table(couples$patho_f, exclude = NULL))
df$Perc <- df$Freq / sum(df$Freq) * 100

X <- couples[,(colnames(couples) %in% c( "patho_f", "bh_f", "ct_f", "bmi_h", "diplome_h", "age_f"))]
aggr_plot <- aggr(X, col=c('navyblue','red'), numbers=T, sortVars=T, labels=names(X), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#patho_f imputation ct_f != NA and bh_f != NA
couples_for_patho_f <- couples[,(colnames(couples) %in% c( "patho_f", "ct_f", "bh_f",  "fecondite","trt_aucun","trt_iac","trt_fiv","trt_icsi", "enfant"))]

desc <- couples_for_patho_f[!is.na(couples_for_patho_f$patho_f) & !is.na(couples_for_patho_f$bh_f) & !is.na(couples_for_patho_f$ct_f),] 
#show significant var
table(desc$patho_f, desc$ct_f, desc$bh_f, desc$enfant)

v1 <- vector("character",4)
v1[1] = rep("pb tubaire unilatéral",1)
v1[2:4]= rep("non",3)

v2 <- vector("character",7)
v2[1] = rep("endométriose",1)
v2[2]= rep("autre",1)
v2[3:7]= rep("non",5)

v3 <- vector("character",30)
v3[1] = rep("autre",1)
v3[2:4]= rep("endométriose",3)
v3[5:26]= rep("non",22)
v3[27:30]= rep("pb tubaire unilatéral ",4)

v4 <- vector("character",252)
v4[1] = rep("autre",1)
v4[2:4]= rep("endométriose",3)
v4[5]= rep("hydrosalpinx",1)
v4[6:237]= rep("non",232)
v4[237:240]= rep("pb tubaire bilatéral",4)
v4[241:252]= rep("pb tubaire unilatéral",12)

v5 <- vector("character",16)
v5[1:2] = rep("endométriose",2)
v5[3]= rep("hydrosalpinx",1)
v5[4:15]= rep("non",12)
v5[16] = rep("pb tubaire unilatéral",1)


v6 <- vector("character",20)
v6[1] = rep("autre",1)
v6[2:3]= rep("endométriose",2)
v6[4:19]= rep("non",16)
v6[20]= rep("pb tubaire unilatéral",1)


v7 <- vector("character",197)
v7[1:2] = rep("autre",2)
v7[2:3]= rep("endométriose",2)
v7[4:163]= rep("non",160)
v7[164:169]= rep("pb tubaire bilatéral",6)
v7[170:196]= rep("pb tubaire unilatéral",27)

data_patho = couples[is.na(couples$patho_f) & !is.na(couples$bh_f) & !is.na(couples$ct_f),]
for (i in 1:length(data_patho)){
#patho_f = NA and ct_f et bh_f not NA
	if (couples[couples$ID == data_patho$ID[i],]$bh_f == "anormal" & couples[couples$ID == data_patho$ID[i],]$enfant == "0"){
		couples[couples$ID == data_patho$ID[i],]$patho_f = 'non'}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '1' & couples[couples$ID == data_patho$ID[i],]$bh_f == 'anormal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'anovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = sample(v1,replace = TRUE)[1]}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '1' & couples[couples$ID == data_patho$ID[i],]$bh_f == 'anormal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'dysovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = sample(v2,replace = TRUE)[1]}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '1' & couples[couples$ID == data_patho$ID[i],]$bh_f == 'anormal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'ovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = sample(v3,replace = TRUE)[1]}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '0' & couples[couples$ID == data_patho$ID[i],]$bh_f == 'normal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'anovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = sample(v5,replace = TRUE)[1]}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '0' &couples[couples$ID == data_patho$ID[i],]$bh_f == 'normal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'dysovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = sample(v6,replace = TRUE)[1]}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '0' & couples[couples$ID == data_patho$ID[i],]$bh_f == 'normal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'ovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = sample(v7,replace = TRUE)[1]}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '1' & couples[couples$ID == data_patho$ID[i],]$bh_f == 'normal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'anovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = 'non'}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '1' & couples[couples$ID == data_patho$ID[i],]$bh_f == 'normal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'dysovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = 'non'}

	else if (couples[couples$ID == data_patho$ID[i],]$enfant == '1' & couples[couples$ID == data_patho$ID[i],]$bh_f == 'normal' & couples[couples$ID == data_patho$ID[i],]$ct_f == 'ovulation'){
		couples[couples$ID == data_patho$ID[i],]$patho_f = sample(v4,replace = TRUE)[1]}

	else { print("nothing") }}

data_bh_f = couples[!is.na(couples$patho_f) & !is.na(couples$bh_f) & !is.na(couples$ct_f),]
table( data_bh_f$bh_f, data_bh_f$ct_f, data_bh_f$age_F_29_31, data_bh_f$trt_aucun)



data_bh_f = couples[!is.na(couples$patho_f) & is.na(couples$bh_f) & !is.na(couples$ct_f),]

v1_bh = vector("character",30)
v1_bh[1:5] = rep("anormal",5)
v1_bh[6:30] = rep("normal",25)

v2_bh = vector("character",59)
v2_bh[1:11] = rep("anormal",11)
v2_bh[12:59] = rep("normal",48)

v3_bh = vector("character",411)
v3_bh[1:4] = rep("anormal",4)
v3_bh[5:103] = rep("normal",99)

v3_bh = vector("character",411)
v3_bh[1:43] = rep("anormal",43)
v3_bh[44:411] = rep("normal",368)

v4_bh = vector("character",7)
v4_bh[1] = rep("anormal",1)
v4_bh[2:7] = rep("normal",6)

v5_bh = vector("character",13)
v5_bh[1:3] = rep("anormal",3)
v5_bh[4:13] = rep("normal", 10)

v6_bh = vector("character",103)
v6_bh[1:4] = rep("anormal",4)
v6_bh[5:103] = rep("normal",99)

v7_bh = vector("character",4)
v7_bh[1] = rep("anormal",1)
v7_bh[2:4] = rep("normal",3)

v8_bh = vector("character",9)
v8_bh[1:3] = rep("anormal",3)
v8_bh[4:9] = rep("normal", 6)

v9_bh = vector("character",17)
v9_bh[1:4] = rep("anormal",4)
v9_bh[5:17] = rep("normal",13)


v10_bh = vector("character",2)
v10_bh[1] = rep("anormal",1)
v10_bh[2] = rep("normal",1)

v11_bh = vector("character",3)
v11_bh[1:2] = rep("anormal",2)
v11_bh[3] = rep("normal", 1)

v12_bh = vector("character",2)
v12_bh[1:4] = rep("normal",2)




for (i in 1:dim(data_bh_f)[1]){
#bh_f = NA and ct_f et patho_f not NA
	if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "anovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v1_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "dysovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v2_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "ovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v3_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "anovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v4_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "dysovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v5_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "ovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v6_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "anovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v7_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "dysovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v8_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "ovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v9_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "anovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v10_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "dysovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v11_bh, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bh_f$ID[i],]$ct_f == "ovulation" & couples[couples$ID == data_bh_f$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bh_f$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bh_f$ID[i],]$bh_f = sample(v12_bh, replace = TRUE)[1]}

	else {print("nothing")}}


data_ct_f = couples[!is.na(couples$patho_f) & !is.na(couples$bh_f) & !is.na(couples$ct_f),]
table( data_ct_f$ct_f, data_ct_f$patho_f, data_ct_f$trt_icsi, data_ct_f$spermo_anormal) 



#0 - 0
#autre
v1_ct = vector("character",3)
v1_ct[1] = rep("dysovulation",1)
v1_ct[2:3] = rep("ovulation",2)

#endométriose
v2_ct = vector("character",4)
v2_ct[1:3] = rep("dysovulation",3)
v2_ct[4] = rep("ovulation",1)

#hydrosalpinx and pb tubaire bilatéral - unilatéral = "ovulation"

#non
v3_ct = vector("character",162)
v3_ct[1:8] = rep("anovulation",8)
v3_ct[9:46] = rep("dysovulation",38)
v3_ct[47:162] = rep("ovulation",116)


#1 - 0
#tubaire bilatéral et endométriose = "ovulation"
#autre et hydrosalpinx
v4_ct = vector("character",3)
v4_ct[1] = rep("anovulation",1)
v4_ct[2] = rep("dysovulation",1)
v4_ct[3] = rep("ovulation",1)

#pb tubaire unilatéral
v6_ct = vector("character",5)
v6_ct[1:2] = rep("anovulation",2)
v6_ct[3:5] = rep("ovulation",3)

#non
v5_ct = vector("character",71)
v5_ct[1] = rep("anovulation",1)
v5_ct[2:5] = rep("dysovulation",4)
v5_ct[6:71] = rep("ovulation",66)

#0 - 1
#hydrosalpinx et pb tubaire bilatéral = "anovulation"
#pb tubaire unilatéral = ovulation
#autre 
v7_ct = vector("character",8)
v7_ct[1:5] = rep("dysovulation",5)
v7_ct[6:8] = rep("ovulation",3)

#endométriose 
v8_ct = vector("character",5)
v8_ct[1:3] = rep("dysovulation",3)
v8_ct[4:5] = rep("ovulation",2)

#non
v9_ct = vector("character",141)
v9_ct[1:15] = rep("anovulation",15)
v9_ct[16:33] = rep("dysovulation",18)
v9_ct[34:141] = rep("ovulation",108)

#1 - 1
# pb tubaire bilatéral - pb tubaire unilatéral = ovulation
#autre et hydrosalpinx  : random
v10_ct = vector("character",3)
v10_ct[1] = rep("anovulation",1)
v10_ct[2] = rep("dysovulation",1)
v10_ct[3] = rep("ovulation",1)

# endométriose 
v11_ct = vector("character",5)
v11_ct[1:2] = rep("anovulation",2)
v11_ct[3:5] = rep("ovulation",3)

#non
v12_ct = vector("character",229)
v12_ct[1:14] = rep("anovulation",14)
v12_ct[15:32] = rep("anovulation",18)
v12_ct[33:228] = rep("ovulation",196)

data_ct_f_trans = couples[!is.na(couples$patho_f) & !is.na(couples$bh_f) & is.na(couples$ct_f),]

for (i in 1:dim(data_ct_f_trans)[1]){
#ct_f = NA and bh_f et patho_f not NA
	if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "autre" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v1_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "endométriose" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v2_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "hydrosalpinx" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire bilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire unilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "non" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v3_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "endométriose" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire bilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "autre" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v4_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "hydrosalpinx" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v4_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire unilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v6_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "non" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v5_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "hydrosalpinx" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "anovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire bilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "anovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire unilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "autre" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v7_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "endométriose" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v8_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "non" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v9_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire bilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire unilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "autre" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v10_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "hydrosalpinx" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v10_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "endométriose" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v11_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "non" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v12_ct, replace = TRUE)[1]}

	else {print("nothing")}}


#ct_f = NA and bh_f = NA

data_bh_f = couples[!is.na(couples$patho_f) & !is.na(couples$bh_f) & !is.na(couples$ct_f),]
table( data_bh_f$ct_f, data_bh_f$spermo_azoo, data_bh_f$age_F_29_31, data_bh_f$trt_aucun)

#bh_f
#0 - 0
#spermo_azoo = 0
v1_bhf = vector("character",489)
v1_bhf[1:69] = rep("anormal",69)
v1_bhf[70:489] = rep("normal",420)

#spermo_azoo = 1
v2_bhf = vector("character",85)
v2_bhf[1:7] = rep("anormal",7)
v2_bhf[8:85] = rep("normal",78)

#1 - 0
#spermo_azoo = 0
v3_bhf = vector("character",113)
v3_bhf[1:7] = rep("anormal",7)
v3_bhf[8:113] = rep("normal",106)

#spermo_azoo = 1
v4_bhf = vector("character",23)
v4_bhf[1:2] = rep("anormal",2)
v4_bhf[3:23] = rep("normal",21)

#0 - 1
#spermo_azoo = 0
v5_bhf = vector("character",45)
v5_bhf[1:9] = rep("anormal",9)
v5_bhf[10:45] = rep("normal",36)

#spermo_azoo = 1
#bhf = normal

#1 - 1
#spermo_azoo = 0
v6_bhf = vector("character",3)
v6_bhf[1] = rep("anormal",1)
v6_bhf[2:3] = rep("normal",2)

#spermo_azoo = 1
v7_bhf = vector("character",2)
v7_bhf[1] = rep("anormal",1)
v7_bhf[2] = rep("normal",1)

data_ct_f_trans = couples[!is.na(couples$patho_f) & is.na(couples$bh_f) & is.na(couples$ct_f),]
table( data_bh_f$bh_f, data_bh_f$spermo_azoo, data_bh_f$age_F_29_31, data_bh_f$trt_aucun)

for (i in 1:dim(data_ct_f_trans)[1]){
	if (couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$bh_f = sample(v1_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$bh_f = sample(v2_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$bh_f = sample(v3_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$bh_f = sample(v4_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$bh_f = sample(v5_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$bh_f = "normal"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$bh_f = sample(v6_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$bh_f = sample(v6_bhf, replace = TRUE)[1]}

	else {print("nothing")}}


for (i in 1:dim(data_ct_f_trans)[1]){
#ct_f = NA and bh_f = NA et patho_f not NA
	if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "autre" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v1_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "endométriose" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v2_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "hydrosalpinx" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire bilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire unilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "non" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v3_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "endométriose" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire bilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "autre" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v4_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "hydrosalpinx" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v4_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire unilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v6_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "non" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v5_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "hydrosalpinx" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "anovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire bilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "anovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire unilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "autre" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v7_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "endométriose" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v8_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "non" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "0" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v9_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire bilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "pb tubaire unilatéral" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = "ovulation"}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "autre" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v10_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "hydrosalpinx" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v10_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "endométriose" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v11_ct, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ct_f_trans$ID[i],]$patho_f == "non" & couples[couples$ID == data_ct_f_trans$ID[i],]$trt_icsi == "1" & couples[couples$ID == data_ct_f_trans$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ct_f_trans$ID[i],]$ct_f = sample(v12_ct, replace = TRUE)[1]}

	else {print("nothing")}}


#patho_f = NA and bh_f = NA

data_bhf_patho = couples[is.na(couples$patho_f) & is.na(couples$bh_f) & !is.na(couples$ct_f),]

for (i in 1:dim(data_bhf_patho)[1]){
	if (couples[couples$ID == data_bhf_patho$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bhf_patho$ID[i],]$bh_f = sample(v1_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bhf_patho$ID[i],]$bh_f = sample(v2_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bhf_patho$ID[i],]$bh_f = sample(v3_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bhf_patho$ID[i],]$bh_f = sample(v4_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bhf_patho$ID[i],]$bh_f = sample(v5_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bhf_patho$ID[i],]$bh_f = "normal"}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bhf_patho$ID[i],]$bh_f = sample(v6_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bhf_patho$ID[i],]$bh_f = sample(v6_bhf, replace = TRUE)[1]}

	else {print("nothing")}}

#patho_f = NA

table(desc$patho_f, desc$fecondite, desc$trt_aucun)

#fecondite=0
#trt_aucun = 0
v1_patho <- vector("character",396)
v1_patho[1] = rep("autre",1)
v1_patho[2:11]= rep("endométriose",10)
v1_patho[12]= rep("hydrosalpinx",1)
v1_patho[13:359]= rep("non",347)
v1_patho[360:367]= rep("pb tubaire bilatéral",8)
v1_patho[368:396]= rep("pb tubaire unilatéral",29)

#trt_aucun = 1
v2_patho <- vector("character",182)
v2_patho[1:4] = rep("autre",4)
v2_patho[5:6]= rep("endométriose",2)
v2_patho[7]= rep("hydrosalpinx",1)
v2_patho[8:161]= rep("non",154)
v2_patho[162:164]= rep("pb tubaire bilatéral",3)
v2_patho[165:182]= rep("pb tubaire unilatéral",18)


#fecondite=1
#trt_aucun = 0
v3_patho <- vector("character",18)
v3_patho[1:2] = rep("autre",2)
v3_patho[3]= rep("endométriose",1)
v3_patho[4:18]= rep("non",15)

#trt_aucun = 1
v4_patho <- vector("character",8)
v4_patho[1] = rep("pb tubaire unilatéral",1)
v4_patho[2]= rep("endométriose",1)
v4_patho[3:8]= rep("non",6)

for (i in 1:dim(data_bhf_patho)[1]){
	if (couples[couples$ID == data_bhf_patho$ID[i],]$fecondite == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bhf_patho$ID[i],]$patho_f = sample(v1_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$fecondite == "0" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bhf_patho$ID[i],]$patho_f = sample(v2_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$fecondite == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_bhf_patho$ID[i],]$patho_f = sample(v3_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_bhf_patho$ID[i],]$fecondite == "1" & couples[couples$ID == data_bhf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_bhf_patho$ID[i],]$patho_f = sample(v4_patho, replace = TRUE)[1]}

	else {print("nothing")}}


#ct_f = NA and patho_f = NA
data_ctf_patho = couples[is.na(couples$patho_f) & !is.na(couples$bh_f) & is.na(couples$ct_f),]

for (i in 1:dim(data_ctf_patho)[1]){
	if (couples[couples$ID == data_ctf_patho$ID[i],]$fecondite == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$patho_f = sample(v1_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$fecondite == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$patho_f = sample(v2_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$fecondite == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$patho_f = sample(v3_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$fecondite == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$patho_f = sample(v4_patho, replace = TRUE)[1]}

	else {print("nothing")}}

table(data_ctf_patho$ct_f, data_ctf_patho$trt_aucun, data_ctf_patho$spermo_anormal)

#trt_aucun = 0
#spermo_anormal = 0

v1_patho_ctf <- vector("character",212)
v1_patho_ctf[1:9] = rep("anovulation",9)
v1_patho_ctf[10:39]= rep("dysovulation",30)
v1_patho_ctf[40:212]= rep("ovulation",173)

#spermo_anormal = 1

v2_patho_ctf <- vector("character",13)
v2_patho_ctf[1:4]= rep("dysovulation",4)
v2_patho_ctf[5:13]= rep("ovulation",9)


#trt_aucun = 1
#spermo_anormal = 0

v3_patho_ctf <- vector("character",366)
v3_patho_ctf[1:25] = rep("anovulation",25)
v3_patho_ctf[26:56]= rep("dysovulation",31)
v3_patho_ctf[57:366]= rep("ovulation",310)


#spermo_anormal = 1

v4_patho_ctf <- vector("character",13)
v4_patho_ctf[1:3] = rep("anovulation",3)
v4_patho_ctf[4:7]= rep("dysovulation",4)
v4_patho_ctf[8:13]= rep("ovulation",6)

for (i in 1:dim(data_ctf_patho)[1]){
	if (couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$ct_f = sample(v1_patho_ctf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$ct_f = sample(v2_patho_ctf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun== "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$ct_f = sample(v3_patho_ctf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun== "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$ct_f = sample(v4_patho_ctf, replace = TRUE)[1]}

	else {print(data_ctf_patho$ID[i])}}

#patho, ct et bh_f NAN

data_ctf_patho = couples[is.na(couples$bh_f) & is.na(couples$patho_f) & is.na(couples$ct_f),]

for (i in 1:dim(data_ctf_patho)[1]){
	if (couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$spermo_anormal == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$ct_f = sample(v1_patho_ctf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$spermo_anormal == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$ct_f = sample(v2_patho_ctf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun== "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$ct_f = sample(v3_patho_ctf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun== "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$ct_f = sample(v4_patho_ctf, replace = TRUE)[1]}

	else {print(data_ctf_patho$ID[i])}}


for (i in 1:dim(data_ctf_patho)[1]){
	if (couples[couples$ID == data_ctf_patho$ID[i],]$fecondite == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$patho_f = sample(v1_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$fecondite == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$patho_f = sample(v2_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$fecondite == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$patho_f = sample(v3_patho, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$fecondite == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$patho_f = sample(v4_patho, replace = TRUE)[1]}

	else {print("nothing")}}


for (i in 1:dim(data_ctf_patho)[1]){
	if (couples[couples$ID == data_ctf_patho$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$bh_f = sample(v1_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$bh_f = sample(v2_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$bh_f = sample(v3_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "0"){
		couples[couples$ID == data_ctf_patho$ID[i],]$bh_f = sample(v4_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$bh_f = sample(v5_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$age_F_29_31 == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$bh_f = "normal"}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$spermo_azoo == "0" & couples[couples$ID == data_ctf_patho$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$bh_f = sample(v6_bhf, replace = TRUE)[1]}

	else if (couples[couples$ID == data_ctf_patho$ID[i],]$spermo_azoo == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$age_F_29_31 == "1" & couples[couples$ID == data_ctf_patho$ID[i],]$trt_aucun == "1"){
		couples[couples$ID == data_ctf_patho$ID[i],]$bh_f = sample(v6_bhf, replace = TRUE)[1]}

	else {print("nothing")}}


rownames(couples) <- NULL

#create new var and NA bh_f

couples$bh_f_anormal <- as.factor(ifelse(couples$bh_f == "anormal" & !is.na(couples$bh_f), 1, 0))
couples$bh_f_normal <- as.factor(ifelse(couples$bh_f == "normal" & !is.na(couples$bh_f), 1, 0))
couples$bh_f_NA <- as.factor(ifelse(is.na(couples$bh_f)  , 1, 0))

#create new var and NA ct_f

couples$ct_f_ovulation <- as.factor(ifelse(couples$ct_f == "ovulation" & !is.na(couples$ct_f) , 1, 0))
couples$ct_f_dysovulation <- as.factor(ifelse(couples$ct_f == "dysovulation" & !is.na(couples$ct_f), 1, 0))
couples$ct_f_anovulation <- as.factor(ifelse(couples$ct_f == "anovulation" & !is.na(couples$ct_f)  , 1, 0))


pm <- c('endométriose','hydrosalpinx')
couples$patF_maladie = as.factor(ifelse(couples$patho_f %in% pm, 1, 0))
pt <- c('pb tubaire bilatéral','pb tubaire unilatéral')
couples$patF_tubaire = as.factor(ifelse(couples$patho_f %in% pt, 1, 0))

couples$patF_na = as.factor(ifelse(is.na(couples$patho_f), 1, 0))
couples$patF_autre = as.factor(ifelse(couples$patho_f == 'autre' & !is.na(couples$patho_f), 1, 0))
couples$patF_non = as.factor(ifelse(couples$patho_f == 'non' & !is.na(couples$patho_f) , 1, 0))

rownames(couples) <- NULL
#create csv
write.csv(couples, file = "clean_data.csv", fileEncoding = "UTF-8")



#apprentissage

names(couples)
couples <-couples[,!(colnames(couples) %in% c( "ddn","dconception"))]

#patho_f -> ne marche pas
df_app_patho_f <- couples[,!(colnames(couples) %in% c("id","age_F", "dconsultation","age_h", "age_f", "diplome_h", "bmi_h", "patho_h", "diplome_f", "bh_f", "ct_f", "bmi_h_class", "duree_infertilite", "ddnconcept", "dconsultation", "couples$diff_consult_ddnconcept", "couples$duree_infertilite_class",
 "ct_f_NA"df_app_patho_f <- couples[,!(colnames(couples) %in% c("id","age_F", "dconsultation","age_h", "age_f", "diplome_h", "bmi_h", "patho_h", "diplome_f", "bh_f", "ct_f", "bmi_h_class", "duree_infertilite", "ddnconcept", "dconsultation", "couples$diff_consult_ddnconcept", "couples$duree_infertilite_class",
 "ct_f_NA"))]

train_patho_autre_df <- df_app_patho_f[df_app_patho_f$patho_f == "autre",] %>% na.omit(df_app_patho_f$patho_f)
  # Then we sample our original dataframe to avoid over-sampling
train_patho_endo_df <- df_app_patho_f[df_app_patho_f$patho_f == "endométriose",] %>% na.omit(df_app_patho_f$patho_f)
train_patho_hydro_df <- df_app_patho_f[df_app_patho_f$patho_f == "hydrosalpinx",] %>% na.omit(df_app_patho_f$patho_f)
train_patho_non_df <- df_app_patho_f[df_app_patho_f$patho_f == "non",] %>% na.omit(df_app_patho_f$patho_f)
train_patho_tubbi_df <- df_app_patho_f[df_app_patho_f$patho_f == "pb tubaire bilatéral",] %>% na.omit(df_app_patho_f$patho_f)
train_patho_tubuni_df <- df_app_patho_f[df_app_patho_f$patho_f == "pb tubaire unilatéral",] %>% na.omit(df_app_patho_f$patho_f)


train_patho_autre_df <- sample_n(train_patho_autre_df, size = nrow(train_patho_autre_df))
train_patho_endo_df <- sample_n(train_patho_endo_df, size = nrow(train_patho_endo_df))
train_patho_hydro_df <- sample_n(train_patho_hydro_df, size = nrow(train_patho_hydro_df))
train_patho_non_df <- sample_n(train_patho_non_df, size = nrow(train_patho_non_df))
train_patho_tubbi_df <- sample_n(train_patho_tubbi_df, size = nrow(train_patho_tubbi_df))
train_patho_tubuni_df <- sample_n(train_patho_tubuni_df, size = nrow(train_patho_tubuni_df))

train_df_patho <- data.frame(rbind(
    train_patho_autre_df,
    train_patho_endo_df,
    train_patho_hydro_df,
    train_patho_non_df,
    train_patho_tubbi_df,
    train_patho_tubuni_df
  ))

train_df_patho$patho_f = droplevels(train_df_patho$patho_f)

train_df_patho <- data.frame(rbind(
    train_patho_autre_df,
    train_patho_endo_df,
    train_patho_hydro_df,
    train_patho_non_df,
    train_patho_tubbi_df,
    train_patho_tubuni_df
  ))

train_df_patho$patho_f = droplevels(train_df_patho$patho_f)
test_df <- train_df_patho[which(is.na(train_df_patho$patho_f)),]

rf1 <- randomForest(patho_f ~ ., train_df_patho, ntree=50, norm.votes=FALSE)
rf1$confusion
svm.model <- svm(patho_f ~ ., data = train_df_patho, cost = 100, gamma = 1, probability=TRUE)

rf2 <- randomForest(patho_f ~ ., train_df_patho, ntree=50, norm.votes=FALSE)
rf3 <- randomForest(patho_f ~ ., train_df_patho, ntree=50, norm.votes=FALSE)
rf.all <- combine(rf1, rf2, rf3)
print(rf.all$c onfusion)

#bh_f
df_app_bh_f <- couples[,!(colnames(couples) %in% c("id","age_F","patho_f", "dconsultation","age_h", "age_f", "diplome_h", "bmi_h", "patho_h", "diplome_f", "ct_f", "bmi_h_class", "duree_infertilite", "ddnconcept", "dconsultation", "couples$diff_consult_ddnconcept", "couples$duree_infertilite_class","ct_f_NA", "bh_f_anormal", "bh_f_normal"))]

train_bh_anormal <- df_app_bh_f[df_app_bh_f$bh_f == "anormal",] %>% na.omit(df_app_bh_f$bh_f)
train_bh_normal <- df_app_bh_f[df_app_bh_f$bh_f == "normal",] %>% na.omit(df_app_bh_f$bh_f)

train_bh_anormal <- sample_n(train_bh_anormal, size = nrow(train_bh_anormal))
train_bh_normal <- sample_n(train_bh_normal, size = nrow(train_bh_normal))

train_df_bh_f <- data.frame(rbind(
    setDT(train_bh_anormal),
    setDT(train_bh_normal) 
  ))

test_df <- df_app_bh_f[which(is.na(df_app_bh_f$bh_f)),]
test_df$bh_f = droplevels(test_df$bh_f)

train_df_bh_f$bh_f = droplevels(train_df_bh_f$bh_f)
library(e1071)
library(rpart)
rf1 <- randomForest(bh_f ~ ., train_df_bh_f, ntree=50, rm.votes=FALSE)
rf1$confusion
svm.model <- svm(bh_f ~ ., data = train_df_bh_f,family=binomial, cost = 100, gamma = 1, probability=TRUE)

testPred = predict(svm.model, newdata=test_df, probability=TRUE)

predict <- fitted(svm.model)
pred <- predict(svm.model,test_df)
pred
cm <- table(predict, train_df_bh_f$bh_f)
rf2 <- randomForest(bh_f ~ ., train_df_bh_f, ntree=500, norm.votes=FALSE)
rf3 <- randomForest(bh_f ~ ., train_df_bh_f, ntree=500, norm.votes=FALSE)
rf4 <- randomForest(bh_f ~ ., train_df_bh_f, ntree=500, norm.votes=FALSE)
rf5 <- randomForest(bh_f ~ ., train_df_bh_f, ntree=500, norm.votes=TRUE)


preds <- predict(rf1, test_df, type="response")

train_patho_hydro_
df_app_bh_f <- couples[,!(colnames(couples) %in% c("id","age_F", "dconsultation","age_h", "age_f", "diplome_h", "bmi_h", "patho_h", "diplome_f", "ct_f", "bmi_h_class", "duree_infertilite", "ddnconcept", "dconsultation", "couples$diff_consult_ddnconcept", "couples$duree_infertilite_class","ct_f_NA"))]
library(caret)
library(dplyr)
library(ipSolve)
	
learn <- function(Y, X) {
  # Specify cross validation tuning
  fitControl <- trainControl(method = "cv", 
                             number = 10)
  rpart_grid <- expand.grid(.cp=0.2)}

train_bh_fN_df <- couples[couples$bh_f == "normal",] %>% na.omit(couples$bh_f)
  # Then we sample our original dataframe to avoid over-sampling
train_bh_fA_df <- couples[couples$bh_f == "anormal",] %>% na.omit(couples$bh_f)

train_bh_fN_df <- sample_n(train_bh_fN_df, size = nrow(train_bh_fN_df))
train_bh_fA_df <- sample_n(train_bh_fA_df, size = nrow(train_bh_fA_df))





train_df_bh_f <- data.frame(rbind(
    train_bh_fN_df,
    train_bh_fA_df
  ))



#create csv
#creation de 2 classes pour la variable duree_infertilite
couples$duree_infertilite_class <- as.factor(ifelse(couples$duree_infertilite <= 24, "infe_2_ans", "sup_2_ans")) 
#test independance
#1 ok
test_enfant_trait <- chisq.test(table(couples$enfant,couples$traitement)) 
#2 ok
test_enfant_age_M <- chisq.test(table(couples$enfant,couples$age_M)) 
#3 ok
test_enfant_age_F <- chisq.test(table(couples$enfant,couples$age_F))
#4 no
test_enfant_ct_f_ovulation) <- chisq.test(table(couples$enfant,couples$ct_f_ovulation))
#5 no
test_enfant_bmi_h_class <- chisq.test(table(couples$enfant,couples$bmi_h_class))

df_test_reg <- couples[,!(colnames(couples) %in% c("id","age_F", "dconsultation", "dconception", "ddn", "age_h", "age_f", "diplome_h", "bmi_h", "patho_h", "patho_f", "diplome_f", "bh_f", "ct_f", "bmi_h_class", "duree_infertilite", "ddnconcept", "dconsultation", "couples$diff_consult_ddnconcept", "couples$duree_infertilite_class"))]


reg <- glm(enfant ~ ., family=binomial, data=df_test_reg)
reg2 = step(reg)                                                                 
enf.pred <- predict(reg2, type = "response", newdata = df_test_reg)                                                                   
table(enf.pred >0.5 , df_test_reg$enfant)                                                                  
                                                                
library("caret")
Train <- createDataPartition(df_test_reg$enfant, p=0.7, list=FALSE)
training <- df_test_reg[ Train, ]
testing <- df_test_reg[ -Train, ]

mod_fit <- train(enfant ~ .,  data=training, method="glm", family="binomial")use 
pred_d_test <- predict(mod_fit, newdata=testing)

accuracy <- table(pred_d_test , testing[,"enfant"])
summary(df_test_reg[,-1])


#abb
'''
table(couples$enfant, couples$ct_f_anovulation, couples$trt_iac)#5
table(couples$enfant, couples$ct_f_anovulation, couples$trt_iad)#0
table(couples$enfant, couples$ct_f_anovulation, couples$trt_aucun)#4
table(couples$enfant, couples$ct_f_anovulation, couples$trt_fiv)#6 dont 4 avec enfant

table(couples$enfant, couples$spermo_azoo, couples$trt_iac)#8
table(couples$enfant, couples$spermo_azoo, couples$trt_fiv)#1
table(couples$enfant, couples$spermo_azoo, couples$trt_icsi)#37 enfant
table(couples$enfant, couples$spermo_azoo, couples$trt_aucun)#0
table(couples$enfant, couples$ct_f_anovulation, couples$spermo_azoo)

table(couples$enfant, couples$ct_f_anovulation, couples$trt_iad)#1 et pas d'enfant

barplot(table(couples$enfant, couples$age_F),
main = "enfant en fonction de la presence de crytorchidie",
xlab = "Class",
col = c("red","green"))
legend("topright",
c("no child"," child"),
fill = c("red","green")
)


barplot(table(couples$enfant, couples$duree_infertilite_class),
main = "enfant en fonction de la presence de crytorchidie",
xlab = "Class",
col = c("red","green"))
legend("topright",
c("no child"," child"),
fill = c("red","green")
)'''

library("survival")
couples.surv=Surv(as.numeric(couples$diff_consult_ddnconcept ==1),as.numeric(couples$enfant))

couples.plot <- coxph(couples.surv ~ couples$trt_iac + couples$trt_iad)
plot(survfit(couples.plot), ylim =c(.7 , 1))


