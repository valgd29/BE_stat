
couples <- read.table(file = "C:/Users/valentin/Documents/BE_stat/couples.csv", sep = ",", header= TRUE)

couples$enfant = as.factor(couples$enfant)

library("ggplot2")
library("reshape2")


pairs(couples[, c(2, 7, 8, 9, 10)])

pairs(couples)
couples[couples == ""] <- NA
couples[couples == "."] <- NA



library("VIM")
#graph valeurs manquantes par variable
pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(apply(couples,2,pMiss))


X <- couples[,(colnames(couples) %in% c("ddn", "dconception", "patho_f", "bh_f", "ct_f", "bmi_h", "diplome_h", "age_f"))]
aggr_plot <- aggr(X, col=c('navyblue','red'), numbers=T, sortVars=T, labels=names(X), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#suppression valeurs na pour age_f
couples <- subset(couples, !is.na(couples$age_f))
summary(aggr_plot)
aggr_plot$percent

#Date format
couples$dconsultation = as.Date(couples$dconsultation ,format='%d/%m/%Y')
couples$dconception= as.Date(couples$dconception ,format='%d/%m/%Y')
couples$ddn = as.Date(couples$ddn ,format='%d/%m/%Y')

#couples ecart ddn, dconception et dconsultation
#var ddn et dconsultation
couples$ddnconcept <- ifelse(!is.na(couples$ddn), couples$ddn, couples$dconception)
couples$ddnconcept <- as.Date(couples$ddnconcept, origin = "1970-01-01")

couples$diff_consult_ddnconcept <-  couples$ddnconcept - couples$dconsultation 
  
#create new var and NA bh_f

couples$bh_f_anormal <- as.factor(ifelse(couples$bh_f == "anormal" & !is.na(couples$bh_f), 1, 0))
couples$bh_f_normal <- as.factor(ifelse(couples$bh_f == "normal" & !is.na(couples$bh_f), 1, 0))
couples$bh_f_NA <- as.factor(ifelse(is.na(couples$bh_f)  , 1, 0))

#create new var and NA ct_f

couples$ct_f_ovulation <- as.factor(ifelse(couples$ct_f == "ovulation" & !is.na(couples$ct_f) , 1, 0))
couples$ct_f_dysovulation <- as.factor(ifelse(couples$ct_f == "dysovulation" & !is.na(couples$ct_f), 1, 0))
couples$ct_f_anovulation <- as.factor(ifelse(couples$ct_f == "anovulation" & !is.na(couples$ct_f)  , 1, 0))
couples$ct_f_NA <- as.factor(ifelse(is.na(couples$ct_f)  , 1, 0))


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


pm <- c('endométriose','hydrosalpinx')
couples$patF_maladie = as.factor(ifelse(couples$patho_f %in% pm, 1, 0))
pt <- c('pb tubaire bilatéral','pb tubaire unilatéral')
couples$patF_tubaire = as.factor(ifelse(couples$patho_f %in% pt, 1, 0))

couples$patF_na = as.factor(ifelse(is.na(couples$patho_f), 1, 0))
couples$patF_autre = as.factor(ifelse(couples$patho_f == 'autre' & !is.na(couples$patho_f), 1, 0))
couples$patF_non = as.factor(ifelse(couples$patho_f == 'non' & !is.na(couples$patho_f) , 1, 0))


couples$bmi_h_class <- cut(couples$bmi_h, breaks = c(0, 18.5, 25, 30, 35, 40, 55), na.rm = FALSE)
couples$bmi_h_0_18_5 <- as.factor(ifelse(couples$bmi_h_class == "(0,18.5]", 1, 0))
couples$bmi_h_18_5_25 <- as.factor(ifelse(couples$bmi_h_class == "(18.5,25]", 1, 0))
couples$bmi_h_25_30 <- as.factor(ifelse(couples$bmi_h_class == "(25,30]", 1, 0))
couples$bmi_h_35_40 <- as.factor(ifelse(couples$bmi_h_class == "(35,40]", 1, 0))
couples$bmi_h_40_55 <- as.factor(ifelse(couples$bmi_h_class == "(40,55]", 1, 0))

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

df_test_reg <- couples[,!(colnames(couples) %in% c("id","age_F", "dconsultation", "dconception", "ddn", "age_h", "age_f", "diplome_h", "bmi_h", "patho_h", "patho_f", "diplome_f", "bh_f", "ct_f", "bmi_h_class", "duree_infertilite", "ddnconcept", "dconsultation" ))]


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

table(couples$enfant, couples$ct_f_anovulation, couples$trt_iac)#5
table(couples$enfant, couples$ct_f_anovulation, couples$trt_iad)#0
table(couples$enfant, couples$ct_f_anovulation, couples$trt_aucun)#4
table(couples$enfant, couples$ct_f_anovulation, couples$trt_fiv)#6 dont 4 avec enfant

table(couples$enfant, couples$spermo_azoo, couples$trt_iac)#8
table(couples$enfant, couples$spermo_azoo, couples$trt_fiv)#1
table(couples$enfant, couples$spermo_azoo, couples$trt_icsi)#37 enfant
table(couples$enfant, couples$ct_f_anovulation, couples$trt_fiv)#6 dont 4 avec enfant

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
)