############# Statistique bivariée ###############

# importation du jeu de données nettoyé
couples_clean <- read.table(file = "C:/Users/Mehdi/Desktop/M2 2017-2018/S2/BE - Statistique et santé/Git/Data/couples_clean.csv", sep = ";", header = TRUE)

######### A FAIRE : pour chaque variable par rapport à la variable cible "enfant"
# tableau croisé 
# analyse de variance (aov)
# test de chi² (verifier s'il y a une dependance entre les variables)

tab  <- xtabs(~enfant + fecondite, couples_clean)
fit <- aov(enfant ~ fecondite, data=couples_clean) # fecondite ne pourra pas expliquer correctement la variable enfant
summary(fit)
khi2 <- chisq.test(tab) # on ne rejette pas l'hypothese d'independance => enfant et fecondite sont independants


df_cc <- couples_clean[,!(colnames(couples_clean) %in% c("id", "dconsultation", "dconception", "ddn", "age_h", "diplome_h",
                                                               "bmi_h", "patho_h", "age_f", "diplome_f", "bh_f", "ct_f", "patho_f",
                                                               "bh_f_NA", "duree_infertilite", "ct_f_NA","patF_na","spermo",
                                                               "ddconcept","traitement","diff_consult_ddnconcept","ddnconcept",
                                                               "age_F","age_M"))]

df_cc <- couples_clean[,!(colnames(couples_clean) %in% c("id", "dconsultation", "dconception", "ddn", "age_h", "diplome_h",
                                                         "bmi_h", "patho_h", "age_f", "diplome_f","bh_f_NA",
                                                         "duree_infertilite", "ct_f_NA","patF_na","spermo", "ddconcept",
                                                         "traitement","diff_consult_ddnconcept","ddnconcept","age_F",
                                                         "age_M","ct_f_anovulation","ct_f_ovulation","ct_f_dysovulation",
                                                         "bh_f_normal","bh_f_anormal","patF_non","patF_autre","patF_tubaire","patF_maladie"))]

'''
variables <- colnames(df_cc)
nb_variable <- length(variables)
listPValues <- list(NULL)
for(i in 1:nb_variable){
  khi2 <- chisq.test(couples_clean$enfant, df_cc[,i])
  listPValues[[i]] <- khi2$p.value
}
v1 <- as.data.frame(variables)
v2 <- as.data.frame(listPValues)
v2 <- t(v2)
colnames(v1) <- "varNames"
rownames(v2) <- 1:length(v2)
colnames(v2) <- "pValues"
v3 <- cbind(v1,v2)
'''

variables <- colnames(df_cc)
nb_variable <- length(variables)
v1 <- as.data.frame(variables)
colnames(v1) <- "varNames"
v3 <- data.frame()
for (j in 1:nb_variable) {
  listPValues <- list(NULL)
  for(i in 1:nb_variable){
    khi2 <- chisq.test(df_cc[,j], df_cc[,i])
    listPValues[[i]] <- khi2$p.value
  }
  v2 <- as.data.frame(listPValues)
  v2 <- t(v2)
  rownames(v2) <- 1:length(v2)
  colnames(v2) <- as.character(colnames(df_cc[j]))
  if(length(v3) == 0){
    v3 <- cbind(v1,v2)
  }else{
    v3 <- cbind(v3,v2)
  }
}

row.names(v3) <- v3$varNames
v3 <- subset(v3, select = -varNames)
v3 = as.matrix(v3)
for (i in 1:length(v3)){
  v3[i,i] <- 0
}
v3 = as.matrix(v3)

#### HeatMAP

library(reshape2)


# Obtenir le triangle inférieur
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Obtenir le triangle supérieur
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(v3)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
#melted_cormat$Y1 <- cut(upper_tri$value,breaks = c(0,0.00001:0.01,1),right = FALSE)
#p <- ggplot(data =  melted_cormat, aes(x = Row, y = Col)) + geom_tile(aes(fill = Y1), colour = "white") +
#  scale_fill_brewer(palette = "PRGn")

intervals <- c(-1,0,0.001,0.01,0.05,0.1, 0.2,1)
binned <- cut(melted_cormat$value,breaks=intervals)
colfunc <- colorRampPalette(c("black","#990000", "#FF3333", "#FF8000", "#FFB266", "#FFE600", "#FFFFCC"))
colgroups <- colfunc(length(levels(binned)))
res <- colgroups[as.integer(binned)]
res <- factor(res,levels=colgroups)

p <- ggplot(melted_cormat,aes(x=Var1, y=Var2,fill=res)) + 
  geom_tile(color="white") +
  scale_fill_manual(values=levels(res),labels=levels(binned), name = "P_values")

base_size <- 10
p <- p + labs(x="Variables explicatives", y="variables d'interets")
p <- p + scale_x_discrete(expand = c(0,0))
p <- p + theme(axis.ticks=element_blank(),
               axis.text.x=element_text(size=base_size*0.8, angle=90, hjust = 1, colour="grey20"))


## https://mintgene.wordpress.com/2012/11/22/controlling-heatmap-colors-with-ggplot2/


######## graph avec pourcentage
library(ggplot2)
library(gridExtra)

tab  <- xtabs(~enfant + trt_iac, couples_clean)
tab <- as.data.frame(tab)
sumIndividu <- aggregate(Freq ~ trt_iac, tab, sum)
colnames(sumIndividu) <- c("trt_iac","CumFreq")
tab <- merge(sumIndividu,tab)

p1<-ggplot(tab, aes(x = trt_iac, y = Freq, fill=factor(enfant))) + 
  geom_bar(stat = "identity") +
  labs(x="Traitement IAC", y="Effectifs") +
  geom_text(aes(label=sprintf("%.2f%%", Freq/CumFreq * 100)), position= position_stack(vjust=0.5), color="black") +
  scale_fill_discrete(name ="Enfant", labels=c("Non", "Oui"))
