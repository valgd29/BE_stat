
couples <- read.table(file = "C:/Users/valentin/Documents/BE_stat/couples.csv", sep = ",", header= TRUE)

couples$enfant = as.factor(couples$enfant)

couples[couples == ""] <- NA
couples[couples == "."] <- NA
#Date format

couples$dconsultation = as.Date(couples$dconsultation, ,format='%m/%d/%Y')
couples$dconception= as.Date(couples$dconception, ,format='%m/%d/%Y')
couples$ddn = as.Date(couples$ddn, ,format='%m/%d/%Y')


#create new var and NA bh_f

couples$bh_f_anormal <- as.factor(ifelse(couples$bh_f == "anormal" & !is.na(couples$bh_f), 1, 0))
couples$bh_f_normal <- as.factor(ifelse(couples$bh_f == "normal" & !is.na(couples$bh_f), 1, 0))
couples$bh_f_NA <- as.factor(ifelse(is.na(couples$bh_f) & !is.na(couples$bh_f)  , 1, 0))

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

#convert var age_M to numeric
couples$age_h = as.numeric(as.character(couples$age_h))
couples$age_M = quant.cut(couples$age_h, 7)
summary(couples)


#bmi and class
couples$bmi_h = as.numeric(as.character(couples$bmi_h))
#mean inputation
couples$bmi_h[is.na(couples$bmi_h)] <- round(mean(couples$bmi_h, na.rm = TRUE))

couples$bmi_h_class <- cut(couples$bmi_h, breaks = c(0, 18.5, 25, 30, 35, 40, 55), na.rm = FALSE)
#couples$bmi_h_0_18_5 <- as.factor(ifelse(couples$bmi_h_class == (0,18.5], 1, 0))
#couples$bmi_h_0_18_5 <- as.factor(ifelse(couples$bmi_h_class == (0,18.5], 1, 0))
#couples$bmi_h_0_18_5 <- as.factor(ifelse(couples$bmi_h_class == (0,18.5], 1, 0))
#couples$bmi_h_0_18_5 <- as.factor(ifelse(couples$bmi_h_class == (0,18.5], 1, 0))
par(mfrow=c(3,3))
hist(couples$age_h)
hist(couples$age_f)

df[,!(colnames(df) %in% c("date", "insee", "ddH10_rose4", "ecart", "test"))]
df <-  couples[, colnames(couples) %in% c("enfant",  "bh_f_anormal" , "bh_f_normal" , "bh_f_NA" , "ct_f_ovulation" , "ct_f_dysovulation" ,"ct_f_anovulation" , "ct_f_NA" ,
"age_F", "age_M", "bmi_h_class")]
                                                                 
                                                                   
                                                                   
                                                                   
                                                                
couples$enfant = as.factor(couples$enfant)
couples$enfant = as.factor(couples$enfant)
couples$enfant = as.factor(couples$enfant)
summary(couples)