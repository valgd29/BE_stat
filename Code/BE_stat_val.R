couples <- read.table(file = "C:/Users/valentin/Documents/BE_stat/couples.csv", sep = ",", header= TRUE)

couples$enfant = as.factor(couples$enfant)


#Date format
couples$dconsultation = as.Date(couples$dconsultation, ,format='%m/%d/%Y')
couples$dconception= as.Date(couples$dconception, ,format='%m/%d/%Y')
couples$ddn = as.Date(couples$ddn, ,format='%m/%d/%Y')


#create new var and NA bh_f
couples$bh_f[couples$bh_f == ""] <- NA
couples$bh_f[couples$bh_f == "."] <- NA

couples$bh_f_anormal <- as.factor(ifelse(couples$bh_f == "anormal", 1, 0))
couples$bh_f_normal <- as.factor(ifelse(couples$bh_f == "normal", 1, 0))
couples$bh_f_NA <- as.factor(ifelse(is.na(couples$bh_f)  , 1, 0))

#create new var and NA ct_f
couples$ct_f[couples$ct_f == ""] <- NA
couples$ct_f[couples$ct_f == "."] <- NA

couples$ct_f_ovulation <- as.factor(ifelse(couples$ct_f == "ovulation", 1, 0))
couples$ct_f_dysovulation <- as.factor(ifelse(couples$ct_f == "dysovulation", 1, 0))
couples$ct_f_anovulation <- as.factor(ifelse(couples$ct_f == "anovulation"  , 1, 0))
couples$ct_f_NA <- as.factor(ifelse(is.na(couples$ct_f)  , 1, 0))


#convert var age_f to numeric
couples$age_f = as.numeric(couples$age_f)


summary(couples)

par(mfrow=c(3,3))
hist(couples$age_h)
hist(couples$age_f)


couples$enfant = as.factor(couples$enfant)
couples$enfant = as.factor(couples$enfant)
couples$enfant = as.factor(couples$enfant)
summary(couples)