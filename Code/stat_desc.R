
couples <- read.table(file = "C:/Users/valentin/Documents/BE_stat/couples.csv", sep = ",", header= TRUE)

barplot(table(couples$enfant, couples$spermo),
main = "Survival of Each Class",
xlab = "Class",
col = c("red","green"))
legend("topright",
c("no child"," child"),
fill = c("red","green")
)


barplot(table(couples$enfant, couples$cryptorchidie),
main = "enfant en fonction de la presence de crytorchidie",
xlab = "Class",
col = c("red","green"))
legend("topright",
c("no child"," child"),
fill = c("red","green")
)


barplot(table(couples$enfant, couples$ct_f),
main = "enfant en fonction de la presence de crytorchidie",
xlab = "Class",
col = c("red","green"))
legend("topright",
c("no child"," child"),
fill = c("red","green")


barplot(table(couples$enfant, couples$fecondite),
main = "enfant en fonction de la presence de crytorchidie",
xlab = "Class",
col = c("red","green"))
legend("topright",
c("no child"," child"),
fill = c("red","green")

barplot(table(couples$enfant, couples$traitement),
main = "enfant en fonction de la presence de crytorchidie",
xlab = "Class",
col = c("red","green"))
legend("topright",
c("no child"," child"),
fill = c("red","green")
)