

setwd("H:/OneDrive/Studium/3. Semester/Assistenzsysteme/R/student")

Daten <- read.csv("student-mat.csv",header=TRUE,sep=";",fill=TRUE)


Daten[,"school"] <- as.factor(Daten[,"school"])
Daten[,"sex"] <- as.factor(Daten[,"sex"])
Daten[,"address"] <- as.factor(Daten[,"address"])
Daten[,"famsize"] <- as.factor(Daten[,"famsize"])
Daten[,"Pstatus"] <- as.factor(Daten[,"Pstatus"])
Daten[,"Medu"] <- as.factor(Daten[,"Medu"])
Daten[,"Fedu"] <- as.factor(Daten[,"Fedu"])
Daten[,"Mjob"] <- as.factor(Daten[,"Mjob"])
Daten[,"Fjob"] <- as.factor(Daten[,"Fjob"])
Daten[,"reason"] <- as.factor(Daten[,"reason"])
Daten[,"guardian"] <- as.factor(Daten[,"guardian"])
Daten[,"traveltime"] <- as.factor(Daten[,"traveltime"])
Daten[,"studytime"] <- as.factor(Daten[,"studytime"])
Daten[,"failures"] <- as.factor(Daten[,"failures"])
Daten[,"schoolsup"] <- as.factor(Daten[,"schoolsup"])
Daten[,"famsup"] <- as.factor(Daten[,"famsup"])
Daten[,"paid"] <- as.factor(Daten[,"paid"])
Daten[,"activities"] <- as.factor(Daten[,"activities"])
Daten[,"nursery"] <- as.factor(Daten[,"nursery"])
Daten[,"higher"] <- as.factor(Daten[,"higher"])
Daten[,"internet"] <- as.factor(Daten[,"internet"])
Daten[,"romantic"] <- as.factor(Daten[,"romantic"])
Daten[,"famrel"] <- as.factor(Daten[,"famrel"])
Daten[,"freetime"] <- as.factor(Daten[,"freetime"])
Daten[,"goout"] <- as.factor(Daten[,"goout"])
Daten[,"Dalc"] <- as.factor(Daten[,"Dalc"])
Daten[,"Walc"] <- as.factor(Daten[,"Walc"])
Daten[,"health"] <- as.factor(Daten[,"health"])


library(ANN2)

X1 <- model.matrix( G1 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                          failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health, Daten)
X2 <- model.matrix( G2 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                          failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health+G1, Daten)
X3 <- model.matrix( G3 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                          failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health+G1+G2, Daten)


X1 <- X1[,-1]
X2 <- X2[,-1]
X3 <- X3[,-1]
y1 <- Daten[,"G1"]
y2 <- Daten[,"G2"]
y3 <- Daten[,"G3"]


model1 <- neuralnetwork(X1, y1, hidden.layers=c(8,6), regression = TRUE, 
                        loss.type = "absolute", learn.rates = 1e-04,n.epochs = 1000,
                        verbose=FALSE)
model2 <- neuralnetwork(X2, y2, hidden.layers=c(8,6), regression = TRUE, 
                        loss.type = "absolute", learn.rates = 1e-04,n.epochs = 1000,
                        verbose=FALSE)
model3 <- neuralnetwork(X3, y3, hidden.layers=c(8,6), regression = TRUE, 
                        loss.type = "absolute", learn.rates = 1e-04,n.epochs = 1000,
                        verbose=FALSE)



library(shiny)

runApp("App")


