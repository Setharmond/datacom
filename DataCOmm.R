rm(list=ls()) #Clears Workspace (Variables and Loaded Data)
cat("\014") #Clears Consol
#dev.off() works to clear plots, but can't be run in program

library(tidyverse)
library(haven)
library(plyr)

df <- read_dta(file = "C:/Users/glide/OneDrive/Desktop/DATACOM_NEW.dta")

a <- rep(0, 532163)
b <- rep(0, 532163)
c <- rep(0, 532163)
u <- rep(0, 532163)
v <- rep(0, 532163)
w <- rep(0, 532163)
x <- rep(0, 532163)
y <- rep(0, 532163)
z <- rep(0, 532163)

testdf <- df

keepeduc <- df[c("caseid","pernum", "educ")]
keepeducyrs <- df[c("caseid", "pernum", "educyrs")]

keepeduc <- spread(keepeduc, pernum, educ)
newTest <- merge(testdf, keepeduc, by = c("caseid"))
newTest <- select(newTest, -c("13","14","15","16"))
names(newTest)[50:61] <- c("educPer1","educPer2","educPer3","educPer4","educPer5","educPer6","educPer7","educPer8","educPer9","educPer10","educPer11","educPer12")

y[df[,"momloc"]==1] <- newTest[,"educPer1"]
y[df[,"momloc"]==2] <- newTest[,"educPer2"]
y[df[,"momloc"]==3] <- newTest[,"educPer3"]
y[df[,"momloc"]==4] <- newTest[,"educPer4"]
y[df[,"momloc"]==5] <- newTest[,"educPer5"]
y[df[,"momloc"]==6] <- newTest[,"educPer6"]
y[df[,"momloc"]==7] <- newTest[,"educPer7"]
y[df[,"momloc"]==8] <- newTest[,"educPer8"]
y[df[,"momloc"]==9] <- newTest[,"educPer9"]
y[df[,"momloc"]==10] <- newTest[,"educPer10"]
y[df[,"momloc"]==11] <- newTest[,"educPer11"]
y[df[,"momloc"]==12] <- newTest[,"educPer12"]

newTest[,"momEducLevel"] <- y
newTest$momEducLevel[is.na(newTest$momEducLevel)] <- 0


z[df[,"poploc"]==1] <- newTest[,"educPer1"]
z[df[,"poploc"]==2] <- newTest[,"educPer2"]
z[df[,"poploc"]==3] <- newTest[,"educPer3"]
z[df[,"poploc"]==4] <- newTest[,"educPer4"]
z[df[,"poploc"]==5] <- newTest[,"educPer5"]
z[df[,"poploc"]==6] <- newTest[,"educPer6"]
z[df[,"poploc"]==7] <- newTest[,"educPer7"]
z[df[,"poploc"]==8] <- newTest[,"educPer8"]
z[df[,"poploc"]==9] <- newTest[,"educPer9"]
z[df[,"poploc"]==10] <- newTest[,"educPer10"]
z[df[,"poploc"]==11] <- newTest[,"educPer11"]

newTest[, "popEducLevel"] <- z
newTest$popEducLevel[is.na(newTest$popEducLevel)] <- 0

keephours <- df[c("caseid","pernum", "uhrsworkt_cps8")]
keephours <- spread(keephours, pernum, uhrsworkt_cps8)
newTest <- merge(newTest, keephours, by = c("caseid"))

newTest <- select(newTest, -c("13","14","15","16"))
names(newTest)[64:75] <- c("workPer1","workPer2","workPer3","workPer4","workPer5","workPer6","workPer7","workPer8","workPer9","workPer10","workPer11","workPer12")

w[df[,"momloc"]==1] <- newTest[,"workPer1"]
w[df[,"momloc"]==2] <- newTest[,"workPer2"]
w[df[,"momloc"]==3] <- newTest[,"workPer3"]
w[df[,"momloc"]==4] <- newTest[,"workPer4"]
w[df[,"momloc"]==5] <- newTest[,"workPer5"]
w[df[,"momloc"]==6] <- newTest[,"workPer6"]
w[df[,"momloc"]==7] <- newTest[,"workPer7"]
w[df[,"momloc"]==8] <- newTest[,"workPer8"]
w[df[,"momloc"]==9] <- newTest[,"workPer9"]
w[df[,"momloc"]==10] <- newTest[,"workPer10"]
w[df[,"momloc"]==11] <- newTest[,"workPer11"]
w[df[,"momloc"]==12] <- newTest[,"workPer12"]

newTest[,"momWorkLevel"] <- w


x[df[,"poploc"]==1] <- newTest[,"workPer1"]
x[df[,"poploc"]==2] <- newTest[,"workPer2"]
x[df[,"poploc"]==3] <- newTest[,"workPer3"]
x[df[,"poploc"]==4] <- newTest[,"workPer4"]
x[df[,"poploc"]==5] <- newTest[,"workPer5"]
x[df[,"poploc"]==6] <- newTest[,"workPer6"]
x[df[,"poploc"]==7] <- newTest[,"workPer7"]
x[df[,"poploc"]==8] <- newTest[,"workPer8"]
x[df[,"poploc"]==9] <- newTest[,"workPer9"]
x[df[,"poploc"]==10] <- newTest[,"workPer10"]
x[df[,"poploc"]==11] <- newTest[,"workPer11"]

newTest[, "popWorkLevel"] <- x

keepage <- df[c("caseid","pernum", "age_cps8")]
keepage <- spread(keepage, pernum, age_cps8)
newTest <- merge(newTest, keepage, by = c("caseid"))

newTest <- select(newTest, -c("13","14","15","16"))
names(newTest)[78:89] <- c("agePer1","agePer2","agePer3","agePer4","agePer5","agePer6","agePer7","agePer8","agePer9","agePer10","agePer11","agePer12")

u[df[,"momloc"]==1] <- newTest[,"agePer1"]
u[df[,"momloc"]==2] <- newTest[,"agePer2"]
u[df[,"momloc"]==3] <- newTest[,"agePer3"]
u[df[,"momloc"]==4] <- newTest[,"agePer4"]
u[df[,"momloc"]==5] <- newTest[,"agePer5"]
u[df[,"momloc"]==6] <- newTest[,"agePer6"]
u[df[,"momloc"]==7] <- newTest[,"agePer7"]
u[df[,"momloc"]==8] <- newTest[,"agePer8"]
u[df[,"momloc"]==9] <- newTest[,"agePer9"]
u[df[,"momloc"]==10] <- newTest[,"agePer10"]
u[df[,"momloc"]==11] <- newTest[,"agePer11"]
u[df[,"momloc"]==12] <- newTest[,"agePer12"]

newTest[,"momAge"] <- u

v[df[,"poploc"]==1] <- newTest[,"agePer1"]
v[df[,"poploc"]==2] <- newTest[,"agePer2"]
v[df[,"poploc"]==3] <- newTest[,"agePer3"]
v[df[,"poploc"]==4] <- newTest[,"agePer4"]
v[df[,"poploc"]==5] <- newTest[,"agePer5"]
v[df[,"poploc"]==6] <- newTest[,"agePer6"]
v[df[,"poploc"]==7] <- newTest[,"agePer7"]
v[df[,"poploc"]==8] <- newTest[,"agePer8"]
v[df[,"poploc"]==9] <- newTest[,"agePer9"]
v[df[,"poploc"]==10] <- newTest[,"agePer10"]
v[df[,"poploc"]==11] <- newTest[,"agePer11"]

newTest[, "popAge"] <- v

newTest <- select(newTest, -c(50:61,64:75,78:89))

# This is Fucked Up
a <- newTest$momEducLevel
newTest[, "higherParentEduc"] <- a
i = 1
while ( i <= 532163 ){
  if(newTest[i,"higherParentEduc"] < newTest[i,"popEducLevel"]){
     newTest[i,"higherParentEduc"] <- newTest[i,"popEducLevel"]
   }
 i = i +1
}
i = 1

newTest[,"momExist"] <- b
newTest[,"popExist"] <- c

while (i <= 532163){
  if(newTest[i, "momloc"] > 0){newTest[i, "momExist"] = 1}
  i = i + 1
}
i = 1

while(i <= 532163){
  if(newTest[i, "poploc"] > 0){newTest[i, "popExist"] = 1}
  i = i + 1
}

##New
parentNumber <- rep(0, 532163)
parentNumber <- newTest[,"popExist"] + newTest[,"momExist"]
newTest[,"parentNumber"] <- parentNumber


newTest$momWorkLevel[is.na(newTest$momWorkLevel)] <- 9999
newTest$popWorkLevel[is.na(newTest$popWorkLevel)] <- 9999

newTest[, "minHRSWKT"] <- rep(9999, 532163)


i = 1
while(i <= 532163){
  if(newTest[i, "momExist"] == 1){newTest[i, "minHRSWKT"] = newTest[i, "momWorkLevel"]}
  i = i + 1
}

i = 1
newTest[, "hrsSave"] <- rep(9999, 532163)
while(i <= 532163){
  if(newTest[i, "popExist"] == 1){newTest[i, "hrsSave"] = newTest[i, "popWorkLevel"]}
  i = i + 1
}
i = 1
while(i <= 532163){
  if(newTest[i, "minHRSWKT"] > newTest[i, "hrsSave"]){newTest[i, "minHRSWKT"] = newTest[i, "hrsSave"]}
  i = i + 1
}





#write.csv(newTest, file = "DataComm.csv")