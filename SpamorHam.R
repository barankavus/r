library(readr)
library(dplyr)
if(!require("tree")) { install.packages("tree"); require("tree") }
if(!require("ISLR")) { install.packages("ISLR"); require("ISLR") }
if(!require("rgl")) { install.packages("rgl"); require("rgl") }

#shuffled and split ProjectTrainingData.csv using Cygwin into 50-50
#Read in training and validation data and check number of rows
FinalTrain <- read_csv("Train-00.csv")
FinalTrain <- na.omit(FinalTrain)
head(FinalTrain)

#adding column names to Training Data
colnames(FinalTrain) <- c("id", "click", "hour", "C1", "banner_pos", "site_id",	"site_domain", "site_category",	"app_id",	"app_domain",	"app_category", "device_id", "device_ip",	"device_model",	"device_type",	"device_conn_type",	"C14","C15","C16","C17","C18","C19","C20","C21")
head(FinalTrain)
nrow(FinalTrain)

#Getting a sample dataset from training data with 3 million records to start data exploration and model building process
SampleTraining3mil <- sample_n(FinalTrain, size=3000000, replace=F) 
write.table(SampleTraining3mil, file="SampleTraining3mil.csv", sep=",", row.names=F, col.names = T) #writing this data out to build models in Python
SampleTrain <- SampleTraining3mil

remove(FinalTrain) #to reduce strain on memory

#Importing all our validation data
FinalVal <- read_csv("Train-01.csv")
FinalVal <- na.omit(FinalVal)

#adding column names to Validation Data
colnames(FinalVal) <- c("id", "click", "hour", "C1", "banner_pos", "site_id",	"site_domain", "site_category",	"app_id",	"app_domain",	"app_category", "device_id", "device_ip",	"device_model",	"device_type",	"device_conn_type",	"C14","C15","C16","C17","C18","C19","C20","C21")
head(FinalVal)
nrow(FinalVal)

#taking a sample of validation data for 1 million records to make our predictions
SampleVal <- sample_n(FinalVal, size=1000000, replace=F)
write.table(SampleVal, file="SampleVal.csv", sep=",", row.names=F, col.names = T) #writing this data out to build models in Python

remove(FinalVal) #to reduce strain on memory

# Data Exploration --------------------------------------------------------
#click is target variable - let's start by looking at proportion
#about 17% times the online ad was clicked on
table(SampleTrain$click)
prob_click <- mean(SampleTrain$click) #16.9%

#looking at the columns
length(unique(SampleTrain[["id"]])) #not all unique - one value repeated
length(unique(SampleTrain[["hour"]])) #217 different values 
length(unique(SampleTrain[["C1"]])) #8 different values
length(unique(SampleTrain[["banner_pos"]])) #8 different values 
length(unique(SampleTrain[["site_id"]])) #4189 different values
length(unique(SampleTrain[["site_domain"]])) #6168 different values
length(unique(SampleTrain[["site_category"]])) #26 different values
length(unique(SampleTrain[["app_id"]])) #6949 different values
length(unique(SampleTrain[["app_domain"]])) #444 different values
length(unique(SampleTrain[["app_category"]])) #37 different values
length(unique(SampleTrain[["device_id"]])) #1440888 different values
length(unique(SampleTrain[["device_ip"]])) #3943375
length(unique(SampleTrain[["device_model"]])) #7506
length(unique(SampleTrain[["device_type"]])) #6 different values
length(unique(SampleTrain[["device_conn_type"]])) #5
length(unique(SampleTrain[["C14"]])) #2434
length(unique(SampleTrain[["C15"]])) #9
length(unique(SampleTrain[["C16"]])) #10
length(unique(SampleTrain[["C17"]])) #408
length(unique(SampleTrain[["C18"]])) #5
length(unique(SampleTrain[["C19"]])) #67
length(unique(SampleTrain[["C20"]])) #170
length(unique(SampleTrain[["C21"]])) #56

# Hour --------------------------------------------------------------------
# data is for 9 days so we can split it up first based on day of the week
SampleTrain$newhour <- strptime(as.character(SampleTrain$hour), "%y%m%d%H")
SampleTrain$weekday <- weekdays(as.Date(SampleTrain$newhour))
table(SampleTrain$weekday) #tuesday and wednesday - most activity
SampleTrain %>%
  group_by(weekday) %>%
  summarize_at("click", mean) #no really high click thru rate 

#now we will do the same but for the 24 hours in day
SampleTrain$hourday <- format(SampleTrain$newhour, "%H")
table(SampleTrain$hourday) #increases throughout the day from 7 am to 7 pm
SampleTrain %>%
  group_by(hourday) %>%
  summarize_at("click", mean) %>%
  arrange(desc(click)) #nothing too high

#creating a regression tree to find the most informative hours
NNodes <- 10
tc <- tree.control(nrow(SampleTrain),minsize=2,mincut=1,mindev=0.00)
out <- tree(click ~ hour,data=SampleTrain,control=tc)
out1 <- prune.tree(out,best=NNodes)
print(out1)
summary(out1)

# C1 ----------------------------------------------------------------------
#running a regression tree for most informative categories
tc <- tree.control(nrow(SampleTrain),minsize=2,mincut=1,mindev=0.00)
out <- tree(click ~ C1,data=SampleTrain,control=tc)
out1 <- prune.tree(out,best=6)
print(out1)
summary(out1) #1006, 1003, 1011 

table(SampleTrain$C1)
SampleTrain %>%
  group_by(C1) %>%
  summarize_at("click", mean) #CTR - not very high

C1fm <- click ~ 1 + factor(C1)
OutC1 <- glm(C1fm,data=SampleTrain) 
summary(OutC1)

# Banner Position ---------------------------------------------------------
SampleTrain$banner_pos <- as.factor(SampleTrain$banner_pos)

unique(SampleTrain$banner_pos)
table(SampleTrain$banner_pos)
SampleTrain %>%
  group_by(banner_pos) %>%
  summarize_at("click", mean) #7 has high click-thru rate 

tc <- tree.control(nrow(SampleTrain),minsize=2,mincut=1,mindev=0.00)
out <- tree(click ~ banner_pos,data=SampleTrain,control=tc)
out1 <- prune.tree(out,best=6)
print(out1)

bannerfm <- click ~ 1 + factor(banner_pos)

OutBanner <- glm(bannerfm,data=SampleTrain) 
summary(OutBanner) #1, 2, 7 are significant

#site Id and site domain - too many factors, so will skip 

# Site Category -----------------------------------------------------------
length(unique(SampleTrain$site_category))
tmp <- sort(table(SampleTrain[,"site_category"]), decreasing = T)
SampleTrain %>%
  group_by(site_category) %>%
  summarize_at("click", mean) %>%
  arrange(desc(click)) #good click thru rate - dedf689d 

sum(cumsum(tmp)/sum(tmp)>0.95)+1 #only need 18 categories to give most info - do from top 1-7 and club the rest as others

BigFm <- click ~ 1 + site_category
OutBig <- glm(BigFm,data=SampleTrain) 
summary(OutBig) #significant - 28905ebd, 335d28a8, 3e814130, 42a36e14,50e219e0,70fb0e29, 75fa27f6, c0dd3be3, dedf689d, f028772b

#app-id and app domain - too many factors

# App Category ------------------------------------------------------------
length(unique(SampleTrain$app_category))
tmp1 <- sort(table(SampleTrain[,"app_category"]), decreasing = T)
SampleTrain %>%
  group_by(app_category) %>%
  summarize_at("click", mean) %>%
  arrange(desc(click)) #don't really see a very high click rate w/ any

sum(cumsum(tmp1)/sum(tmp1)>0.95)+1 #need 28 to give info - think about this  

BigApp<- click ~ 1 + app_category

OutApp <- glm(BigApp,data=SampleTrain) 
summary(OutApp) #significant - 09481d60, 0f2161f8, 0f9a328c, 2281a340, 4ce2e9fc, 75d80bbe, 879c24eb, 8ded1f7a, a3c42688, cef3e649, d1327cf5, dc97ec06, f95efa07, fc6fa53d


# Device Type -------------------------------------------------------------
unique(SampleTrain$device_type)
tmp2 <- sort(table(SampleTrain[,"device_type"]), decreasing =T)
SampleTrain %>%
  group_by(device_type) %>%
  summarize_at("click", mean) #not very high

sum(cumsum(tmp2)/sum(tmp2)>0.95)+1 #only need 4 categories - but we can keep this for now

BigDT <- click ~ 1 + factor(device_type)
OutDT <- glm(BigDT, data=SampleTrain)
summary(OutDT) #all significant


# Device Connection Type --------------------------------------------------
unique(SampleTrain$device_conn_type)
tmp3 <- sort(table(SampleTrain[,"device_conn_type"]), decreasing =T)
SampleTrain %>%
  group_by(device_conn_type) %>%
  summarize_at("click", mean) #not very high - 0,2 maybe

sum(cumsum(tmp3)/sum(tmp3)>0.95)+1 #3 categories

BigCDT <- click ~ 1 + factor(device_conn_type)
OutCDT <- glm(BigDT, data=SampleTrain)
summary(OutCDT) #all significant 

# C15 ---------------------------------------------------------------------
unique(SampleTrain$C15)
tmp4 <- sort(table(SampleTrain[,"C15"]), decreasing = T)
SampleTrain %>%
  group_by(C15) %>%
  summarize_at("click", mean) #300, 768, 1024 - high CTR

sum(cumsum(tmp4)/sum(tmp4)>0.95)+1 #8 categories - think about this

BigC15 <- click ~ 1 + factor(C15)
OutC15 <- glm(BigC15, data=SampleTrain)
summary(OutC15) #significant - 215, 300, 320, 480, 768, 1024 

# C16 ---------------------------------------------------------------------
unique(SampleTrain$C16)
RG <- sort(table(SampleTrain[,"C16"]), decreasing = T)
SampleTrain %>%
  group_by(C16) %>%
  summarize_at("click", mean) #250, 480, 768, 1024 - high CTR

sum(cumsum(RG)/sum(RG)>0.95)+1 #9 

BigC16 <- click ~ 1 + factor(C16)
OutC16 <- glm(BigC16, data=SampleTrain)
summary(OutC16) #significant - 36, 50, 250, 320, 480, 768, 1024 

# C17 ---------------------------------------------------------------------
length(unique(SampleTrain$C17))
hh <- sort(table(SampleTrain[,"C17"]), decreasing = T)
kk <- SampleTrain %>%
  group_by(C17) %>%
  summarize_at("click", mean) %>%
  arrange(desc(click))
print(kk, n=nrow(kk))#2567, 2518, 1694, 2662, 2286, 2295, 2569, 827 - all above 0.5

sum(cumsum(hh)/sum(hh)>0.95)+1 #need 241 - keep it and do 134 + others

BigC17 <- click ~ 1 + factor(C17)
OutC17 <- glm(BigC17, data=SampleTrain)
summary(OutC17) #do on AWS

# C18 ---------------------------------------------------------------------
unique(SampleTrain$C18)
nn <- sort(table(SampleTrain[, "C18"]), decreasing=T)
SampleTrain %>%
  group_by(C18) %>%
  summarize_at("click", mean) #not high

sum(cumsum(nn)/sum(nn)>0.95)+1 #2 categories 

BigC18 <- click ~ 1 + factor(C18)
OutC18 <- glm(BigC18, data=SampleTrain)
summary(OutC18) #all significant

# C19 ---------------------------------------------------------------------
length(unique(SampleTrain$C19))
mm <- sort(table(SampleTrain[,"C19"]), decreasing=T)
qq <- SampleTrain %>%
  group_by(C19) %>%
  summarize_at("click", mean) %>%
  arrange(desc(click))
print(qq, n = nrow(qq))#1071 - CTR above 0.5 

sum(cumsum(mm)/sum(mm)>0.95)+1 #40 categories - cut off at 12 

BigC19 <- click ~ 1 + factor(C19)
OutC19 <- glm(BigC19, data=SampleTrain)
summary(OutC19) #34,35,38,39,41,43,47,161,163,167,169,171,175,303,419,432,547,551,555,675,679,681,687,811,813,815,935,939,1059,1063,1065,1071,1319,1327,1711 - significant

# C20 ---------------------------------------------------------------------
length(unique(SampleTrain$C20))
ll <- sort(table(SampleTrain[,"C20"]), decreasing=T)
aq <- SampleTrain %>%
  group_by(C20) %>%
  summarize_at("click", mean) %>%
  arrange(desc(click)) #100175 - at 0.5 and the rest are 0.375 or below
print(aq, n=nrow(aq))
sum(cumsum(ll)/sum(ll)>0.95)+1 #125 categories - 47 + others

BigC20 <- click ~ 1 + factor(C20)
OutC20 <- glm(BigC20, data=SampleTrain)
summary(OutC20) #lots of significant

# C21 ---------------------------------------------------------------------
length(unique(SampleTrain$C21))
ml <- sort(table(SampleTrain[,"C21"]), decreasing=T)
mmk <- SampleTrain %>%
  group_by(C21) %>%
  summarize_at("click", mean) %>%
  arrange(desc(click)) #none above 0.5
print(mmk, n=nrow(mmk))
sum(cumsum(ml)/sum(ml)>0.95)+1 #34 categories - 15+others

BigC21 <- click ~ 1 + factor(C21)
OutC21 <- glm(BigC21, data=SampleTrain)
summary(OutC21) #16,32,33,35,52,71,82,90,110,195,204,212 - significant

#picking the following variables - banner_pos, device_type, site_category, C16, C17, C21 

# Model Building ----------------------------------------------------------
#Using Sample train and Sample Val for encoding and running 3 classifier models in Python 
# Will reupload them here to evaluate using Log loss function

# Evaluating Model --------------------------------------------------------
# Random Forest Model 
ValRFOutput <- read.table("ValRFPred.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

LogLoss <- function(PHat,Click) {
  Y <- as.integer(Click)
  eps <- 1e-15
  out <- -mean(Y*log(pmax(PHat,eps))+(1-Y)*log(pmax(1-PHat,eps)))
  return(out)
}

LogLoss(ValRFOutput$PHat, ValRFOutput$click) #0.45

# kNN 
ValKNNOutput <- read.table("ValKNNPred.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

LogLoss(ValKNNOutput$phat, ValKNNOutput$click) #0.50

# Logistic Regression 
ValLROutput <- read.table("ValLRPred.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

LogLoss(ValLROutput$phat, ValLROutput$click) #0.69


# Prediction on Test Data -------------------------------------------------
#Importing data from python after making predictions 
TestPredOutput <- read.table("TestPred.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
TestPredOutput <- read_csv("TestPred.csv")

# Submission --------------------------------------------------------------
# Read in the submission file with correct data types
Data <- read.table("ProjectSubmission-Team1.csv",colClasses=c("character","numeric"),header=T,sep=",")

# Your code here puts the probabilities in Data[[2]]
Data[[2]] <- TestPredOutput[[188]]

# Round to 10 digits accuracy and prevent scientific notation.
# This converts Data[[2]] to strings.
Data[[2]] <- format(round(Data[[2]],10), scientific = FALSE)

# Write out the data in the correct format.
write.table(Data,file="ProjectSubmission-Team1-Output.csv",quote=F,sep=",",
            row.names=F,col.names=c("id","P(click)"))
