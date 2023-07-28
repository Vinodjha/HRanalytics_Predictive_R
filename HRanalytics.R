### --HR Analytics Study-----###
#Vinod Jha
## PROBLEM STATEMENT ###

### A large Company XYZ wants to carry out this case study to find out the reasons for attrition in the company.
### Every year around 15% of it's employees leaves the organization which impacts project deliverables and timelines thus resulting in 
### reputation loss among customers and partners

##--------- Business Objective --------------###
 ## Find out the factors responsible for attrition
 ## Most important influencing factor which needs to be addressed with topmost priority

## ---------Goal of the case study----------###

## Model the probability of attrition using logistic regression

# Install below packages if required 
## Installing packages for StepAIC & VIF ##

#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("corrplot")
#install.packages("MASS")
#install.packages("car")
#install.packages("caTools")
#install.packages("scales")
#install.packages("ggpubr")
#install.packages("e1071")
#install.packages("caret")
#install.packages("ROCR")


## Load the different libraries required for doing the data cleaning and important analysis

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(corrplot)
library(MASS)
library(car)
library(caTools)
library(scales)
library(ggpubr)
library(e1071)
library(caret)
library(ROCR)


## Load the datasets in R ##

## If required, Set the working directory 
#setwd("G:/Upgrad/HR Analytics Case Study")

empl_survey_data<-read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
mgr_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
general_data<-read.csv("general_data.csv", stringsAsFactors =FALSE)
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)

## View the data
View (empl_survey_data)
View(mgr_survey_data )
View(in_time )
View(out_time)
View(general_data)

#Backup Check Point
## Creating a backup of the data sets
empl_survey_data_bkup <-empl_survey_data
mgr_survey_data_bkup<-mgr_survey_data
in_time_bkup<-in_time
out_time_bkup<-out_time

## check for Duplicate values in the data set

sum(duplicated(empl_survey_data))  ## Returns 0
sum(duplicated(mgr_survey_data))   ## Returns 0
sum(duplicated(general_data))     ## Returns 0
sum(duplicated(in_time))         ## Returns 0
sum(duplicated(out_time))      ## Returns 0

# now lets find out the primary key from the above entities so that the data can be merged 

str(empl_survey_data)
sum(duplicated(empl_survey_data$EmployeeID)) ## Returns 0

str(mgr_survey_data)
sum(duplicated(mgr_survey_data$EmployeeID))   ## Returns 0
str(general_data)
sum(duplicated(general_data$EmployeeID))     ## Returns 0
str(in_time)
sum(duplicated(in_time$X))         ## Returns 0
str(out_time)
sum(duplicated(out_time$X))      ## Returns 0

#data cleansing 
#checking NA values
#sum(is.na(general_data))
sum(is.na(mgr_survey_data)) ##returns 0
sum(is.na(empl_survey_data))## returns 83, which is only 1.8% of total observations
#we will check for the columns and replace NA's with mode of the values as mode gives the 
#maximum occurrence of feedback points and also results in integral value unlike mean.

#creating a function to find mode of a vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#checking NA values in the columns of empl_survey_data and replacing them with mode values
sum(is.na(empl_survey_data$EmployeeID)) #returns 0
sum(is.na(empl_survey_data$EnvironmentSatisfaction))#returns 25,0.57% of total observations
#replacing them with mode value 
empl_survey_data$EnvironmentSatisfaction[which(is.na(empl_survey_data$EnvironmentSatisfaction))]=getmode(empl_survey_data$EnvironmentSatisfaction)

sum(is.na(empl_survey_data$JobSatisfaction)) ## returns 20
#replacing them with mode value again
empl_survey_data$JobSatisfaction[which(is.na(empl_survey_data$JobSatisfaction))]=getmode(empl_survey_data$JobSatisfaction)

sum(is.na(empl_survey_data$WorkLifeBalance))#returns 38
#replacing these NA's again with the mode values

empl_survey_data$WorkLifeBalance[which(is.na(empl_survey_data$WorkLifeBalance))]=getmode(empl_survey_data$WorkLifeBalance)

sum(is.na(empl_survey_data)) # returns 0, so all NA values has been replaced

sum(is.na(general_data)) # returns 28, which is a fairly small number considering 4410 
#observations of 24 variables. 
#checking which columns are having NA values
x=sapply(general_data[1:24],is.na)
colSums(x) ## only two columns are having NA values and they are NumCompaniesWorked (19 NAs) &  TotalWorkingYears(9 NAs) 
# will remove the rows with NA values after merging

sum(is.na(in_time))#too many NA values
sum(is.na(out_time))# same number of NA values as in_time, which might be there because of the absence of person from office on particular dates
# we will look at them after merging the data frames

#checking of blank values
sapply(general_data, function(x) length(which(x == ""))) # checking for blank "" values; there are none
sapply(empl_survey_data, function(x) length(which(x == ""))) ##there are none
sapply(mgr_survey_data, function(x) length(which(x == ""))) ##there are none
sapply(in_time, function(x) length(which(x == "")))   ##there are none
sapply(out_time, function(x) length(which(x == "")))  ##there are none


## Merging the files empl_survey_data & mgr_survey_data with general_data
#####Checking for any difference in the emp id's of the files before merging as EmployeeId is the primary key in all the datasets
setdiff(empl_survey_data$EmployeeID,general_data$EmployeeID)
setdiff(empl_survey_data$EmployeeID,mgr_survey_data$EmployeeID)

## No difference hence merging the two data sets

Merged_empl_data<- merge(empl_survey_data,mgr_survey_data, by="EmployeeID", all = F)
Merged_empl_data <-merge(Merged_empl_data,general_data, by="EmployeeID", all = F)


## it seems the name of the column is missing hence changing the name of the column 
## Changing the name of the first column to EmployeeID in in_time and out_time datasets
colnames(in_time) [1]  <- "EmployeeID"
colnames(out_time) [1] <- "EmployeeID"

# since we have in and out time of the employees , hence doing analysis of the data as per below 

#in time 
# checking the number of leaves given by employer in time 
sum(colSums(is.na(in_time),dims=1) == 4410) # 12 leaves 
#identifying the column index where the company leave is given 
which(colSums(is.na(in_time),dims=1) == 4410)

#out time 
# checking the number of leaves given by employer  in out time 
sum(colSums(is.na(out_time),dims=1) == 4410) # 12 leaves 
#identifying the column index where the company leave is given 
which(colSums(is.na(out_time),dims=1) == 4410)

# per above analysis we got to know that company has given 12 leaves in complete year each date having complete NA is matching with in and out time 
# removing the leave days from the analysis 


in_time <- in_time [,-which(colSums(is.na(in_time),dims=1) == 4410)]
out_time <- out_time [,-which(colSums(is.na(out_time),dims=1) == 4410)]


# changing in_time and out_time from wide format to long format for deriving the variables 


in_time <- gather(in_time,date,time,colnames(in_time)[-1])
out_time<-gather(out_time,date,time,colnames(out_time)[-1])

merge_time<-merge(in_time,out_time,by=c("EmployeeID","date"),all=F)
merge_time_bckup<-merge_time
x1=is.na(merge_time$time.x)
x2=is.na(merge_time$time.y)

setdiff(x1,x2) # results : logical 0:  all the NA values are in the same row in in_time and out_time records, stating that the data input time and 
#output time are NA on the same date.Which signals that the employee is absent on respective dates.

distinct(merge_time[x1,c(3,4)])
# above distinct values shows particular employee is on leave for the day 

#Cleaning time data 
merge_time$date<-str_replace_all(merge_time$date,"X","")
merge_time$date<-as.POSIXct(merge_time$date,format="%Y.%m.%d")
merge_time$time.x<-as.POSIXct(merge_time$time.x)
merge_time$time.y<-as.POSIXct(merge_time$time.y)

#deriving the office stay time to analysis 
merge_time$office_stay<-(merge_time$time.y)-(merge_time$time.x)# office_stay indicates the time the employee is staying in office

# checking weekend attandance, may be potential reason 
sum((wday(merge_time$date)==7 | wday(merge_time$date)==1))## returns zero, so there is no weekand data, nobody working on weekend, hence weekend working
#is not the reason for churning


# finding the average working time of employee
z2<-group_by(merge_time,EmployeeID )
z3<-summarize(z2,avg_working_time=mean(office_stay,na.rm=T))

# merging this average working time data with above merged employee data

Merged_empl_data <-merge(Merged_empl_data,z3, by="EmployeeID", all = F)

#creating a varibale which determines whether an employee is working overtime

Merged_empl_data$working_overtime<-ifelse(Merged_empl_data$avg_working_time>8,1,0)

table(Merged_empl_data$working_overtime)

#checking the frequency of leave taken by employee.
# number of times employee took holiday in a year= number of NA's for in and out time of that employee for a date .


z4<-summarize(z2, leave_count=sum(is.na(office_stay)))

head(z4)

# verify if the count is good , taking example for employee id 1 
merge_time[which(merge_time$EmployeeID == 1 & is.na(merge_time$time.x)),]
# the leave count for employee is correct 

#merging leave_count with employee data
Merged_empl_data <-merge(Merged_empl_data,z4, by="EmployeeID", all = F)


#finding how many times an employee did overtime
z5<-summarize(z2, overtime_count=sum(office_stay>8,na.rm=T))
head(z5)

#merging office_stay with employee data
Merged_empl_data <-merge(Merged_empl_data,z5, by="EmployeeID", all = F)

str(Merged_empl_data)
Merged_empl_data_bckup<-Merged_empl_data

sum(is.na(Merged_empl_data)) # count is 28, as found out earlier in general_data df
#removing the remaining rows having NA's
Merged_empl_data<-Merged_empl_data[-which(is.na(Merged_empl_data$NumCompaniesWorked)),]
Merged_empl_data<-Merged_empl_data[-which(is.na(Merged_empl_data$TotalWorkingYears)),]

sum(is.na(Merged_empl_data)) #returns 0

#backup check point 
Merged_empl_data_bckup1<-Merged_empl_data


#so the derived metrices created as of now are avg_working_time, working_overtime(Yes or NO) and overtime_count and leave_counts

############Univariate, Bivariate and outlier treatment #######



Merged_empl_data$Attrition<-as.factor(Merged_empl_data$Attrition)
summary(as.factor(Merged_empl_data$Attrition))
#Merged_empl_data$Attrition<-ifelse(Merged_empl_data$Attrition=="Yes",1,0)

ggplot(Merged_empl_data, aes(x=Department,fill=Attrition))+geom_bar(position = "fill") 
## HR department does have 
#more attrition rate, but it is not high enought to conclude anything from here


#ggplot(Merged_empl_data, aes(x=Age,fill=Attrition))+geom_bar(position = "fill") #attrition is hugh for lower ages
summary(Merged_empl_data$Age)  #NO outlier in Variable Age, as visible from its summary
ggplot(Merged_empl_data, aes(x=Age,fill=Attrition))+geom_histogram(binwidth = 15,position = "fill") 
#attrition is alarming for lower ages
 
ggplot(Merged_empl_data, aes(x=BusinessTravel,fill=Attrition))+geom_bar(position = "fill")
#who travel frequently have high attrition rate

summary(Merged_empl_data$DistanceFromHome)# No outlier in DistanceFromHOme as well
ggplot(Merged_empl_data, aes(x=DistanceFromHome,fill=Attrition))+geom_bar(position = "dodge") 
#DistanceFromHome doesn't seem to be a factor effecting attrition


summary(as.factor(Merged_empl_data$Education)) # values are clean
ggplot(Merged_empl_data, aes(x=Education,fill=Attrition))+geom_bar(position = "fill") 
#Education doesn't see to be a factor effecting attrition

summary(as.factor(Merged_empl_data$EducationField)) # categories are clean, no need of further cleaning
ggplot(Merged_empl_data, aes(x=EducationField,fill=Attrition))+geom_bar(position = "fill") #Like Department, Education Field Also suggesting that 
#education in the field of Human Resources has high attrition rate

ggplot(Merged_empl_data, aes(x=Gender,fill=Attrition))+geom_bar(position = "fill") 
# No apparant effect of Gender on attrition rate

summary(Merged_empl_data$EmployeeCount) # all values are one, hence it will be removed
Merged_empl_data<-Merged_empl_data[,-which(colnames(Merged_empl_data)=="EmployeeCount")]

summary(as.factor(Merged_empl_data$JobLevel)) #JobLevel variable is clean as well
ggplot(Merged_empl_data, aes(x=JobLevel,fill=Attrition))+geom_bar(position = "fill") 
#Joblevel doesnt seem to effect attrition rate much

summary(as.factor(Merged_empl_data$JobRole)) # no need of cleaning, categories are clear
ggplot(Merged_empl_data, aes(x=JobRole,fill=Attrition))+geom_bar(position = "fill") 
#Joblevel doesnt seem to effect attrition rate much

ggplot(Merged_empl_data, aes(x=MaritalStatus,fill=Attrition))+geom_bar(position = "fill") 
#Employees who are single have high attrition rate, but it has correlation with lower age.

summary(Merged_empl_data$MonthlyIncome) # the minimum and maximum values and business understanding
#says that there is no outlier in the data.
boxplot(Merged_empl_data$MonthlyIncome)
ggplot(Merged_empl_data, aes(x=MonthlyIncome,fill=Attrition))+geom_histogram(binwidth = 20000,position = "dodge") 
#MOnthlyincome also doesnt seem to have huge effect on attrition rate

summary(Merged_empl_data$NumCompaniesWorked)
ggplot(Merged_empl_data, aes(x=NumCompaniesWorked,fill=Attrition))+geom_bar(position = "fill") 
#NumCompaniesworked doesnt seem to effect attrition rate much

summary(as.factor(Merged_empl_data$Over18))
#it will be reoved as it has only one value.
Merged_empl_data<-Merged_empl_data[,-which(colnames(Merged_empl_data)=="Over18")]

summary(Merged_empl_data$PercentSalaryHike) # no outlier here
#percentage salaryhike is less for most of the employees and its clear from the business understanding
#that most of the employees are in the lower level and salary hikes increases with the joblevel 
Merged_empl_data$JobLevel<-as.factor(Merged_empl_data$JobLevel)
summary(Merged_empl_data$JobLevel) # JobLevel is clean and no outlier is present
ggplot(Merged_empl_data, aes(x=PercentSalaryHike,fill=JobLevel))+geom_bar(position = "dodge") 
#above understanding is clear from the graph of percentagesalaryhike for different joblevels

summary(Merged_empl_data$PercentSalaryHike) # No outlier present
ggplot(Merged_empl_data, aes(x=PercentSalaryHike,fill=Attrition))+geom_bar(position = "dodge") 
#PercentSalaryHike doesnt seem to effect attrition rate much

summary(Merged_empl_data$StandardHours)
#this column will be removed as it contains same value in all rows
Merged_empl_data<-Merged_empl_data[,-which(colnames(Merged_empl_data)=="StandardHours")]

summary(as.factor(Merged_empl_data$StockOptionLevel)) # no outlier in this column and it is clean as well
ggplot(Merged_empl_data, aes(x=StockOptionLevel,fill=Attrition))+geom_bar(position = "fill") 
#No apparant effect of stockoptionlevel on attrition rate

summary(Merged_empl_data$TotalWorkingYears) # no outlier here as well
ggplot(Merged_empl_data, aes(x=TotalWorkingYears,fill=Attrition))+geom_histogram(binwidth = 3,position = "fill") 
#Employees with 0-3 years of experience have considerably high attrition rate

summary(as.factor(Merged_empl_data$TrainingTimesLastYear)) # this column is clean as well
ggplot(Merged_empl_data, aes(x=TrainingTimesLastYear,fill=Attrition))+geom_bar(position = "dodge") 
#number of trainings in one year does not seem to effect attrition

summary(Merged_empl_data$YearsAtCompany) # no outlier in YearsAtCompany
ggplot(Merged_empl_data, aes(x=YearsAtCompany,fill=Attrition))+geom_bar(position = "dodge") 
# Employee who are in the company for one year have the highest possibility of leaving the company

summary(Merged_empl_data$YearsSinceLastPromotion)# no outlier in YearsSinceLAstPromotion
ggplot(Merged_empl_data, aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_bar(position = "dodge") 
#trend suggests no effect of year since last promotion on attrition rate

summary(Merged_empl_data$YearsWithCurrManager) # no outlier in YearsSinceCurrManager
ggplot(Merged_empl_data, aes(x=YearsWithCurrManager,fill=Attrition))+geom_bar(position = "stack") 
#It does not give any new trend, new employees with 1 year of experince in this company will mostly have same managerthroughout the year
#and 1-year experienced employee have the highest attrition rate
#avg_working_time into numeric before putting it into analysis
#converting 
Merged_empl_data$avg_working_time<-as.numeric(Merged_empl_data$avg_working_time)
plot1<-ggplot(Merged_empl_data, aes(x=avg_working_time,fill=Attrition))+geom_histogram(binwidth = 1,position = "fill") 
plot2<-ggplot(Merged_empl_data, aes(x=avg_working_time,fill=Attrition))+geom_histogram(binwidth = 1,position = "stack")
plot3<-ggplot(Merged_empl_data,aes(x=working_overtime,fill=Attrition))+geom_bar(position="fill")
ggarrange(plot1,plot2,plot3, nrow=3,ncol=1)
#attrition rate for employees working less than assigned working hour is quite small than 
#those who are working overtime, though such overtime working employees are less.

summary(Merged_empl_data$leave_count)
#finding mode value of the leaves
mode_leave<-getmode(Merged_empl_data$leave_count)
mode_leave #7 number of leaves on an average have been taken by maximum employees in a  year

#updating the leave count
#Merged_empl_data$leave_count<-Merged_empl_data$leave_count-12
ggplot(Merged_empl_data, aes(x=leave_count,fill=Attrition))+geom_bar(position = "dodge") 
# leave_count does not seem to much effect on attrition rate
#boxplot(Merged_empl_data$overtime_count)


#creating dummy variables
Merged_empl_data$EnvironmentSatisfaction<-as.factor(Merged_empl_data$EnvironmentSatisfaction)
levels(Merged_empl_data$EnvironmentSatisfaction)
dummy_1 <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_1 <- dummy_1[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="EnvironmentSatisfaction")],dummy_1) 

Merged_empl_data$JobSatisfaction<-as.factor(Merged_empl_data$JobSatisfaction)
levels(Merged_empl_data$JobSatisfaction)
dummy_2 <- data.frame(model.matrix( ~JobSatisfaction, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_2 <- dummy_2[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="JobSatisfaction")],dummy_2) 

Merged_empl_data$WorkLifeBalance<-as.factor(Merged_empl_data$WorkLifeBalance)
levels(Merged_empl_data$WorkLifeBalance)
dummy_3 <- data.frame(model.matrix( ~WorkLifeBalance, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_3 <- dummy_3[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="WorkLifeBalance")],dummy_3) 


Merged_empl_data$JobInvolvement<-as.factor(Merged_empl_data$JobInvolvement)
levels(Merged_empl_data$JobInvolvement)
dummy_4 <- data.frame(model.matrix( ~JobInvolvement, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_4 <- dummy_4[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="JobInvolvement")],dummy_4) 

Merged_empl_data$PerformanceRating<-as.factor(Merged_empl_data$PerformanceRating)
levels(Merged_empl_data$PerformanceRating)
levels(Merged_empl_data$PerformanceRating)=c(1,0)#3=1,4=0
Merged_empl_data$PerformanceRating<-as.numeric(levels(Merged_empl_data$PerformanceRating)[Merged_empl_data$PerformanceRating])

Merged_empl_data$BusinessTravel<-as.factor(Merged_empl_data$BusinessTravel)
levels(Merged_empl_data$BusinessTravel)
dummy_5 <- data.frame(model.matrix( ~BusinessTravel, data = Merged_empl_data)) #convertible is assumed to be base of comparison
dummy_5 <- dummy_5[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="BusinessTravel")],dummy_5) 

Merged_empl_data$Department<-as.factor(Merged_empl_data$Department)
levels(Merged_empl_data$Department)
dummy_6 <- data.frame(model.matrix( ~Department, data = Merged_empl_data)) #convertible is assumed to be base of comparison
dummy_6 <- dummy_6[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="Department")],dummy_6) 

#Merged_empl_data$DistanceFromHome<-as.factor(Merged_empl_data$DistanceFromHome)
#levels(Merged_empl_data$DistanceFromHome)
#dummy_7 <- data.frame(model.matrix( ~DistanceFromHome, data = Merged_empl_data)) #convertible is assumed to be base of comparison
#dummy_7 <- dummy_7[,-1]
#Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="DistanceFromHome")],dummy_7) 

#DistanaceFromHome variable could have been binned and made categorical variable for analysis, but
#as evident from its plot vs attrition, it doesn't have significant effect.

Merged_empl_data$Education<-as.factor(Merged_empl_data$Education)
levels(Merged_empl_data$Education)
dummy_8 <- data.frame(model.matrix( ~Education, data = Merged_empl_data)) #convertible is assumed to be base of comparison
dummy_8 <- dummy_8[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="Education")],dummy_8) 

#backup checkpoint 
Merged_empl_data_bckup3<-Merged_empl_data
#Merged_empl_data <- Merged_empl_data_bckup3

Merged_empl_data$EducationField<-as.factor(Merged_empl_data$EducationField)
levels(Merged_empl_data$EducationField)
dummy_9 <- data.frame(model.matrix( ~EducationField, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_9 <- dummy_9[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="EducationField")],dummy_9) 


Merged_empl_data$Gender<-as.factor(Merged_empl_data$Gender)
levels(Merged_empl_data$Gender)
levels(Merged_empl_data$Gender)<-c(1,0) #male=0,female=1
Merged_empl_data$Gender<-as.numeric(levels(Merged_empl_data$Gender)[Merged_empl_data$Gender])


Merged_empl_data$JobLevel<-as.factor(Merged_empl_data$JobLevel)
levels(Merged_empl_data$JobLevel)
dummy_10 <- data.frame(model.matrix( ~JobLevel, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_10 <- dummy_10[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="JobLevel")],dummy_10) 


Merged_empl_data$JobRole<-as.factor(Merged_empl_data$JobRole)
levels(Merged_empl_data$JobRole)
dummy_11 <- data.frame(model.matrix( ~JobRole, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_11 <- dummy_11[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="JobRole")],dummy_11) 

Merged_empl_data$MaritalStatus<-as.factor(Merged_empl_data$MaritalStatus)
levels(Merged_empl_data$MaritalStatus)
dummy_11_1 <- data.frame(model.matrix( ~MaritalStatus, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_11_1 <- dummy_11_1[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="MaritalStatus")],dummy_11_1) 

#StockOptionLevel
Merged_empl_data$StockOptionLevel<-as.factor(Merged_empl_data$StockOptionLevel)
levels(Merged_empl_data$StockOptionLevel)
dummy_12 <- data.frame(model.matrix( ~StockOptionLevel, data = Merged_empl_data)) #convertible is assumed to be base of comparision
dummy_12 <- dummy_12[,-1]
Merged_empl_data<-cbind(Merged_empl_data[,-which(colnames(Merged_empl_data)=="StockOptionLevel")],dummy_12) 

Merged_empl_data$working_overtime<-as.factor(Merged_empl_data$working_overtime)

#bacup check point
Merged_empl_data_bckup4<-Merged_empl_data
#scaling the numeric variables

Merged_empl_data$Age<-scale(Merged_empl_data$Age)
summary(Merged_empl_data$Age)
Merged_empl_data$DistanceFromHome<-scale(Merged_empl_data$DistanceFromHome)
Merged_empl_data$MonthlyIncome<-scale(Merged_empl_data$MonthlyIncome)
Merged_empl_data$NumCompaniesWorked<-scale(Merged_empl_data$NumCompaniesWorked)
Merged_empl_data$PercentSalaryHike<-scale(Merged_empl_data$PercentSalaryHike)
Merged_empl_data$TotalWorkingYears<-scale(Merged_empl_data$TotalWorkingYears)
Merged_empl_data$TrainingTimesLastYear<-scale(Merged_empl_data$TrainingTimesLastYear)
Merged_empl_data$YearsAtCompany<-scale(Merged_empl_data$YearsAtCompany)
Merged_empl_data$YearsSinceLastPromotion<-scale(Merged_empl_data$YearsSinceLastPromotion)
Merged_empl_data$YearsWithCurrManager<-scale(Merged_empl_data$YearsWithCurrManager)
Merged_empl_data$avg_working_time<-scale(Merged_empl_data$avg_working_time)
Merged_empl_data$leave_count<-scale(Merged_empl_data$leave_count)
Merged_empl_data$overtime_count<-scale(Merged_empl_data$overtime_count)

# dependent variable Attiration to be converted to numeric 
Merged_empl_data$Attrition<-as.factor(Merged_empl_data$Attrition)
levels(Merged_empl_data$Attrition)
levels(Merged_empl_data$Attrition)<-c(0,1) #yes=1,No=0
Merged_empl_data$Attrition<-as.numeric(levels(Merged_empl_data$Attrition)[Merged_empl_data$Attrition])

##############################################################
#####################     Modelling    ########################
##############################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(Merged_empl_data$Attrition, SplitRatio = 0.7)

train = Merged_empl_data[indices,]

test = Merged_empl_data[!(indices),]

########################################################################
# since we are suppose to find the probability hence using "Logistic Regression" 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
# now to avoid the insignificant variables using stepwise selection method 
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)[order(vif(model_2))]
# using order function so that we can easily see the variables with high collinearity

# as per the analysis the EducationFieldLife.Sciences variable is the first , the VIF is high 

model_3 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + avg_working_time + 
                 overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusMarried + 
                 MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)
  
# checking summary 
summary(model_3)

vif(model_3)[order(vif(model_3))]


# as per above analysis the avg_working_time should be removed from the model because of insignificant level 


model_4 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusMarried + 
                 MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)

# checking summary 
summary(model_4)

vif(model_4)[order(vif(model_4))]

# as per above analysis the YearsAtCompany should be removed from the model
# YearsAtCompany has high VIF hence removing 

model_5 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + Education5 + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusMarried + 
                 MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)

# checking summary 
summary(model_5)

vif(model_5)[order(vif(model_5))]


# as per above analysis the BusinessTravelTravel_Rarely should be removed from the model
# BusinessTravelTravel_Rarely has high VIF hence removing 

model_6 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance3 + 
                 WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusMarried + 
                 MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)

# checking summary 
summary(model_6)

vif(model_6)[order(vif(model_6))]

# checking the correlation between WorkLifeBalance2 and  WorkLifeBalance3 because the VIF is close enough and significane level is also very high 
cor(train$WorkLifeBalance2,train$WorkLifeBalance3)
# -0.6964517 (-ve correlation is expected ) so lets remove WorkLifeBalance3 and check the model 
# also expecting good work-life balance is low attraction to attrition hence removing 

model_7 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusMarried + 
                 MaritalStatusSingle + StockOptionLevel1, family = "binomial", 
               data = train)

# checking summary 
summary(model_7)

vif(model_7)[order(vif(model_7))]

# MaritalStatusMarried can be removed since high VIF and low significant level 


model_8 <- glm(formula = Attrition ~ Age + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1, family = "binomial", 
               data = train)

# checking summary 
summary(model_8)

vif(model_8)[order(vif(model_8))]

# identifying the correlation between Age and TotalWorkingYears to check if they are affecting each other 
cor(train$Age,train$TotalWorkingYears)
# high correlation 
# Age may not be affecting much because some buddy might start job at higher age 
# now removing Age    


model_9 <- glm(formula = Attrition ~ TotalWorkingYears + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance2 + WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1, family = "binomial", 
               data = train)

# checking summary 
summary(model_9)

vif(model_9)[order(vif(model_9))]

# all the VIF values are lower than 2 , which is acceptable value for a model 
# now we will consider significant level (P-value) for variable removal 

# now removing the variable WorkLifeBalance2 because of the lower significant level 


model_10 <- glm(formula = Attrition ~ TotalWorkingYears + Gender + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                 EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                 JobSatisfaction4 + WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                 StockOptionLevel1, family = "binomial", 
               data = train)

# checking summary 
summary(model_10)

vif(model_10)[order(vif(model_10))]

# now removing EducationFieldMedical because of lower significant level 

model_11 <- glm(formula = Attrition ~ TotalWorkingYears + Gender + MonthlyIncome + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + WorkLifeBalance4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldMarketing + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + MaritalStatusSingle + 
                  StockOptionLevel1, family = "binomial", 
                data = train)

# checking summary by  P value 
summary(model_11)

#coef(summary(model_11))[order(coef(summary(model_11))[,4]),]
vif(model_11)[order(vif(model_11))]

# now removing  WorkLifeBalance4 variable based on lower significant level 


model_12 <- glm(formula = Attrition ~ TotalWorkingYears + Gender + MonthlyIncome + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobRoleSales.Executive + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldMarketing + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + MaritalStatusSingle + 
                  StockOptionLevel1, family = "binomial", 
                data = train)

# checking summary by  P value 
summary(model_12)

vif(model_12)[order(vif(model_12))]
# checking vif to observe major change in vif if any 


# now removing MonthlyIncome variable based on lower significant level 

model_13 <- glm(formula = Attrition ~ TotalWorkingYears + Gender + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobRoleSales.Executive + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldMarketing + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + MaritalStatusSingle + 
                  StockOptionLevel1, family = "binomial", 
                data = train)

# checking summary by  P value 
summary(model_13)

vif(model_13)[order(vif(model_13))]



# now removing JobRoleSales.Executive variable based on lower significant level 

model_14 <- glm(formula = Attrition ~ TotalWorkingYears + Gender + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldMarketing + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + MaritalStatusSingle + 
                  StockOptionLevel1, family = "binomial", 
                data = train)

# checking summary by  P value 
summary(model_14)

vif(model_14)[order(vif(model_14))]

# now removing Gender variable based on lower significant level 

model_15 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldMarketing + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + MaritalStatusSingle + 
                  StockOptionLevel1, family = "binomial", 
                data = train)

# checking summary by P value 
summary(model_15)

vif(model_15)[order(vif(model_15))]


# now removing EducationFieldOther variable based on lower significant level 

model_16 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldMarketing + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + MaritalStatusSingle + 
                  StockOptionLevel1, family = "binomial", 
                data = train)

# checking summary by  P value 
summary(model_16)

vif(model_16)[order(vif(model_16))]


# now removing JobLevel2 variable based on lower significant level 

model_17 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldMarketing + 
                  EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + MaritalStatusSingle + 
                  StockOptionLevel1, family = "binomial", 
                data = train)

# checking summary by  P value 
summary(model_17)

vif(model_17)[order(vif(model_17))]

# now removing EducationFieldMarketing variable based on lower significant level 

model_18 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + MaritalStatusSingle + 
                  StockOptionLevel1, family = "binomial", 
                data = train)

# checking summary by  P value 
summary(model_18)

vif(model_18)[order(vif(model_18))]

# now removing JobRoleResearch.Director variable based on lower significant level 

model_19 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobInvolvement2 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  MaritalStatusSingle + StockOptionLevel1, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_19)

vif(model_19)[order(vif(model_19))]


# now removing JobInvolvement2 variable based on lower significant level 

model_20 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + JobInvolvement3 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  MaritalStatusSingle + StockOptionLevel1, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_20)

vif(model_20)[order(vif(model_20))]


# now removing JobInvolvement3 variable based on lower significant level 

model_21 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + BusinessTravelTravel_Frequently + 
                  Education5 + EducationFieldTechnical.Degree + JobRoleManufacturing.Director + 
                  MaritalStatusSingle + StockOptionLevel1, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_21)

vif(model_21)[order(vif(model_21))]

# now removing EducationFieldTechnical.Degree   & Education5 variable based on lower significant level 

model_22 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + MaritalStatusSingle + StockOptionLevel1, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_22)

vif(model_22)[order(vif(model_22))]


# now removing StockOptionLevel1 & TrainingTimesLastYear variable based on lower significant level 

model_23 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + MaritalStatusSingle, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_23)

vif(model_23)[order(vif(model_23))]

# now the model has all the significant variables but simplifying it further we are removing few lower significant variables 
# now removing JobRoleManufacturing.Director variable based on lower significant level 

model_24 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction2 + JobSatisfaction3 + 
                  JobSatisfaction4 + BusinessTravelTravel_Frequently + 
                  MaritalStatusSingle, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_24)

vif(model_24)[order(vif(model_24))]


# now removing JobSatisfaction2 variable based on lower significant level 

model_25 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction4 + JobSatisfaction3 + 
                  BusinessTravelTravel_Frequently + 
                  MaritalStatusSingle, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_25)

vif(model_25)[order(vif(model_25))]

# after removing JobSatisfaction2 we saw that the JobSatisfaction3 become insignificant 
# now removing JobSatisfaction3 variable based on lower significant level 

model_26 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + EnvironmentSatisfaction2 + EnvironmentSatisfaction3 + 
                  EnvironmentSatisfaction4 + JobSatisfaction4 + 
                  BusinessTravelTravel_Frequently + MaritalStatusSingle, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_26)

vif(model_26)[order(vif(model_26))]

#Now lets check the model by removing EnvironmentSatisfaction2 since these are categorical variable and impacts the model

model_27 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + 
                  EnvironmentSatisfaction3 + EnvironmentSatisfaction4 + JobSatisfaction4 + 
                  BusinessTravelTravel_Frequently + MaritalStatusSingle, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_27)

vif(model_27)[order(vif(model_27))]

#Now after removing EnvironmentSatisfaction2 the variable EnvironmentSatisfaction3 become low significant
# now remvoing EnvironmentSatisfaction3 based on the p value

model_28 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + 
                  EnvironmentSatisfaction4 + JobSatisfaction4 + 
                  BusinessTravelTravel_Frequently + MaritalStatusSingle, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_28)

vif(model_28)[order(vif(model_28))]

# Now we can say model 28 is ideal model now 

# as we are aware that we have removed "EducationFieldLife.Sciences " variable because of High VIF 
# but the significant level was very high 
# lets check adding this variable "EducationFieldLife.Sciences "


model_29 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + 
                  EnvironmentSatisfaction4 + JobSatisfaction4 + EducationFieldLife.Sciences +
                  BusinessTravelTravel_Frequently + MaritalStatusSingle, 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_29)

vif(model_29)[order(vif(model_29))]

# the variable EducationFieldLife.Sciences  is insignificant now hence removing it 

# as per business understanding people search for job when they are not happy with the income genrated with the current salary or not happy with the hike given in appraisal 
  
model_30 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                        YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + 
                        EnvironmentSatisfaction4 + JobSatisfaction4 + 
                        BusinessTravelTravel_Frequently + MaritalStatusSingle +
                        PercentSalaryHike + MonthlyIncome , 
                      family = "binomial", data = train)
    
# checking summary by  P value 
summary(model_30)
    
vif(model_30)[order(vif(model_30))]


# both the variables are insignificant for the model hence removing them and 
# adding BusinessTravelTravel_Rarely to the model since this was significat variable which we removed in earlier model 

model_31 <- glm(formula = Attrition ~ TotalWorkingYears + NumCompaniesWorked + 
                  YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + 
                  EnvironmentSatisfaction4 + JobSatisfaction4 + 
                  BusinessTravelTravel_Frequently + MaritalStatusSingle +
                  BusinessTravelTravel_Rarely , 
                family = "binomial", data = train)

# checking summary by  P value 
summary(model_31)
vif(model_31)[order(vif(model_31))]

# adding BusinessTravelTravel_Rarely is not good for our model since it is highly correlated to BusinessTravelTravel_Frequently 
# hence removing BusinessTravelTravel_Rarely 

# in previous models we have removed the Age variable which was highly correlated to TotalWorkingYears
# lets check new model after adding Age variable and removing TotalWorkingYears 


model_32 <-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                              YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + 
                              EnvironmentSatisfaction4 + JobSatisfaction4 + 
                              BusinessTravelTravel_Frequently + MaritalStatusSingle , 
                            family = "binomial", data = train)

# checking summary by  P value 
summary(model_32)
vif(model_32)[order(vif(model_32))]

# model 32 can also be considered as ideal model because the variables are significant and AIC value is high

# so for now below are the two best models identified as ideal model , now starting the comparison among them   
# model_28
# model_32

#######################################################################

### Model Evaluation , evaluating the model now for three different models 

### Test Data ####
#colnames(test)

#predicted probabilities of Attrition  for test data

test_pred_model_28 = predict(model_28, type = "response", 
                    newdata = test[,-4])

test_pred_model_32 = predict(model_32, type = "response", 
                    newdata = test[,-4])


# Let's see the summary 
summary(test_pred_model_28)
summary(test_pred_model_32)

# lets associate the probability with test data model wise 
test$prob_model_28 <- test_pred_model_28
test$prob_model_32 <- test_pred_model_32


# now lets do analysis based on probability cutoff 

# Let's use the probability cutoff of 50%.
test_pred_attr_model_28 <- factor(ifelse(test_pred_model_28 >= 0.50, "Yes", "No"))
test_pred_attr_model_32 <- factor(ifelse(test_pred_model_32 >= 0.50, "Yes", "No"))

test_actual_attr <- factor(ifelse(test$Attrition == 1 ,"Yes", "No"))
# actual attrition of test data 

# tabulating for analysis 
table(test_pred_attr_model_28,test_actual_attr)
table(test_pred_attr_model_32,test_actual_attr)

# confusion matrix ############
test_conf_model_28 <- confusionMatrix(test_pred_attr_model_28, test_actual_attr, positive = "Yes")
test_conf_model_28
test_conf_model_32 <- confusionMatrix(test_pred_attr_model_32, test_actual_attr, positive = "Yes")
test_conf_model_32


# Let's use the probability cutoff of 40%.

test_pred_attr_model_28 <- factor(ifelse(test_pred_model_28 >= 0.40, "Yes", "No"))
test_pred_attr_model_32 <- factor(ifelse(test_pred_model_32 >= 0.40, "Yes", "No"))

# tabulating for analysis 
table(test_pred_attr_model_28,test_actual_attr)
table(test_pred_attr_model_32,test_actual_attr)

# confusion matrix ############
test_conf_model_28 <- confusionMatrix(test_pred_attr_model_28, test_actual_attr, positive = "Yes")
test_conf_model_28
test_conf_model_32 <- confusionMatrix(test_pred_attr_model_32, test_actual_attr, positive = "Yes")
test_conf_model_32

#######################################################################

# Lets identify the ideal cutoff values by attempting lot of values in the above two models 

perform_fn_model_28 <- function(cutoff) 
{
  predicted_attiration <- factor(ifelse(test_pred_model_28 >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attiration,test_actual_attr , positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

perform_fn_model_32 <- function(cutoff) 
{
  predicted_attiration <- factor(ifelse(test_pred_model_32 >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attiration,  test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


s = seq(.01,.80,length=100)
OUT_model_28 = matrix(0,100,3)
OUT_model_32 = matrix(0,100,3)

for(i in 1:100)
{
  OUT_model_28[i,] = perform_fn_model_28(s[i])
} 

for(i in 1:100)
{
  OUT_model_32[i,] = perform_fn_model_32(s[i])
} 


par(mfrow = c(1,2))

#Plotting model 28 
plot(s, OUT_model_28[,1],xlab="model 28 Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_model_28[,2],col="darkgreen",lwd=2)
lines(s,OUT_model_28[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_model_28 <- s[which(abs(OUT_model_28[,1]-OUT_model_28[,2])<0.01)]
cutoff_model_28
#0.1536364


# plotting model 32
plot(s, OUT_model_32[,1],xlab="model 32 Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_model_32[,2],col="darkgreen",lwd=2)
lines(s,OUT_model_32[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_model_32 <- s[which(abs(OUT_model_32[,1]-OUT_model_32[,2])<0.01)]
cutoff_model_32
#0.1616162

# as per above graph and cutoff codes we can identify that 15.36 % is the cutoff for model 28 
# and 16.16 % is the ideal cutoff for model 32 
# on these cutoff levels we will get maximum "Sensitivity","Specificity","Accuracy"

test_pred_attr_model_28 <- factor(ifelse(test_pred_model_28 >= 0.1536, "Yes", "No"))
test_pred_attr_model_32 <- factor(ifelse(test_pred_model_32 >= 0.1616, "Yes", "No"))

# tabulating for analysis 
table(test_pred_attr_model_28,test_actual_attr)
table(test_pred_attr_model_32,test_actual_attr)


# confusion matrix ############
test_conf_model_28 <- confusionMatrix(test_pred_attr_model_28, test_actual_attr, positive = "Yes")
test_conf_model_28
test_conf_model_32 <- confusionMatrix(test_pred_attr_model_32, test_actual_attr, positive = "Yes")
test_conf_model_32

#Though model_32 are model_28 are almost similar in terms of variables used, Age seems to be a better criteria
#than TotalWorkingYears. Employees with less age and less working experience both have higher rate of attrition
#but And these two independent variables are definitely correlated. 

#Since model_32 is giving better values of accuracy, specificity ans sensitivity, so its a better model than model_28.

--------------------------
#plotting lift and gain chart
  
  lift <- function(labels , predicted_prob,groups=10) {
    
    if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
    if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
    helper = data.frame(cbind(labels , predicted_prob))
    helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
      summarise_at(vars(labels ), funs(total = n(),
                                       totalresp=sum(., na.rm = TRUE))) %>%
      
      mutate(Cumresp = cumsum(totalresp),
             Gain=Cumresp/sum(totalresp)*100,
             Cumlift=Gain/(bucket*(100/groups))) 
    return(gaintable)
  }
test_actual_attr<-ifelse(test_actual_attr=="Yes",1,0)

attrition_decile_model_28 = lift(test_actual_attr, test_pred_model_28, groups = 10)   
attrition_decile_model_32 = lift(test_actual_attr, test_pred_model_32, groups = 10)   

attrition_decile_model_28$total_no_attrtion=attrition_decile_model_28$total- attrition_decile_model_28$totalresp
attrition_decile_model_28$cum_no_attrition<-cumsum(attrition_decile_model_28$total_no_attrtion)
attrition_decile_model_28$per_cum_no_attrition<-attrition_decile_model_28$cum_no_attrition*100/1103
attrition_decile_model_28$difference<-attrition_decile_model_28$Gain- attrition_decile_model_28$per_cum_no_attrition
attrition_decile_model_28$difference
# KS-Statistics for model 28
which.max(attrition_decile_model_28$difference) #decile number=3, ks-statistics=41.80 %

attrition_decile_model_32$total_no_attrtion=attrition_decile_model_32$total- attrition_decile_model_32$totalresp
attrition_decile_model_32$cum_no_attrition<-cumsum(attrition_decile_model_32$total_no_attrtion)
attrition_decile_model_32$per_cum_no_attrition<-attrition_decile_model_32$cum_no_attrition*100/1103
attrition_decile_model_32$difference<-attrition_decile_model_32$Gain- attrition_decile_model_32$per_cum_no_attrition
attrition_decile_model_32$difference
# KS-Statistics for model 32
which.max(attrition_decile_model_32$difference) #decile number=3, ks-statistics=44.61 %

#Gain Chart for mdeil_28
ggplot(attrition_decile_model_28,aes(x=factor(bucket),y=Gain))+geom_point()
#Gain chart for model_32
ggplot(attrition_decile_model_32,aes(x=factor(bucket),y=Gain))+geom_point()

#lift chart for model_28
ggplot(attrition_decile_model_28,aes(x=factor(bucket),y=Cumlift))+geom_point()
#lift chart for model_32
ggplot(attrition_decile_model_32,aes(x=factor(bucket),y=Cumlift))+geom_point()

## Hence we can see that our model_32 has better ks-statistics as well. It is able to cover 
# more than 75% of churning employees till the forth decile.

## Model_32 contains the variables "Age + NumCompaniesWorked + YearsSinceLastPromotion + YearsWithCurrManager + overtime_count + 
#  EnvironmentSatisfaction4 + JobSatisfaction4 + BusinessTravelTravel_Frequently + MaritalStatusSingle

##Bivariate analysis also justifies the presence of variables like Age, MaritalStatusSingle, BusinessTravelTravel_Frequently etc.

# for now we can conclude Model_32 is better model for prediction
