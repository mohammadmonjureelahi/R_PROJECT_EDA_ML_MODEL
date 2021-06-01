# Let us first check and set the working directory.
getwd()

setwd("C:/Users/ruzdomain/Desktop/R/R_PROJECT")

getwd()

adult <- read.csv('adult_sal.csv')

head(adult)

str(adult)

library(dplyr)

adult <- select(adult,-X)

head(adult)

str(adult)

summary(adult)

adult[adult == ''] <- NA

summary(adult)

sum(duplicated(adult))

r1 <- which(duplicated(adult))

adult <- adult[-r1,]
summary(adult)

sum(!complete.cases(adult))

r2 <- which(!complete.cases(adult))

adult[r2,]

adult <- adult[-r2,]
summary(adult)

sum(is.na(adult$age))

sum(is.na(adult$type_employer))

sum(is.na(adult$fnlwgt))

sum(is.na(adult$education)) 

sum(is.na(adult$education_num)) 

sum(is.na(adult$marital)) 

sum(is.na(adult$occupation)) 

sum(is.na(adult$relationship)) 

sum(is.na(adult$race))

sum(is.na(adult$sex))

sum(is.na(adult$capital_gain))

sum(is.na(adult$capital_loss))

sum(is.na(adult$hr_per_week))

sum(is.na(adult$country))

sum(is.na(adult$income))

summary(adult)

table(adult$type_employer)

table(adult$country)

adult[adult == '?'] <- NA

table(adult$type_employer)

table(adult$country)

adult$type_employer <- sapply(adult$type_employer,factor)

adult$country <- sapply(adult$country,factor)

table(adult$type_employer)

table(adult$country)

install.packages("Amelia")

library(Amelia)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

adult <- na.omit(adult)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

table(adult$type_employer)

unemp <- function(job){
    job <- as.character(job)
    if (job=='Never-worked' | job=='Without-pay'){
        return('Unemployed')
    }else{
        return(job)
    }
}

adult$type_employer <- sapply(adult$type_employer,unemp)

table(adult$type_employer)

group_emp <- function(job){
    if (job=='Local-gov' | job=='State-gov'){
        return('SL-gov')
    }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
        return('self-emp')
    }else{
        return(job)
    }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)

table(adult$type_employer)

table(adult$education)

summary(adult$education)

edugroup <- function(edu){
  edu <- as.character(edu)
  if (edu == 'Preschool' | edu == '1' | edu =='1st-4th' | edu =='5th-6th'){
    return('Upto_Primary')
  }else if ( edu == '7th-8th'){
    return('UptoInter')
    } else if (edu == '9th' | edu == '10th' | edu == '11th' | edu == '12th' | edu == 'HS-grad'){
      return('Upto_HS_Grad')
    }else if (edu == 'Assoc-acdm' | edu == 'Assoc-voc' | edu == 'Prof-school' | edu == 'Some-college' ){
      return('Post_HS-School')
    }else{
    return(edu)
  }
}

adult$education <- sapply(adult$education,edugroup)

table(adult$education)

table(adult$marital)

summary(adult$marital)

which((adult$marital == "1"))

group_marital <- function(mar){
    mar <- as.character(mar)
    
    # Not-Married
    if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
        return('Not-Married')
    
    # Never-Married   
    }else if(mar=='Never-married'){
        return(mar)
    
     #Married
    }else{
        return('Married')
    }
}

adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

table(adult$country)

levels(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                            'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
    if (ctry %in% Asia){
        return('Asia')
    }else if (ctry %in% North.America){
        return('North.America')
    }else if (ctry %in% Europe){
        return('Europe')
    }else if (ctry %in% Latin.and.South.America){
        return('Latin.and.South.America')
    }else{
        return('Other')      
    }
}

adult$country <- sapply(adult$country,group_country)

table(adult$country)

names(adult)[names(adult)=="country"] <- "region"

table(adult$region)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$region <- sapply(adult$region,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)
adult$education <- sapply(adult$education,factor)

table(adult$occupation)

table(adult$relationship)

table(adult$race)

table(adult$sex)

table(adult$income)

str(adult)

summary(adult)

library(dplyr)

adult %>% select(hr_per_week, income) %>% filter(hr_per_week > 100)

adult$hr_per_week<-ifelse(adult$hr_per_week > 100, adult$hr_per_week/10, adult$hr_per_week)

summary(adult)

tbl <-table(adult$income)
tbl

library(ggplot2)

ggplot(adult,aes(income)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(adult$age)

ggplot(adult,aes(age)) + geom_histogram(aes(fill=age), color='black', fill='orange') + theme_bw()

tbl1 <-table(adult$type_employer)
tbl1

ggplot(adult,aes(type_employer)) + geom_bar(aes(fill=type_employer),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(adult$fnlwgt)

ggplot(adult,aes(fnlwgt)) + geom_histogram(aes(fill=fnlwgt), color='black', fill='yellow') + theme_bw()

tbl2 <-table(adult$education)
tbl2

ggplot(adult,aes(education)) + geom_bar(aes(fill=education),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(adult$education_num)

ggplot(adult,aes(education_num)) + geom_histogram(aes(fill=education_num), color='black', fill='yellow') + theme_bw()

tbl3 <-table(adult$marital)
tbl3

ggplot(adult,aes(marital)) + geom_bar(aes(fill=marital),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

tbl4 <-table(adult$occupation)
tbl4

ggplot(adult,aes(occupation)) + geom_bar(aes(fill=occupation),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

tbl5 <-table(adult$relationship)
tbl5

ggplot(adult,aes(relationship)) + geom_bar(aes(fill=relationship),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

tbl6 <-table(adult$race)
tbl6

ggplot(adult,aes(race)) + geom_bar(aes(fill=race),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

tbl7 <-table(adult$sex)
tbl7

ggplot(adult,aes(sex)) + geom_bar(aes(fill=sex),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(adult$hr_per_week)

ggplot(adult,aes(hr_per_week)) + geom_histogram(aes(fill=hr_per_week), color='black', fill='yellow') + theme_bw()

tbl8 <-table(adult$region)
tbl8

ggplot(adult,aes(region)) + geom_bar(aes(fill=region),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

agg1 <- aggregate(age ~ income, adult , mean)
agg1
names(agg1) <- c("Income Group","Mean of Age")
agg1

t.test(age ~ income, data=adult)

ggplot(adult,aes(factor(income),age)) + geom_boxplot(aes(color=factor(income))) +theme_bw()

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

tbl9<-table(adult$type_employer,adult$income)
tbl9

chisq2 <- table(adult$income,adult$type_employer)
chisq.test(chisq2)

ggplot(adult,aes(type_employer)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

agg2 <- aggregate(fnlwgt ~ income, adult , mean)
agg2
names(agg2) <- c("Income Group","Mean of Final Weight")
agg2

t.test(fnlwgt ~ income, data=adult)

ggplot(adult,aes(factor(income),fnlwgt)) + geom_boxplot(aes(color=factor(income))) +theme_bw()

ggplot(adult,aes(fnlwgt)) + geom_histogram(aes(fill=income),color='black') + theme_bw()

tbl10<-table(adult$education,adult$income)
tbl10

chisq3 <- table(adult$income,adult$education)
chisq.test(chisq3)

ggplot(adult,aes(education)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

agg3 <- aggregate(education_num ~ income, adult , mean)
agg3
names(agg3) <- c("Income Group","Mean of Education Expressed in Number")
agg3

t.test(education_num ~ income, data=adult)

ggplot(adult,aes(factor(income),education_num)) + geom_boxplot(aes(color=factor(income))) +theme_bw()

ggplot(adult,aes(education_num)) + geom_histogram(aes(fill=income),color='black') + theme_bw()

tbl11<-table(adult$marital,adult$income)
tbl11

chisq4 <- table(adult$income,adult$marital)
chisq.test(chisq4)

ggplot(adult,aes(marital)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

tbl12<-table(adult$occupation,adult$income)
tbl12

chisq5 <- table(adult$income,adult$occupation)
chisq.test(chisq5)

ggplot(adult,aes(occupation)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

tbl13<-table(adult$relationship,adult$income)
tbl13

chisq6 <- table(adult$income,adult$relationship)
chisq.test(chisq6)

ggplot(adult,aes(relationship)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

tbl14<-table(adult$race,adult$income)
tbl14

chisq7 <- table(adult$income,adult$race)
chisq.test(chisq7)

ggplot(adult,aes(race)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

tbl15<-table(adult$sex,adult$income)
tbl15

chisq8 <- table(adult$income,adult$sex)
chisq.test(chisq8)

ggplot(adult,aes(sex)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

agg4 <- aggregate(hr_per_week ~ income, adult , mean)
agg4
names(agg4) <- c("Income Group","Mean of Hours Per Week")
agg4

t.test(hr_per_week ~ income, data=adult)

ggplot(adult,aes(factor(income),hr_per_week)) + geom_boxplot(aes(color=factor(income))) +theme_bw()

ggplot(adult,aes(hr_per_week)) + geom_histogram(aes(fill=income),color='black') + theme_bw()

tbl16<-table(adult$region,adult$income)
tbl16

chisq9 <- table(adult$income,adult$region)
chisq.test(chisq9)

ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

head(adult, 10)

adult$income <- sapply(adult$income,factor)

# Install and Import Library

install.packages("caTools")

library(caTools)

set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

model <- glm(formula=income ~ .,
            family = binomial(logit),
            data = train)

summary(model)

test$predicted.income = predict(model, newdata=test, type="response")

confusion <- table(actual=test$income, predicted=test$predicted.income > 0.5)

confusion

accuracy <- (confusion[[1,1]] + confusion[[2,2]])/sum(confusion)

accuracy

misclassError <- 1 - accuracy

misclassError
