1.
creditRiskData = read.csv('C:/Users/harsha/Desktop/Stats+R/CSV Files/creditRiskData.csv',
                            na.strings = c(""," ","NA","NULL"), stringsAsFactors = T)
creditRiskData

View(creditRiskData)

head(creditRiskData)

dim(creditRiskData)
summary(creditRiskData)

str(creditRiskData)


#creditRiskData$GoodCredit=as.factor(creditRiskData$GoodCredit)
#creditRiskData$liable18=as.factor(creditRiskData$liable18)

factor_cols=c("GoodCredit","liable18")

for(cat_cols in factor_cols){
  creditRiskData[ , cat_cols]=as.factor(creditRiskData[ , cat_cols])
}

class(creditRiskData)

#Removing useless columns in the data, and explore the rest
#UselessColumns=c('employ7','status9','foreign20','tele19','job17')
#creditRiskData[ ,UselessColumns]=NULL

str(creditRiskData)

colSums(is.na(creditRiskData))

#continuous column Histogram

library(RColorBrewer)

ColsForHist = c("duration2","amount5","installment8","residence11","age13","cards16")

par(mfrow=c(2,3))

for (ColumnName in ColsForHist) {
  hist(creditRiskData[ ,(ColumnName)], main=paste('Histogram of:',ColumnName),
     col=brewer.pal(8,"Paired"))
}


#Mutiple categorical column Barplot

ColsForBar=c("GoodCredit","checkingstatus1","history3","purpose4","savings6",
             "employ7","status9","others10","property12","otherplans14","housing15","job17",
             "liable18","tele19","foreign20")

par(mfrow=c(3,5))

for (Col in ColsForBar) {
  barplot(table(creditRiskData[ ,c(Col)]), main = paste('Barplt of:', Col),
             col = brewer.pal(8,"Paired"))
  
}

# Visual Relationship between predictors and target variable 

#ColsForBar =c("duration2","amount5","age13")

#par(mfrow=c(2,2))

#for (Box_cols in ColsForBar) {
 # boxplot(GoodCredit~(creditRiskData[ ,c(Box_cols)]), data = creditRiskData,
  #            main=paste('Box plot of:' ,Box_cols),
   #           Col=brewer.pal(8,"Paired"))
  
#}

# Categorical Vs Continuous Visual analysis: Boxplot
par(mfrow=c(1,1))
boxplot(duration2~GoodCredit, data = creditRiskData, col=brewer.pal(8,"Paired"))

par(mfrow=c(1,1))
boxplot(amount5~GoodCredit, data = creditRiskData, col=brewer.pal(8,"Paired"))

par(mfrow=c(1,1))
boxplot(age13~GoodCredit, data = creditRiskData, col=brewer.pal(8,"Paired"))

# Categorical Vs Categorical Visual analysis: Grouped Bar chart

table(creditRiskData$job17)
table(creditRiskData$purpose4)
table(creditRiskData$property12)


CrossTabResult=table(creditRiskData[ , c("GoodCredit","savings6")])
CrossTabResult

barplot(CrossTabResult, beside = T, col=brewer.pal(4,"Paired"))

# Statistical Relationship between target variable (Categorical) and predictors
# Continuous Vs Categorical relationship strength: ANOVA

summary(aov(duration2~GoodCredit, data = creditRiskData))
summary(aov(amount5~GoodCredit, data = creditRiskData))
summary(aov(age13~GoodCredit, data = creditRiskData))


colsForBar=c("duration2","amount5","age13")





# in all above 3 cases we reject the null Hypothesis as P <5% ,which means
#duration and Amount are highly correlated and age also have good correlation with 
# target variable GoodCredit 

# Categorical Vs Categorical -- Chi-square test
#H0:the two columns are not correlated

chisq.test(CrossTabResult)

Chisqcols = c("checkingstatus1","history3","purpose4","savings6","employ7",
              "status9","others10","property12","otherplans14","housing15","job17",
             "liable18","tele19","foreign20")

for (Chi_cols in Chisqcols) {
  CrossTabResult=table(creditRiskData[ ,c('GoodCredit',Chi_cols)])
  ChiResult=chisq.test(CrossTabResult)
  print(Chi_cols)
  print(ChiResult)
}

# for Checkingstatus, History,Saving,Property has very low P value so we reject the null hypothesis
#and conclude that these 3 variable are correlated with TV
  

# MOdel for Machine learning
  
InputData=creditRiskData
TargetVariableName= 'GoodCredit'
TargetVariable=InputData[ ,c(TargetVariableName)]
str(TargetVariable)

PredictorVariables=InputData[, !names(InputData) %in% TargetVariableName]
str(PredictorVariables)

#PredictorVariables=c("checkingstatus1","history3","purpose4","savings6",
                     "others10","property12","otherplans14","housing15",
                     "liable18","duration2","amount5","installment8",
                     "residence11","age13","cards16")
#PredictorVariables
                     
#PredictorVariable=InputData[ ,PredictorVariables]
#PredictorVariable


DataForML=data.frame(TargetVariable,PredictorVariables)
str(DataForML)
head(DataForML)

#Sampling | Splitting data into 70% for training 30% for testing

TrainingSampleIndex=sample(1:nrow(DataForML), size = 0.7*nrow(DataForML))
DataForMLTrain=DataForML[TrainingSampleIndex,  ]
DataForMLTest=DataForML[ -TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)

# Logistic Regression

LR_Model=glm(TargetVariable~. ,data = DataForMLTrain, family = 'binomial' )
summary(LR_Model)

LR_Model_2=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")+I(employ7=="A72")+I(employ7=="A73")
               +(employ7=="A74")
                 +I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A142")+I(otherplans14=="A143")
                 +I(job17=="A172")+I(job17=="A173")+I(job17=="A174")+I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+I(liable18==2)+duration2+amount5+installment8+residence11+age13+cards16,
                 data = DataForMLTrain, family = 'binomial')
             

summary(LR_Model_2)

LR_Model_3=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")+I(employ7=="A72")+I(employ7=="A73")
               +(employ7=="A74")
               +I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A142")+I(otherplans14=="A143")
               +I(job17=="A172")+I(job17=="A173")+I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+I(liable18==2)+duration2+amount5+installment8+residence11+age13+cards16,
               data = DataForMLTrain, family = 'binomial')



summary(LR_Model_3)

LR_Model_4=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")+I(employ7=="A72")+I(employ7=="A73")
               +(employ7=="A74")
               +I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A142")+I(otherplans14=="A143")
               +I(job17=="A173")+I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+I(liable18==2)+duration2+amount5+installment8+residence11+age13+cards16,
               data = DataForMLTrain, family = 'binomial')


summary(LR_Model_4)

LR_Model_5=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")+I(employ7=="A72")+I(employ7=="A73")
               +(employ7=="A74")
               +I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A142")+I(otherplans14=="A143")
               +I(job17=="A173")+I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+I(liable18==2)+duration2+amount5+installment8+residence11+age13+cards16,
               data = DataForMLTrain, family = 'binomial')

summary(LR_Model_5)


LR_Model_6=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")+I(employ7=="A72")+I(employ7=="A73")
               +(employ7=="A74")
               +I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A142")+I(otherplans14=="A143")
               +I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+I(liable18==2)+duration2+amount5+installment8+residence11+age13+cards16,
               data = DataForMLTrain, family = 'binomial')

summary(LR_Model_6)

LR_Model_7=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")+I(employ7=="A72")+I(employ7=="A73")
               +(employ7=="A74")
               +I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A142")+I(otherplans14=="A143")
               +I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+residence11+age13+cards16,
               data = DataForMLTrain, family = 'binomial')



summary(LR_Model_7)

LR_Model_8=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")+I(employ7=="A73")
               +(employ7=="A74")
               +I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A142")+I(otherplans14=="A143")
               +I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+residence11+age13+cards16,
               data = DataForMLTrain, family = 'binomial')
summary(LR_Model_8)

LR_Model_9=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")
               +(employ7=="A74")
               +I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A142")+I(otherplans14=="A143")
               +I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+residence11+age13+cards16,
               data = DataForMLTrain, family = 'binomial')

summary(LR_Model_9)

LR_Model_10=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
               +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
               +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                 I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
               +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")
               +(employ7=="A74")
               +I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                 I(property12=="A124")+I(otherplans14=="A143")
               +I(tele19=="A192")+I(foreign20=="A202")
               +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+residence11+age13+cards16,
               data = DataForMLTrain, family = 'binomial')

summary(LR_Model_10)

LR_Model_11=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A102")+I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(tele19=="A192")+I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_11)

LR_Model_12=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(tele19=="A192")+I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_12)

LR_Model_13=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(tele19=="A192")+I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_13)

LR_Model_14=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A62")+I(savings6=="A63")+I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(tele19=="A192")+I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')


summary(LR_Model_14)

LR_Model_15=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A62")+I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+I(property12=="A122")+I(property12=="A123")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(tele19=="A192")+I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')


summary(LR_Model_15)


LR_Model_16=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A62")+I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+I(property12=="A122")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(tele19=="A192")+I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_16)

LR_Model_17=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A62")+I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(tele19=="A192")+I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_17)

LR_Model_18=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(tele19=="A192")+I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_18)

LR_Model_19=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A45")+I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_19)

LR_Model_20=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A93")+I(status9=="A94")+
                  I(others10=="A103")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_20)

LR_Model_21=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A48")+I(purpose4=="A49")
                +I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(status9=="A94")+
                  I(others10=="A103")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_21)

LR_Model_22=glm(TargetVariable~I(checkingstatus1=="A12")+I(checkingstatus1=="A14")
                +I(history3=="A31")+I(history3=="A33")+I(history3=="A34")+I(purpose4=="A41")
                +I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+
                  I(purpose4=="A49")
                +I(savings6=="A64")+I(savings6=="A65")
                +(employ7=="A74")
                +I(others10=="A103")+
                  I(property12=="A124")+I(otherplans14=="A143")
                +I(foreign20=="A202")
                +I(housing15=="A152")+I(housing15=="A153")+duration2+amount5+installment8+age13+cards16,
                data = DataForMLTrain, family = 'binomial')

summary(LR_Model_21)


PredictionProb=predict(LR_Model_21,DataForMLTest,type = "response")
PredictionProb

## Creating the Confusion Matrix to calculate overall accuracy, 
#precision and recall on TESTING data

install.packages('caret',dependencies = TRUE)
library(caret)

#Iteration 

IterationData=data.frame(
  Threshold=numeric(0),
  Accuracy=numeric(0)
)

thresholds=seq(0.5,0.75,0.01)
for (i in thresholds){
  
  DataForMLTest$Prediction=ifelse(PredictionProb>i,1,0)
  DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
  AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")
  IterationData=rbind(IterationData,data.frame(
    Threshold=i,
    Accuracy=round(100* AccuracyResults[['overall']][1])))
}

IterationData

IterationData[IterationData$Accuracy==max(IterationData$Accuracy), ]



#considering a threshold of 0.69

DataForMLTest$Prediction=ifelse(PredictionProb>0.75,1,0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)


AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")

AccuracyResults[['table']]
AccuracyResults[['byClass']]
AccuracyResults[['overall']][1]

print(paste('## Overall Accuracy of Logistic Reg Model is: ', round(100* AccuracyResults[['overall']][1]) , '%'))



#######################################################################
##  Ctree Decision  Tree ####

library(party)

DT_Model_1=ctree(TargetVariable ~., data = DataForMLTrain )

plot(DT_Model_1)


# Checking Accuracy of model on Testing data
DataForMLTest$Prediction=Predict(DT_Model_1, DataForMLTest)
head(DataForMLTest)


# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)

AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")

# Since AccuracyResults is a list of multiple items, fetching useful components only

AccuracyResults[['table']]
AccuracyResults[['byclass']]

print(paste('## Overall Accuracy of Ctree Model is:', round(100* AccuracyResults[['overall']][1]), '%'))
