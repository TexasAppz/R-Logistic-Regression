##NAME: JAIME GARCIA
##ASSIGNMENT 2
homework2 = read.csv("Homework2.csv")
str(homework2)
# 16 var, 234 obs
summary(homework2)
head(homework2)
library(mice)
library(VIM)
library(lattice)
###### Question A   ##################################################
summary(homework2)
str(homework2)
#234 observations
#Missing Data: Age: 4;  SocialFabric: 2; MonthlyPayment: 105, ParticipatesInCommunity:3


md.pattern(homework2)

# Multiple imputation
simple = homework2[c("Age", "SocialFabric", "MonthlyPayment","ParticipatesInCommunity")]
summary(simple)
set.seed(88)



md.pattern(simple)
simple_aggr = aggr(simple, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(simple), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
#Variables sorted by number of missings: 
#  Variable:       Count
#MonthlyPayment: 0.448717949
#Age: 0.017094017
#ParticipatesInCommunity: 0.012820513
#SocialFabric: 0.008547009

####### Question B   ##################################################

library(ggplot2)
scatterplot = ggplot(homework2, aes(x = MonthlyIncome, y = LoanSizeAvg))
scatterplot + geom_point()
scatterplot + geom_point(color = "blue", size = 3) + ggtitle("MonthylIcome vs. LoanSizeAvg")
ggplot(homework2, aes(x = MonthlyIncome, y = LoanSizeAvg, color = NotPayBackLoan)) + ggtitle("MonthylIcome vs. LoanSizeAvg") + geom_point()
# MonthlyIncomr > 20,000 && LoanSizeAvg > 20,000 are not correlated

###### Question C ##################################################
homework2_omit = na.omit(homework2)
summary(homework2_omit)
str(homework2_omit)
# 122 observations

library(caTools)

spl_homework2 = sample.split(homework2_omit$NotPayBackLoan, SplitRatio = 0.65)
homework2_omit_Train = subset(homework2_omit, spl_homework2==TRUE)
homework2_omit_Test = subset(homework2_omit, spl_homework2==FALSE)
summary(homework2_omit_Train)
str(homework2_omit_Train)
write.csv(homework2_omit_Train,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assigment2/homework2_omit_Train.csv")
write.csv(homework2_omit_Test,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assigment2/homework2_omit_Test.csv")

# Building a Logistic Regression Model: First Try
homework2_omit_Train_Model = glm(NotPayBackLoan ~ MonthlyPayment + LoanPeriodMonths + UpfrontPaymentAvg + LoanSizeAvg + LoanOpinion + SocialFabric + ParticipatesInCommunity + MonthlyExpenses + YearsAtThisHome + FamilySize + Age + Sex, data = homework2_omit_Train, family=binomial)
summary(homework2_omit_Train_Model)

# After Refining
homework2_omit_Train_Model = glm(NotPayBackLoan ~ MonthlyPayment + LoanPeriodMonths + LoanSizeAvg + SocialFabric, data = homework2_omit_Train, family=binomial)
summary(homework2_omit_Train_Model)
# logit = 7.6331883 - 0.0005362(MonthlyPayment) - 0.1691183(LoanPeriodMonths) + 0.0002922(LoanSizeAvg) - 2.6592855(SocialFabric)
# Prob = 1 / (1 + e^-(-logit)) 

######  (D) ##################################################

# Evaluating the Model

homework2_omit_Train_predict = predict(homework2_omit_Train_Model, type="response")
homework2_omit_Train_predict
table(homework2_omit_Train$NotPayBackLoan, homework2_omit_Train_predict > 0.5)
# Accuracy for cutoff =0.5: (34+22) / (34+22+8+15) = 70.8%
#Sensitivity = 22 / (22+15) = 59.4%
#Specificity = 34 / (34+8) = 80.9%
#baseline= (34+8)/(34+8+15+22) = 53%

table(homework2_omit_Train$NotPayBackLoan, homework2_omit_Train_predict > 0.7)
# Accuracy for cutoff =0.7: (40+15)/(40+15+22+2) = 69.6%
#Sensitivity = 15 / (15+22) = 40.5%
#Specificity = 40 / (40+ 2) = 95.2%

table(homework2_omit_Train$NotPayBackLoan, homework2_omit_Train_predict > 0.2)
# Accuracy for cutoff = 0.2: (20+35)/(20+35+22+2)= 69.6%
#Sensitivity = 35 / (35+20) = 63.6%
#Specificity = 20 / (20+22) = 47.6%
# I pick up T=0.5; because Sensitivity > 50% && Specificity > 50%

#Baseline = (24+18)/(24+18+8+29) = 0.53
table(homework2_omit_Train$NotPayBackLoan, homework2_omit_Train_predict > 0.6)
# Accuracy for cutoff = 0.6: (27+21)/(21+38+16+4) = 60.7%
#Sensitivity = 38 /(38+4) = 90.4%
#Specificity = 21/(21+16) = 56.7% - 100% = 43.2%


table(homework2_omit_Train$NotPayBackLoan, homework2_omit_Train_predict > 0.4)
# Accuracy for cutoff = 0.4: (27+32)/(27+32+15+5) = 74.6%
#Sensitivity = 32 /(32+5) = 86.4%;
#Specificity =27/(27+15) = 64.2%;   1 - 0.642 = 35.8% = FPR
# I pick up T=0.4; because Sensitivity > 50% && Specificity > 50%


library(stats)
library(gplots)
library(ROCR)

# plotting it in the Training set
homework2_omit_Train_ROC_predict = prediction(homework2_omit_Train_predict, homework2_omit_Train$NotPayBackLoan)
ROC_Curve_homework2_omit_Train = performance(homework2_omit_Train_ROC_predict, "tpr", "fpr")

plot(ROC_Curve_homework2_omit_Train)
plot(ROC_Curve_homework2_omit_Train, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(homework2_omit_Train_ROC_predict, "auc")@y.values)
#### 0.8462033 ~ 84.62%

###### (E) #############################################################
# plotting it in the Testing set
summary(homework2_omit_Test)
str(homework2_omit_Test)
# Building a Logistic Regression Model on the testinv dataset:
homework2_omit_Test_Model = glm(NotPayBackLoan ~ MonthlyPayment + LoanPeriodMonths + LoanSizeAvg + SocialFabric, data = homework2_omit_Test, family=binomial)
summary(homework2_omit_Test_Model)
#logit = - 3.0109921 - 0.0002863(MonthlyPayment) - 0.1344781(LoanPeriodMonths) + 0.0006338(LoanSizeAvg) + 0.4565672(SocialFabric)


homework2_omit_Test_predict = predict(homework2_omit_Test_Model, type="response")
homework2_omit_Test_predict
table(homework2_omit_Test$NotPayBackLoan, homework2_omit_Test_predict > 0.4)
# Accuracy for cutoff =0.4: (18+16)/(18+16+5+4) = 79%
#Sensitivity =  16/(16+4) = 80%
#Specificity = 18/(18+5) = 78.26%; 1 - 0.7826087 = 21.73%
#Baseline = (18+5)/(18+5+4+16) = 53.48%


homework2_omit_Test_ROC_predict = prediction(homework2_omit_Test_predict, homework2_omit_Test$NotPayBackLoan)
ROC_Curve_homework2_omit_Test = performance(homework2_omit_Test_ROC_predict, "tpr", "fpr")

plot(ROC_Curve_homework2_omit_Test)
plot(ROC_Curve_homework2_omit_Test, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7), ylab ="true positive rate", xlab ="false positive rate")
as.numeric(performance(homework2_omit_Test_ROC_predict, "auc")@y.values)
#AUC: 0.9163043 ~ 90.21%
# 


##### (f)  ###############################################################

imp = mice(simple, m=5, printFlag=FALSE, maxit = 30, seed=2525)
imp
xyplot(imp, MonthlyPayment ~ ParticipatesInCommunity | .imp, pch = 20, cex = 1.4)
imputed = complete(imp)
summary(imputed)

homework2$Age = imputed$Age
homework2$SocialFabric =  imputed$SocialFabric
homework2$MonthlyPayment = imputed$MonthlyPayment
homework2$ParticipatesInCommunity = imputed$ParticipatesInCommunity


summary(homework2)
split_homework2 = sample.split(homework2_omit$NotPayBackLoan, SplitRatio = 0.65)
Train = subset(homework2, split_homework2==TRUE)
Test = subset(homework2, split_homework2==FALSE)

cor(Train)
str(Train)
cor(Train[c("Age", "SocialFabric", "MonthlyPayment","ParticipatesInCommunity")])

# Before Refining
Log_Reg1 = glm(NotPayBackLoan ~ MonthlyPayment + LoanPeriodMonths + UpfrontPaymentAvg + LoanSizeAvg + LoanOpinion + SocialFabric + ParticipatesInCommunity + MonthlyExpenses + YearsAtThisHome + FamilySize + Age + Sex, data = Train, family="binomial")
summary(Log_Reg1)


#After some refining
Log_Reg1 = glm(NotPayBackLoan ~ 0 + MonthlyPayment + LoanPeriodMonths + UpfrontPaymentAvg + LoanSizeAvg + ParticipatesInCommunity, data = Train, family="binomial")
summary(Log_Reg1)
#Logit = -0.0007924(MonthlyPayment) - 0.1377043(LoanPeriodMonths) - 0.0008785(UpfrontPaymentAvg) + 0.0009528(LoanSizeAvg) - 0.8234301(ParticipatesInCommunity)


######## (G) ###############################################################
pred_0 = predict(Log_Reg1, type="response")
table(Train$NotPayBackLoan, pred_0 > 0.5)
table(Train$NotPayBackLoan, pred_0 > 0.2)
# Accuracy for cutoff = (73+40)/(73+40+34+4)= 0.7483444 ~ 74.8%
#Sensitivity =  40/(40+4) = 0.9090909 ~ 90.9%
#Specificity = 73 /(73+34) = 0.682243; 1 - 0.682243 = 31.7%
#Baseline = (73+34) /(73+34+4+40) = 0.7086093 ~ 70.8%


table(Train$NotPayBackLoan, pred_0 > 0.7)
table(Train$NotPayBackLoan, pred_0 > 0.4)
# Accuracy for cutoff =0.4: (93+14)/(83+20+14+24) = 0.7588652 ~ 75.8%
#Sensitivity =  24/(24+20) = 0.5454545 ~ 54.5%
#Specificity = 93/(93+14) = 0.8691589; 1 - 0.8691589 = 0.1308411 ~ 13.1% = FPR
#Baseline = (93+14)/(93+14+20+24) = 0.7086093 ~ 70.8%


ROC_predict = prediction(pred_0, Train$NotPayBackLoan)
ROC_Curve = performance(ROC_predict, "tpr", "fpr")

plot(ROC_Curve)
plot(ROC_Curve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7), ylab ="true positive rate", xlab ="false positive rate")
as.numeric(performance(ROC_predict, "auc")@y.values)

# AUC: 0.8532285

#####(h)   ###############################################################

Log_Reg1_test = glm(NotPayBackLoan ~ 0 + MonthlyPayment + LoanPeriodMonths + UpfrontPaymentAvg + LoanSizeAvg + ParticipatesInCommunity, data = Test, family="binomial")
summary(Log_Reg1_test)
#Logit = 0.0005202(MonthlyPayment) - 0.1248395(LoanPeriodMonths) - 0.0010611(UpfrontPaymentAvg) + 0.0009593(LoanSizeAvg) - 0.9036841(ParticipatesInCommunity)

####(g)   ###############################################################
pred_0_test = predict(Log_Reg1_test, type="response")

table(Test$NotPayBackLoan, pred_0_test > 0.2)
# Accuracy for cutoff = (47+17)/(47+17+16+3) = 0.77 ~ 77%
#Sensitivity =  17/(17+3) = 0.85 ~ 85%
#Specificity = 47 /(47+16) = 0.7460317; 1 - 0.7460317 = 0.2539683 ~ 25.4%
#Baseline = (47+16)/(47+16+3+17) = 0.759 ~ 75.9%


ROC_predict_test = prediction(pred_0_test, Test$NotPayBackLoan)
ROC_Curve_test = performance(ROC_predict_test, "tpr", "fpr")

plot(ROC_Curve_test)
plot(ROC_Curve_test, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7), ylab ="true positive rate", xlab ="false positive rate")
as.numeric(performance(ROC_predict_test, "auc")@y.values)

# AUC: 0.8904762 ~ 89%


