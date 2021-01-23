# example 1.1 of section 1.2.3 
# Title: Building a decision tree 

setwd("/Users/juck30808/Documents/Github/USC_R_Git/3.Manning/data")
library('rpart')
load('GCDData.RData')

model <- rpart(Good.Loan ~
   Duration.in.month +
   Installment.rate.in.percentage.of.disposable.income +
   Credit.amount  +
   Other.installment.plans,
   data=d,
   control=rpart.control(maxdepth=4),
   method="class");model



# example 1.2 of section 1.2.4 
# Title: Plotting the confusion matrix 

creditdata <- d
resultframe <- data.frame(Good.Loan=creditdata$Good.Loan,
                          pred=predict(model, type="class"))
rtab <- table(resultframe) 	# Note: 1  創建混淆矩陣，Row代表實際貸款狀態； Col代表預測的貸款狀態。對角線條目表示正確的預測。
rtab
##           pred
## Good.Loan  BadLoan GoodLoan
##   BadLoan       41      259
##   GoodLoan      13      687

sum(diag(rtab))/sum(rtab)  	# Note: 2  準確性混淆矩陣總體模型準確性：73％的預測是正確的。
## [1] 0.728
sum(rtab[1,1])/sum(rtab[,1]) 	# Note: 3 精度混淆矩陣模型精度：預測為不良的申請人中有76％確實確實違約。
## [1] 0.7592593
sum(rtab[1,1])/sum(rtab[1,]) 	# Note: 4 召回混淆矩陣模型召回：模型發現了14％的違約貸款。
## [1] 0.1366667
sum(rtab[2,1])/sum(rtab[2,]) 	# Note: 5 誤報率混淆矩陣誤報率：2％的好申請人被誤認為是不好的。
## [1] 0.01857143



# example 1.3 of section 1.3.1 
# Title: Plotting the relation between disposable income and loan outcome 
tab1 <- as.table(matrix(data=c(50,6,0,44),nrow=2,ncol=2))
dimnames(tab1) <- list('loan.as.pct.disposable.income'= c('LT.15pct','GT.15pct'),
                       'loan.quality.pop1'= c('goodloan','badloan'))
tab2 <- as.table(matrix(data=c(34,18,16,32),nrow=2,ncol=2))
dimnames(tab2) <- list('loan.as.pct.disposable.income'= c('LT.15pct','GT.15pct'),
                       'loan.quality.pop2'= c('goodloan','badloan'))
tab1
##                              loan.quality.pop1   
## loan.as.pct.disposable.income goodloan badloan
##                      LT.15pct       50       0
##                      GT.15pct        6      44
## 正確預測的計數在tab1的對角線上。 在這第一批人口中，所有低於(LT)可支配收入15％的貸款都是好貸款，
## 而除了6筆外，高於(GT)可支配收入 15％的貸款都是違約的。 因此該模型在此人口中的貸款質量很好。 
sum(diag(tab1))/sum(tab1)      
## [1] 0.94 它的準確率為94％。

tab2
##                              loan.quality.pop2  
## loan.as.pct.disposable.income goodloan badloan
##                      LT.15pct       34      16
##                      GT.15pct       18      32
## 在第二批人口中，約1/3 可支配收入不足的貸款違約，而一半以上的可支配收入超過15％的貸款合格。 
## 因此，您知道該模型(loan.as.pct.disposable.income)不能很好地模擬此人群的貸款質量。
sum(diag(tab2))/sum(tab2)     
## [1] 0.66 經驗法則只有66％的準確度。               

