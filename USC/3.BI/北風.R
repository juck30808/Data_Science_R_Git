setwd("D:/課程資料/Northwind")   
library(data.table)
Z = read.csv("Employee.csv");head(Z,2)
X = read.csv("Orders.csv");   dim(X);   head(X,2)  

# [1] 16818    14

# Id CustomerId EmployeeId  OrderDate RequiredDate ShippedDate ShipVia Freight                  ShipName
# 1 10248      VINET          5 2012-07-04   2012-08-01  2012-07-16       3   16.75 Vins et alcools Chevalier
# 2 10249      TOMSP          6 2012-07-05   2012-08-16  2012-07-10       1   22.25       Toms Spezialit瓣ten
# ShipAddress ShipCity     ShipRegion ShipPostalCode ShipCountry
# 1 59 rue de l'Abbaye    Reims Western Europe          51100      France
# 2      Luisenstr. 48 M羹nster Western Europe          44087     Germany

Y = read.csv("OrderDetail.csv");     dim(Y);   head(Y,2)
# [1] 621883      6
# Id OrderId ProductId UnitPrice Quantity Discount
# 1 10248/11   10248        11      14.0       12        0
# 2 10248/42   10248        42       9.8       10        0

##############################################################
Y$Amount = Y$UnitPrice * Y$Quantity * (1-Y$Discount)  ##計算amount


Y$OrderDate = X$OrderDate[match(Y$OrderId,X$Id)]
Y$CustomerId = X$CustomerId[match(Y$OrderId,X$Id)]
head(Y,2)

Y$EmployeeId = X$EmployeeId[match(Y$OrderId,X$Id)]
Y$EmployeeTitle = Z$Title[match(Y$EmployeeId,Z$Id)]
head(Y,2)

TCC = (table(Y$CustomerId,Y$EmployeeTitle));   dim(TCC);   head(TCC,2)
TCC.fit = hclust(dist(TCC),method="ward.D");   plot(TCC.fit)
TCC.group = cutree(TCC.fit,k=5);   
table(TCC.group)



#Group name
k=1
gk=round( colMeans(TCC[TCC.group==k,]), 1 )
paste0("T",names(gk))
paste0(round(gk,0),"*",paste0("T",names(gk)))

