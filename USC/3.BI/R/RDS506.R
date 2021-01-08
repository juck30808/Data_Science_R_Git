# RDS506A.R: basic R program for Retail Data Analysis - 06
# Jia-Sheng Heh, 05/09/2020, revised from RDS306A.R

wkDir = "C:/Users/Mike/Desktop/working/RDS5/";   setwd(wkDir)

#####===== (1) 數據擷取 (XXXFM.csv) =====#####
XXXFM = read.csv("XXXFM.csv",fileEncoding="Big5");   dim(XXXFM);   head(XXXFM,2)   #-- [1] 84008    17
#   X invoiceNo channel customer product category price            datetime quantity amount       date hour CvFF0       CvMM0 weekdays year month
# 1 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:18:20        1   1692 2015-01-07   20 (0,1] (999,1e+04]   星期三 2015     1
# 2 2        N1      s1       c1     p11    kind1  1080 2015-01-07 20:14:41        1    550 2015-01-07   20 (0,1] (999,1e+04]   星期三 2015     1
XXXFM$price0 = cut(XXXFM$price,breaks=c(-1,0,999,2999,9999))   #---  要加這一行才會對 ***

#####===== (2) 數據選擇 (XXXFM-->dataset0-->dataset) =====#####
dataset0 = XXXFM[,c("CvFF0","CvMM0","customer","category","price0")]
table(dataset0$CvFF0,dataset0$CvMM0)
#             (-5e+03,0] (0,999] (1e+04,1e+05] (1e+05,1e+06] (1e+06,2e+07] (999,1e+04]
# (0,1]               28    1174           299             0             0        7405
# (1,10]              26      97         13334             0             0       12838
# (10,40]              0       0         10883           273             0           0
# (200,2e+04]          0       0             0           579         30895           0
# (40,200]             0       0          1325          4852             0           0
dataset = dataset0[which((dataset0$CvFF0=="(10,40]")&(dataset0$CvMM0=="(1e+05,1e+06]")),]
dim(dataset)   #-- [1] 273    5

#####===== (3) 數據轉換 (dataset-->DS[.., price0, category0]) =====##### ===> Power BI 與 R 的界面 #####
DS = dataset
sort(table(DS$category),decreasing=TRUE)[1:16]
# kind2 kind17 kind12  kind3 kind16  kind1 kind24 kind18 kind41 kind10 kind11 kind19 kind21 kind13 kind14 kind15
# 159     45     25     16     10      6      4      2      2      1      1      1      1      0      0      0 
Tcategory = sort(table(DS$category),decreasing=TRUE)
Tclist = c(names(Tcategory)[1:9],"others");   Tclist    #-- [1] "kind2"  "kind1"  "kind3"  "kind12" "kind16" "kind17" "kind11" "kind56" "kind27" "others"
DS$category0 = sapply(as.character(DS$category), FUN=function (x) ifelse(x %in% Tclist[1:9], x, "others"))
table(DS$price0,DS$category0)
#               kind1 kind12 kind16 kind17 kind18 kind2 kind24 kind3 kind41 others
# (-1,0]            0     23      0      0      0     0      0     0      0      0
# (0,999]           1      2      1     12      0    30      4     4      2      2
# (999,3e+03]       3      0      4     32      1   110      0    10      0      1
# (3e+03,1e+04]     2      0      5      1      1    19      0     2      0      1
DS$c0p0 = paste0(DS$category0,DS$price0)
head(DS,3)
#        CvFF0         CvMM0 customer category price      price0 category0             c0p0
# 2560 (10,40] (1e+05,1e+06]     c126    kind2  1690 (999,3e+03]     kind2 kind2(999,3e+03]
# 2561 (10,40] (1e+05,1e+06]     c126    kind2  2290 (999,3e+03]     kind2 kind2(999,3e+03]
# 2562 (10,40] (1e+05,1e+06]     c126    kind2  1690 (999,3e+03]     kind2 kind2(999,3e+03]

#####===== (4) 數據關連 (DS-->CpK-->rules0-->rules.df0-3-->rules) =====#####
library(arules)
customer = unique(DS$customer);   length(customer)   #-- 33
CpK = lapply(1:length(customer), function(k)unique(DS$c0p0[which(DS$customer==customer[k])]) )
#-- It doesn't support executing R script within R visual dynamically based on selection in slicer.
#   [https://community.powerbi.com/t5/Desktop/Passing-a-parameter-to-an-R-visual/td-p/189739]
#   --> 所以必須自己建 lib 來找最佳的 support 和 confidence --> 來找到約 20-30 條 rules
for (supp in seq(0.9,0.1,by=-0.1)) {  
  rules0 = apriori( CpK, parameter=list(support=supp, confidence=0.1), control=list(verbose=FALSE));  
  if (length(rules0)==0) next
  rules.df0 = data.frame( lhs=labels(lhs(rules0)), rhs=labels(rhs(rules0)), rules0@quality )
  indX = union( union( grep("],",rules.df0$lhs), which(nchar(as.character(rules.df0$lhs))==2)), 
                union( which(rules.df0$lift>1), which(rules.df0$count==1)  ) )
  nRules = dim(rules.df0)[1] - length(indX)
  print(c(supp,min(rules.df0$supp),min(rules.df0$conf),nRules))
  if (nRules >= 5) break
}
rules = cbind(rules.df0[,c("lhs","rhs")],round(rules.df0[,c("support","confidence","lift")],2),
              rules.df0[,"count"])[-indX,]
colnames(rules) = c("lhs","rhs","supp","conf","lift","count")
rules
#                  lhs                   rhs supp conf lift count
# 4     {kind2(0,999]} {kind2(999,3e+03]}    1    1    1     3
# 5 {kind2(999,3e+03]}     {kind2(0,999]}    1    1    1     3
# 6     {kind2(0,999]}    {kind24(0,999]}    1    1    1     3
# 7    {kind24(0,999]}     {kind2(0,999]}    1    1    1     3
# 8 {kind2(999,3e+03]}    {kind24(0,999]}    1    1    1     3
# 9    {kind24(0,999]} {kind2(999,3e+03]}    1    1    1     3

#####===== (5) 數據關連圖 (rules-->gW) =====#####
library(igraph)
xyTick <- function(xV,yV,xTick,yTick) {
  uxV = sort(unique(xV));   minX=min(xV);   maxX=max(xV);   tickX=2/(maxX-minX);   
  for (k in uxV) text(-1+(k-min(uxV))*tickX, -1.2, xTick[k])
  uyV = sort(unique(yV));   minY=min(yV);   maxY=max(yV);   tickY=2/(maxY-minY);   
  for (k in uyV) text(-1.5, -1+(k-min(uyV))*tickY, yTick[k])
}
plotPassoc <- function(gW,ruleW.df,Vsg,maintitle,xV0,yV0,Vg,xTick,yTick) {
  labelVg = paste0(V(gW)$name,"\n",Vsg[V(gW)$name])
  sizeVg  = 2+5*as.integer(cut(log10(Vsg[V(gW)$name])+0.1, breaks=c(0, 1, 2, 3, 4, 10) ) )
  colorVg = as.character(cut(log10(Vsg[V(gW)$name])+0.1, breaks=c(0, 1, 2, 3, 4, 10), labels=c("yellow","orange","green","cyan","#BF8EFF")))
  colorEg = as.character(cut(as.numeric(ruleW.df$conf), breaks=c(0, 0.7, 0.8, 0.9, 1), labels=c("darkgoldenrod1","darkorange1","green","blue")))
  xV=NULL;  yV=NULL;  for (v in 1:length(V(gW)$name)) { indV=match(V(gW)$name[v],Vg);  xV[v]=xV0[indV];  yV[v]=yV0[indV] }
  par(family="STKaiti")
  plot(gW, layout=cbind(xV,yV), vertex.label.family="STKaiti", main=maintitle,
       vertex.label = labelVg, vertex.size = sizeVg, vertex.color = colorVg,
       edge.arrow.size=0.1, edge.curved=0.3, edge.color = colorEg, 
       edge.label=format(as.numeric(ruleW.df$conf),digits=2), edge.label.color = colorEg
  )
  abline(v=-1.05);   abline(h=-1.05)
  xyTick(xV,yV,xTick,yTick)
}
gW = graph.edgelist(cbind(as.character(rules$lhs),as.character(rules$rhs)))
# plot(gW)
xTick = as.character(unique(DS$category0));   yTick = levels(DS$price0);    xV0 = NULL;   yV0 = NULL;  Vg = NULL
for (i in 1:length(xTick))
  for (j in 1:length(yTick)) { xV0 = c(xV0, i);   yV0 = c(yV0, j);   Vg = c(Vg, paste0("{",xTick[i],yTick[j],"}")) }
Vs = table(DS$c0p0)[order(table(DS$c0p0), decreasing=TRUE)];   Vs
Vsg = Vs;   names(Vsg) = paste0("{",names(Vsg),"}");   Vsg
plotPassoc(gW, rules, Vsg, 
           paste0("造訪頻次",unique(DS$CvFF0),"及消費金額",unique(DS$CvMM0),"客層之\n產品關連圖 (",dim(DS)[1],"筆交易)"), 
           xV0, yV0, Vg, xTick, yTick)


#######################################################################################################################
#####===== (6) 數據關連教學例 (XXXFM.csv) =====#####
##--- (6A) 數據擷取 (XXXFM-->XXXX) =====#####
XXXFM = read.csv("XXXFM.csv");   dim(XXXFM);   head(XXXFM,2)   #-- [1] 84008    18
#   X invoiceNo channel customer product category price            datetime quantity amount       date year
# 1 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:18:20        1   1692 2015-01-07 2015
# 2 2        N1      s1       c1     p11    kind1  1080 2015-01-07 20:14:41        1    550 2015-01-07 2015
#   month CvFF0       CvMM0 weekdays hour      price0
# 1     1 (0,1] (999,1e+04]   星期三   20 (999,3e+03]
# 2     1 (0,1] (999,1e+04]   星期三   20 (999,3e+03]
table(dataset0$CvFF0,dataset0$CvMM0)
#             (-5e+03,0] (0,999] (1e+04,1e+05] (1e+05,1e+06] (1e+06,2e+07] (999,1e+04]
# (0,1]               28    1174           299             0             0        7405
# (1,10]              26      97         13334             0             0       12838
# (10,40]              0       0         10883           273             0           0
# (200,2e+04]          0       0             0           579         30895           0
# (40,200]             0       0          1325          4852             0           0
XXXX = XXXFM[which((XXXFM$CvFF0=="(40,200]")&(XXXFM$CvMM0=="(1e+05,1e+06]")), c("CvFF0","CvMM0","customer","category","price0")]
##--- (6B) 數據轉換 (XXXX-->XXXX) =====#####
Tcategory = sort(table(XXXX$category),decreasing=TRUE)
Tclist = c(names(Tcategory)[1:9],"others");   Tclist    
  #--  [1] "kind1"  "kind2"  "kind17" "kind12" "kind3"  "kind16" "kind11" "kind27" "kind6"  "others"
XXXX$category0 = sapply(as.character(XXXX$category), FUN=function (x) ifelse(x %in% Tclist[1:9], x, "others"))
XXXX$c0p0 = paste0(XXXX$category0,XXXX$price0)
head( XXXX[which(XXXX$customer=="c120"),], 3)
#      customer category      price0 category0             c0p0
# 1877     c120    kind2 (999,3e+03]     kind2 kind2(999,3e+03]
# 1878     c120    kind2 (999,3e+03]     kind2 kind2(999,3e+03]
# 1879     c120    kind2 (999,3e+03]     kind2 kind2(999,3e+03]
##--- (6B) 數據轉換 (XXXX-->CpK) =====#####
customer = unique(XXXX$customer);   length(customer)   #-- 29
CpK = lapply(1:length(customer), function(k)unique(XXXX$c0p0[which(XXXX$customer==customer[k])]) )
length(CpK);   head(CpK,3)   #-- [1] 29
# [[1]]
#   [1] "kind2(999,3e+03]"    "kind2(3e+03,1e+04]"  "kind3(999,3e+03]"    "kind17(999,3e+03]"   "others(999,3e+03]"   "kind16(999,3e+03]"  
#   [7] "kind16(0,999]"       "others(0,999]"       "kind41(0,999]"       "kind12(-1,0]"        "kind3(3e+03,1e+04]"  "kind2(0,999]"       
#   [13] "kind18(3e+03,1e+04]"
# [[2]]
#   [1] "kind3(999,3e+03]"    "kind1(0,999]"        "kind2(999,3e+03]"    "others(0,999]"       "kind24(0,999]"       "kind1(3e+03,1e+04]" 
#   [7] "kind12(0,999]"       "kind1(999,3e+03]"    "kind2(0,999]"        "others(999,3e+03]"   "others(3e+03,1e+04]" "kind3(0,999]"       
#   [13] "kind41(0,999]"       "kind16(3e+03,1e+04]" "kind3(3e+03,1e+04]"  "kind16(999,3e+03]"   "kind2(3e+03,1e+04]"  "kind16(0,999]"      
#   [19] "kind17(999,3e+03]"  
# [[3]]
#   [1] "kind1(999,3e+03]"    "kind1(3e+03,1e+04]"  "others(999,3e+03]"   "kind3(0,999]"        "kind3(999,3e+03]"    "kind1(0,999]"       
#   [7] "kind12(-1,0]"        "others(0,999]"       "kind2(999,3e+03]"    "kind17(999,3e+03]"   "kind2(0,999]"        "kind16(3e+03,1e+04]"
#   [13] "kind24(0,999]"       "kind16(999,3e+03]"  
##--- (6C) 數據模型:數據關連 (CpK-->rules0.df) =====#####
library(arules)
rules0 = apriori( CpK, parameter=list(support=0.6, confidence=0.5), control=list(verbose=FALSE));  length(rules0)  #-- [1] 803
rules.df0 = data.frame( lhs=labels(lhs(rules0)), rhs=labels(rhs(rules0)), rules0@quality );  rules.df0[c(1:6,13:15,104:106),]
#                                        lhs                  rhs   support confidence     lift count
# 1                                       {} {kind2(3e+03,1e+04]} 0.6896552  0.6896552 1.000000    20
# 2                                       {}  {kind17(999,3e+03]} 0.6896552  0.6896552 1.000000    20
# 3                                       {}   {kind1(999,3e+03]} 0.7586207  0.7586207 1.000000    22
# 4                                       {}       {kind1(0,999]} 0.7586207  0.7586207 1.000000    22
# 5                                       {}       {kind3(0,999]} 0.7241379  0.7241379 1.000000    21
# 6                                       {}       {kind2(0,999]} 0.8275862  0.8275862 1.000000    24
# 13                    {kind2(3e+03,1e+04]}       {kind2(0,999]} 0.6896552  1.0000000 1.208333    20
# 14                          {kind2(0,999]} {kind2(3e+03,1e+04]} 0.6896552  0.8333333 1.208333    20
# 15                    {kind2(3e+03,1e+04]}  {others(999,3e+03]} 0.6551724  0.9500000 1.020370    19
# 104                     {kind2(999,3e+03]}      {others(0,999]} 0.9655172  0.9655172 1.000000    28
# 105      {kind2(0,999],kind2(3e+03,1e+04]}  {others(999,3e+03]} 0.6551724  0.9500000 1.020370    19
# 106 {kind2(3e+03,1e+04],others(999,3e+03]}       {kind2(0,999]} 0.6551724  1.0000000 1.208333    19
##--- (6D) 數據模型:提取數據關連規則中單項規則 (rules.df0-->rules) =====#####
indX = union( union( grep("],",rules.df0$lhs), which(nchar(as.character(rules.df0$lhs))==2)), 
              union( which(rules.df0$lift<1.1), which(rules.df0$count==1)  ) )
rules = cbind(rules.df0[,c("lhs","rhs")],round(rules.df0[,c("support","confidence","lift")],2),
              rules.df0[,"count"])[-indX,]
colnames(rules) = c("lhs","rhs","supp","conf","lift","count");   rules
#                     lhs                  rhs supp conf lift count
# 13 {kind2(3e+03,1e+04]}       {kind2(0,999]} 0.69 1.00 1.21    20
# 14       {kind2(0,999]} {kind2(3e+03,1e+04]} 0.69 0.83 1.21    20
# 29   {kind1(999,3e+03]}       {kind1(0,999]} 0.76 1.00 1.32    22
# 30       {kind1(0,999]}   {kind1(999,3e+03]} 0.76 1.00 1.32    22
# 31   {kind1(999,3e+03]} {kind1(3e+03,1e+04]} 0.76 1.00 1.21    22
# 32 {kind1(3e+03,1e+04]}   {kind1(999,3e+03]} 0.76 0.92 1.21    22
# 41       {kind1(0,999]} {kind1(3e+03,1e+04]} 0.76 1.00 1.21    22
# 42 {kind1(3e+03,1e+04]}       {kind1(0,999]} 0.76 0.92 1.21    22
##--- (6E) 數據呈現:繪製規則關連圖 (rules-->gW) =====#####
library(igraph)
gW = graph.edgelist(cbind(as.character(rules$lhs),as.character(rules$rhs)))
plot(gW)
##--- (6F) 數據呈現:繪製直角座標之規則關連圖 (gW) =====#####
xyTick <- function(xV,yV,xTick,yTick) {
  uxV = sort(unique(xV));   minX=min(xV);   maxX=max(xV);   tickX=2/(maxX-minX);   
  for (k in uxV) text(-1+(k-min(uxV))*tickX, -1.2, xTick[k])
  uyV = sort(unique(yV));   minY=min(yV);   maxY=max(yV);   tickY=2/(maxY-minY);   
  for (k in uyV) text(-1.5, -1+(k-min(uyV))*tickY, yTick[k])
}
plotPassoc <- function(gW,ruleW.df,Vsg,maintitle,xV0,yV0,Vg,xTick,yTick) {
  labelVg = paste0(V(gW)$name,"\n",Vsg[V(gW)$name])
  sizeVg  = 2+5*as.integer(cut(log10(Vsg[V(gW)$name])+0.1, breaks=c(0, 1, 2, 3, 4, 10) ) )
  colorVg = as.character(cut(log10(Vsg[V(gW)$name])+0.1, breaks=c(0, 1, 2, 3, 4, 10), labels=c("yellow","orange","green","cyan","#BF8EFF")))
  colorEg = as.character(cut(as.numeric(ruleW.df$conf), breaks=c(0, 0.7, 0.8, 0.9, 1), labels=c("darkgoldenrod1","darkorange1","green","blue")))
  xV=NULL;  yV=NULL;  for (v in 1:length(V(gW)$name)) { indV=match(V(gW)$name[v],Vg);  xV[v]=xV0[indV];  yV[v]=yV0[indV] }
  par(family="STKaiti")
  plot(gW, layout=cbind(xV,yV), vertex.label.family="STKaiti", main=maintitle,
       vertex.label = labelVg, vertex.size = sizeVg, vertex.color = colorVg,
       edge.arrow.size=0.1, edge.curved=0.3, edge.color = colorEg, 
       edge.label=format(as.numeric(ruleW.df$conf),digits=2), edge.label.color = colorEg
  )
  abline(v=-1.05);   abline(h=-1.05)
  xyTick(xV,yV,xTick,yTick)
}
xTick = as.character(unique(XXXX$category0));   yTick = levels(XXXX$price0);    xV0 = NULL;   yV0 = NULL;  Vg = NULL
for (i in 1:length(xTick))
  for (j in 1:length(yTick)) { xV0 = c(xV0, i);   yV0 = c(yV0, j);   Vg = c(Vg, paste0("{",xTick[i],yTick[j],"}")) }
Vs = table(XXXX$c0p0)[order(table(XXXX$c0p0), decreasing=TRUE)];   Vs
Vsg = Vs;   names(Vsg) = paste0("{",names(Vsg),"}");   Vsg
plotPassoc(gW, rules, Vsg, 
           paste0("造訪頻次",unique(XXXX$CvFF0),"及消費金額",unique(XXXX$CvMM0),"客層之\n產品關連圖 (",dim(XXXX)[1],"筆交易)"), 
           xV0, yV0, Vg, xTick, yTick)


