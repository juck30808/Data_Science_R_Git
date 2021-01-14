# cyBDA04.R: Customer Clusters
# Jia-Sheng Heh, 05/12/2020,  revised from RDS505A.R

setwd("c:/Users/Mike/Desktop/working/CYbda教發/")   ##--所有R程式的第一行，用來設定工作目錄／數據目錄等
library(data.table)

#####===== (1).數據擷取 (KDD1, read.csv) =====#####
X = read.csv("XXX2.csv");   dim(X);   head(X,2)   #-- 84008   11 
#   invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2        N2      s1       c2      p2    kind1  1400 2015-01-18 19:56:06        1   1197      sub2 793.36

#####===== (2).基本數據探索 (KDD2, range) =====#####
range(X$amount)       #-- [1] -22320  37100     #-- range() 可以算出數據變量的範圍(最小值與最大值)
range(X$price)    #-- [1] 0 8280

#####===== (3).基本數據轉換 (KDD3, cut) =====#####
X$date = as.Date(X$datetime,format="%Y-%m-%d")  #-- as.--() 是萬用數據轉換函式，此處是將字串型態 轉為 日期型態
range(X$date)         #-- [1] "2015-01-01" "2017-12-31"
X$price0 = cut( X$price, breaks = c(-0.001,0,99,999,9999)) 
head(X,2)
#   invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2        N2      s1       c2      p2    kind1  1400 2015-01-18 19:56:06        1   1197      sub2 793.36
#         date      price0
# 1 2015-01-07 (999,1e+04]
# 2 2015-01-18 (999,1e+04]

#####===== (4).SPC模型的數據轉換(X->Cv) =====#####
setDT(X, key=c("customer","date"))
Cv = X[, .(D0=min(date), Df=max(date), DD=length(unique(date)),
           FF=length(unique(invoiceNo)), MM=sum(amount), TT=sum(quantity)), by=customer ] 
range(Cv$FF)   #--     1    11025
range(Cv$MM)   #-- -2400 18251044
Cv$FF0 = cut( Cv$FF, breaks=c(0,1,10,40,200,20000));  table(Cv$FF0)  
# (0,1]      (1,10]     (10,40]    (40,200] (200,2e+04] 
#  4169        3281         275          44           5 
Cv$MM0 = cut( Cv$MM, breaks=c(-5000,0,999,9999,99999,999999,19999999));   table(Cv$MM0)
# (-5e+03,0]       (0,999]   (999,1e+04] (1e+04,1e+05] (1e+05,1e+06] (1e+06,2e+07] 
#         20           937          5587          1193            33             4 
Cv$BB = (Cv$Df-Cv$D0)/Cv$DD
Cv$BB0 = cut( as.numeric(Cv$BB), breaks=c(-1,0,7,30,100,999));  table(Cv$BB0)
# (-1,0]     (0,7]    (7,30]  (30,100] (100,999] 
#   4544       482       712      1146       890 
Cv$Q0 = paste0(substr(Cv$D0,3,4), cut(as.integer(substr(Cv$D0,6,7)),breaks=c(0,3,6,9,12),labels=c("Q1","Q2","Q3","Q4")))
Cv$Qf = paste0(substr(Cv$Df,3,4), cut(as.integer(substr(Cv$Df,6,7)),breaks=c(0,3,6,9,12),labels=c("Q1","Q2","Q3","Q4")))
dim(Cv);   head(Cv,2)       #-- [1] 7774   12
#    customer         D0         Df DD FF   MM TT   FF0         MM0    BB0   Q0   Qf     BB
# 1:       c1 2015-01-07 2015-01-07  1  1 3335  3 (0,1] (999,1e+04] (-1,0] 15Q1 15Q1 0 days
# 2:      c10 2015-01-28 2015-01-28  1  1 2770  1 (0,1] (999,1e+04] (-1,0] 15Q1 15Q1 0 days

#####===== (5).幾種有用的模型 =====#####
table(Cv$FF0,Cv$MM0)  #-- 客戶價值模型
table(Cv$Q0,Cv$Qf)    #-- 客戶進出模型
table(Cv$Qf,Cv$BB0)   #-- 客戶回購/流失模型

#####===== (6).客戶商品聚類的函式庫 =====#####
reflectC <- function(XXX,C,terms) {  
  XXX$indC = match(XXX$customer,C$customer);  
  if (!missing(terms)) {  for (k in 1:length(terms)) { XXX[,terms[k]] = C[XXX$indC,eval(parse(text=terms[k]))] }   }
  return(XXX) } #-- C標籤投射回X
optCluster <- function (BBSS.fit) {
  NNupper = length(BBSS.fit$height)
  maxTT0 = NNupper;   inc0 = 0;   gain = NULL;
  for (kk in 1:(1.5*sqrt(NNupper))) {  
    BBSS.group=cutree(BBSS.fit, k=kk);  TT=table(BBSS.group);
    ## print( c(kk, length(TT), max(TT), maxTT0-max(TT), inc0, as.integer(maxTT0-max(TT)-inc0*kk/10)));
    gain = c(gain, maxTT0-max(TT)-inc0*kk/10)
    if (maxTT0==max(TT)) { inc0 = inc0+1 } else { inc0 = 0 }
    maxTT0=max(TT);
  }
  maxTT0
  posKK = which(gain>0);  kOpt = max(posKK)
} #-- 計算最佳聚類數
buildCp4FM <- function(XXX,Cv) {
  Cp = XXX[, .(PP=paste(unique(category),collapse="/")), by=c("customer")]; 
  Cp$FF0[match(Cv$customer,Cp$customer)] = as.character(Cv$FF0);     Cp$MM0[match(Cv$customer,Cp$customer)] = as.character(Cv$MM0)
  Cp$FM0 = paste0(Cp$FF0,Cp$MM0);          Cp$group = 0;             dim(Cp);  #-- [1] 19254  3
  levelsFM0 = unique(XXX$FM0);   levelsFM0
  library(fastcluster)
  for (k in 1:length(unique(XXX$FM0))) {
    INDk = which(XXX$FM0==levelsFM0[k]);  # length(INDk)        #-- (1) 提取 INDk 客群 (XXX->XXXX->CpFM)
    print(paste0(">> analyze for customer community - ",k," with ",length(INDk)," transactions..."))
    XXXX = XXX[INDk,];   #-- 41445
    CpFM = XXXX[, .(PP=paste(unique(category),collapse="/")), by=c("customer")];   dim(CpFM);  #-- [1] 13645  2
    TbCpFM = table(CpFM$PP)                                     #-- (2) 表格化 產品購買組合 (以降低組合數) (CpFM->TbCpFM)
    vecKind = unique(XXXX$category);   vecKind
    Ack = matrix(0, nrow=length(TbCpFM), ncol=length(vecKind)); #-- (3) 產品購買組合 矩陣化 (以進行聚類) (names(TbCpFM)->Ack)
    colnames(Ack) = vecKind;   colnames(Ack)[which(is.na(colnames(Ack)))] = "NA"
    label = NULL
    if (dim(Ack)[1]==1) {
      TbCbFM.group = 1 
      chVec = Ack
      chLabel = cut(chVec,breaks=c(-1,2,8,10),labels=c("幾無","某些","幾全"));   chLabels = c("幾無","某些","幾全")
      labelStr = NULL;   for (j in 1:length(chLabels)) labelStr[j] = paste0(chLabels[j],paste(names(chVec)[which(chLabel==chLabels[j])],collapse="/"))
      label[1] = ifelse( nchar(labelStr[3])==2, labelStr[1], labelStr[3] )
    } else {
      for (i in 1:length(TbCpFM)) {  Ack[i, strsplit( names(TbCpFM)[i], split="/")[[1]] ] = 1  }  
      Ack.fit = hclust.vector( dist(Ack), method="median" )       #-- (4) 產品購買組合 聚類 (Ack->TbCpFM.group)
      kk = optCluster(Ack.fit);    if(kk<0) kk=1  
      TbCpFM.group = cutree(Ack.fit, k=kk);   # table(TbCpFM.group) #-- (5) 表格類別號(TbCpFM.group)與表格類別特徵(TbCpFM.groupname)
      label = NULL
      for (i in 1:kk) {
        INDi = which(TbCpFM.group==i)
        if (length(INDi)==1) { chVec = 10*Ack[which(TbCpFM.group==i),]
        } else { chVec = 10*colMeans(Ack[which(TbCpFM.group==i),]) }  
        chLabel = cut(chVec,breaks=c(-1, 1.5, 3, 10),labels=c("幾無","某些","幾全"));   chLabels = c("幾無","某些","幾全")
        labelVec = NULL;   for (j in 1:length(chLabel)) labelVec[j] = ifelse(chVec[j]<=1.5, NA, paste0(as.character(chLabel[j]),names(chVec)[j]))
        labelStr = paste(labelVec[which(!is.na(labelVec))],collapse="/")
        label[i] = ifelse( labelStr=="", "not obvious", labelStr )
      }
    }
    TbCpFM.groupname = sapply(TbCpFM.group, FUN=function(x) label[x])
    CpFM$table = match(CpFM$PP,names(TbCpFM))                   #-- (5) 傳回客群(CpFM) 予 表格類別號(TbCpFM.group)與表格類別特徵(TbCpFM.groupname)
    CpFM$group = TbCpFM.group[match(CpFM$PP,names(TbCpFM))]
    CpFM$groupname = TbCpFM.groupname[match(CpFM$PP,names(TbCpFM))]
    Cp$group[match(CpFM$customer, Cp$customer)] = CpFM$group    #-- (6) 傳回 Cp 予 客群(CpFM)表格類別號($group)與表格類別特徵($groupname)
    Cp$groupname[match(CpFM$customer, Cp$customer)] = CpFM$groupname
  }
  return(Cp)  
}    #-- 就各客層(FF0,MM0)進行商品聚類

#####===== (7).數據模型:客戶消費基因分析 (KDD4, X,Cv->Cp) =====#####
X = reflectC(X,Cv,c("FF0","MM0"))
X$FM0 = paste0(X$FF0,X$MM0);   head(X,2)
#    invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1:        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2:        N1      s1       c1     p11    kind1  1080 2015-01-07 20:26:35        1    550      sub1   0.00
#          date price0 CvFF0       CvMM0 indC   FF0         MM0              FM0
# 1: 2015-01-07 千元品 (0,1] (999,1e+04]    1 (0,1] (999,1e+04] (0,1](999,1e+04]
# 2: 2015-01-07 千元品 (0,1] (999,1e+04]    1 (0,1] (999,1e+04] (0,1](999,1e+04]
Cp = buildCp4FM(X,Cv);   head(Cp,2)  #== 產品購買組合聚類(X-->Cp)
#    customer    PP   FF0         MM0              FM0 group                                 groupname
# 1:       c1 kind1 (0,1] (999,1e+04] (0,1](999,1e+04]     1 某些kind1/幾全kind2/某些kind11/某些kind16
# 2:      c10 kind1 (0,1] (999,1e+04] (0,1](999,1e+04]     1 某些kind1/幾全kind2/某些kind11/某些kind16

#####===== (8).數據轉換:客戶消費基因標籤 (KDD3, Cp->X->XXXcls.csv) =====#####
X = reflectC(X,Cp,c("group","groupname"));   dim(X);   head(X,2)   #-- [1] 84008    21
#    invoiceNo channel customer product category price            datetime quantity amount category2   cost
# 1:        N1      s1       c1      p1    kind1  1980 2015-01-07 20:07:11        1   1692      sub1 931.39
# 2:        N1      s1       c1     p11    kind1  1080 2015-01-07 20:26:35        1    550      sub1   0.00
#          date price0 CvFF0       CvMM0 indC   FF0         MM0              FM0 group                                 groupname
# 1: 2015-01-07 千元品 (0,1] (999,1e+04]    1 (0,1] (999,1e+04] (0,1](999,1e+04]     1 某些kind1/幾全kind2/某些kind11/某些kind16
# 2: 2015-01-07 千元品 (0,1] (999,1e+04]    1 (0,1] (999,1e+04] (0,1](999,1e+04]     1 某些kind1/幾全kind2/某些kind11/某些kind16
write.csv(X,"XXXcls.csv", fileEncoding="Big-5")

#####===== (*9).階層聚類教學例 (X) =====#####
table(X$FM0)   ##-- (9.1) 每個客戶購買商品的向量式編碼(vectorization)
# (0,1](-5e+03,0]             (0,1](0,999]       (0,1](1e+04,1e+05]         (0,1](999,1e+04]         (1,10](-5e+03,0] 
#              28                     1174                      299                     7405                       26 
# (1,10](0,999]      (1,10](1e+04,1e+05]        (1,10](999,1e+04]     (10,40](1e+04,1e+05]     (10,40](1e+05,1e+06] 
#            97                    13334                    12838                    10883                      273 
# (200,2e+04](1e+05,1e+06] (200,2e+04](1e+06,2e+07]    (40,200](1e+04,1e+05]    (40,200](1e+05,1e+06] 
#                      579                    30895                     1325                     4852 
A = table(as.character(X$customer[which(X$FM0=="(1,10](-5e+03,0]")]),as.character(X$category[which(X$FM0=="(1,10](-5e+03,0]")]));  A
#       kind1 kind11 kind12 kind2 kind50 kind56 kind6
# c3728     0      0      0     4      0      0     0
# c5644     2      0      2     0      2      0     2
# c5986     0      2      0     0      0      0     0
# c6511     0      0      0     8      0      0     0
# c6777     0      0      0     0      0      2     0
# c7270     0      0      0     2      0      0     0
##-- (9.2) 兩個客戶商品向量的距離(distance) ===###
A[1,]-A[2,]                  #-- c3728和c5644購買商品的差異 (error)
# kind1 kind11 kind12  kind2 kind50 kind56  kind6 
#    -2      0     -2      4     -2      0     -2 
(A[1,]-A[2,])^2              #-- 平方差異 (squared error)
# kind1 kind11 kind12  kind2 kind50 kind56  kind6 
#     4      0      4     16      4      0      4 
sum((A[1,]-A[2,])^2)         #-- [1] 32       平方差異和 (sum of squared error)
sqrt(sum((A[1,]-A[2,])^2))   #-- [1] 5.656854 平方差異和的根號 = 兩個點的距離
dist(A)  ##-- (9.3) 以最近鄰(k-th nearest neighbor,kNN)演算法進行聚類
#          c3728    c5644    c5986    c6511    c6777   c7270
# c3728        -
# c5644 5.656854        -                                    
# c5986 4.472136 4.472136        -                           
# c6511 4.000000 8.944272 8.246211        -                  
# c6777 4.472136 4.472136 2.828427 8.246211        -         
# c7270 2.000000 4.472136 2.828427 6.000000 2.828427       -
L = hclust(dist(A))
plot(L)   ##-- (9.4) 以蟹爪圖(dendrogram)設定階層式聚類(hierarchical clustering)數
Acluster = cutree(L, k=3);   Acluster
# c3728 c5644 c5986 c6511 c6777 c7270 
#     1     2     2     3     2     1   ##-- (9.5) 各聚類的特徵
A[which(Acluster==1),]                   #-- (9.5.1) 第1類的特徵: 只有一般(2-4個)kind2
#       kind1 kind11 kind12 kind2 kind50 kind56 kind6
# c3728     0      0      0     4      0      0     0
# c7270     0      0      0     2      0      0     0
A[which(Acluster==2),]                   #-- (9.5.2) 第2類的特徵: 沒有kind2
#       kind1 kind11 kind12 kind2 kind50 kind56 kind6
# c5644     2      0      2     0      2      0     2
# c5986     0      2      0     0      0      0     0
# c6777     0      0      0     0      0      2     0
A[which(Acluster==3),]                   #-- (9.5.3) 第3類的特徵: 有特大的(8個)kind2
# kind1 kind11 kind12  kind2 kind50 kind56  kind6 
#     0      0      0      8      0      0      0 
