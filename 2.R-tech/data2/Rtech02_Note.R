# Rtech02.R: 大數據實務技術 - 02: 文本數據之詞語分析 (4hr)
# Jia-Sheng Heh (賀嘉生), 11/26/2020, revised from HUT03.R

#setwd("/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data2/")
#install.packages( c("data.table","jiebaR","stringr","text2vec","wordcloud") )


########## (1) 詞性標註的概念  ##########

wordset = c("鄭州市","蓮花街","中原區","思明區","李大爺","張三豐","李四")  #設定一個字串
nKind = NULL

nKind[grep("*市$",wordset)] = "nCity"    #取所有 市$ 標籤 nCity
nKind[grep("*區$",wordset)] = "nArea"    #取所有 區$ 標籤 nArea 
nKind[grep("^李+",wordset)] = "nPerson"  #取所有 李+ 開頭，標籤 nPerson
nKind[grep("[李,張]+",wordset)] = "nPerson"
nKind

#"鄭州市", "蓮花街", "中原區", "思明區", "李大爺", "張三豐", "李四"
# nCity"    NA       "nArea"   "nArea"   "nPerson" "nPerson" "nPerson


#data clear

########## (2) 中文文本分析工具 ##########

##== (1) (KDD1) 讀取數據 (RRlist/RR....csv-->RR) 
library(data.table)
Rlist = c("RR1_500r6810")                                   #Rlist = c("RR1_500r6810","RR501_1000r6217","RR1001_1500r7707","RR1501_2000r8262")
for (k in 1:length(Rlist)) {
  print(paste0(">> reading file - ",Rlist[k],".csv..."))
  RRk = fread(paste0(Rlist[k],".csv"), encoding="UTF-8" )
  if (k==1) { RR = RRk }   else { RR = rbind(RR,RRk) }
}
dim(RR);         #-- 6810   12
head(RR,2)       #-- 28996 / 464636 


##== (2) 中文文本分析工具jieba 
#install.packages("jiebaR")
library("jiebaR") 
wkr1 = worker()             #-- 最基本的 jieba 分詞引擎
RR$title[1]                 #-- title 標題 第1筆資料
wkr1 = RR$title[1] ; wkr1   #-- jiebaR 調用，worker引擎直接分詞：


##== (3) (KDD3) 實務數據的斷詞 (RR$title/RR$text-->content/Ncontent)
RRtitle = unique(RR$title)     ##== 列出全部 $title
RRtitle
length(RRtitle)                ##== $title 有 500個
content = NULL
Ncontent = NULL       
for (title in RRtitle) {                                   
  content = c(content, paste(title, paste(RR$text[which(RR$title==title)],collapse="\n\n"),collapse="\n\n"))
  Ncontent = c(Ncontent, length(which(RR$title==title)))  
}
head(content,2)     #== 把 RRtitle(content) 資料列出2筆， 輸出：\r\n 換行，\n\n\n\n 下一筆資料
#Ncontent[1:10]     #== Ncontent: 每個討論主題的回文數


##== (4) 在工作引擎中加上"tag"參數，使分詞時同時產生詞性，以 sapply()，對向量content的各個元素
wkr2 = worker(type="tag"); wkr2
Dword0 = sapply(content, function(x) segment(tolower(as.character(x)), wkr2));   #-- 要先轉成文字型式(as.character)，並將其中的所有英文字轉為小寫(tolower)
length(Dword0);        #-- 500
head(Dword0[[1]],5)    #-- $title 標題資料第一筆，sapply 自動切分出各個元素


##== (5) 過濾掉 單字詞 與 數字帶頭的詞語 (Dword0-->Dword)

Dword = lapply(Dword0, function(x){ 
  nx=sapply(x,nchar); xA=x[nx>=2];       #取出字>=2的詞語
  return( xA[-grep("[[:digit:]]+",xA)] ) #過濾數字帶頭的詞語 [[:digit:]]+ 
} )   

length(Dword);   
head(Dword[[1]],3)  #-- 上方 lappy 僅保留字數 >=2 的值做回傳
#  r(代詞)    n（名詞）      r（代詞）   
#  "自己"     "腰痠背痛"     "自己"     
