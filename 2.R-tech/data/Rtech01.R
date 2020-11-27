# Rtech01.R: 大數據實務技術 - 01: 數據獲取與預處理
# Jia-Sheng Heh (賀嘉生), 11/20/2020, revised from HUT02.R

#setwd("c:/Users/jsheh/Desktop/working/USC/AIbda/")
install.packages( c("RCurl","XML","rvest","xml2","curl","datesets","MASS","readxl","jsonlite") )
library(RCurl)                
library(MASS) 
library(datasets)     
Insurance

########## (A) 數據來源 ##########

##== (1) 方法：讀取檔案
write.csv(Insurance, "insurance.csv")
csv1 = read.csv("insurance.csv");       
csv2 <- read.table("insurance.csv")

##== (2) 方法：txt檔案格式兩種讀取方法的比較 read.table(), read.csv()
write.table(Insurance, "insurance.txt")
txt1 = read.csv("insurance.txt");       txt2 = read.table("insurance.txt")
write.table(Insurance, "insurance.comma",sep=",")
comma1 = read.csv("insurance.comma");   
comma2 = read.table("insurance.comma")
comma2A = read.table("insurance.comma",sep=",")
write.table(Insurance, "insurance.slash",sep="/")
slash1 = read.csv("insurance.slash");   slash2 <- read.table("insurance.slash")
slash2A = read.table("insurance.slash",sep="/")

##== (3) 方法：readLines()
ftpfile1 = readLines("ftpfile1.txt")    #-- [Munzert, Ch.13: 加州天氣數據]

##== (4) 方法：讀取網絡檔案 (scan())
kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)   #-- 從威廉一世開始的英國國王的去世年份數据
births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")   #-- 1946/01-1959/12 的紐約市每月出生人口數量
souvenir = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")   #-- 1987/1-1993/12 昆士蘭海濱度假圣地的某紀念品商店販售數据

##== (5) 方法：通用的.xlsx檔
#install.packages("readxl") 
library(readxl)
xls1 = read_excel("insurance.xls")
csv123 = read.csv("???商123.csv",fileEncoding="Big5");       
xls123 = read_excel("???商123.xls")   #-- 繁體windows系統不能用簡體字

##== (6) 方法：用於數據交換.json格式
library(jsonlite)
airbox = fromJSON("airbox.json")      #-- 台北市空氣盒子開放數據
airbox.df = as.data.frame(airbox[[2]])

##== (7) 方法：網頁適用的.xml格式 
library(XML)
x <- xmlParse("county_h_10508.xml")   #-- 台灣鄉鎮郵區及英文名稱表  
xRoot = xmlRoot(x)
x.df = xmlToDataFrame(xRoot)


##== (8) 方法：物聯網位置
library(jsonlite)
download.file("https://tpairbox.blob.core.windows.net/blobfs/AirBoxData.gz","airbox.gz")
airbox = fromJSON(gzfile("airbox.gz"))              #-- 台北市空氣盒子開放數據
airbox.df = as.data.frame(airbox$entries);  
dim(airbox.df);     # row / col
head(airbox.df,2)   

##== (9) 方法：物聯網 UBIKE/GPS
download.file("http://data.taipei/youbike","ubike.gz")
ubike = fromJSON(gzfile("ubike.gz"))                #-- 台北市UBIKE開放數據 <-- 因其中有中文,在windows系統中無法解譯,在Mac可以
library(plyr)
ubike.df = ldply(ubike[[2]], function(x) rbind(data.frame(x)))  

##== (10) 方法：雲端網絡爬虫
library(RCurl)
download.file("http://www.wcc.nrcs.usda.gov/ftpref/data/climate/table/temperature/history/california/19l03s_tavg.txt","ftpfile1.txt") 
ftplist = "http://www.wcc.nrcs.usda.gov/ftpref/data/climate/table/temperature/history/california/"
filelist = getURL(ftplist, dirlistonly = TRUE) #-- 加州天氣數據檔案列表

# 網頁中表格格式
url = "http://www.openintro.org/cont/donorProcess.php"
result = postForm(url, name="David Diez", email="david@openintro.org", phone="857-288-8547")

# 網頁+參數格式
url = "http://www.amazon.com/s/ref=nb_sb_noss_2?url=node%3D2407749011&field-keywords=Apple"
firstSearchPage = getURL(url)
parsedFirstSearchPage = htmlParse(firstSearchPage)  #-- 亞馬遜網站之行動電話數據-->現會要求權限,無法取得



########## (B) 網路爬蟲 ##########

##== (1) 簡單HTML網頁頁面爬取  =====#####
library(RCurl)  # 數據爬取軟件包 RCurl
response = getURL("http://www.weather.com.cn/weathern/101340101.shtml")
print(response)

##== (2) 分析網頁信息 ##########
response.parser = htmlParse(response, asText=TRUE)
print(response.parser)

##== (3) 提取網頁樹: xpathSApply [殷3.3] =====#####
xpathSApply(response.parser, "/html/body/div/div/div/div/div/ul/li[2]")
xpathSApply(response.parser, "/html/body/div/div/div/div/div/h2")
xpathSApply(response.parser, "//div[@class='col-xs-6 col-sm-12']/h2")  ##-- 以相對路徑和識別碼來找比較快




########## (C) 網路爬蟲2 ##########

##== (1) (KDD1数据取得) 網絡爬文 (URL->X) 
library(RCurl)
URL = "http://www.weather.com.cn/weathern/101340101.shtml"
X = getURL(URL, .encoding="GB")
head(X)


##== (2) (KDD2数据探索) 檢視網頁樹 (X->XX)
library(XML)
XX = xpathSApply(htmlParse(X), "//ul[@class='t clearfix']/li")  #-- 相對路徑 //... 無法傳值
length(XX);   
XX[[1]]   #-- [1] 7

##== (3) (KDD3数据转换) 转为天气数据框 (X->XXX)
Xday = NULL;   Xweather = NULL;   Xtemp = NULL;   Xwind = NULL
for (k in 1:length(XX)) {
  Xday[k] = xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/h1")[[k]] )
  Xweather[k] = xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='wea']")[[k]] )
  Xtemp[k] = xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='tem']")[[k]] )
  Xwind[k] = xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']")[[k]] )
}
XXX = data.frame(day=Xday, weather=Xweather, temp=Xtemp, wind=Xwind);  dim(XXX);  head(XXX,3)  #-- [1] 7 4
write.xlsx(XXX,"/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data/XXX.xlsx")  #-- 解析後將標籤(tag)存檔


##== (4) (KDD4数据模型) 这一周天气的简要显示 (table(XXX$..))
table(XXX$weather)
table(XXX$wind)   #-- 這數據不太乖，有沒有改進的方法？
table(XXX$weather, XXX$wind)

##== (5) (KDD5数据呈现) 天气数据图 (plot(XXX$..)) =====#####
par(family="STKaiti");   
barplot(table(XXX$weather), main="本周天气分布图")
pie(table(XXX$weather), main="本周天气比例图")
levels(XXX$weather) = c("晴","多云","小雨转晴")
plot(XXX$day,as.integer(XXX$weather), main="本周天气时序图")

##== (6) 几个进阶的处理 =====#####
k = 6
xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']")[[k]] )
library(stringr)
str_replace_all( xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']")[[k]] ), "\n","")
A = str_extract_all( xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='tem']")[[k]] ), "[:digit:]*")[[1]]
A[which(nchar(A)>0)]
# [1] "21" "11"


##== (7) 提取風向 ==###
A = xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']" )[[k]];   A
xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']/em/span" )
A = xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li[position()=2]/p[@class='win']/em/span" );  A
xmlGetAttr( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li[position()=2]/p[@class='win']/em/span")[[1]], "title" )
xmlGetAttr( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li[position()=2]/p[@class='win']/em/span")[[2]], "title" )
# [1] "西北风"




########## (D) 多重網頁爬文實例 (mobile01 - 健康與養生 討論區) ##########

##== (1) 環境設定 =====#####

#mobile01 - 健康與養生網址: addrE = "https://www.mobile01.com/topiclist.php?f=330"
wkDir = "/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data";    setwd(wkDir)     
dataDir = "/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data";    
library(RCurl);   library(XML);          library(stringr);    library(readxl); 
library(rvest);   library(data.table);   library(xml2);    library(curl)
library(xlsx);   #-- there is warning for this packages  (need to install JDK first)

##== (2) 爬取首頁與解析 (addrE->H01-->pageNo) =====#####

addrE = "https://www.mobile01.com/topiclist.php?f=330"     
H1 <- read_html(curl(addrE, handle = new_handle("useragent" = "Mozilla/5.0")))  #== (1) 爬取首頁
write(as.character(H1),paste0(dataDir,paste0("H",as.character(k),".html")))     #== (2) 儲存首頁(爬文的習慣:爬到後立刻儲存)
HH1 = htmlParse(H1)                                                             #== (3) 解析首頁
xpathSApply(HH1,"//div[@class='pagination']//a[last()]")                        #== (4) 提取頁數位置與頁數(pageStr2)
pageStr1 = xmlValue( xpathSApply(HH1,"//div[@class='contentfoot']//p[@class='numbers']")[[1]])
pageStr2 = str_extract(pageStr,"共[0-9]*頁")
pageNo = as.integer(str_sub(pageStr2,2,nchar(pageStr2)-1))                      #== (5) 提取頁數(pageNo)

##== (3) 爬取第k頁與分析 (pageNo->Hk.html) =====#####
addrE0 = "https://www.mobile01.com/topiclist.php?f=330&p=1"
for (k in 1:pageNo) {
  urlH = str_replace(addrE0,"=1",paste0("=",as.character(k)))                   
  print(paste0(">> Crawling for page - ",k," with urlH = ",urlH," ..."))        #== (1) 爬文時,要顯示爬文進程(progress)
  Hk <- read_html(curl(urlH, handle = new_handle("useragent" = "Mozilla/5.0"))) #== (2) 找出第k主題頁網址(urlH)並爬文之
  write(as.character(Hk),paste0(dataDir,paste0("H",as.character(k),".html")))   #== (3) 儲存第k頁 (Hk.html)
  Sys.sleep(1)                                                                  #== (4) 爬文不能太快,否則可能被擋
}

##== (4) 主題頁第k頁解析 (Hk-->TTbody-->TT.xlsx) =====#####
for (k in 1:pageNo) {  
  print(paste0(">>> crawling page - ",as.character(k)," ---"))                               #== (1) 顯示解析進程(progress)
  Hk = htmlParse( getURL(paste0("file://",dataDir,paste0("H",as.character(k),".html")), .encoding="UTF8" ) )  #== (2) 讀取儲存之爬文檔
  HkBlk1 = xpathSApply(Hk,"//div[@class='tablelist forumlist']/table//tbody/tr/td/span/a")   #== (3) 逐段解析
  href = NULL;    response = NULL;    title = NULL
  for (i in 1:length(HkBlk1)) {
    href = c(href, xmlGetAttr(HkBlk1[[i]],"href"));    response = c(response, xmlGetAttr(HkBlk1[[i]],"title"));   title = c(title, xmlValue(HkBlk1[[i]]))
  }
  HkBlk2 = xpathSApply(Hk,"//div[@class='tablelist forumlist']/table//tbody/tr/td[@class='reply']")
  replyNo = sapply(HkBlk2, xmlValue)
  HkBlk31 = xpathSApply(Hk,"//div[@class='tablelist forumlist']/table//tbody/tr/td[@class='authur']//p[position()=1]")
  HkBlk32 = xpathSApply(Hk,"//div[@class='tablelist forumlist']/table//tbody/tr/td[@class='authur']//p[position()=2]")
  date = sapply(HkBlk31, xmlValue);   author = sapply(HkBlk32, xmlValue)
  HkBlk41 = xpathSApply(Hk,"//div[@class='tablelist forumlist']/table//tbody/tr/td[@class='latestreply']//p[position()=1]")
  HkBlk42 = xpathSApply(Hk,"//div[@class='tablelist forumlist']/table//tbody/tr/td[@class='latestreply']//p[position()=2]")
  latestreplydate = sapply(HkBlk41, xmlValue);     latestreplier = sapply(HkBlk42, xmlValue)
  TTT = data.frame( page=k, postNo=1:length(HkBlk1), title=title, href=href, response=response, 
                    replyNo=replyNo, date=date, author=author, latestreplydate=latestreplydate, latestreplier=latestreplier )
  if (k==1) { TTbody = TTT } else { AAA1 = rbind(TTbody,TTT);   TTbody = AAA1 }               #== (4) 將爬取之主文數據存入數據框(TTT)
}
dim(TTbody);  head(TTbody,3)                                                                  
# write.xlsx(TTbody,"mobile01/TT.xlsx")                #== (5) 由於 HTML數據中,有許多特殊碼,建議存成 xlsx檔, 才不會欄位亂掉

##== (5) 探索主題檔(TT.xlsx-->TT) =====#####
TT = as.data.frame(read_excel("mobile01/TT.xlsx"));     dim(TT)   #-- 19505  11   #== (1) 主題數
sum(as.integer(TT$replyNo),na.rm=T)   #-- 207901 / 19505 = 10.66                  #== (2) 回文數總和  
sort(unique(substr(TT$date,1,7)))     #-- "2005-01"..."2017-09"                   #== (3) 每月回文數
range( table(TT$author))  #-- 1, 260                                              #== (4) 簡易author數據探索


table( cut(table(TT$author), breaks=c(0,1,3,9,20,40,80,300)))
# (0,1]    (1,3]    (3,9]   (9,20]  (20,40]  (40,80] (80,300] 
# 8634     2048      569       88       36        9        3 
sum( table( cut(table(TT$author), breaks=c(0,1,3,9,20,40,80,300))) )  #-- 11387

table( cut(as.integer(TT$replyNo), breaks=c(-1,0,1,5,12,25,50,100,200,400,800)) ) #== (5) 簡易回文數(replyNo)數據探索
# (0,1]    (1,3]    (3,9]   (9,20]  (20,40]  (40,80] (80,300] 
#  8634     2048      569       88       36        9        3
range(as.integer(TT$replyNo),na.rm=T)   #-- 0.. 785
# (-1,0]     (0,1]     (1,5]    (5,12]   (12,25]   (25,50]  (50,100] (100,200] (200,400] (400,800] 
#   2284      2069      6461      4579      2371      1133       422       138        31        14 
sum(as.integer(TT$replyNo), na.rm=T)    #-- 207901

for (kStart in 1901:1901)  {    #-- (kStart in seq(from=18501,to=dim(TT)[1],by=500))
  for (k in 19147:dim(TT)[1]) {    #-- dim(TT)[1] = 19505    #-- kStart:(kStart+499)
    ##== (F2) 爬取主題頁頁面 (TT->Ahref->Tk)
    Ahref = paste0("https://www.mobile01.com/",TT$href[k])
    Tk <- read_html(curl(Ahref, handle = new_handle("useragent" = "Mozilla/5.0")))
    ##== (F3) 解析得回文則數 (Tk->RpageNo) =====#####
    PNblock = xpathSApply(htmlParse(Tk), "//div[@class='pagination']//a[last()]")
    RpageNo = as.integer(xmlValue(PNblock[[length(PNblock)]]))
    if (length(PNblock)==1)  { RpageNo = 1 }  else if (is.na(RpageNo)) { RpageNo = length(PNblock)-1}  
    print(paste0(">>> processing title - ",as.character(k),"/",as.character(dim(TT)[1])," titles with ",as.character(RpageNo)," pages..."))
    ##== (F4) 爬取回文頁面 (Rpage: j->Rhref->RjBlk*) 
    if (RpageNo>0) {
      ktitle=k;   title = TT$title[k];   replyNo = TT$replyNo[k];  Tdate = TT$date[k];  Tauthor = TT$author[k]
      for (j in 1:RpageNo) {
        Rhref = paste0("https://www.mobile01.com/",TT$href[k],"&p=",as.character(j))
        print(paste0("    >> crawling reply page - ",as.character(k),": ",as.character(j),"/",as.character(RpageNo)," pages with Rhref = ",Rhref))
        Tk <- read_html(curl(Rhref, handle = new_handle("useragent" = "Mozilla/5.0")))
        RjBlk1 = xpathSApply(htmlParse(Tk),"//div[@class='fn']/a")
        RjBlk2 = xpathSApply(htmlParse(Tk),"//div[@class='date']")
        RjBlk3 = xpathSApply(htmlParse(Tk),"//ul[@class='author-detail']/li[last()]/span[last()]")
        RjBlk4 = xpathSApply(htmlParse(Tk),"//div[@class='single-post-content']/div")
        ##== (F5) 解析得回文數據框 (RjBlk->RRR/RRbody)
        author = NULL;   authorclass = NULL;    date = NULL;   rPostNo = NULL;   authorcredit = NULL;   text = NULL
        for (i in 1:length(RjBlk1)) { 
          author = c(author, xmlValue(xpathSApply(htmlParse(Tk),"//div[@class='fn']/a")[[i]]))
          authorclass = c(authorclass, xmlGetAttr(xpathSApply(htmlParse(Tk),"//div[@class='fn']/a")[[i]], "title"))
          date = c(date, substr(xmlValue( xpathSApply(htmlParse(Tk),"//div[@class='date']")[[i]]),1,16))
          rPostNo = c(rPostNo, substr(xmlValue( xpathSApply(htmlParse(Tk),"//div[@class='date']")[[i]]),19,23))
          authorcredit = c(authorcredit, xmlValue( xpathSApply(htmlParse(Tk),"//ul[@class='author-detail']/li[last()]/span[last()]")[[i]]) )
          text = c(text, xmlValue( xpathSApply(htmlParse(Tk),"//div[@class='single-post-content']/div")[[2*i-1]] ))
        }
        RRR = data.frame( ktitle=ktitle, title=title, replyNo=replyNo, Tdate=Tdate, Tauthor=Tauthor, 
                          author=author, authorclass=authorclass, date=date, rPostNo=rPostNo, authorcredit=authorcredit, text=text)
        if (j==1) { RRbody = RRR } else { BBB1 = rbind(RRbody,RRR);   RRbody = BBB1 }
      }
      
      #####===== (C1e) form Reply data frame (RRbody->RR) =====#####
      if (k==kStart) { RR = RRbody } else { BBB2 = rbind(RR,RRbody);   RR = BBB2 }
    }   ##== (F6) 迴圈得同一主題之所有回文 (RjBlk->RRR/RRbody)
    print(paste0("    ***** ",as.character(dim(RR)[1])," posts obtained in totals."))
    Sys.sleep(1)
  }     ##== (F7) 每500個主題儲存一個檔案 (RRR-->RR-->RRaaaa_bbbbrcccc.xlsx)
  fname = paste0(dataDir,"RR",as.character(kStart),"_",as.character(kStart+499),"r",as.character(dim(RR)[1]),".xlsx")
  write.xlsx(RR,fname)  #-- RR5501_6000v5143.xlsx
}
##  write.xlsx(RR,paste0(dataDir,"RR19000_19505r6022.xlsx"))  

#####===== (6G) 最後的回文檔案名稱 (Rlist) =====#####
Rlist = c("1_500r15696","501_1000r5643","1001_1500r4830","1501_2000r4928","2001_2500r6286",
          "2501_3000r5400","3001_3500r4763","3501_4000r5709","4001_4500r6017","4501_5000r5864",
          "5001_5500r5957","5501_6000r5143","6001_6500r4962","6501_7000r6249","7001_7500r4159",
          "7501_8000r3545","8001_8500r4260","8501_9000r5256","9001_9500r4498","9501_10000r5589",
          "10001_10500r4974","10501_11000r5887","11001_11500r6595","11501_12000r6333","12001_12500r7869",
          "12501_13000r5523","13001_13500r6580","13501_14000r4974","14001_14500r4781","14501_15000r6201",
          "15001_15500r4564","15501_16000r6421","16001_16500r7996","16501_17000r8609","17001_17500r6791",
          "17501_18000r5813","18001_18500r9314","18501_19000r5227","19000_19505r6022")   #-- 39 files


########## (R) 本單元的複習 ##########

#####===== (R-HW) 演練作業 =====#####
## 试着爬取你想要分析的網頁:
##== (A) 你是用什麼關鍵字，找到這個網頁的呢? (關鍵字?)
##== (B) 這個網頁的對映HTML，你可以如何觀察? (截取螢幕畫面說明) 
##== (C) 你需要的信息，在這網頁的什麼位置? (XPATH?)
##== (D) 你可以用什麼指令，提取這些信息? (xpathSApply())

#####===== (R-RV) 重點複習 =====#####
##== (A) 調用軟件包的指令是 "library()"。
##== (B) 搜尋引擎中強制搜尋時要加 "英文雙引號"。
##== (C) 在調用RCurl爬取特定網址時，要用 "getURL()" 指令來擷取網頁。
##== (D) XPATH路徑表達相對位置時，要以 "//" 開始。
##== (E) xpathSApply可用過濾器[]符號加上 "@"符號指定選配屬性，選擇特定屬性的元素。
##== (F) 在多重網頁爬取時，要以 "print()"指令顯示目前爬取的網頁。



########## (Z) 數據清洗實作 ##########

##== (Z0) 讀取數據 (ftpfile1.txt-->X) 
X = readLines("ftpfile1.txt");      X[1:10]  # [1] 1163
length(X)   #row

##== (Z1) 抓出表格首行 (X-->grep()-->indB) 
indB = grep("day    oct",X); indB   #-- 抓出字串的pattern , x
length(indB);  
# [1]    6   49   92  135  178  221  264  307  350  393  436  479  522  565  608  651

X[6:36]              #含有表頭(6) 到 6+30項 (36)
X[(6+2):(38)]        #無表頭(8) 到 8+30項 (38)
X[(49+2):(79)]       #next 無表頭(49) 到 49+30 (79)

##== (Z2) 抓出第k表格 (k-->indB[k]-->BB) 
k = 2;   indB[k]   #-- 49
BB = X[(indB[k]+2):(indB[k]+2+30)];   BB

##== (Z3) 切出數字1 (BB[1:2]-->strsplit()-->BBi) 
BB[1:2]
# [1] "  1     10     3    -0   -14    -9    -0    -1    -4    -3    13    16    13"
# [2] "  2      9     2     0   -10   -10    -2    -1    -5     6    14    16    14"
strsplit(BB[1:2],split=" ")
# [[1]]
# [1] ""    ""    "1"   ""    ""    ""    ""    "10"  ""    ""    ""    ""   
# [13] "3"   ""    ""    ""    "-0"  ""    ""    "-14" ""    ""    ""    "-9" 
# [25] ""    ""    ""    "-0"  ""    ""    ""    "-1"  ""    ""    ""    "-4" 
# [37] ""    ""    ""    "-3"  ""    ""    ""    "13"  ""    ""    ""    "16" 
# [49] ""    ""    ""    "13" 
# [[2]]
# [1] ""    ""    "2"   ""    ""    ""    ""    ""    "9"   ""    ""    ""   
# [13] ""    "2"   ""    ""    ""    ""    "0"   ""    ""    "-10" ""    ""   
# [25] "-10" ""    ""    ""    "-2"  ""    ""    ""    "-1"  ""    ""    ""   
# [37] "-5"  ""    ""    ""    ""    "6"   ""    ""    ""    "14"  ""    ""   
# [49] ""    "16"  ""    ""    ""    "14"
i = 1
BBi = strsplit(BB[i],split=" ")[[1]];   BBi
# [1] ""    ""    "1"   ""    ""    ""    ""    "10"  ""    ""    ""   
# [12] ""    "3"   ""    ""    ""    "-0"  ""    ""    "-14" ""    ""   
# [23] ""    "-9"  ""    ""    ""    "-0"  ""    ""    ""    "-1"  ""   
# [34] ""    ""    "-4"  ""    ""    ""    "-3"  ""    ""    ""    "13" 
# [45] ""    ""    ""    "16"  ""    ""    ""    "13"

##== (Z4) 去除長度為0者 (BBi-->BBii) 
nchar(BBi)
# [1] 0 0 1 0 0 0 0 2 0 0 0 0 1 0 0 0 2 0 0 3 0 0 0 2 0 0 0 2 0 0 0 2 0 0
# [35] 0 2 0 0 0 2 0 0 0 2 0 0 0 2 0 0 0 2
which(nchar(BBi)>0)   #-- [1]  3  8 13 17 20 24 28 32 36 40 44 48 52
BBii = BBi[which(nchar(BBi)>0)];  BBii
# [1] "1"   "10"  "3"   "-0"  "-14" "-9"  "-0"  "-1"  "-4"  "-3"  "13" 
# [12] "16"  "13" 

##== (Z5) 轉為數值 (BBii-->BBiii) 
BBiii = as.integer(BBii);   BBiii
# [1]   1  10   3   0 -14  -9   0  -1  -4  -3  13  16  13

##== (Z6) 整個表格 (BBiii-->TT) 
i = 1
BBi = strsplit(BB[i],split=" ")[[1]];   BBi
BBii = BBi[which(nchar(BBi)>0)];  BBii
BBiii = as.integer(BBii);   BBiii
#--
TT = NULL
for (i in 1:31) {
  BBi = strsplit(BB[i],split=" ")[[1]];   BBi
  BBii = BBi[which(nchar(BBi)>0)];  BBii
  BBiii = as.integer(BBii);   BBiii
  TT = rbind(TT,BBiii)
}
TT

##== (Z7) 26(k=2-27)個表格 (TT-->TTT) 
TTT = NULL
for (k in c(2:27)) {
  BB = X[(indB[k]+2):(indB[k]+2+30)];   BB
  TT = NULL
  for (i in 1:31) {
    BBi = strsplit(BB[i],split=" ")[[1]];   BBi
    BBii = BBi[which(nchar(BBi)>0)];  BBii
    BBiii = as.integer(BBii);   BBiii
    if (length(BBiii)!=13) next
    TT = rbind(TT,BBiii)
  }
  TTT = rbind(TTT,TT)
}
dim(TTT);   tail(TTT,4)   #-- [1] 761 / 743(沒有k=7) / 775(錯)  13
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
# BBiii   28    3    3   -8   -6   -2    3    8    5    15    14    11     3
# BBiii   29    4    3   -9   -8   NA    3    8    6    16    13    13     7
# BBiii   30    5    4   -6    2   NA    3    9    7    17    13    14     9
# BBiii   31    5   NA   -9    1   NA    4   NA    7    NA    13    14    NA

##== (Z8) 加上行列名 (TTT) 
rownames(TTT) = 1:dim(TTT)[1]
X[6]
# [1] "day    oct   nov   dec   jan   feb   mar   apr   may   jun   jul   aug   sep"
CC = strsplit(X[6],split=" ")[[1]];  CC
# [1] "day" ""    ""    ""    "oct" ""    ""    "nov" ""    ""    "dec" ""    ""   
# [14] "jan" ""    ""    "feb" ""    ""    "mar" ""    ""    "apr" ""    ""    "may"
# [27] ""    ""    "jun" ""    ""    "jul" ""    ""    "aug" ""    ""    "sep"
colnames(TTT) = CC[nchar(CC)>0]
head(TTT,2)
#   day oct nov dec jan feb mar apr may jun jul aug sep
# 1   1  10   3   0 -14  -9   0  -1  -4  -3  13  16  13
# 2   2   9   2   0 -10 -10  -2  -1  -5   6  14  16  14

##== (Z9) 輸出結果 (TTT-->TTT.csv) 
write.csv(TTT,"TTT.csv")
