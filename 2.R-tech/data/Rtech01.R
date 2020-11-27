# Rtech01.R: 大數據實務技術 - 01: 數據獲取與預處理
# Jia-Sheng Heh (賀嘉生), 11/20/2020, revised from HUT02.R

#setwd("c:/Users/jsheh/Desktop/working/USC/AIbda/")
install.packages( c("RCurl","XML","rvest","xml2","curl","datesets","MASS","readxl","jsonlite") )
library(RCurl)                
library(MASS)   #  library(datasets);     
Insurance

########## (1) 數據來源 ##########

#####===== (1A) 數據的種類 [Andrew Brust, 2012] =====#####
##-- 1.企業數據：較封閉的、較開放的 (括號的數字號為原分類,共十種)
#    -- (4) 雲端應用(Software as a Service (SaaS) and cloud applications)-—Systems like Salesforce.com, Netsuite, SuccessFactors, etc.
#    -- (7) 數據倉儲設備(Data warehouse appliances)-—Teradata, IBM Netezza, EMC Greenplum, etc. are collecting from operational systems the internal, transactional data 
##-- 2.新型技術及其數據庫： 新的數據來源 --> 物聯數據
#    -- (6) 平行計算應用(Hadoop MapReduce application results)-—The next generation technology architectures for handling and parallel parsing of data from logs, Web posts, etc
#    -- (8) 非結構化數據源(Columnar/NoSQL data sources)-—MongoDB,Cassandra,InfoBright, etc. – examples of a new type of map reduce repository and data aggregator
##-- 3.雲端數據：用戶、發言、互動、網流
#    -- (1) 社群網絡圖像(Social network profiles)-—user profiles from Facebook, LinkedIn, Yahoo, Google, and specific-interest social or travel sites
#    -- (2) 社群影響者(Social influencers)-—Editor, analyst and subject-matter expert blog comments, user forums, Twitter & Facebook “likes,” Yelp-style catalog and review sites, and other review-centric sites 
#    -- (3) 活動產生的數據(Activity-generated data)-—Computer and mobile device log files, aka “The Internet of Things.” 
#    -- (9) 網絡流量監控技術(Network and in-stream monitoring technologies)-—Packet evaluation and distributed query processing-like applications
##-- 4.公開數據：已開放的、尚為文件型的
#    -- (5) 公開數據(Public)-—Microsoft Azure MarketPlace/DataMarket, The World Bank, SEC/Edgar, Wikipedia, IMDb, etc. – data that is publicly available on the Web
#    -- (10) 文檔(Legacy documents)-—Archives of statements, insurance forms, medical record and customer correspondence


#####===== (1C) 企業(檔案)數據 [黃文3.2] =====#####
##== (1)最常用的.csv檔案格式 (read.csv) [黃文3.2.1]
write.csv(Insurance, "insurance.csv")
csv1 = read.csv("insurance.csv");       
csv2 <- read.table("insurance.csv")

##== (2).txt檔案格式兩種讀取方法的比較 (read.table(), read.csv()) [黃文3.2.1]
write.table(Insurance, "insurance.txt")
txt1 = read.csv("insurance.txt");       txt2 = read.table("insurance.txt")
write.table(Insurance, "insurance.comma",sep=",")
comma1 = read.csv("insurance.comma");   
comma2 = read.table("insurance.comma")
comma2A = read.table("insurance.comma",sep=",")
write.table(Insurance, "insurance.slash",sep="/")
slash1 = read.csv("insurance.slash");   slash2 <- read.table("insurance.slash")
slash2A = read.table("insurance.slash",sep="/")

##== (3) 最強悍的讀取指令 (readLines())
ftpfile1 = readLines("ftpfile1.txt")    #-- [Munzert, Ch.13: 加州天氣數據]

##== (4) 特別的讀取指令,可用於讀取網絡檔案 (scan())
#-- [Using R for Time Series Analysis, http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html]
kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)   #-- 從威廉一世開始的英國國王的去世年份數据
births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")   #-- 1946/01-1959/12 的紐約市每月出生人口數量
souvenir = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")   #-- 1987/1-1993/12 昆士蘭海濱度假圣地的某紀念品商店販售數据

##== (5) 最通用的.xlsx檔案格式
### install.packages("readxl") 
library(readxl)
xls1 = read_excel("insurance.xls")
csv123 = read.csv("???商123.csv",fileEncoding="Big5");       
xls123 = read_excel("???商123.xls")   #-- 繁體windows系統不能用簡體字

##== (6) 用於數據交換的.json檔案格式 [DATA.TAIPEI]
library(jsonlite)
airbox = fromJSON("airbox.json")      #-- 台北市空氣盒子開放數據
airbox.df = as.data.frame(airbox[[2]])

##== (7) 網頁適用的.xml檔案格式 [DATA.GOV.TW]
library(XML)
x <- xmlParse("county_h_10508.xml")   #-- 台灣鄉鎮郵區及英文名稱表  
xRoot = xmlRoot(x)
x.df = xmlToDataFrame(xRoot)


#####===== (1D) 物聯(設備)數據 =====#####

##== (1)..位置 [Data Taipei]
library(jsonlite)
download.file("https://tpairbox.blob.core.windows.net/blobfs/AirBoxData.gz","airbox.gz")
airbox = fromJSON(gzfile("airbox.gz"))              #-- 台北市空氣盒子開放數據
airbox.df = as.data.frame(airbox$entries);  
dim(airbox.df);     # row / col
head(airbox.df,2)   

##== (2)..UBIKE/GPS [Data Taipei]
download.file("http://data.taipei/youbike","ubike.gz")
ubike = fromJSON(gzfile("ubike.gz"))                #-- 台北市UBIKE開放數據 <-- 因其中有中文,在windows系統中無法解譯,在Mac可以
library(plyr)
ubike.df = ldply(ubike[[2]], function(x) rbind(data.frame(x)))  

##== (3) 網絡爬虫 =====##### 
library(RCurl)
#兩種寫法 download || ftplist.
download.file("http://www.wcc.nrcs.usda.gov/ftpref/data/climate/table/temperature/history/california/19l03s_tavg.txt","ftpfile1.txt") 
ftplist = "http://www.wcc.nrcs.usda.gov/ftpref/data/climate/table/temperature/history/california/"
filelist = getURL(ftplist, dirlistonly = TRUE) #-- 加州天氣數據檔案列表

#網頁中格式
url = "http://www.openintro.org/cont/donorProcess.php"
result = postForm(url, name="David Diez", email="david@openintro.org", phone="857-288-8547")
url = "http://www.amazon.com/s/ref=nb_sb_noss_2?url=node%3D2407749011&field-keywords=Apple"
firstSearchPage = getURL(url)
parsedFirstSearchPage = htmlParse(firstSearchPage)  #-- 亞馬遜網站之行動電話數據-->現會要求權限,無法取得


#####=====*(3D) 簡單HTML網頁頁面爬取 [殷3.5.2] =====#####
library(RCurl)  # 數據爬取軟件包 RCurl
response = getURL("http://www.weather.com.cn/weathern/101340101.shtml")
print(response)


########## (4) 分析網頁信息 ##########
response.parser = htmlParse(response, asText=TRUE)
print(response.parser)



#####===== (4C) 提取網頁樹: xpathSApply [殷3.3] =====#####
##== xpathSApply(XML檔, XPATH): 以XPATH 抓取XML樹上的分支
##== 續實驗2:數據提取 [殷3.5]
xpathSApply(response.parser, "/html/body/div/div/div/div/div/ul/li[2]")
# [[1]] <li>  <a href="/logo/">Logo</a>  </li> 
# [[2]] <li>  <a href="/foundation/board.html">Board</a>  </li> 
# [[3]] <li>  <a href="http://cran.r-project.org/faqs.html">FAQs</a>  </li> 
# [[4]] <li>  <a href="/other-projects.html">Related Projects</a>  </li> 
xpathSApply(response.parser, "/html/body/div/div/div/div/div/h2")
# [[1]] <h2 id="download">Download</h2> 
# [[2]] <h2 id="r-project">R Project</h2> 
# [[3]] <h2 id="r-foundation">R Foundation</h2> 
# [[4]] <h2 id="help-with-r">Help With R</h2> 
# [[5]] <h2 id="documentation">Documentation</h2> 
# [[6]] <h2 id="links">Links</h2> 
xpathSApply(response.parser, "//div[@class='col-xs-6 col-sm-12']/h2")  ##-- 以相對路徑和識別碼來找比較快


########## (5) 單一網頁數據爬取 (鄭州每週天氣預報) ##########

#####===== (5A) (KDD1数据取得) 網絡爬文 (URL->X) =====#####
library(RCurl)
URL = "http://www.weather.com.cn/weathern/101340101.shtml"
X = getURL(URL, .encoding="GB");  head(X)

#####===== (5B) (KDD2数据探索) 檢視網頁樹 (X->XX) =====#####
library(XML)
XX = xpathSApply(htmlParse(X), "//ul[@class='t clearfix']/li")  #-- 相對路徑 //...
length(XX);   XX[[1]]   #-- [1] 7
# <li class="sky skyid lv2 on">
#   <h1>24日（今天）</h1>
#   <big class="png40"/>
#   <big class="png40 n01"/>
#   <p title="多云" class="wea">多云</p>
#   <p class="tem">
#     <i>13℃</i>
#   </p>
#   <p class="win">
#     <em>
#       <span title="南风" class="S"/>
#     </em>
#     <i>
#     </i></p>
#   <div class="slid"/>
# </li> 

#####=====*(5C) (KDD3数据转换) 转为天气数据框 (X->XXX) =====#####
Xday = NULL;   Xweather = NULL;   Xtemp = NULL;   Xwind = NULL
for (k in 1:length(XX)) {
  Xday[k] = xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/h1")[[k]] )
  Xweather[k] = xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='wea']")[[k]] )
  Xtemp[k] = xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='tem']")[[k]] )
  Xwind[k] = xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']")[[k]] )
}
XXX = data.frame(day=Xday, weather=Xweather, temp=Xtemp, wind=Xwind);  dim(XXX);  head(XXX,3)  #-- [1] 7 4
#           day  weather          temp                     wind
# 1 24日（今天）     多云      \n13℃\n               \n\n\n\n\n
# 2 25日（明天） 小雨转晴 \n21℃/11℃\n        \n\n\n\n\n5-6级\n
# 3 26日（后天）       晴  \n19℃/9℃\n        \n\n\n\n\n4-5级\n
write.xlsx(XXX,"/Users/mcsl/Desktop/XXX.xlsx")  #-- 通常，爬到文後，解析後，趕快將標籤(tag)存檔

#####===== (5D) (KDD4数据模型) 这一周天气的简要显示 (table(XXX$..)) =====#####
table(XXX$weather)
# 多云       晴 小雨转晴 
#    1        5        1 
table(XXX$wind)   #-- 這數據不太乖，有沒有改進的方法？
# \n\n\n\n\n             \n\n\n\n\n\n        \n\n\n\n\n3-4级\n \n\n\n\n\n3-4级转4-5级\n        \n\n\n\n\n4-5级\n 
#          1                        1                        1                        1                        2 
# \n\n\n\n\n5-6级\n 
#                 1 
table(XXX$weather, XXX$wind)
#          \n\n\n\n\n \n\n\n\n\n\n \n\n\n\n\n3-4级\n \n\n\n\n\n3-4级转4-5级\n \n\n\n\n\n4-5级\n \n\n\n\n\n5-6级\n
# 多云              1            0                 0                        0                 0                 0
# 晴                0            1                 1                        1                 2                 0
# 小雨转晴          0            0                 0                        0                 0                 1

#####===== (5E) (KDD5数据呈现) 天气数据图 (plot(XXX$..)) =====#####
par(family="STKaiti");   
barplot(table(XXX$weather), main="本周天气分布图")
pie(table(XXX$weather), main="本周天气比例图")
levels(XXX$weather) = c("晴","多云","小雨转晴")
plot(XXX$day,as.integer(XXX$weather), main="本周天气时序图")

#####===== (5F) 几个进阶的处理 =====#####
k = 6
##== (1) 去除 \n ==###
xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']")[[k]] )
# [1] "\n\n\n\n\n5-6级\n"
library(stringr)
str_replace_all( xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']")[[k]] ), "\n","")
# [1] "5-6级"
##== (2) 提取最高气温与最低气温 ==###
A = str_extract_all( xmlValue( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='tem']")[[k]] ), 
                     "[:digit:]*")[[1]]
A[which(nchar(A)>0)]
# [1] "21" "11"
##== (3) 提取風向 ==###
A = xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']" )[[k]];   A
# <p class="win">
#   <em>
#     <span title="西北风" class="NW"/>
#     <span title="西北风" class="NW"/>
#   </em>
#   <i>3-4级</i>
# </p> 
xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li/p[@class='win']/em/span" )
# [[1]]
# <span title="南风" class="S"/> 
#   ...
# [[13]]
# <span title="西北风" class="NW"/> 
A = xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li[position()=2]/p[@class='win']/em/span" );  A
# [[1]]
# <span title="西风" class="W"/> 
# [[2]]
# <span title="西北风" class="NW"/> 
xmlGetAttr( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li[position()=2]/p[@class='win']/em/span")[[1]], "title" )
# [1] "西风"
xmlGetAttr( xpathSApply( htmlParse(X),"//ul[@class='t clearfix']/li[position()=2]/p[@class='win']/em/span")[[2]], "title" )
# [1] "西北风"


########## (6) 多重網頁爬文實例 (mobile01 - 健康與養生 討論區) ##########

#####===== (6A) 環境設定 =====#####
##== mobile01 - 健康與養生網址: addrE = "https://www.mobile01.com/topiclist.php?f=330"
##== 爬文時間: 09/12/2017 by J.-S. Heh
wkDir = "/Users/mcsl/Desktop/1060928TPC/BDAprojects/HI/";    setwd(wkDir)      #== (1) 工作目錄設定
dataDir = "/Users/mcsl/Desktop/1060928TPC/BDAprojects/HI/mobile01/";           #== (2) 數據目錄設定
library(RCurl);   library(XML);          library(stringr);    library(readxl); #== (3) 載入軟件包   
library(rvest);   library(data.table);   library(xml2);    library(curl)
library(xlsx);   #-- there is warning for this packages  (need to install JDK first)

#####===== (6B) 爬取首頁與解析 (addrE->H01-->pageNo) =====#####
addrE = "https://www.mobile01.com/topiclist.php?f=330"     
H1 <- read_html(curl(addrE, handle = new_handle("useragent" = "Mozilla/5.0")))  #== (1) 爬取首頁
write(as.character(H1),paste0(dataDir,paste0("H",as.character(k),".html")))     #== (2) 儲存首頁(爬文的習慣:爬到後立刻儲存)
HH1 = htmlParse(H1)                                                             #== (3) 解析首頁
xpathSApply(HH1,"//div[@class='pagination']//a[last()]")                        #== (4) 提取頁數位置與頁數(pageStr2)
pageStr1 = xmlValue( xpathSApply(HH1,"//div[@class='contentfoot']//p[@class='numbers']")[[1]])
pageStr2 = str_extract(pageStr,"共[0-9]*頁")
pageNo = as.integer(str_sub(pageStr2,2,nchar(pageStr2)-1))                      #== (5) 提取頁數(pageNo)

#####=====*(6C) 爬取第k頁與分析 (pageNo->Hk.html) =====#####
addrE0 = "https://www.mobile01.com/topiclist.php?f=330&p=1"
for (k in 1:pageNo) {
  urlH = str_replace(addrE0,"=1",paste0("=",as.character(k)))                   
  print(paste0(">> Crawling for page - ",k," with urlH = ",urlH," ..."))        #== (1) 爬文時,要顯示爬文進程(progress)
  Hk <- read_html(curl(urlH, handle = new_handle("useragent" = "Mozilla/5.0"))) #== (2) 找出第k主題頁網址(urlH)並爬文之
  write(as.character(Hk),paste0(dataDir,paste0("H",as.character(k),".html")))   #== (3) 儲存第k頁 (Hk.html)
  Sys.sleep(1)                                                                  #== (4) 爬文不能太快,否則可能被擋
}
##== 主題頁面分析 (Hk)
# <tr>
#   <td class="subject">
#     <span class="subject-text" style="max-width: 417px; overflow: hidden;">
#       <span class="greenpoint"/>
#       <a href="topicdetail.php?f=330&amp;t=5261895" class="topic_gen" title="人氣: 0">各位刷牙時間花費多久？</a>   ##-- (HkBlk1) href, response, title
#     </span>
#   </td>
#   <td width="7%" class="reply">0</td>   ##-- (HkBlk2) reply
#   <td width="17%" class="authur">
#     <a href="topicdetail.php?f=330&amp;t=5261895">  
#       <p>2017-09-11 23:47</p>           ##-- (HkBlk3) date
#       <p>oggyoggy</p>                   ##-- author
#     </a>
#   </td>
#   <td width="17%" class="latestreply">
#     <a href="topicdetail.php?f=330&amp;t=5261895&amp;p=1#pb">
#       <p>2017-09-11 23:47</p>           ##-- (HkBlk4) latestreplydate
#       <p>oggyoggy</p>                   ##-- latestreplier
#     </a>
#   </td>
# </tr>  

#####===== (6D) 主題頁第k頁解析 (Hk-->TTbody-->TT.xlsx) =====#####
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

#####===== (6E) 探索主題檔(TT.xlsx-->TT) =====#####
TT = as.data.frame(read_excel("mobile01/TT.xlsx"));     dim(TT)   #-- 19505  11   #== (1) 主題數
sum(as.integer(TT$replyNo),na.rm=T)   #-- 207901 / 19505 = 10.66                  #== (2) 回文數總和  
sort(unique(substr(TT$date,1,7)))     #-- "2005-01"..."2017-09"                   #== (3) 每月回文數
# 2005-01 2005-07 2006-05 2006-10 2006-11 2007-09 2007-10 2007-11 2007-12 2008-01 2008-02 2008-03 2008-04 2008-05 2008-06 2008-07 2008-08 2008-09 2008-10 2008-11 2008-12 2009-01 
# 1       1       2       1       1       1       1      70      91      95      85     103     110      95     127     109     110     103     118     104      94      99 
# 2009-02 2009-03 2009-04 2009-05 2009-06 2009-07 2009-08 2009-09 2009-10 2009-11 2009-12 2010-01 2010-02 2010-03 2010-04 2010-05 2010-06 2010-07 2010-08 2010-09 2010-10 2010-11 
# 120     135     148     151     157     173     181     165     166     167     147     190      98     193     170     172     171     173     156     167     162     156 
# 2010-12 2011-01 2011-02 2011-03 2011-04 2011-05 2011-06 2011-07 2011-08 2011-09 2011-10 2011-11 2011-12 2012-01 2012-02 2012-03 2012-04 2012-05 2012-06 2012-07 2012-08 2012-09 
# 149     130     131     148     143     222     229     180     202     162     166     166     138     151     177     180     223     204     187     183     227     182 
# 2012-10 2012-11 2012-12 2013-01 2013-02 2013-03 2013-04 2013-05 2013-06 2013-07 2013-08 2013-09 2013-10 2013-11 2013-12 2014-01 2014-02 2014-03 2014-04 2014-05 2014-06 2014-07 
# 215     189     142     188     126     204     196     198     227     212     192     195     226     219     191     173     181     205     186     195     202     226 
# 2014-08 2014-09 2014-10 2014-11 2014-12 2015-01 2015-02 2015-03 2015-04 2015-05 2015-06 2015-07 2015-08 2015-09 2015-10 2015-11 2015-12 2016-01 2016-02 2016-03 2016-04 2016-05 
# 193     222     231     179     142     159     127     166     163     185     151     156     150     135     149     180     168     142     136     186     159     168 
# 2016-06 2016-07 2016-08 2016-09 2016-10 2016-11 2016-12 2017-01 2017-02 2017-03 2017-04 2017-05 2017-06 2017-07 2017-08 2017-09 
# 181     176     167     136     159     155     147     146     143     219     207     188     184     187     202      54 
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

#####===== (6F) 主題之回文頁爬取/分析/解析(TT-->Tk-->RR) =====#####
##== (6F-1) 回文頁面分析 (Tk-->RpageNo) 
# <article>
#   <div class="single-post">
#     <div class="single-post-author group">
#       <div class="inner">
#         <a href="userinfo.php?id=1085077&amp;from=avatar" target="_blank" class="single-post-author-img"><div class="anon_icon">無圖示</div></a>
#         <div class="fn"><a href="userinfo.php?id=1085077" id="id44791978" title="高級會員">jupid</a></div>    ##-- (RjBlk1) authorclass  ##-- author
#         <div class="info"> 
#            <a name="3"></a>
#            <a name="44791978"></a>
#         </div>
#         <div class="date">2013-07-05 10:21..#3</div>                                   ##-- (RjBlk2) date   ##-- rpostNo
#           <ul class="author-detail" style="display:none;">
#             <li>
#               <span class="label">文章編號:</span>
#               <span>44791978</span>
#             </li>
#             <li>
#                <span class="label">個人積分:</span>
#                <span>10</span>                                                         ##-- (RjBlk3) authorcredit
#             </li>
#           </ul>
#           <ul class="tools btns group">
#             <li><a href="pm.php?folder=post&amp;id=1085077" class="btn">私訊</a></li>
#             <li><a href="javascript:showurl(44791978);" class="btn">連結</a></li>
#           </ul>
#         </div>
#       </div>
#       <div class="single-post-content">                                                ##-- (RjBlk4) text
#         <div id="ct44791978">我也開過這手術..(刪4行)....懷著惡意抨擊他人的也是他的福氣啦<br><br>
#           總之多積善心吧
#           <img src="//attach2.mobile01.com/images/smile/51.gif" title="笑" class="post-smile"><br><br><blockquote>
#           <b>阿彭小弟 wrote:</b><br>
#           首先，此篇文章絕非廣...(恕刪)</blockquote>
#         </div>
#         <div class="single-post-content-sig"></div>
#      </div>
#    </div>
#  </article>
# 6001  6501  7001  7501  8001  8501  9001  9501 10001 10501 11001 11501 12001 12501 13001 13501 14001 14501 15001 15501 16001 16501 17001 17501 18001 18501 19001 19501
for (kStart in 1901:1901)  {    #-- (kStart in seq(from=18501,to=dim(TT)[1],by=500))
  for (k in 19147:dim(TT)[1]) {    #-- dim(TT)[1] = 19505    #-- kStart:(kStart+499)
    ##== (F2) 爬取主題頁頁面 (TT->Ahref->Tk)
    Ahref = paste0("https://www.mobile01.com/",TT$href[k])
    Tk <- read_html(curl(Ahref, handle = new_handle("useragent" = "Mozilla/5.0")))
    ##== (F3) 解析得回文則數 (Tk->RpageNo) =====#####
    ## <div class="pagination"> 
    ##   <span class="disable">1</span> 
    ##   <a href="topicdetail.php?f=330&amp;t=3440865&amp;p=2">2</a>  ...
    ##   <span>…</span> 
    ##   <a href="topicdetail.php?f=330&amp;t=3440865&amp;p=2">下一頁 ›    <span>.</span>  </a> 
    ##   <a href="topicdetail.php?f=330&amp;t=3440865&amp;p=9">9</a> 
    ## </div>
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

#####===== (Z0) 讀取數據 (ftpfile1.txt-->X) =====#####
X = readLines("ftpfile1.txt");   length(X);   X[1:10]  # [1] 1163
#####===== (Z1) 抓出表格首行 (X-->indB) =====#####
indB = grep("day    oct",X);   length(indB);   indB   #-- 27  #-- 抓出字串的pattern
#####===== (Z2) 抓出第k表格 (k-->indB[k]-->BB) =====#####
k = 2;   indB[k]   #-- 49
BB = X[(indB[k]+2):(indB[k]+2+30)];   BB
########## (Z) 數據清洗實作 ##########

#####===== (Z0) 讀取數據 (ftpfile1.txt-->X) =====#####
X = readLines("ftpfile1.txt");   length(X);   X[1:10]  # [1] 1163


i = 1
BBi = strsplit(BB[i],split=" ")[[1]];   BBi
BBii = BBi[which(nchar(BBi)>0)]; BBii
BBiii = as.integer(BBii); BBii

TT = NULL
for(i in 1:31){
  BBi = strsplit(BB[i],split=" "[[1]]); BBi
  BBii = BBi[]
}


