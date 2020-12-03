# Rtech02.R: 大數據實務技術 - 02: 文本數據之詞語分析 (4hr)
# Jia-Sheng Heh (賀嘉生), 11/26/2020, revised from HUT03.R

setwd("/Users/juck30808/Documents/Github/USC_R_Git/2.R-tech/data/")
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



########## (2) 中文文本分析工具 ##########

##== (1) (KDD1) 讀取數據 (RRlist/RR....csv-->RR) 
library(data.table)
Rlist = c("RR1_500r6810")   #Rlist = c("RR1_500r6810","RR501_1000r6217","RR1001_1500r7707","RR1501_2000r8262")
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
length(content)     
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



##== (6) 語言學的語詞分類 [C.C.Fries, 1952] 

Dword = lapply(Dword0, function(x){ nx=sapply(x,nchar); xA=x[nx>=2]; return( xA[substr(names(xA),1,1) %in% c("n","v")] ) } );   
Dword[[1]]  #-- grep() 似乎有出入,改用substr()  



########## (4) 詞語向量 (Dword-->tokens) ##########

##== (4A) 提取關鍵詞 
##== TF（Term Frequency,詞频) = Nij/sum_k Nkj：詞語(Ti)在該檔案(Dj)中出現的頻率,並對詞數(Nij)歸一化以避免偏向長的檔案
##.. IDF（Inverse Document Frequency, 逆文檔頻率) = log( |D| / |{j: Ti in Dj}：詞語普遍重要性的度量
##.. TF-IDF = TF x IDF：檔案中的高詞語頻率，及該詞語在整個檔案集合中的低檔案頻率，可以產生出高權重的TF-IDF
doc1 = "Apple Inc. is an American multinational technology company headquartered in Cupertino, California, that designs, develops, and sells consumer electronics, computer software, and online services." 
doc2 = "Apple was founded by Steve Jobs, Steve Wozniak, and Ronald Wayne on April 1, 1976, to develop and sell personal computers."
doc3 = "Apple is the world's largest information technology company by revenue, the world's largest technology company by total assets, and the world's second-largest mobile phone manufacturer."
doc4 = "Apple's worldwide annual revenue totaled $233 billion for the fiscal year ending in September 2015."
doc.vec <- c(doc1, doc2, doc3, doc4);   doc.list <- strsplit( tolower(doc.vec), " ");  doc.list
# [[1]]
# [1] "apple"         "inc."          "is"            "an"            "american"      "multinational" "technology"    "company"       "headquartered"
# [10] "in"            "cupertino,"    "california,"   "that"          "designs,"      "develops,"     "and"           "sells"         "consumer"     
# ...
tf <- table(doc.list[[3]]);   tf  
# and          apple        assets,             by        company    information             is        largest  manufacturer.         mobile 
#   1              1              1              2              2              1              1              2              1              1 
# phone       revenue, second-largest     technology            the          total        world's 
#     1              1              1              2              3              1              3 
idf1 <- log(length(doc.list)/length(grep("largest",doc.list)));      idf1  #-- = log(4/length(3)) = 1.386294
tfidf1 <- tf["largest"]*idf1;     tfidf1           #-- 2.772589
idf2 <- log(length(doc.list)/length(grep("technology",doc.list)));   idf2  #-- = log(4/length(c(1,3)) = 0.6931472
tfidf2 <- tf["technology"]*idf2;  tfidf2           #-- 1.386294
##== 這僅是最基本的算法，或說是基本觀念的說明。實務上，TF和IDF各有許多種算法 [https://en.wikipedia.org/wiki/Tf-idf]##

##== (4B) 設定關鍵詞數目的分詞引擎 (worker("keywords", topn=?) 
wkr3 = worker("keywords",topn=5);   wkr3   ##== 在工作引擎中加上"keywords"參數，並可由 topn 指定關鍵詞數目
# Worker Type:  Keyword Extraction
# Detect Encoding :  TRUE
# Default Encoding:  UTF-8
# Keep Symbols    :  FALSE
# Fixed Model Components:  
#   $top_n_word   [1] 5
#   $dict         [1] "C:\\Users\\Mike\\AppData\\Local\\Temp\\RtmpgR3cHx/jiebaR_dict/dict/jieba.dict.utf8"
#   $hmm          [1] "C:\\Users\\Mike\\AppData\\Local\\Temp\\RtmpgR3cHx/jiebaR_dict/dict/hmm_model.utf8"
#   $user         [1] "C:/Users/Mike/Documents/R/win-library/4.0/jiebaRD/dict/user.dict.utf8"
#   $idf          [1] "C:\\Users\\Mike\\AppData\\Local\\Temp\\RtmpgR3cHx/jiebaR_dict/dict/idf.utf8"
#   $stop_word    [1] "C:/Users/Mike/Documents/R/win-library/4.0/jiebaRD/dict/stop_words.utf8"
#   $timestamp    [1] 1600362766
#   $detect $encoding $symbol can be reset.
words = segment(c("這是實驗1的例子：說明結巴用法","第二個實驗的例子：結巴引擎","結巴切文句的第三種做法"),wkr)
vector_keywords(words,wkr3)
#     35.2176  23.4784  14.1723  11.7392  11.7392 
#     "結巴"   "實驗"   "例子" "第二個" "第三種" 
##--> 詞語的TF-IDF值越大，其在文章中的重要性就越高

##== (4C) 結巴中的提取關鍵詞 (wkr3,Dword-->tokens) 
tokens = lapply(Dword, FUN=function(x) vector_keywords(x,wkr3));   length(tokens);   head(tokens,2)   #-- [1] 1993
# $...
# 24.5825    23.4784    23.4784    21.6295    16.8737 
#   "背痛" "腰痠背痛"     "討論"     "肌肉"     "伸展" 
# $...
# 207.586 93.9136 82.1744 82.1744 79.2623 
#  "小牛"  "應該"  "國王"  "禁區"  "加油" 


########## (5) 文本詞語矩陣 (Document-Term Matrix, dtm) ##########

##== (5A) 文本數據挖掘(Text Mining) 
##== 數據挖掘(Data Mining)中的分類--
#    -- 數據挖掘(Data Mining): 以數據分析技術，擷取數據庫(結構化資料)涵蓋的信息或知識。
#    -- 文本挖掘(Text Mining): 以文本分析技術，擷取文本涵蓋的信息或知識。
##== 文本挖掘技術: 自然語言處理(NLP, Natural Language Processing)、統計分析、機率模式、機器學習等技術
##== 文本挖掘應用: 
#    -- 概念擷取(concept extraction)、文件摘要(text summarization)、資訊過濾(information filtering)
#    -- 文本分類(text classification)、文本分類(text clustering)、名稱標註(named entities tagging)
#    -- 意見分析(opinion analysis)、情緒分析(sentiment analysis)、關係發掘(relation discovery)

##== (5B) text2vec軟件包 
##== text2vec軟件包: 简单高效的文本分析和自然语言处理API框架 [Dmitriy Selivanov,2016]
##== 建構於C++上，充分考虑NLP处理数据特徵，包括运用RcppParallel并行化操作，並有效利用内存
##== 四大功能 --
##== (1)GloVe词向量表达: 比Word2Vec升级的GloVe(Global Vectors for word representation)词嵌入(word embedding)
#       -- 分词器: word_tokenizer()英语分词器, worker()中文分词器
#       -- I/O 处理(迭代器)：支持create_<type>函数 --> itoken()
#       -- 创建词汇表：(N-grams参数) create_vocabulary()
#       -- 修剪词汇： prune_vocabulary()
#       -- 词汇向量化：vocab_vectorizer(), hash_vectorizer()
##== (2)快速文本表达: 以单独词组、n-grams、特征hashing化等方法，將内容表达成
#                     文档-词组矩阵(document-term矩阵,DTM)或词组共现矩阵(term-co-occurrence矩阵,TCM)
#       -- DTM,TCM (Document-Term matrices, Term co-occurence matrices): create_dtm(), create_tcm()
##== (3)主题模型(topic model): LDA（latent dirichlet allocation）、LSA(latent sematic analysis)
#       -- 常用模型: LSA$new(),LDA$new()
#       -- 处理模型的统一规范: model$new(), $fit(), $fit_transform(), $transform()
##== (4)相似性度量(similarity)
#       -- 文档相似性和距离(不相似性): sim2(),dist2(),psim2(),dist2()
#          method=cosine/euclidean/jaccard
library(text2vec)

##==*(5C) 產生文本詞語矩陣(Document-Term Matrix, dtm) 
##== (1) 设置分词迭代器: itoken() --
it = itoken(tokens, progressbar = FALSE)  #-- 可在此處加上參數 preprocessor = tolower 可以轉換成小寫，也可以在此處自行設置 tokenizer 
##== (2) 分词: create_vocabulary() -- 
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 3L);   dim(vocab);   head(vocab,3)   #-- 606  3
# Number of docs: 1993 
# 0 stopwords:  ... 
# ngram_min = 1; ngram_max = 1 
# Vocabulary: 
#    term term_count doc_count
# 1: 一壘          3         3
# 2: 上過          3         3
# 3: 不到          3         3
##== (3) 设置、形成语料文件: vocab_vectorizer()
vectorizer = vocab_vectorizer(vocab);   vectorizer
# function (iterator, grow_dtm, skip_grams_window_context, window_size, 
#           weights, binary_cooccurence = FALSE) {...}
##== (4) 构建DTM矩阵，create_dtm
dtm = create_dtm(it, vectorizer);   dim(dtm);    #-- [1]  1993  606

##== (5D) 探討文本詞語矩陣(dtm) 
rownames(dtm)[1]
# [1] "自己的腰痠背痛自己救 板上偶爾有人在討論背痛或是腰痛問題\r\n然而上班族會腰痠背痛的原因不外乎：坐姿不正確 (我就是)、
#      搬重物\r\n姊爬文看了一下，腰酸背痛的主因是肌肉緊繃，可透過『肌肉伸展』改善下背疼痛症狀。\r\n肌肉伸展最重要的部分是：
#      核心肌群 和 大腿後肌群。\r\n講這麼多，還是來看教學影片最實在，感謝姊夫和翻譯者呀!只能幫分享回報了\n\n\n\nvivavida7749 
#      wrote:\r\n板上偶爾有人在討論背...(恕刪)\n\r\n感覺是貼錯了⋯⋯應該是下背痛那幾篇...Jeff和譯者都佛心來著\r\nh
#      https://youtu.be/Z3-mYrVjBEo\r\nhttps://youtu.be/gYsXOKAfxTs\r\nhttps://youtu.be/vw-LIovZ_Os\r\nhttps://youtu.be/oXj3O0quR0E"
colnames(dtm)[which(dtm[1,]>0)]   #-- [1] "伸展" "背痛" "討論" "肌肉"
rownames(dtm)=RRtitle;   as.matrix(dtm[1:3,1:45])
#                                                 一壘 上過 不到 不吃 不準 不用 中壢 中心 介面 付費 代謝 伸展 俄羅斯 保齡球 俱樂部 倒立
# 自己的腰痠背痛自己救                               0    0    0    0    0    0    0    0    0    0    0    1      0      0      0    0
# 德國坦克Dirk Nowitzki - 我可以得分，我可以創...    0    0    0    0    0    0    0    0    0    0    0    0      0      0      0    0
# [NBA] 冠軍賽  誰來守 Dirk Nowitzki ?...            0    0    0    0    0    0    0    0    0    0    0    0      0      0      0    0
#                                                 健行 側彎 像是 兒子 入門 內湖 內衣 八極拳 公式 分開 前臂 力竭 加入 動力 區間 升級 協會
# 自己的腰痠背痛自己救                               0    0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0
# 德國坦克Dirk Nowitzki - 我可以得分，我可以創...    0    0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0
# [NBA] 冠軍賽  誰來守 Dirk Nowitzki ?...            0    0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0
#                                                 台北市 呼吸 單車 噴霧 國民 地板 坐墊 坐姿 塑身 多少錢 奶昔 季後賽
# 自己的腰痠背痛自己救                                 0    0    0    0    0    0    0    0    0      0    0      0
# 德國坦克Dirk Nowitzki - 我可以得分，我可以創...      0    0    0    0    0    0    0    0    0      0    0      0
# [NBA] 冠軍賽  誰來守 Dirk Nowitzki ?...              0    0    0    0    0    0    0    0    0      0    0      1

########## (6) 詞語雲(word cloud) ##########

##== (6A) 基本的詞語雲作法 
##== 詞語雲
#    -- 定義: 由各種詞語組合成、如雲一般的圖形
#    -- 意義: 讓讀者在讀取所有文章之前，即可快速擷取在這些文章中的關鍵內容
library(wordcloud)
##== 詞語的權重
dtmSum = colSums(as.matrix(dtm));   length(dtmSum);   head(dtmSum,15)   #-- [1] 606
# 一壘   上過   不到   不吃   不準   不用   中壢   中心   介面   付費   代謝   伸展 俄羅斯 保齡球 俱樂部 
#    3      3      3      3      3      3      3      3      3      3      3      3      3      3      3 
##== 詞語雲的作法
wordcloud(names(dtmSum), dtmSum, col= rainbow(length(dtmSum)), family="STKaiti")

##==*(6B) 進階的詞語雲作法 
##== 詞語排序
dtmSum[order(dtmSum,decreasing=T)][1:20]
# 運動 沒有 請問 應該 訓練 體重 開始 飲食 覺得 不會 問題 肌肉 推薦 時間 減肥 感覺 動作 有氧 跑步 脂肪 
# 650  194  185  142  130  127  112  112  109   98   94   88   82   82   79   75   71   70   69   67
##== 重要詞語的詞語雲
ind = which(dtmSum>40);   ind   #--> 下列數字不是權重，是詞語的位置
# 有沒有   飛輪   謝謝   教練   醫生   健身   比賽   小時   膝蓋 健身房   熱量   建議   小弟   脂肪   身體   跑步   有氧 
#    573    574    575    576    577    578    579    580    581    582    583    584    585    586    587    588    589 
# 動作   感覺   減肥   推薦   時間   肌肉   問題   不會   覺得   開始   飲食   體重   訓練   應該   請問   沒有   運動 
# 590    591    592    593    594    595    596    597    598    599    600    601    602    603    604    605    606 
wordcloud(names(dtmSum[ind]), dtmSum[ind], col= rainbow(length(dtmSum[ind])), family="STKaiti")
#--> 可以看到權重最大的詞語是 運動, 其次是 訓練/體重/飲食, 再來是 問題/肌肉/減肥/動作/有氧/跑步/脂肪 等等 

##== (6C) 後續的改進 
##==> 有許多詞語，似乎不該列入重要詞語，像 沒有/請問/應該/開始 等等，要怎麼改進呢？　===> 當做 作業...


########## (R) 第三单元的复习 ##########

##== (R-HW3) 演练作业HW3 
## 试着就你爬取解析的網頁數據，或教材中提供的數據:
##== (HW3A) 列出其中的重要詞語
##== (HW3B) 找出其中的文本詞語矩陣
##== (HW3C) 畫出詞語雲
##== (HW3D) 如果可以的話，改善其中的詞語，去除不重要的詞語 (教材(6C)中所示)
## 请在本周内一起缴交

##== (R-RV3) 重点复习RV3 
##== (RV3A) jieba中是以 "worker()" 指令，啟動分詞引擎。
##== (RV3B) jieba分詞時，常以 "segment()"指令調用worker分詞引擎，以解析詞語。
##== (RV3C) jieba分詞時要同時產生詞性，要在worker()工作引擎中加上"tag"參數。
##== (RV3D) jieba分詞加上 keywords參數時，可由 topn 指定 "關鍵詞數目"。
##== (RV3E) 文本分析一般都產生dtm矩陣，意思是 "文本詞語矩陣"。
##== (RV3F) 詞語雲可快速擷取文章中的關鍵內容，是以 "wordcloud()" 指令來完成。
