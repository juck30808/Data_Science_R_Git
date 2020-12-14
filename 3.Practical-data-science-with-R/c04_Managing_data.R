# example 4.1 of section 4.1.1 
# (example 4.1 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Checking locations of missing data 
setwd("/Users/juck30808/Documents/Github/USC_R_Git/3.Manning/data")
custdata <- read.table('custdata.tsv',header=TRUE,sep='\t')
summary(custdata[is.na(custdata$housing.type),       	# Note: 1  where housing.type is NA. 
                   c("recent.move","num.vehicles")]) 	# Note: 2  Look only at the columns 



# (example 4.2 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Remapping NA to a level 
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),"missing",                       #如果is.employed 丟失，分配“ missing”。            
                                   ifelse(custdata$is.employed==T,"employed","not employed"))  	# 如果is.employed == TRUE
summary(as.factor(custdata$is.employed.fix)) 	# Note: 轉換已將變量類型從factor轉換為string。
##     employed      missing not employed
##          599          328           73



# informalexample 4.1 of section 4.1.1 
# (informalexample 4.1 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),"not in active workforce",
                                   ifelse(custdata$is.employed==T,"employed","not employed"))
summary(as.factor(custdata$is.employed.fix))
# employed            not employed not in active workforce 
# 599                      73                     328 



# (example 4.3 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
# Title: Converting missing numeric data to a level 
breaks <-c(0, 10000, 50000, 100000, 250000, 1000000)                    	# Note: 1 
Income.groups <- cut(custdata$income, breaks=breaks, include.lowest=T)  	# Note: 2 
summary(Income.groups)                                                  	# Note: 3 
# [0,1e+04]   (1e+04,5e+04]   (5e+04,1e+05] (1e+05,2.5e+05] (2.5e+05,1e+06]            NA's 
#             184             469             215             105              26               1 

Income.groups <- as.character(Income.groups)                   	# Note: 4 
Income.groups <- ifelse(is.na(Income.groups),                  	# Note: 5 
                        "no income", Income.groups)

summary(as.factor(Income.groups))

##  (1e+04,5e+04] (1e+05,2.5e+05] (2.5e+05,1e+06]  (5e+04,1e+05]  [0,1e+04]
##            312              98              21            178         63
##      no income
##            328

# Note 1: 
#   Select some income ranges of interest. To 
#   use the cut() function, the upper and lower bounds 
#   should encompass the full income range of the 
#   data. 

# Note 2: 
#   Cut the data into income ranges. The 
#   include.lowest=T argument makes sure that zero 
#   income data is included in the lowest income range 
#   category. By default it would be excluded. 

# Note 3: 
#   The cut() function produces factor 
#   variables. Note the NAs are preserved. 

# Note 4: 
#   To preserve the category names before adding 
#   a new category, convert the variables to strings. 

# Note 5: 
#   Add the "no income" category to replace the 
#   NAs. 


