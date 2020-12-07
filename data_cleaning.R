# Data Filtering ------------------------------------------------------


### 待做的事

### 看看中美差距

2365 - sum(is.na(dat$China_DD_Score_Cleaned))



library(haven)
library(tidyverse)
dat = read_sav('/Users/moran/Google_Drive/Course/CS760/Proj/data/COVID-19.public.2020_March-May.sav.sav')

# Session Variable
# 删掉如下变量：
# Session ID
# url of site that referred user to Project Implicit
# What brought you to this website?
# url of current study
# user browser
# Session date in SPSS Date format
# Month of the session date
# Day of the session date
# Week of Study
# Year of the session date
# Hour of the session date (24h format)
idx_session = 12 # 只留一个表示周几的变量，说不定周中和周末确实有区别。

# Questionaire Variable:
# 删掉所有特定的某一道题的答案
# 因为这些特定问题的答案，都会被转变为IAT分数
# 感觉IAT是一个更有说服力的summary statistics
# 这一部分我们只保留demographic
# 删除被recoded的demographic：
# 30 78 113 114 都是描述政治倾向（保守/开放）
# 暂时选114，打成左派右派和中立
# 31 77 102 110 删除，因为103更详细（人种）
# 26 27 33 109 都是gender 选用109
# 112 删除 （专注美国人的一个变量）
# 115 删除 冗余信息 保留118

idx_demo = c(28, 29, 32, 103, 108, 109, 111, 114, 116, 118) - 9

# IAT Variable
# 只保留最后清洗之后的DD_Clean_Score
idx_iat = c(127, 134, 141, 148) - 9
dat1 = dat[,c(idx_session,idx_demo,idx_iat)]


# Data Cleaning -----------------------------------------------------------

# 刚才选了我们能用的变量（现在剩15个）
# 现在开始仔细冲洗数据

### 1. 简单处理NA：
# 首先去掉那些刚点进问卷就退出的人
#（除了答题时间，其他自变量都是NA）
# 发现这样的人有2157个 霍
dat1 = dat1[apply(dat1[,1:11],1, function(x) sum(is.na(x)) < 10),]


# 接着去掉那些response是NA的数据
# 这样的人有912个
dat1 = dat1[apply(dat1[,12:15],1, function(x) sum(is.na(x)) != 4),]

# write_sav(dat1,'1.sav')

### 2. 开始考虑我们要做的问题

dat = read_sav('/Users/moran/Google_Drive/Course/CS760/Proj/data/dat.sav') %>%
  .[,-c(2,3)]
sum(is.na(dat))
# dat_us = as.data.frame(dat[,-(13:15)]) # 只要USA Score
# dat_complete = dat_us[apply(dat_us, 1, function(x) sum(is.na(x)) == 0),] 
# dat_complete$weekday = as.factor(dat_complete$weekday)
# dat_complete$fieldofstudy = as.factor(dat_complete$fieldofstudy)
# dat_complete$Race_White_Asian_Black_Other = as.factor(dat_complete$Race_White_Asian_Black_Other)
# fit = lm(USA_DD_Score_Cleaned~., data = dat_lm)

colnames(dat) = c("Weekday", "FieldOfStudy", "Race", "Age", "Gender", "Edu", "Political", "SocialStatus", 
                  "Religious", "USAScore", "UKScore", "ItalyScore", "ChinaScore")

dat$Race = factor(dat$Race, levels = c(0,1,2,3), labels = c('Other', 'White', 'Asian', 'Black'))
dat$Gender = factor(dat$Gender, levels = c(0,1), labels = c('Female', 'Male'))
dat$Political = factor(dat$Political, levels = c(1,2,3), labels = c('Left', 'Moderate', 'Right'))

# GUIDE -------------------------------------------------------------------

### Regression
z = dat

### Classification
dat_c = dat
y_new = factor(,levels = c('COVID', 'Non_COVID', 'No_Bias'))
for(i in 1:nrow(dat_c)){
  if(is.na(dat_c$USAScore[i])){
    y_new[i] = NA
  }else if(dat_c$USAScore[i] > 0.65){
    y_new[i] = 'COVID'
  }else if(dat_c$USAScore[i] < -0.65){
    y_new[i] = 'Non_COVID'
  }else{
    y_new[i] = 'No_Bias'
  }
}
dat_c$USAScore = y_new


# 产生dsc -------------------------------------------------------------------

z = dat_c
for (i in 1:ncol(z)){
  if(is.character(z[,i])){ z[,i]=factor(z[,i]) }
}  #将'1'识别为categorical Data

#下面是要开始建立dsc文件
# names(z)
z.names <- names(z)
k <- ncol(z)
role <- rep("n",(k))
# for(j in 1:k){
#   if(class(z[,j]) == "factor") role[j] <- "c"
# }
# role[z.names %in% "INTRDVX"] <- "d"
write.table(z, "first.rdata",row.names=FALSE,col.names=TRUE,quote=FALSE)
write("first.rdata","first.dsc")
write("NA",file="first.dsc",append=TRUE)
write("2",file="first.dsc",append=TRUE)
write.table(cbind(1:k,names(z),role),file="first.dsc",
            append=TRUE,row.names=FALSE,col.names=FALSE,quote=FALSE)


write.csv(dat_c,'fenlei.csv',row.names = F)

