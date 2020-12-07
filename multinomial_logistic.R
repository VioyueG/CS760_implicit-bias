library(MASS)
library(tidyverse)
library(rstatix)
library(haven)
library(stargazer)
library(caret)

bias_data<-read_sav('dat.sav')
factor_cols = c('Race', 'gender', 'political', 'fieldofstudy', 'citizenship','edu')
us_data<- bias_data%>%
  rename(Weekday = weekday, Race = Race_White_Asian_Black_Other, gender = gender01,
         religion = religion_switch, political = Lib_Moderate_Conservative,
         us_score = USA_DD_Score_Cleaned, citizenship = countrycit_num,
         residency = countryres_num)%>%
  dplyr::select(Race, gender, religion, political, 
                us_score, citizenship, fieldofstudy, Age, edu)%>%
  mutate_each_(as_factor, factor_cols)%>%
  mutate(political = factor(political, levels = c('Moderate', 'Left leaning', 'Right leaning')))%>%
  filter(citizenship == 'U.S.A.')%>%
  dplyr::select(-citizenship)%>%
  na.omit()%>%
  add_column(us_bias = factor(rep('No_Bias'),
                              levels = c( 'No_Bias', 'Non_COVID','COVID')))%>%
  mutate(us_bias = replace(us_bias, us_score > 0.65, 'COVID'))%>%
  mutate(us_bias = replace(us_bias, us_score < -0.65, 'Non_COVID'))%>%
  mutate(edu = as.numeric(edu))
  

#--------------------------------------------------------------------------------
# Visualize

us_data%>%
  #filter(us_score >0)%>%
  ggplot(aes(y = us_score))+
  geom_jitter(aes(x= edu, color = political))+
  facet_wrap(.~gender)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))
  
us_data%>%
  mutate(edu = as.numeric(edu))%>%
  #filter(us_score >0)%>%
  ggplot(aes(x= Age, color = political, y = us_score))+
  geom_point()+
  stat_smooth(aes(group = political),method = "lm")+
  #facet_wrap(.~gender)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))

us_data%>%
  mutate(edu = as.numeric(edu))%>%
  filter(us_score >0)%>%
  ggplot(aes(x= edu, color = political, y = us_score))+
  geom_jitter()+
  stat_smooth(aes(group = political),method = "lm")+
  #facet_wrap(.~gender)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))

us_data%>%
  mutate(edu = as.numeric(edu))%>%
  #filter(us_score >0)%>%
  ggplot(aes(x= edu, color = gender, y = us_score))+
  geom_jitter()+
  stat_smooth(aes(group = gender),method = "lm")+
  #facet_wrap(.~gender)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))

us_data%>%
  mutate(edu = as.numeric(edu))%>%
  #filter(us_score >0)%>%
  ggplot(aes(x = political, y = us_score, color = gender))+
  geom_boxplot()+
  geom_jitter(alpha = 0.5)+
  #facet_wrap(.~gender)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1))

#--------------------------------------------------------------------------------
# Multinomial Logistic
multi_log.fit <- us_data%>%
  multinom(formula = us_bias~ gender+ Race + edu+ religion +political+ Age)

table(predict(multi_log.fit, us_data) == us_data$us_bias)/length(us_data$us_bias)

library(ISLR)
library(purrr)
library(modelr)
library(broom)
library(nnet)

# 10-fold Cross-Validation
cv_samples<- us_data %>%
  dplyr::select(-us_score) %>%
  modelr::crossv_kfold(k = 10) 

models <- map(cv_samples$train, ~ multinom(us_bias ~ ., data = .))

predictions <-
  map2(models, cv_samples$test, ~ predict(.x, newdata = .y))

accuracy = rep(0,10)
for(i in 1:10){
    accuracy[i]<- sum(predictions[[i]] == as.data.frame(cv_samples$test[[i]])$us_bias)/length(predictions[[i]])
}



