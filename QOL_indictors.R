# loading packages
library(tidyverse)
library(dplyr)
library(plyr)
library(lavaan)
library(psych)
library(car)
library(magrittr)
library(lubridate)
library(QuantPsyc)

# explore data with graph
EDA <- function (x){
  par(mfrow=c(2,2)) # 同时显示4个图  
  hist(x) # 直方图  
  dotchart(x) # 点图 
  boxplot(x,horizontal=T) # 箱式图 
  qqnorm(x);qqline(x) # 正态概率图 
  par(mfrow=c(1,1)) # 恢复单图 
}

# description function
mydes <- function(x, na.omit = FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  return(c(n = n, mean = m, stdev = s))
}

# read data
df.all <- data.frame(read.csv("bl-1228.csv",stringsAsFactors=FALSE))
# column names 
names(df.all)

# pick variables
dfall.qol <- df.all %>% 
  select(q1_id, number, group, q3_gender, q4_birth, date, q8_huji, q9_education, 
         q10_sexori, q11_marriage, q12_child, q13_dusheng, q15_job, 
         q16_income, q17_m_daily, q18_m_medical, q19_infectiondate, period, 
         q20_infectionroute, q23_smokepast,q24_smokejie, q26_winepast, q29_fspast, q30_fsfre,
         q31_dlpast, q32_dlidea, q49_1_webxingban, q49_2_comsex, MET, q50_suiidea,
         q51_suitry, q52_sex, q53_condom, q54_drug, q55_1_yn, cd4, ends_with('cope'), 
         contains('score'), contains('qoldomain'))
names(dfall.qol)

dfall.qol$age_bd <- format(as.numeric((as.Date(dfall.qol$date) - ymd(dfall.qol$q4_birth))/365), 
                           digits = 3) %>% as.numeric()

as.Date(df.all$date)
table(df.all$MET == 0)
dfall.qol$age_bd

dfall.qol %$% mydes(qolscore)

##### Univariate analysis ####
shapiro.test(dfall.qol$qolscore) ## 生存质量得分满足正态
dfall.qol %$% EDA(qolscore)

# q3_gender
dfall.qol %$% t.test(qolscore ~ q3_gender)
dfall.qol %$% table(q3_gender) %>% prop.table()
dfall.qol %$% by(qolscore, q3_gender, mydes)

# age_bd
dfall.qol %>% lm(qolscore ~ age_bd, .) %>% summary()
dfall.qol %$% mydes(age_bd)

# q8_huji
# 将户籍转化为分类变量，并比较
dfall.qol$q8_huji %<>% as.character(.(q8_huji))
dfall.qol %$% aov(qolscore ~ q8_huji) %>% summary()
# 分为城市和其他
dfall.qol$q8_hj <- dfall.qol %$% if_else(q8_huji %in% c('1'), q8_hj <- '1', q8_hj <- '2')
dfall.qol %$% t.test(qolscore ~ q8_hj)
dfall.qol$q8_hj %>% table() %>% prop.table()
# 分组描述
dfall.qol %$% by(qolscore, q8_hj, mydes)

# q9_education
data.class(dfall.qol$q9_education)
dfall.qol$q9_edu[dfall.qol$q9_education <= 4] <- '1'
dfall.qol$q9_edu[dfall.qol$q9_education >= 5] <- '2'
dfall.qol$q9_edu %>% table() %>% prop.table()
# 组间比较
dfall.qol %$% aov(qolscore ~ q9_edu) %>% summary()
dfall.qol %$% by(qolscore, q9_edu, mydes)

# q10_sexori
data.class(dfall.qol$q10_sexori)
# 原分类比较
dfall.qol$q10_sexori %<>% as.character(.(q10_sexori))
dfall.qol %$% aov(qolscore ~ q10_sexori) %>% summary()
# 重新分类，异性和其他
dfall.qol$q10_ori <- dfall.qol %$% if_else(q10_sexori == '1', q10_ori <- '1', q10_ori <- '2')
dfall.qol %$% t.test(qolscore ~ q10_ori)
dfall.qol$q10_ori %>% table() %>% prop.table()
dfall.qol %$% by(qolscore, q10_ori, mydes)

# q11_marriage
data.class(dfall.qol$q11_marriage)
# 原分类比较
dfall.qol$q11_marriage %<>% as.character(.(q11_marriage))
dfall.qol %$% aov(qolscore ~ q11_marriage) %>% summary()
# 重新分类
dfall.qol$q11_mrg[dfall.qol$q11_marriage == '1'] <- '1'
dfall.qol$q11_mrg[dfall.qol$q11_marriage == '2'] <- '2'
dfall.qol$q11_mrg[dfall.qol$q11_marriage %in% c('3', '4')] <- '3'
dfall.qol$q11_mrg %>% table() %>% prop.table()
#组间比较
dfall.qol %$% aov(qolscore ~ q11_mrg) %>% summary()
dfall.qol %$% by(qolscore, q11_mrg, mydes)

# q12_child
table(dfall.qol$q12_child)
dfall.qol$q12_child <- as.character(dfall.qol$q12_child)
dfall.qol %$% t.test(qolscore ~ q12_child)

# q13_dusheng
table(dfall.qol$q13_dusheng)
dfall.qol$q13_dusheng <- as.character(dfall.qol$q13_dusheng)
dfall.qol %$% t.test(qolscore ~ q13_dusheng)

# q15_job
table(dfall.qol$q15_job)
dfall.qol$q15_jb <- dfall.qol %$% if_else(q15_job == 1, q15_jb <- '1', q15_jb <- '2')
# 两组比较
dfall.qol %$% t.test(qolscore ~ q15_jb)
dfall.qol$q15_jb %>% table() %>% prop.table()
dfall.qol %$% by(qolscore, q15_jb, mydes)

# q16_income
table(dfall.qol$q16_income)
dfall.qol$q16_incm[dfall.qol$q16_income %in% c(1, 2, 3)] <- '1'
dfall.qol$q16_incm[dfall.qol$q16_income %in% c(4, 5)] <- '2'
dfall.qol$q16_incm[dfall.qol$q16_income %in% c(6, 7)] <- '3'
# 组间比较
dfall.qol %$% aov(qolscore ~ q16_incm) %>% summary()
dfall.qol$q16_incm %>% table() %>% prop.table()
dfall.qol %$% by(qolscore, q16_incm, mydes)

# q17_m_daily
table(dfall.qol$q17_m_daily)
dfall.qol$q17_daily <- dfall.qol %$% if_else(q17_m_daily == 1, q17_daily <- '1', q17_daily <- '2')
dfall.qol %$% t.test(qolscore ~ q17_daily)
dfall.qol$q17_daily %>% table() %>% prop.table()
dfall.qol %$% by(qolscore, q17_daily, mydes)

# q18_m_medical
table(dfall.qol$q18_m_medical)
dfall.qol$q18_medi <- dfall.qol %$% if_else(q18_m_medical == 1, q18_medi <- '1', q18_medi <- '2')
dfall.qol %$% t.test(qolscore ~ q18_medi)
dfall.qol$q18_medi %>% table() %>% prop.table()
dfall.qol %$% by(qolscore, q18_medi, mydes)

# q19_infectiondata period
summary(dfall.qol$period)
shapiro.test(dfall.qol$period)
EDA(dfall.qol$period)
fit.period <- dfall.qol %>% lm(qolscore ~ period, .)
fit.period %>% summary()
par(mfrow = c(2, 2))
fit.period %>% plot()

# q23_smokepast q24_smokejie
dfall.qol$q23_smokepast %>% table()
dfall.qol$q24_smokejie %>% table()
data.class(dfall.qol$q24_smokejie)
# 现在是否抽烟
dfall.qol$q24_smokenow <- dfall.qol %$% if_else(q24_smokejie == 2, q24_smokenow <- '1', q24_smokenow <- '0')
dfall.qol$q24_smokenow %>% table() %>% prop.table()
dfall.qol %$% t.test(qolscore ~ q24_smokenow)

# q26_winepast
dfall.qol$q26_winepast %>% table()
dfall.qol %$% t.test(qolscore ~ q26_winepast)

# q29_fspast
dfall.qol$q29_fspast %>% table() %>% prop.table()
dfall.qol %$% t.test(qolscore ~ q29_fspast)
dfall.qol %$% by(qolscore, q29_fspast, mydes)

# q30_fsfre
dfall.qol$q30_fsfre %>% table()

# q31_dlpast
dfall.qol$q31_dlpast %>% table() %>% prop.table()
dfall.qol %$% t.test(qolscore ~ q31_dlpast)
dfall.qol %$% by(qolscore, q31_dlpast, mydes)

# q49_1_webxingban
dfall.qol$q49_websex <- dfall.qol %$% if_else(q49_1_webxingban == 1, q49_websex <- '0', q49_websex <- '1')
dfall.qol$q49_websex %>% table()
dfall.qol %$% t.test(qolscore ~ q49_websex)

# q49_2_comsex
dfall.qol$q49_cmsex <- dfall.qol %$% if_else(q49_2_comsex == 1, q49_cmsex <- '0', q49_cmsex <- '1')
dfall.qol$q49_cmsex %>% table() %>% prop.table()
dfall.qol %$% t.test(qolscore ~ q49_cmsex)

# q50_suiidea
dfall.qol$q50_suiidea %>% table()
dfall.qol$q50_suiTh <- dfall.qol %$% if_else(q50_suiidea == 1, q50_suiTh <- '0', q50_suiTh <- '1')
dfall.qol$q50_suiTh %>% table() %>% prop.table()
dfall.qol %$% t.test(qolscore ~ q50_suiTh)
dfall.qol %$% by(qolscore, q50_suiTh, mydes)

# q51_suitry
dfall.qol$q51_suitry %>% table()
dfall.qol$q51_suiTr <- dfall.qol %$% if_else(q51_suitry == 1, q50_suiTr <- '0', q50_suiTr <- '1')
dfall.qol$q51_suiTr %>% table() %>% prop.table()
dfall.qol %$% t.test(qolscore ~ q51_suiTr)
dfall.qol %$% by(qolscore, q51_suiTr, mydes)

# MET
dfall.qol$metgp <- dfall.qol %$% if_else(MET < 600, metgp <- '0', metgp <- '1')
dfall.qol$metgp %>% table() %>% prop.table()
dfall.qol %$% t.test(qolscore ~ metgp)
dfall.qol %$% by(qolscore, metgp, mydes)

# q55_1_yn
dfall.qol$q55_1_yn %>% table()

# stigmascore
shapiro.test(dfall.qol$stigmascore)
cor.test(dfall.qol$qolscore, dfall.qol$stigmascore, method = 'spearman')
summary(dfall.qol$stigmascore)
fit.stigma <- dfall.qol %>% lm(qolscore ~ stigmascore, .)
fit.stigma %>% summary()

# gsesscore
shapiro.test(dfall.qol$gsesscore)
cor.test(dfall.qol$qolscore, dfall.qol$gsesscore, method = 'spearman')
summary(dfall.qol$gsesscore)
fit.gses <- dfall.qol %>% lm(qolscore ~ gsesscore, .)
fit.gses %>% summary()

# pssscore
shapiro.test(dfall.qol$pssscore)
cor.test(dfall.qol$qolscore, dfall.qol$pssscore, method = 'spearman')
summary(dfall.qol$pssscore)
fit.pss <- dfall.qol %>% lm(qolscore ~ pssscore, .)
fit.pss %>% summary()

# PHQ9score
shapiro.test(dfall.qol$phq9score)
cor.test(dfall.qol$qolscore, dfall.qol$phq9score, method = 'spearman')
summary(dfall.qol$phq9score)
fit.phq <- dfall.qol %>% lm(qolscore ~ phq9score, .)
fit.phq %>% summary()

# cesdscore
shapiro.test(dfall.qol$cesdscore)
cor.test(dfall.qol$qolscore, dfall.qol$cesdscore, method = 'spearman')
summary(dfall.qol$cesdscore)
fit.cesd <- dfall.qol %>% lm(qolscore ~ cesdscore, .)
fit.cesd %>% summary()

dfall.qol %$% cor.test(qolscore, phq9score, method = 'spearman')

# poscope
shapiro.test(dfall.qol$poscope)
cor.test(dfall.qol$qolscore, dfall.qol$poscope, method = 'spearman')
summary(dfall.qol$poscope)
fit.poscope <- dfall.qol %>% lm(qolscore ~ poscope, .)
fit.poscope %>% summary()

# negcope
shapiro.test(dfall.qol$negcope)
cor.test(dfall.qol$qolscore, dfall.qol$negcope, method = 'spearman')
summary(dfall.qol$negcope)
fit.negcope <- dfall.qol %>% lm(qolscore ~ negcope, .)
fit.negcope %>% summary()

# CD4
data.class(dfall.qol$cd4)
shapiro.test(dfall.qol$cd4)
EDA(dfall.qol$cd4)
summary(dfall.qol$cd4)
mydes(dfall.qol$cd4)
fit.cd4 <- dfall.qol %>% lm(qolscore ~ cd4, .)
fit.cd4 %>% summary()
# cd4分组
dfall.qol$cd4gp[dfall.qol$cd4 < 200] <- '1'
dfall.qol$cd4gp[dfall.qol$cd4 >= 200 & dfall.qol$cd4 <= 400] <- '2'
dfall.qol$cd4gp[dfall.qol$cd4 > 400] <- '3'
dfall.qol$cd4gp %>% table()
dfall.qol %$% aov(qolscore ~ cd4gp) %>% summary()
dfall.qol %$% by(qolscore, cd4gp, mydes)

###### Multivariate regression
model.qol <- qolscore ~ q3_gender + q8_hj + q9_edu + q10_ori + q11_mrg +
  q15_jb + q16_incm + q17_daily + q18_medi + q29_fspast + q31_dlpast +
  metgp + q50_suiTh + q51_suiTr + stigmascore + cesdscore + gsesscore +
  phq9score + pssscore + poscope + negcope

# df.qol.std <- dfall.qol %>% mutate_each_(funs(scale), all.vars(model.qol))

fit.qol <- dfall.qol %>% lm(qolscore ~ q3_gender + q8_hj + q9_edu + q10_ori + q11_mrg +
                              q15_jb + q16_incm + q17_daily + q18_medi + q29_fspast + q31_dlpast +
                              metgp + q50_suiTh + q51_suiTr + stigmascore + cesdscore + gsesscore +
                              phq9score + pssscore + poscope + negcope, .)
fit.qol %>% summary()
par(mfrow = c(2, 2))
fit.qol %>% plot()
# 标化系数
lm.beta(fit.qol)

# 有意义变量
fit.qol.sub <- dfall.qol %>% lm(qolscore ~ q17_daily + cesdscore + gsesscore +
                              phq9score + pssscore + poscope, .)
fit.qol.sub %>% summary()
lm.beta(fit.qol.sub)

# qol coping stress
fit.qol.cs <- dfall.qol %>% lm(qolscore ~ q3_gender + q8_hj + q9_edu + q10_ori + q11_mrg +
                                  q15_jb + q16_incm + q17_daily + q18_medi + q29_fspast + q31_dlpast +
                                  metgp + q17_daily + pssscore + poscope, .)
fit.qol.cs %>% summary()


fit.qol.s <- dfall.qol %>% lm(qolscore ~ q3_gender + q8_hj + q9_edu + q10_ori + q11_mrg +
                                q15_jb + q16_incm + q17_daily + q18_medi + q29_fspast + q31_dlpast +
                                metgp + pssscore, .)
fit.qol.s %>% summary()


