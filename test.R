############################################
### Topic: Dataset explore
### Author: Ding Luo
### Date: 11/27/2017
############################################


###### load necessary library 
library(dplyr)
library(tidyr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###### read data 
df = read.csv('UK_companies.csv',sep = "")
df = data.frame(df)
dim(df)
head(df)
str(df)
table(df$sic_code)
table(df$reg_address_country)


# explore by visualization
ggplot(df %>% group_by(sic_code) %>% summarise(n=n()),aes(x=sic_code,y=n))+geom_point()

# change factors to character
df$reg_address_post_code = as.character(factor(df$reg_address_post_code))
df$reg_address_county = as.character(factor(df$reg_address_county))
df$reg_address_country = as.character(factor(df$reg_address_country))
df$reg_address_post_town = as.character(factor(df$reg_address_post_town))
df$company_status = as.character(factor(df$company_status))

# deal with post code
for (i in c(1:dim(df)[1])){
  df$outward[i] = strsplit(df$reg_address_post_code[i],' ')[[1]][1]
  df$inward[i] = strsplit(df$reg_address_post_code[i],' ')[[1]][2]
}

# split the post code to outward and inward 

outward = df %>% group_by(outward) %>% summarise(n=n()) %>% arrange(desc(n))
outward[outward$n>5,]

inward = df %>% group_by(inward) %>% summarise(n=n()) %>% arrange(desc(n))
inward[inward$n>5,]

ggplot(df %>% group_by(outward) %>% summarise(n=n()) %>% arrange(desc(n)))+
  geom_bar(stat='identity',aes(x=outward,y=n))

# missing values 
sapply(df,function(x){sum(x=='')})

# read sic reference table
df_sic = read.xlsx('sic reference table.xlsx',sheet = 1)
colnames(df_sic)[1] = 'sic_code'


# merge two data sets 
df_total = merge(df,df_sic,by.x='sic_code',all.x=TRUE)
table(df_total$Section)
ggplot(df_total %>% group_by(Section) %>% summarise(n=n()),aes(x=Section,y=n))+geom_point()
ggplot(df_total %>% group_by(Section) %>% summarise(n=n()) %>% arrange(desc(n)))+
  geom_bar(stat='identity',aes(x=Section,y=n))

ggplot(d)+geom_bar(stat='identity',aes(x=Section,y=n))


# get the sci area for those liquiadation companies
df_total[df_total$company_status=='Liquidation',]$Section.Name
