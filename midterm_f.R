library(tidyverse)
library(dplyr)
library(xlsx)
library("readxl")
library(ggplot2)
library(skimr)
library(reshape2)
library(corrplot)
df <- read_excel('C:/Users/singh/Documents/SEM 2/GBUS 738/Midterm/emp_data.xlsx')
head(df)

summary(df)

skim(df)


#department&leftCompany
ggplot(df,aes(left_company,fill=left_company))+
  geom_bar()+
  facet_wrap(~department)+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1)+
  labs(title = 'Employees leaving company based on department',
       x='Left Company',y='Count of Values')

ggplot(df,aes(left_company,fill=job_level))+
  geom_bar(stat='count')+
  facet_wrap(~department,nrow=2)+
  labs(title = 'Employees leaving company based on department and position',
       x='Left Company',y='Count of Values')

ggplot(df,aes(department,fill=left_company))+
  geom_bar(stat='count')+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1)

table(df$department,df$left_company)  

#leftcompany&job_level
table(df$left_company,df$job_level)

table(df$job_level,df$left_company)

ggplot(df,aes(job_level,fill=left_company))+
  geom_bar(stat='count')+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.15)+
  labs(title='Employees leaving company based on Job Position',x='Job Position',y='Count of Values')

ggplot(df,aes(job_level,fill=left_company))+
  geom_bar(stat='count')+
  facet_wrap(~department)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(df,aes(job_level,fill=left_company))+
  geom_bar(stat='count')+
  facet_grid(job_satisfaction~department)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(title = 'Department and Position along with Business Travels ',x='Job Position',y='Count of Values')

#left_company and Weekly hours
df %>% group_by(left_company) %>%
  summarise(n_employees = n(),
            min_hrs = min(weekly_hours), 
            avg_hrs = mean(weekly_hours), 
            max_hrs = max(weekly_hours), 
            sd_hrs = sd(weekly_hours))

ggplot(df,aes(weekly_hours,fill=left_company))+
  geom_histogram()+
  facet_wrap(~left_company)+
  labs(x='Weekly Hours',y='Count of Employees', title ='Weekly Hours distribution')

ggplot(df,aes(weekly_hours,salary,color=left_company))+
  geom_jitter()+
  labs(x='Weekly hours (hrs)',y='Salary (dollars)',title='Salary and weekly hours relating to leaving company')

ggplot(df,aes(weekly_hours,salary,color=left_company))+
  geom_jitter()+
  facet_wrap(~department)

#yrs at Company and left company
df%>%group_by(left_company)%>%
  summarize(no_of_EMP = n(),
            min_yrs = min(yrs_at_company),
            avg_yrs = mean(yrs_at_company),
            max_yrs = max(yrs_at_company))

ggplot(df,aes(yrs_at_company,fill = left_company))+
  geom_bar()+
  labs(x='Years (yrs)',y = 'Count of Employees',title = 'Distribution of Years at company ')

ggplot(df,aes(left_company, yrs_at_company,fill = left_company))+
  geom_boxplot()+
  labs(x='Years (yrs)',y = 'Count of Employees',title = 'Distribution of Years at company ')

#Left company and previous companies
df%>%group_by(left_company)%>%
  summarize(no_of_EMP = n(),
            min_prv_companies = min(previous_companies),
            avg_prv_companies = mean(previous_companies),
            max_prv_companies = max(previous_companies))

ggplot(df,aes(previous_companies,fill=left_company))+
  geom_histogram(color='black',binwidth = 1)+
  labs(x="No of Previous Companies",y='Count of Employees',title='Employee distribution based on previous companies')


ggplot(df,aes(left_company,previous_companies,fill=left_company))+
  geom_boxplot()+
  labs(x='Left Company',y='No of Previous Companies (count)',title='Boxplot of Employees leaving company')

ggplot(df,aes(previous_companies,salary,color=left_company))+
  geom_jitter()+
  labs(title = 'Salary and No of previous companies relating  to Leaving Company',x='No of Companies',y='Salary')


#miles_from_home and left company
df%>%group_by(left_company)%>%
  summarize(no_of_EMP = n(),
            min_miles = min(miles_from_home),
            avg_miles = mean(miles_from_home),
            max_miles_companies = max(miles_from_home))

ggplot(df,aes(miles_from_home,fill=left_company))+
  geom_bar()+
  labs(x='Miles from Home (in miles)',y='No of Employees',title = 'Employees leaving based on miles from home')

#left company and marital status

table(df$left_company,df$marital_status)

ggplot(df,aes(marital_status,fill=left_company))+
  geom_bar()+
  labs(x='Marital Status',y='No of Employees',title='Employees leaving the company based on their marital status')

ggplot(df,aes(left_company,fill=marital_status))+
  geom_bar()
