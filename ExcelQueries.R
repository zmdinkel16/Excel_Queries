library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)

rm(list=ls())

setwd('~/DATA-331-OD/R/ExcelQueries')

course <- read_excel('Course.xlsx', .name_repair = 'universal')
registration <- read_excel('Registration.xlsx', .name_repair = 'universal')
student <- read_excel('Student.xlsx', .name_repair = 'universal')

student_info <- registration %>%
  left_join(student, by = c('Student.ID')) %>%
  left_join(course, by = c('Instance.ID')) %>%
  dplyr::mutate(Quarter = quarters(Start.Date))

compSciStudents <- student_info %>%
  dplyr::filter(Title=='Computer Science')

paymentPlanStudents <- student_info %>%
  dplyr::filter(Payment.Plan=='TRUE')

firstQuarterClasses <- student_info %>%
  dplyr::filter(Quarter=='Q1') 
  
totalBalanceByPlan <- student_info %>%
  select(Balance.Due, Payment.Plan) %>%
  group_by(Payment.Plan) %>%
  summarise(totalBalance = sum(Balance.Due))

totalCostByMajor <- student_info %>%
  select(Title, Cost) %>%
  group_by(Title) %>%
  summarise(totalCost = sum(Cost))


ggplot(totalCostByMajor, aes(x=Title, y=totalCost)) +
  geom_bar(stat="identity")

