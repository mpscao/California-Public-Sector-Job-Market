# clear environment 
rm(list=ls())

# use library() command to load packages for use
# (packages already installed from previous scripts)
library(tidyverse)
library(ggplot2)
library(dplyr)


setwd('/Users/darren/Desktop/DataFirst')

dat <- read.csv('/Users/darren/Downloads/OPM_Merged_With_USA_Staffing_Cleaned(2022q2)-4.csv')
dt <- read.csv('/Users/darren/Downloads/Original_Combined_Applications by Demographic Group_OPM_2018-2023.csv')


#dat3 <- dat %>% 
#  group_by(quarter_open) %>%
#  summarise(total_female = sum(opm_female, na.rm = TRUE)) 
dat2 <- filter(dat, agency %in% 'Office of Personnel Management') %>%
  drop_na(Female) %>%
  drop_na(non.white)

test <- dat %>%
  drop_na(Female) %>%
  drop_na(non.white)

total <- test %>% filter(quarter_open %in% c('2017q1', '2017q2', '2017q3', '2017q4','2018q1',
                                                '2018q2','2018q3', '2018q4', '2019q1', '2019q2',
                                                '2019q3', '2019q4', '2020q1', '2020q2', '2020q3',
                                                '2020q4', '2021q1', '2021q2', '2021q3', '2021q4')) %>%
  group_by(quarter_open) %>%
  summarise(total_female = sum(Female),
            total_non_white = sum(non.white),
            total_female_ratio = sum(Female)/sum(Male),
            total_non_white_ratio = sum(non.white)/sum(White))

total2 <- dat2 %>% filter(quarter_open %in% c('2017q1', '2017q2', '2017q3', '2017q4','2018q1',
                                             '2018q2','2018q3', '2018q4', '2019q1', '2019q2',
                                             '2019q3', '2019q4', '2020q1', '2020q2', '2020q3',
                                             '2020q4', '2021q1', '2021q2', '2021q3', '2021q4')) %>%
  group_by(quarter_open) %>%
  summarise(total_female = sum(Female),
            total_male = sum(Male),
            total_gender_ommitted = sum(Gender.Omitted),
            total_white = sum(White),
            total_non_white = sum(non.white),
            total_race_ommitted = sum(Omitted),
            total_applications = sum(total_applications),
            total_female_ratio = sum(Female)/sum(Male),
            total_non_white_ratio = sum(non.white)/sum(White))

write.csv(total2, "summary.csv")

graph_female_ratio <- ggplot(total, aes(x = quarter_open, y = total_female_ratio, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = 'Ratio of Female to Male Applicants from 2017 to 2021',
       x = 'Year', y = 'Ratio') +
  scale_x_discrete(breaks=c("2018q1","2019q1", '2020q1', '2021q1'),
                   labels=c("2018", "2019", '2020', '2021'))+
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
         axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20),
         axis.text.y = element_text(size = 20),
         axis.text.x = element_text(size = 20))

graph_female_ratio



ggsave("Ratio of Female to Male Aplicants from 2017 to 2021.png", width = 15, height = 7, dpi = 400)

graph_non_white_ratio <- ggplot(total, aes(x = quarter_open, y = total_non_white_ratio, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = 'Ratio of Non-White to White Applicants from 2017 to 2021',
       x = 'Year', y = 'Ratio') +
  theme_bw() +
  scale_x_discrete(breaks=c("2018q1","2019q1", '2020q1', '2021q1'),
                   labels=c("2018", "2019", '2020', '2021'))+
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = .5, size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20)) 
graph_non_white_ratio

ggsave("Ratio of Non-White to White Aplicants from 2017 to 2021 Excluding Omitted.png", width = 15, height = 7, dpi = 400)

graph_female_total <- ggplot(total, aes(x = quarter_open, y = total_female, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = 'Total Number of Female Applicants from 2017 to 2021',
       x = 'Year', y = 'Total Applicants') +
  scale_x_discrete(breaks=c("2018q1","2019q1", '2020q1', '2021q1'),
                   labels=c("2018", "2019", '2020', '2021'))+
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20))

graph_female_total

ggsave("Number of Female Applicants from 2017 to 2021.png", width = 15, height = 7,dpi = 400)

graph_nonwhite_total <- ggplot(total, aes(x = quarter_open, y = total_non_white, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = 'Total Number of Non-White Applicants from 2017 to 2021',
       x = 'Year', y = 'Total Applicants') +
  scale_x_discrete(breaks=c("2018q1","2019q1", '2020q1', '2021q1'),
                   labels=c("2018", "2019", '2020', '2021'))+
  scale_y_continuous(labels = scales::comma) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20))

graph_nonwhite_total

ggsave("Number of Non-White Applicants from 2017 to 2021 Excluding Omitted.png", width = 15, height = 7, dpi = 400)

