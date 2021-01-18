library(tidyverse)
library(ggthemes)

setwd('c:/R/data/')

inji <- read.csv('inji.csv', stringsAsFactors = TRUE)

colnames(inji) <- c('div', '2017', '2019')

inji <- inji %>% gather(year, value, `2017`, `2019`)


inji %>% ggplot(aes(x = fct_reorder(div, value), y = value)) + 
  geom_point(aes(color = year, shape = year), size = 3) + 
  coord_flip() + 
  geom_path(arrow = arrow(type = 'closed', angle = 30, length = unit(0.1, "inches")), color = 'black') +
  geom_text(aes(x = div, y = value, label = value), vjust = 1.5) + 
  labs(y = '인지적 성과의 \'향상됨\'이상 응답 비율(일반대)',
       x = NULL, 
       caption = '출처 : 남신동, 대학의 교수·학습 질 제고 전략 탐색 연구(VII), 한국교육개발원, 2019') +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_color_manual(values = c('darkgreen', 'red'), name = '연도') +
  scale_shape_manual(values = c('circle', 'square'), name = '연도')
  

ability <- read.csv('ability.csv', stringsAsFactors = TRUE)

colnames(ability) <- c('div', '2019', '2020')

ability <- ability %>% gather(year, value, `2019`, `2020`)


ability %>% ggplot(aes(x = fct_relevel(fct_reorder(div, value), '전체', after = 9), y = value)) + 
  geom_point(aes(color = year, shape = year), size = 3) + 
  coord_flip() + 
  geom_path(arrow = arrow(type = 'closed', angle = 30, length = unit(0.1, "inches")), color = 'black') +
  geom_text(aes(x = div, y = value, label = value), vjust = 1.5) + 
  labs(y = '대학교육을 통한 능력향상(4점 척도)',
       x = NULL, 
       caption = '출처 : 김은영, 대학의 교수·학습 질 제고 전략 탐색 연구(VIII), 한국교육개발원, 2020') +
#  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_color_manual(values = c('darkgreen', 'red'), name = '연도') +
  scale_shape_manual(values = c('circle', 'square'), name = '연도')


ability_origin <- read.csv('ability.csv', stringsAsFactors = TRUE)
colnames(ability_origin) <- c('div', '2019', '2020')
ability_origin %>% mutate(diff = `2020` - `2019`) %>% arrange(desc(diff))


inji_origin <- read.csv('inji.csv', stringsAsFactors = TRUE)
colnames(inji_origin) <- c('div', '2017', '2019')
inji_origin %>% mutate(diff = `2019` - `2017`) %>% arrange(desc(diff))


