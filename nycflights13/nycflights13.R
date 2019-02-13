library(nycflights13)
library(tidyverse)
data("flights")
flights %>% glimpse()
flights <- flights %>% drop_na()

summary(flights)

#Q1
flights %>% filter(origin == "EWR", dest == "ATL") %>% summarise(mean = mean(arr_delay))

#Q2
(flights %>% filter(origin == "EWR") %>% group_by(carrier) %>%  summarise(avg_dep_delay = mean(dep_delay)) %>% arrange(avg_dep_delay))[1:3,]

#Q3
(flights %>% filter(origin == "EWR") %>% group_by(hour) %>% summarise(avg_dep_delay = mean(dep_delay)) %>% arrange((avg_dep_delay)))

#Q4
# data_mat <- (flights %>% filter(origin == "JFK")%>% count(month))
# data_mat[data_mat[,2]==max(data_mat[,2]),] [1]
((flights %>% filter(origin == "JFK")%>% count(month)) %>% filter(n==max(n)))
