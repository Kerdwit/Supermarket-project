# Customer Analytics Project : Exploratory data analysis and Customer segmentation
# By Jaruwit Kerdphra
# Load any packages for this project==============

library(tidyverse)    # for transform and visualize data before model
library(lubridate)    # for manipulate date-value 
library(scales)       # for add dollar-sign($) on chart
library(ggthemes)     # for modify background on ggplot2's graphic
library(cowplot)      # for combine many chart in 1 page
library(skimr)        # get the numeric summaries
library(ggsci)        # for change color palette

theme_set(theme_minimal())    # set theme for all ggplot2's chart

# Import data -------------------------------------------------------------
raw_data <- read_csv('supermarket.csv')
raw_data %>% head(10)   # show the first 10 rows of data
raw_data %>% tail(10)   # show the bottom 10 rows of data

# Explore data ----------------------------------------------------
# What's in my data
# What's happenning in my data

# transform data before analyze
data<-
  raw_data %>% 
    mutate(year  = as.integer(substr(as.character(SHOP_DATE),1,4)),       # Extract year value from date value
           month = as.integer(substr(as.character(SHOP_DATE),5,6)),       # Extract month value from date value 
           day   = as.integer(substr(as.character(SHOP_DATE),7,8)),       # Extract day value from date value
           date  = make_date(year,month,day),                             # Convert date value in dbl type to date type
           wday  = wday(date,label = TRUE)) %>%                           # Extract day of week from date value
    select(date,year:day,wday,SHOP_HOUR:SPEND)                            # relocate column in tabular data

# Split dataset
product<-
  data %>% 
  filter(year==2007) %>% 
  select(year:SPEND)

# this tell me the dataset contain transaction sale 2007 and the 1st half-year 2008
data %>% 
  group_by(year,month) %>% 
  group_keys()

data %>% 
  group_by(STORE_CODE,wday) %>% 
  group_keys()

# Explore the revenue of supermarket's branch in different time period -----------------------
# Explore the revenue each of supermarket's branchs
# Create week, month and quarter of the year
data<-
  data %>% 
    mutate(week = week(date),                                                 # Extract week of year from date value
           month_name = month(date,label = TRUE),                             # Extract month's name from date value
           quarter = quarter(date)) %>%                                       # Extract quarter of year from date value
    rename(hour = SHOP_HOUR) %>%                                              # Change column's name of hour of day variable
    select(date:month,month_name,day,wday,quarter,week,hour,BASKET_ID:SPEND)  # relocate column in tabular data


# Find revenue per quarter of a supermarket's branch
# line chart
revenue_quarter<-
data %>%
  filter(year == 2007) %>% 
  group_by(STORE_CODE,quarter) %>% 
  summarise(revenue = sum(SPEND)) %>% 
  ggplot() +
  geom_line(aes(x=quarter,y=revenue,color=STORE_CODE),size=1) +
  geom_point(aes(x=quarter,y=revenue,color=STORE_CODE),size=2) +
  scale_color_manual(values=c("red", "lightgreen","darkgreen","blue"),name = "branch", labels = c("1", "2", "3","4")) +
  scale_y_continuous(labels=dollar_format()) +
  scale_x_continuous(labels = c('Q1','Q2','Q3','Q4')) +
  labs(title = "The revenue of a supermarket's branch on 2007",
       subtitle = "Revenue per quarter ") +
  theme(legend.position = "bottom",legend.title=element_text(size=14))

# Find revenue per month of a supermarket's branch
# line chart
revenue_month<-
data %>%
  filter(year == 2007) %>% 
  group_by(STORE_CODE,month_name) %>% 
  summarise(revenue = sum(SPEND)) %>% 
  ggplot() +
  geom_line(aes(x = month_name, y = revenue, group = STORE_CODE, color = STORE_CODE), size = 1) +
  geom_point(aes(x = month_name, y = revenue, group = STORE_CODE, color = STORE_CODE), size = 2) +
  scale_color_manual(values=c("red", "lightgreen","darkgreen","blue"),name = "branch", labels = c("1", "2", "3","4")) +
  scale_y_continuous(labels=dollar_format()) +
  labs(title = "The revenue of a supermarket's branch on 2007",
       subtitle = "Revenue per month",
       x = "month") +
  theme(legend.position = "bottom",legend.title=element_text(size=14)) 


# Find revenue per week of year  of a supermarket's branch
# line chart
revenue_weekofyear<-
data %>%
  filter(year == 2007,week <=52) %>% 
  group_by(STORE_CODE,week) %>% 
  summarise(revenue = sum(SPEND)) %>% 
  ggplot() +
  geom_line(aes(x = week, y = revenue, color = STORE_CODE), size = 1) +
  geom_point(aes(x = week, y = revenue, color = STORE_CODE), size = 2) +
  scale_color_manual(values=c("red", "lightgreen","darkgreen","blue"),name = "branch", labels = c("1", "2", "3","4")) +
  scale_y_continuous(labels=dollar_format()) +
  labs(title = "The revenue of a supermarket's branch on 2007",
       subtitle = "Revenue per week of year") +
  theme(legend.position = "bottom",legend.title=element_text(size=14))
  

# Find revenue per day  of a supermarket's branch
# line chart
revenue_day<-
data %>%
  filter(year == 2007) %>% 
  group_by(STORE_CODE,day) %>% 
  summarise(revenue = sum(SPEND)) %>% 
  ggplot() +
  geom_line(aes(x = day, y = revenue, color = STORE_CODE), size = 1) +
  geom_point(aes(x = day, y = revenue, color = STORE_CODE), size = 2) +
  scale_color_manual(values=c("red", "lightgreen","darkgreen","blue"),name = "branch", labels = c("1", "2", "3","4")) +
  scale_y_continuous(labels=dollar_format()) +
  labs(title = "The revenue of a supermarket's branch on 2007",
       subtitle = "Revenue per day") +
  theme(legend.position = "bottom",legend.title=element_text(size=14))

# Find revenue per day(7-days)  of a supermarket's branch
# line chart
revenue_dayofweek<-
data %>%
    filter(year == 2007) %>% 
    group_by(STORE_CODE,wday) %>% 
    summarise(revenue = sum(SPEND)) %>% 
    ggplot() +
    geom_line(aes(x = wday, y = revenue, group = STORE_CODE, color = STORE_CODE), size = 1) +
    geom_point(aes(x = wday, y = revenue, group = STORE_CODE, color = STORE_CODE), size = 2) +
    scale_color_manual(values=c("red", "lightgreen","darkgreen","blue"),name = "branch", labels = c("1", "2", "3","4")) +
    scale_y_continuous(labels=dollar_format()) +
    labs(title = "The revenue of a supermarket's branch on 2007",
         subtitle = "Revenue per day",
         x = "day") +
    theme(legend.position = "bottom",legend.title=element_text(size=14))

# Find revenue per hour  of a supermarket's branch
# line chart
revenue_hour<-
data %>% 
  filter(year==2007) %>% 
  group_by(STORE_CODE,hour) %>% 
  summarise(revenue = sum(SPEND)) %>% 
  ggplot() +
  geom_line(aes(x = hour, y = revenue, color = STORE_CODE), size = 1) +
  geom_point(aes(x = hour, y = revenue, color = STORE_CODE), size = 2) +
  scale_color_manual(values=c("red", "lightgreen","darkgreen","blue"),name = "branch", labels = c("1", "2", "3","4")) +
  scale_y_continuous(labels=dollar_format()) +
  labs(title = "The revenue of a supermarket's branch on 2007",
       subtitle = "Revenue per hour",
       x = "hour") +
  theme(legend.position = "bottom",legend.title=element_text(size=14))

# Show revenue trend in different time period in one page
plot_grid(revenue_quarter,
          revenue_month,
          revenue_weekofyear,
          revenue_day,
          revenue_dayofweek,
          revenue_hour,
          ncol = 3, nrow = 2)
# Explore active-members customer ------------------------------------------
# Count of active-members customer per supermarket's branch
data %>% 
  filter(year==2007) %>% 
  group_by(STORE_CODE) %>% 
  summarise(number_of_customer = n_distinct(CUST_CODE))

# Customer single view  ----------------------------
# Reference from https://www.medium.com/@thanachart.rit/building-customer-single-view-customer-360-3539c971092c

# begin make data of customer single view
data %>% 
  filter(year == 2007) %>% 
  group_by(CUST_CODE) %>% 
  summarise(last_visit = max(date),                  
            frequency = n_distinct(BASKET_ID),     
            monetary = sum(SPEND))

# I transform SQL to R-code from medium.com/@thanachart.rit/building-customer-single-view-customer-360-3539c971092c
rfm<-
data %>% 
  filter(year == 2007) %>%                               # use transaction history on 2007 only
  mutate(week = replace(week,week==53,52)) %>%           # combine week's 53 and week's 52 in the same value------this data have 53 weeks (2007-12-31) 
  group_by(CUST_CODE) %>% 
  summarise(last_visit = max(date),
            last_week = max(week),
            total_visit = n_distinct(BASKET_ID),
            total_spend = sum(SPEND),
            avg_weekly_visit = total_visit/n_distinct(week),
            avg_weekly_spend = total_spend/n_distinct(week),
            avg_basket_size = sum(SPEND)/n_distinct(BASKET_ID)
            ) 

# the purchase table contains variable about purchase evolution
purchase<-
data %>% 
  filter(year == 2007) %>%
  mutate(week = replace(week,week==53,52)) %>%
  group_by(CUST_CODE,week) %>% 
  summarise(basket_size = sum(SPEND)) %>%
  ungroup() %>% 
  group_by(CUST_CODE) %>%                                                
  mutate(pre_1basket_size = lag(basket_size, n=1L, order_by = week),       # as LAG(basket_size,1) OVER(PARTITION BY CUST_CODE ORDER BY week)
         pre_2basket_size = lag(basket_size, n=2L, order_by = week)) %>%   # as LAG(basket_size,2) OVER(PARTITION BY CUST_CODE ORDER BY week)
  ungroup() 

# Combine between RFM table and purchase table
customer_view<-
rfm %>% 
  inner_join(purchase,by = c("CUST_CODE","last_week" = "week")) %>% 
  select(CUST_CODE,                              # active-members customer id
         last_visit,                             # last-date active-members customer purchased product  
         total_visit,                            # total number of active-members customer visit at supermarket in one year
         total_spend,                            # total money of active-members customer spend to supermarket in one year 
         avg_weekly_visit,                       # the average of total number of active-members customer visit per week 
         avg_weekly_spend,                       # the average of total money of active-members customer purchase product per week
         avg_basket_size,                        # the average of total money of active-members customer purchase product per basket id
         basket_size,
         pre_1basket_size,
         pre_2basket_size) %>% 
  rename(last_basket_size = basket_size)

# Customer data   ------------------------------

# In this data set: some active-members customers purchase product more than one branch
# total money before cut customer
data %>% 
  filter(year==2007) %>% 
  group_by(STORE_CODE) %>% 
  summarise(count_customer = n_distinct(CUST_CODE),
            count_basket = n_distinct(BASKET_ID),
            revenue = sum(SPEND))

# the active-members customer id purchase purchase product from one branch only
customer_view %>% 
  anti_join(
  data %>% 
    filter(year==2007) %>% 
    group_by(CUST_CODE) %>% 
    summarise(count_product = n_distinct(PROD_CODE),
              count_store = n_distinct(STORE_CODE),
              count_day = n_distinct(week),
              count_month = n_distinct(month),
              money = sum(SPEND)) %>% 
    filter(count_store>1) %>% 
    group_by(CUST_CODE) %>% 
    group_keys(), by = "CUST_CODE")

# Cluster analysis by k-mean clustering  -------------------------------------------------------

# Choose appropriate attributes
# Customer's data for modelling
# The attribute in model is total_visit, total_spend, avg_weekly_visit, avg_weekly_spend, avg_basket_size

# active-members customer id purchase product from one branch only
cust_id_one<-
  data %>% 
  filter(year==2007) %>% 
  group_by(CUST_CODE) %>% 
  summarise(count_store = n_distinct(STORE_CODE)) %>% 
  filter(count_store==1) %>%                              # Cut who purchase product more than 1 place (17 persons)
  group_by(CUST_CODE) %>% 
  group_keys()

# Combine 
customer<-
data %>% 
  semi_join(cust_id_one,by= "CUST_CODE") %>% 
  filter(year==2007) %>% 
  group_by(CUST_CODE) %>% 
  summarise(store=STORE_CODE[1]) %>% 
  inner_join(customer_view,by="CUST_CODE") %>% 
  select(CUST_CODE,store,everything())

data_model<-
  customer %>%             
  select(CUST_CODE,store,total_visit:avg_basket_size)

# Exploring attribute with graph numerical summaries -------------
# Visualize distribution/outlier value of numeric variable,
# Find spread of numeric variable
# Scale the data

summary(data_model)
skim(data_model)

# Visualize single quantitative variable

# distribution and outlier of total_spend
# numerical summaries of total_spend
skim(data_model$total_spend)
summary(data_model$total_spend)
outlier_total_spend<-boxplot(log10(data_model$total_spend))$out  # assign outlier value of total_spend

# histogram and density curve of total_spend before transform by log10
ggplot(data_model,aes(total_spend))+
    geom_histogram(aes(y=stat(density)),binwidth = 200, fill = "green")+
    geom_density(col="darkgreen")+
    scale_x_continuous(breaks = scales::breaks_width(1000),labels = dollar_format())

# Transform  distribution to normal distribution by log10
# I can use log10 transform total_spend's distribution to normal distribution b/c no 0 value
  ggplot(data_model) + 
    geom_density(aes(x=total_spend)) +
    scale_x_log10(breaks=c(50,100,1000,2500,5000), labels=dollar) +   
    annotation_logticks(sides="bt")

# distribution and outlier of avg_weekly_spend
skim(data_model$avg_weekly_spend)
summary(data_model$avg_weekly_spend)
outlier_avg_spend<- boxplot(log10(data_model$avg_weekly_spend))$out

# density chart and histogram of avg_weekly_spend before transform by log10
ggplot(data_model,aes(avg_weekly_spend))+
  geom_histogram(aes(y=stat(density)),binwidth = 5, fill = "green")+
  geom_density(col="darkgreen")+
  scale_x_continuous(breaks = scales::breaks_width(10),labels = dollar_format())

# Transform  distribution to normal distribution by log10
# I can use log10 transform avg_weekly_spend's distribution to normal distribution b/c no 0 value
ggplot(data_model) + 
  geom_density(aes(x=avg_weekly_spend)) +
  scale_x_log10(breaks=c(10,25,50,100,200), labels=dollar) +   
  annotation_logticks(sides="bt")

# distribution and outlier of avg_basket_size
skim(data_model$avg_basket_size)
summary(data_model$avg_basket_size)
outlier_avg_basket<- boxplot(log10(data_model$avg_basket_size))$out

# density chart and histogram of avg_basket_size before transform by log10
ggplot(data_model,aes(avg_basket_size))+
  geom_histogram(aes(y=stat(density)),binwidth = 5, fill = "green")+
  geom_density(col="darkgreen")+
  scale_x_continuous(breaks = scales::breaks_width(10),labels = dollar_format())

# Transform  distribution to normal distribution by log10
# I can use log10 transform avg_basket_size's distribution to normal distribution b/c no 0 value
ggplot(data_model) + 
  geom_density(aes(x=avg_basket_size)) +
  scale_x_log10(breaks=c(10,25,50,100,200), labels=dollar) +   
  annotation_logticks(sides="bt")

# pre_processing

test<-
data_model %>% 
  filter(!log10(total_spend) %in% outlier_total_spend,      # filter outlier
         !log10(avg_weekly_spend) %in% outlier_avg_spend,
         !log10(avg_basket_size) %in% outlier_avg_basket) %>%
  mutate(log_total_spend = log10(total_spend),              # log10 transform
         log_weekly_spend = log10(avg_weekly_spend),
         log_basket_size = log10(avg_basket_size)) %>% 
  select(log_total_spend:log_basket_size) %>% scale()

# Check mean/sd of data after standardized
test<-as.tibble(test)
test %>% 
summarise(mean_total_spend = mean(log_total_spend),
          sd_total_spend = sd(log_total_spend),
          mean_weekly_spend = mean(log_weekly_spend),
          sd_weekly_spend = sd(log_weekly_spend),
          mean_basket_size = mean(log_basket_size),
          sd_basket_size = sd(log_basket_size))

test  # test table is data frame for model clustering
# Determine the number of clusters present -----------------------
library(factoextra)
library(NbClust)        # for determining the optimal number of clusters

# Elbow method
elbow<-
fviz_nbclust(test, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")                    # Answer: 4 cluster

# Silhouette method
silhouette<-
fviz_nbclust(test, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")               # Answer: 2 cluster

nb <- NbClust(test, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

nbclust<-fviz_nbclust(nb)   # The result of  NbClust

plot_grid(elbow, silhouette, nbclust, ncol = 3, nrow = 1)

# Obtain k-mean clustering solution====
set.seed(8)

# number of cluster = 2,4 clusters
km_test <- kmeans(test, 2, nstart = 25)
km_test2 <- kmeans(test, 4, nstart = 25)

print(km_test)
print(km_test2)

# from result of k-mean clustering in different number clusters ==> use 4 cluster in customer's data point

customer_4cluster<-
cbind(data_model %>%                                          # combine customer data and cluster
        filter(!log10(total_spend) %in% outlier_total_spend,      
               !log10(avg_weekly_spend) %in% outlier_avg_spend,
               !log10(avg_basket_size) %in% outlier_avg_basket),
        cluster = factor(km_test2$cluster))

customer_4cluster<-as.tibble(customer_4cluster)

ggplot(customer_4cluster) + 
  geom_bar(aes(x=store,fill=cluster),position = "fill") +
  scale_fill_manual(values = c("red","yellow","darkgreen","steelblue")) +
  theme_bw() +
  theme(legend.position = "top")

# Interpret the cluster
fviz_cluster(list(data = test, cluster = km_test2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_bw())
