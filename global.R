library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(glue) 
library(scales) 
library(leaflet)
library(ggwordcloud)
library(lubridate)
library(ggcorrplot)
library(reshape2)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(DT)
library(tm)
library(slam)
library(geojsonio)

airbnb <- read_csv("AB_NYC_2019.csv")

airbnb_clean <- airbnb %>% 
                  drop_na(name, host_name) %>% 
                  mutate(reviews_per_month = replace_na(data = reviews_per_month, replace = 0),
                  last_review = replace_na(data = as.character(last_review), replace = "EMPTY"))

airbnb_clean <- airbnb_clean %>% 
                    mutate(price_group=ifelse(price < 61, "Very Low",
                                          ifelse(price < 91, "Low",
                                          ifelse(price < 150, "Moderate",
                                          ifelse(price < 300, "High", "Very High"
                                          ))))) %>% 
                    mutate(reviews_group=ifelse(number_of_reviews < 1, "None",
                                          ifelse(number_of_reviews < 4, "Low",
                                          ifelse(number_of_reviews < 10, "Moderate",
                                          ifelse(number_of_reviews < 30, "High", "Very High"
                                          ))))) %>% 
                    mutate(est_revenue = price * number_of_reviews * median(minimum_nights))

airbnb_clean_1000 <- airbnb_clean %>% 
  filter(price < 1000) 

airbnb_pvt <- airbnb_clean %>% 
                 filter(room_type == "Private room")
  
airbnb_apt <- airbnb_clean %>% 
                filter(room_type == "Entire home/apt")

airbnb_shr <- airbnb_clean %>% 
                filter(room_type == "Shared room")

airbnb_mn_45 <- airbnb_clean %>% 
  filter(minimum_nights < 45)

airbnb_corr <- airbnb_clean %>% 
  select(price, minimum_nights, number_of_reviews, reviews_per_month, availability_365) %>% 
  set_names("price", "minimum nights", "number of reviews", "reviews per month", "availability") %>% 
  cor()
airbnb_corr_del <- function(data) {
  data[upper.tri(data)] <- NA
  return(data)
}
airbnb_corr_clean <- airbnb_corr_del(airbnb_corr)
airbnb_corr_clean_melt <- melt(airbnb_corr_clean)

host_agg_overview <- airbnb_clean %>% 
                        group_by(host_id,host_name) %>% 
                        summarise(est_total_revenue = sum(est_revenue),
                                  total_properties = length(id)) %>% 
                        arrange(desc(est_total_revenue))

top_40 <-  host_agg_overview[1:40,]

host_rev_10000  <- airbnb_clean %>% 
                        group_by(host_id,host_name, latitude, longitude, room_type, price, price_group, est_revenue, minimum_nights, number_of_reviews, reviews_group, neighbourhood_group) %>% 
                        summarise(est_total_revenue = sum(est_revenue)) %>% 
                        filter(est_revenue>100000) %>% 
                        arrange(desc(est_total_revenue))

m <- list(t = 50,
          b = 50,
          l = 50,
          r = 50)

theme <- theme(legend.key = element_rect(fill="black"),
               legend.background = element_rect(color="white", fill="#263238"),
               plot.subtitle = element_text(size=6, color="white"),
               panel.background = element_rect(fill="#dddddd"),
               panel.border = element_rect(fill=NA),
               panel.grid.minor.x = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(color="darkgrey", linetype=2),
               panel.grid.minor.y = element_blank(),
               plot.background = element_rect(fill="#263238"),
               text = element_text(color="white"),
               axis.text = element_text(color="white")
               )