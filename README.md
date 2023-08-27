---
# title: "Cyclistic 2022 Case Study"
author: "Leopoldine Mirtil"
date: "`r Sys.Date()`"
output: rmarkdown::github_document
---

```{r setup, include=FALSE, echo = FALSE}
knitr::opts_chunk$set(
  fig.path = "README_figs/README-"
)
```


### Disclaimer

This analysis is from the Cyclistic Bike Share Case Study: How Does a Bike-Share Navigate Speedy Success? as part of the Google Data Analytics Certificate Capstone Project. The trip data has been made publicly available by Motivate International Inc. license  [here](https://ride.divvybikes.com/data-license-agreement). The data was downloaded as part of the 2022 monthly Divvy trip data set from this [link here](https://divvy-tripdata.s3.amazonaws.com/index.html). 


## Introduction

### Scenario

You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

### Task
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?


## Let's Get to Work

### Step 1 - Import Data

#### Load Packages

```{r, message=FALSE}
library(dplyr)
library(ggplot2)
library(knitr)
library(lubridate)
library(tidyr)
library(tidyverse)
```

#### Set Directory and Import Data Files

```{r}
setwd("C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Cyclistic Case Study/Cyclistic2022/00_raw_data")

m1_2022 <- read.csv("202201-divvy-tripdata.csv")
m2_2022 <- read.csv("202202-divvy-tripdata.csv")
m3_2022 <- read.csv("202203-divvy-tripdata.csv")
m4_2022 <- read.csv("202204-divvy-tripdata.csv")
m5_2022 <- read.csv("202205-divvy-tripdata.csv")
m6_2022 <- read.csv("202206-divvy-tripdata.csv")
m7_2022 <- read.csv("202207-divvy-tripdata.csv")
m8_2022 <- read.csv("202208-divvy-tripdata.csv")
m9_2022 <- read.csv("202209-divvy-publictripdata.csv")
m10_2022 <- read.csv("202210-divvy-tripdata.csv")
m11_2022 <- read.csv("202211-divvy-tripdata.csv")
m12_2022 <- read.csv("202212-divvy-tripdata.csv")
```


### Step 2 - Combine Data Sets into Single Data Frame 

```{r}
total_trips <- bind_rows(m1_2022, m2_2022, m3_2022, m4_2022, m5_2022, m6_2022, m7_2022, m8_2022, m9_2022, m10_2022, m11_2022, m12_2022)

str(total_trips)  #inspect new data frame
```

#### Set Working Directory and Export File

```{r}
setwd("C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Cyclistic Case Study/Cyclistic2022/01_tidy_data")
#export copy of new data frame just in case
write.csv(total_trips, "total_trips.csv", row.names = FALSE)
```


### Step 3 - Add New Columns

```{r}
total_trips$date <- as.Date(total_trips$started_at)  #The default format is yyyy-mm-dd
total_trips$month <- format(as.Date(total_trips$date), "%m")  #month of start date
total_trips$day_of_week <- format(as.Date(total_trips$date), "%A")  #full name of the day of week of start date(Sunday, Saturday)
total_trips$duration <- difftime(total_trips$ended_at,total_trips$started_at)  #ride length in seconds
```

### Step 4 - Cleaning Process 

#### Inspect Data Frame

```{r}
str(total_trips) 
```

#### Create New Data Frame and Export File 

```{r}
setwd("C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Cyclistic Case Study/Cyclistic2022/01_tidy_data")

total_tripsv2 <- total_trips
write.csv(total_tripsv2, "total_tripsv2.csv", row.names = FALSE)
```

#### Change Data Type of Column

```{r}
#remove seconds for calculations
total_tripsv2$duration <- as.numeric(total_tripsv2$duration)

# confirm data type change
str(total_tripsv2$duration)
```

#### Remove Negative Values from 'duration' column

```{r}
total_tripsv2 <- total_tripsv2[!(total_tripsv2$duration<0),]
```

#### Remove Unneeded Columns

```{r}
total_tripsv2 <- total_tripsv2[, -9:-12] #removing 4 columns: start_lat, start_lng, end_lat, end_lng 
```

#### Rename columns

```{r}
total_tripsv2 <- rename(total_tripsv2, bike_type=rideable_type, rider_type=member_casual)
```

#### Export Final Modified Data Frame

```{r, results='hide'}
setwd("C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Cyclistic Case Study/Cyclistic2022/01_tidy_data")
# export updated data frame
write.csv(total_tripsv2, "total_tripsv2.csv", row.names = FALSE)
```


## Descriptive Analysis

#### Summary Duration by Rider Type 

```{r, message=FALSE}
total_tripsv2 %>%                            
  group_by(rider_type) %>%
    summarize(min = min(duration), 
    q1 = quantile(duration, 0.25), 
    median = median(duration), 
    mean = sprintf("%.2f", mean(duration)), 
    q3 = quantile(duration, 0.75), 
    max = max(duration)) 
```


#### Summary Duration by Rider & Bike Types 

```{r, message=FALSE}
total_tripsv2 %>%                            
  group_by(rider_type, bike_type) %>%
    summarize(min = min(duration), 
    q1 = quantile(duration, 0.25), 
    median = median(duration), 
    mean = sprintf("%.2f", mean(duration)),  
    q3 = quantile(duration, 0.75), 
    max = max(duration))
```

#### Total Trip Count by Rider Type

```{r}
total_tripsv2 %>% count(rider_type, sort = TRUE)
```

#### Total Trip Count by Bike Type

```{r}
total_tripsv2 %>% count(bike_type, sort = TRUE)
```


#### Total Trip Count by Bike & Rider Types

```{r}
total_tripsv2 %>% count(bike_type, rider_type, sort = TRUE)
```


#### Total Weekday Ride Count by Rider Type 

```{r, message=FALSE}
total_tripsv2$day_of_week <- ordered(total_tripsv2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
total_tripsv2 %>%                            
  group_by(rider_type, day_of_week) %>%
    summarize(number_of_rides = n()) %>%
    arrange(day_of_week, rider_type) 
```


#### Average Weekday Ride Duration by Rider Type

```{r, message=FALSE}
total_tripsv2$day_of_week <- ordered(total_tripsv2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
total_tripsv2 %>%                            
  group_by(rider_type, day_of_week) %>%
    summarise(average_duration = sprintf("%.2f", mean(duration))) %>%
    arrange(day_of_week, rider_type) 
```


#### Total Monthly Number of Rides by Rider Type

```{r, message=FALSE}
total_tripsv2 %>%                            
  group_by(rider_type, month) %>%
    summarise(number_of_rides = n()) %>%
    arrange(month, rider_type) %>%
    print(n=24)
```


#### Top 5 Destinations by Riders

```{r, message=FALSE, error=FALSE}
top_5_stations <- total_tripsv2 %>%
  group_by(rider_type, end_station_name) %>%
    summarize(number_of_rides = n()) %>%
    arrange(rider_type, desc(number_of_rides)) %>%
    slice(1:6)
top_5_stations <- top_5_stations[!(is.na(top_5_stations$end_station_name) | top_5_stations$end_station_name==""),]  #remove blank field and values
print(top_5_stations)
``` 

```{r, echo=FALSE}
# export table for future use
setwd("C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Cyclistic Case Study/Cyclistic2022/01_tidy_data")
write.csv(top_5_stations, "top_5_stations.csv", row.names = FALSE)
```



## Plots and Visualizations

#### Preferred Bikes among Riders

```{r, echo=FALSE}
total_tripsv2$bike_type <-factor(total_tripsv2$bike_type,(levels =c("docked_bike", "classic_bike", "electric_bike"))) 
total_tripsv2 %>% 
  count(bike_type, rider_type, sort = TRUE) %>%
  group_by(rider_type) %>%
  mutate(pct= prop.table(n) * 100) %>%
    arrange(bike_type) %>%
      ggplot(aes(fill=bike_type, y=pct, x=rider_type)) +
        geom_bar(stat = "identity") +
        ylab("Percentage of Rides") +
          geom_text(aes(label=paste0(sprintf("%1.2f", pct),"%")),
            position=position_stack(vjust=0.5), color = "white") 
```

The electric bike is the most popular bike among casual riders. The preferred bike type among annual members is the classic bike, with the electric bike close behind. The docked bike was only used by casual riders, but made up a small percentage of their total ride count.


#### Total Weekday Ride Count

```{r, echo=FALSE, message=FALSE}
options(scipen=999) #remove scientific notation from y-axis
library(scales) # load pkg to scale y-axis

total_tripsv2 %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(rider_type, weekday) %>%
    summarize(number_of_rides = n()) %>%
    arrange(rider_type, weekday) %>%
ggplot(mapping = aes(x=weekday, y=number_of_rides, fill=rider_type)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) # add commas to y-axis
```


Annual members consistently had a larger number of rides Monday to Friday, with the maximum number of rides on occurring on Thursday. On the weekends, casual riders had a higher number of riders than members, with the highest amount occurring on Saturday.


#### Average Weekday Trip Duration

```{r, echo=FALSE, message=FALSE}
total_tripsv2 %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(rider_type, weekday) %>%
    summarise(average_duration = mean(duration)) %>%
    arrange(rider_type, weekday) %>%
ggplot(mapping = aes(x=weekday, y=average_duration, fill=rider_type)) + 
  geom_col(position = "dodge") + 
  ylab("average_duration(in seconds)") +
  scale_y_continuous(labels = scales::comma) 
```


The average ride duration during the week among casual riders was consistently twice the length of member riders during the week. The average duration among both rider types is highest on the weekends. Meanwhile, the average ride duration among annual members was steadily under 1,000 seconds throughout the week. 


#### Monthly Ride Count 

```{r, echo=FALSE, message=FALSE}
total_tripsv2 %>% 
  mutate(months = month(started_at, label=TRUE)) %>%
  group_by(rider_type, months) %>%
    summarise(number_of_rides = n()) %>%
      arrange(rider_type, months) %>%
ggplot(aes(x = months, y = number_of_rides, color = rider_type, group = rider_type)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) + 
      scale_y_continuous(labels = scales::comma) 
```


Annual members consistently had a higher number of rides across the year when compared to casual riders. The maximum number of rides occurred in July for casual riders and in August for members. There is a noticeable drop in the number of rides after July among casual riders and after August among annual members. This corresponds with the upcoming fall and winter seasons. The maximum amount of rides for both rider types occurs during the spring and summer seasons. 


#### Top 5 End Station Destinations

```{r, message=FALSE, echo=FALSE}
top_5_stations$end_station_name <- factor(top_5_stations$end_station_name, (levels=c("Clinton St & Washington Blvd", "University Ave & 57th St", "Wells St & Concord Ln","Clark St & Elm St", "Kingsbury St & Kinzie St", "DuSable Lake Shore Dr & North Blvd", "Michigan Ave & Oak St", "Millennium Park", "DuSable Lake Shore Dr & Monroe St","Streeter Dr & Grand Ave")))
top_5_stations %>%
  ggplot(aes(x=rider_type, y=number_of_rides, fill=end_station_name, label=number_of_rides)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label=format(number_of_rides, big.mark = ",")), position=position_stack(vjust=0.5), color="white") +
      scale_y_continuous(labels = scales::comma)
```


The top 5 destinations (end stations) among both riders are different locations. The Streeter Dr & Grand Ave station is the top destination among casual riders while for member riders it is the Clark St & Elm St station. The number of member rides to the destinations is more consistent compared to casual riders. 


### Recommendations


**Increase Electric Bikes**

Replace docked bikes with new electric bikes to bring in more casual riders. Since the electric bike is the most popular bike type among casual riders, increasing the amount available at stations will help bring in more casual riders and convert them to members.This would benefit current annual members as this would increase the number of available bikes to them as well. The company can advertise the increase in supply to attract and convert climate-conscious riders into members.


**Membership Specials**

Offer new membership specials that would offer membership to casual riders at a discount. Since there is a marked increase in the number of casual riders on the weekend and approaching the summer from May to July, it would be  advantageous to offer Weekend, Spring and Summer Specials. The company can also show highlight perks of membership through digital media to influence casual riders to become members. This can include posting member testimonials, ads highlighting the benefits of the bike rides (save time or money, enjoy the sites, get in shape, etc.) 


**Advertise Top 5 Destinations**

Use digital media and ads near start stations to advertise the top 5 end stations used by riders. Since the top 5 station destinations of casual riders is different from those of members, the advertisements will help attract more casual riders and convert them to membership. 


