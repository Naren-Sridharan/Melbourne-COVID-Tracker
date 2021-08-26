library(RSocrata)
library(httr)
library(ggplot2)
library(mi)
library(gridExtra)
library(leaflet)

date_fun <- function(d) {return(as.Date(d, origin='1970-01-01'))}

token <- "GEqZLQSLJw1JgqEUGXlbsHq2y"
melb_pedestrian_counts <- read.socrata("https://data.melbourne.vic.gov.au/resource/b2ak-trbp.json?$select=sensor_id, sensor_name, date_trunc_ymd(date_time) AS date, SUM(hourly_counts) as daily_pedestrian_counts&$where=date > '2020-01-22'&$group=sensor_id, sensor_name, date&$order=sensor_id" ,app_token = token)
melb_pedestrian_counts$date <- sapply(melb_pedestrian_counts$date, FUN=date_fun)
melb_ped_cnt_sensors <- read.socrata("https://data.melbourne.vic.gov.au/resource/h57g-5234.json?$select=sensor_id,latitude,longitude&$where=installation_date <= '2020-01-22T00:00:00.000' AND status = 'A'",app_token = token)

merged_ped_data <- merge.data.frame(melb_ped_cnt_sensors, melb_pedestrian_counts, by="sensor_id")

covid_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = TRUE)
covid_data <- covid_data[covid_data$Country=="Australia",]
covid_data <- stack(aggregate.data.frame(covid_data[, -c(1:4)], by=list(Country=covid_data[,"Country.Region"] == "Australia"), FUN=sum)[, -1])

colnames(covid_data) <- c('daily_case_counts', 'date')
covid_data$date <- sapply(X=levels(covid_data$date), FUN=(function (dt) {
  if (startsWith(x = dt, prefix =  'X')){
    dt <- paste0('0', substring(text = dt, first =  2))
  }
  return(as.Date(x = dt, format="%m.%d.%y", origin='1970-01-01'))
}))
covid_data$daily_case_counts[-1] <- covid_data$daily_case_counts[-1] - head(covid_data$daily_case_counts, -1)

image(missing_data.frame(merged_ped_data))
image(missing_data.frame(covid_data))

city_ped_counts <- aggregate.data.frame(x = merged_ped_data[, 6], by=list(date=merged_ped_data$date), FUN=sum)
colnames(city_ped_counts) <- c("date", "daily_pedestrian_counts")
merge_ped_counts = merge(city_ped_counts, covid_data, by="date")

merge_ped_counts$date <- Reduce(c, lapply(merge_ped_counts$date, FUN=date_fun))
covid_data$date <- Reduce(c, lapply(covid_data$date, FUN=date_fun))
merged_ped_data$date <- Reduce(c, lapply(merged_ped_data$date, FUN=date_fun))
merged_ped_data$sensor_id <- sapply(merged_ped_data$sensor_id, FUN=as.numeric)
merged_ped_data$latitude <- sapply(merged_ped_data$latitude, FUN=as.numeric)
merged_ped_data$longitude <- sapply(merged_ped_data$longitude, FUN=as.numeric)

sensor_names <- unique(merged_ped_data$sensor_name)
size_fun <- function (v, y) { return(findInterval(v, seq(1000, 50000, by=1000))) }

bubble_plot <- function(data) {
  return(leaflet(data) %>% addProviderTiles(providers$OpenStreetMap) %>%
           addCircleMarkers(~longitude, ~latitude, popup = ~as.character(longitude), radius=~size_fun(daily_pedestrian_counts))
  )
}

data <- merged_ped_data[merged_ped_data$date=='2020-03-15',]
bubble_plot(data)
data <- merged_ped_data[merged_ped_data$date=='2020-03-30',]
bubble_plot(data)
data <- merged_ped_data[merged_ped_data$date=='2020-04-15',]
bubble_plot(data)
data <- merged_ped_data[merged_ped_data$date=='2020-04-29',]
bubble_plot(data)


grid.arrange(qplot(sample = sensor_id, data = merged_ped_data),
            qplot(sample = latitude, data = merged_ped_data),
            qplot(sample = longitude, data = merged_ped_data),
            qplot(sample = date, data = merged_ped_data),
            qplot(sample = daily_pedestrian_counts, data = merged_ped_data),
            qplot(sample = daily_case_counts, data = covid_data),
            ggplot(merged_ped_data) + geom_boxplot(aes(x=sensor_id)),
            ggplot(merged_ped_data) + geom_boxplot(aes(x=latitude)),
            ggplot(merged_ped_data) + geom_boxplot(aes(x=longitude)),
            ggplot(merged_ped_data) + geom_boxplot(aes(x=date)),
            ggplot(merged_ped_data) + geom_boxplot(aes(x=daily_pedestrian_counts)),
            ggplot(covid_data) + geom_boxplot(aes(x=daily_case_counts))
, ncol=6, nrow=2, layout_matrix=rbind(c(1:6),c(7:12)))

ggplot(merge_ped_counts, aes(x=daily_case_counts, y=daily_pedestrian_counts)) + geom_point() + geom_smooth()
ggplot(merge_ped_counts, aes(x=date, y=daily_pedestrian_counts)) + geom_col() + geom_smooth()
ggplot(merge_ped_counts, aes(x=date, y=daily_case_counts)) + geom_col() + geom_smooth()
ggplot(covid_data, aes(x=date, y=daily_case_counts)) + geom_col() + geom_smooth()

