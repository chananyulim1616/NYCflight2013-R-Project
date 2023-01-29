library(nycflights13)
library(tidyverse)
library(patchwork)

##flight_avg_per_day
flight_avg_per_day <-flights%>%
    select(year,month,day,origin)%>%
    group_by(year,month,day,origin)%>%
    summarise(total = n())%>%
    ungroup()%>%
    mutate(day_name = c(rep(c("Tuesday","Tuesday","Tuesday","Wednesday","Wednesday","Wednesday","Thursday","Thursday","Thursday","Friday","Friday","Friday","Saturday","Saturday","Saturday","Sunday","Sunday","Sunday","Monday","Monday","Monday"),52),"Tuesday","Tuesday","Tuesday"))%>%
    group_by(day_name,origin)%>%
    summarise(count =n(),
              total = sum(total),
              avg = total/count)

ggplot(flight_avg_per_day, aes(day_name,avg))+
    geom_bar(stat = "identity",aes(fill = factor(day_name)))+
    facet_wrap(~origin,ncol =1)+
    ylim(0,500)+
    labs(title = "Average people in each name and each airport 2013",
         x = "days",
         y = "average_people",
         caption = "Source : nycflights13 package",
         fill = "Day\n")+
    theme(plot.title =  element_text(size = 20))

test2 <- flight_avg_per_day %>%
    group_by(day_name) %>%
    summarise(avg = sum(avg)) %>%
    arrange(desc(avg))
test2$day_name <- factor(test2$day_name,
                         levels = test2$day_name)
ggplot(test2, aes(day_name,avg))+
    geom_bar(stat = "identity",aes(fill = factor(day_name)))+
    ylim(0,1200)+
    labs(title = "Average people each name day at LGA airport 2013",
         x = "days",
         y = "average_people",
         caption = "Source : nycflights13 package",
         fill = "Day\n")+
    theme(plot.title =  element_text(size = 20))

##Competition in each airport
carrier_num <- flights %>%
    count(carrier)%>%
    arrange(desc(n))

carrier_num

ggplot(flights,aes(x=reorder(carrier,carrier,
                             function(x)-length(x))))+
    geom_bar()+
    facet_wrap(~origin)+
    labs(title = "flight per airline each airport 2013",
         x = "airlines",
         y = "total flights",
         caption = "Source : nycflights13 package")+
    theme(plot.title =  element_text(size = 20))

##delay
delay <-flights %>%
    filter(origin == "LGA") %>%
    select(4:9,10,15:16)%>%
    mutate(delay = ifelse(arr_delay+dep_delay>0,TRUE,FALSE))%>%
    filter(is.na(delay) == FALSE)%>%
    group_by(carrier) %>%
    summarise(total_flight = n(),
              delay = sum(delay,na.rm = TRUE),
              avg_arr_come =mean(arr_delay+dep_delay,na.rm = TRUE),
              avg_arr_delay =mean(ifelse(arr_delay+dep_delay> 15,arr_delay+dep_delay,0),na.rm = TRUE),
              delay_ratio = delay/total_flight) %>%
    select(1:2,4:6)%>%
    arrange(avg_arr_come,avg_arr_delay)

ggplot(delay,aes(total_flight,avg_arr_delay))+
    geom_point(size = 3)+
    geom_smooth(method = "lm")+
    labs(title = "Corelation about total flight and average delay each airline at LGA airport 2013",
         x = "total_flights",
         y = "averange delay",
         caption = "Source : nycflights13 package")+
    theme(plot.title =  element_text(size = 15))


gr1 <- ggplot(delay)+
    geom_point(data = delay,aes(x = carrier,y = delay_ratio),size = 4,color ="red")+
    geom_line(data = delay,aes(x = carrier,y = delay_ratio),color = "black",size = 1,group = 1)+
    ylim(c(0.1,0.8))+
    labs(title = "Delay Ratio each airline at LGA airport 2013",
         x = "Carrier",
         y = "Delay_Ratio",
         caption = "Source : nycflights13 package")+
    theme(plot.title =  element_text(size = 20))

gr2 <- ggplot(delay,aes(carrier,total_flight))+
    geom_bar(stat = "identity",fill ="#5875e8")+
    labs(title = "Total flight each airlines at LGA airport 2013",
         x = "Carrier",
         y = "Total_Flights",
         caption = "Source : nycflights13 package")+
    theme(plot.title =  element_text(size = 20))

gr1+gr2

##dest_LGA
dest_LGA <- flights %>%
    filter(origin == "LGA") %>%
    group_by(dest)%>%
    summarise(total_flight =n())%>%
    arrange(desc(total_flight))%>%
    head(15)

dest_LGA$dest = factor(dest_LGA$dest,
                          levels =dest_LGA$dest)

ggplot(dest_LGA,aes(dest,total_flight))+
    geom_bar(stat = "identity",aes(fill = factor(dest)))+
    labs(title = "Flight each destination at LGA airport 2013",
         x = "Destination",
         y = "Total_Flights",
         caption = "Source : nycflights13 package",
         fill = "Destination")+
    theme(plot.title =  element_text(size = 20))


##total_plane
tailnum_air <- flights%>%
    filter(origin == "LGA")%>%
    select(carrier,tailnum)%>%
    group_by(carrier)%>%
    summarise(total_plane = n_distinct(tailnum))%>%
    arrange(desc(total_plane))

ggplot(tailnum_air,aes(carrier,total_plane))+ 
    geom_bar(stat="identity",fill="#5ec8d8")+
    labs(title = "Total plane each airline at LGA airport 2013",
         x = "Airline",
         y = "Total_Planes",
         caption = "Source : nycflights13 package")+
    theme(plot.title =  element_text(size = 20))


##seat_per_airline
tail_LGA <- flights %>%
    filter(origin =="LGA")%>%
    left_join(planes,by= c("tailnum"="tailnum"))

seat_LGA <- tail_LGA %>%
    select(carrier,seats,model)%>%
    mutate(na_seat = ifelse(is.na(seats)== FALSE,TRUE,FALSE))%>%
    group_by(carrier)%>%
    summarise(all_seat = sum(seats,na.rm=TRUE),
              unit = sum(na_seat),
              seat_per_plane = all_seat/unit)%>%
    arrange(desc(seat_per_plane))

seat_LGA$carrier = factor(seat_LGA$carrier,
                                 levels = seat_LGA$carrier)

ggplot(seat_LGA,aes(carrier,seat_per_plane))+ 
    geom_bar(stat="identity",fill="#5ec8d8")+
    labs(title = "Seat per plane each airline at LGA airport 2013",
         x = "carrier",
         y = "seat_per_plane",
         caption = "Source : nycflights13 package")+
    theme(plot.title =  element_text(size = 20))

