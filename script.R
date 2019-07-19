#installing packages
install.packages("tidyverse")
install.packages("maps")
#loading packages
library(tidyverse)
library(maps)
#extracting data
paste0("tar -zxvf trust.no.1.tar.gz") %>% system

#read in the data

ufos<-read_csv("scrubbed.csv")

#What does it look like?
ufos %>% head
ufos %>% head %>% View


WorldData <- map_data('world') 

#What is the global distribution of sightings?
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "grey", colour = "#7f7f7f", size=0.5) + 
  geom_point(data = ufos, aes(x = longitude, y =latitude),colour = "blue")+
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs( x="Global Distribution\nof UFO sightings", y="")


#Which US states see the most UFOs?

ufos %>% filter(country == "us") %>%
  group_by(state) %>%
  summarise(n_days = length(unique(datetime))) %>%
  arrange(-n_days) %>%
  head(10) %>%
  ggplot()+
  geom_bar(aes(x= reorder(state,n_days), y= n_days),stat="identity")+
  coord_flip()+
  theme_bw(base_size =15)+
  labs(x="State", y="# of UFO sightings")


#Has there been an increase over time?

ufos %>% 
  group_by(datetime) %>%
  mutate(year = year_grabber(datetime)) %>%
  group_by(year) %>%
  summarise(n_sightings = length(unique(datetime))) %>%
  ggplot()+
  geom_point(aes(x = year, y= n_sightings))+
  scale_y_log10()
  
#is it significant if I log transform n_sightings??

ufos<-ufos %>% 
  group_by(datetime) %>%
  mutate(year = year_grabber(datetime)) %>%
  group_by(year) %>%
  summarise(n_sightings = length(unique(datetime)))


#run a simple linear model
mod1<-lm(log(n_sightings) ~ year, data = ufos)

summary(mod1)  


#


year_grabber<-function(bad_date){
  bad_date %>% 
    str_split(pattern=" ") %>% 
    unlist() %>% 
    .[1] %>% 
    str_split("/") %>%
    unlist() %>% 
    .[3] %>% 
    as.numeric %>%
    return
}

year_grabber("10/12/2013 23:28")
