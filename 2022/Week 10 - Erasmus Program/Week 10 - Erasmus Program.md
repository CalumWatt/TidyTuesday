# TidyTuesday Week 10 2022 - Erasmus Program

* My submission for the 2022 Week 10 TidyTuesday dataset of the European Erasmus Program
* I have taken inspiration from @katie_press and others with an animated visualisation

![Week 10 - Erasmus Program](https://github.com/CalumWatt/TidyTuesday/blob/main/2022/Week%2010%20-%20Erasmus%20Program/Week%2010%20-%20Erasmus%20Program.gif)

## let's get the libraries that we will likely need
```
library(tidyverse)
library(ggmap)
library(gganimate)
library(MetBrewer)
```

## Download the data and perform some initial cleaning

* You will need to access the Google Maps static API to extract the map. Quite simple to setup!
```
data.df1 <- tidytuesdayR::tt_load('2022-03-08')
data.df <- data.df1$erasmus

# Need to change country codes to full name, can use data from @BjnNowak 

unique(data.df$participant_nationality)
Country_names <- read_delim("https://raw.githubusercontent.com/BjnNowak/TidyTuesday/main/data/iso.csv", delim = ";")
head(Country_names)

# Filter to obtain country exchanges

sent <- data.df %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(sending_country_code) %>%
  summarize(students = sum(participants)) %>%
  arrange(-students) %>%
  head(5)
  
arrived <- data.df %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(receiving_country_code) %>%
  summarize(students = sum(participants)) %>%
  arrange(-students) %>%
  head(5)
  
# Add in country names

top_countries <- as.data.frame(unique(c(sent$sending_country_code, arrived$receiving_country_code)))

colnames(top_countries) <- "top"

top_countries <- top_countries %>%
  left_join(Country_names, by = c("top" = "code")) %>%
  mutate(replace(country_name, top == "EL", "Greece"))
  
top_countries <- top_countries[,c(1,3)]

colnames(top_countries)[2] <- "country_name"

# Similar to code of @tanya_shapiro

data <- data.df %>%
  mutate(from = 1, to = 1) %>%
  left_join(Country_names, by = c("sending_country_code" = "code")) %>%
  left_join(Country_names, by = c("receiving_country_code" = "code"))
  
colnames(data)[c(27:28)] <- c("Country_From", "Country_To")

data <- data %>%
  group_by(Country_From, Country_To) %>%
  filter(Country_From != Country_To) %>%
  summarise(students = sum(participants)) %>%
  arrange(-students)
  
# Now filter out the data that represents those countries which are in the top for sending and receiving students

topdata <- data %>%
  filter(Country_From %in% top_countries$country_name & Country_To %in% top_countries$country_name)
  
topdata$entry <- seq(1:length(topdata$Country_From))

topdata <- topdata %>% 
  pivot_longer(cols = starts_with("Country"), names_to = "From_To", values_to = "country")
  
top_countries <- mutate_geocode(top_countries, country_name, output = "latlon")

topdata <- topdata %>%
  left_join(top_countries, b = c("country" = "country_name")) %>%
  mutate(categories = case_when(students >= 150 ~ "Over 150",
                                students >= 50 ~ "Between 50 and 150",
                                TRUE ~ "Less than 50"))
                                
topdata$categories <- as.factor(topdata$categories)

levels(topdata$categories) <- c("Less than 50","Between 50 and 150","Over 150")

topdata$seq <- rep_len(1:2, nrow(topdata))
```
## Now we get to the fun plotting side of the practical
* I have gone with  a colour scheme from @BlakeRMills MetBrewer package. I think this is a very good colour scheme given what the data is representing.
```
europe <- get_googlemap(center = "Switzerland", zoom = 5, maptype = "roadmap", color = "bw")

ggm <- ggmap(europe) + 
  geom_point(topdata,
            mapping = aes(x = lon,
                y = lat, 
                colour = categories,
                size = categories)) +
 
  theme_bw() +
  
  labs(title = "Erasmus Student Mobility",
    subtitle = "Top 5 countries sending/receiving students across borders",
    caption = "Data from Data.Europe | Animation by @calumwatt12") +
  
  theme(text = element_text(family = "NimbusSan"),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        plot.caption = element_text(hjust = 0.95, size = 12, margin = margin(b = 12)),
        plot.margin = margin(t = 20),
        
        legend.position = "right",
        legend.title = element_blank()) +
  
  scale_color_manual(values = met.brewer("VanGogh2", 3)) +
  
  transition_time(seq) +
  shadow_wake(wake_length = 0.4)
```
## Now we save the .gif animation
```
animate(ggm,fps = 30, duration = 5,height = 800, width =800, rewind = TRUE, renderer = gifski_renderer("Week 10 - Erasmus Program.gif")) 
```

