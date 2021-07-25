# TidyTuesday Week 30 - US droughts

* My submission for the Week 30 TidyTuesday dataset on US droughts by county
* I have taken inspiration from @katie_press and others with an animated visualisation
* Data came from the U.S drought moniter website https://droughtmonitor.unl.edu/ and I have chosen dates going back to pre-COVID 2019
![Frequency of monsters by their gender reveals a large disparity](https://github.com/CalumWatt/TidyTuesday/blob/cc49d854ae82692bbdc8d2bce447955fcfd7b3ad/2021/week29/monster%20genders.png)


## let's get the libraries that we will likely need
```
require(tidyverse)
require(tidytuesdayR)
require(devtools)
require(urbnmapr) # this packages provides coordinate for state and shires (US)

# for animation
require(gganimate)
require(transformr)
require(gifski)
require(Rcpp)
require(maps)
require(janitor)
require(lubridate)
require(tibbletime)
```

## Download the data and perform some initial cleaning
```
drought <- read.csv("all USA data.csv") %>%
  pivot_longer(cols = None:D4, names_to = "Drought_level", values_to = "Percent Area") %>%
  janitor::clean_names() %>%
  mutate(drought_level = factor(drought_level, levels = c("D4", "D3", "D2","D1","D0", "None"))) %>%
  filter(!is.na(drought_level)) 

head(drought)

temp <- drought %>%
  group_by(fips, valid_start) %>%
  arrange(fips, valid_start, desc(percent_area), drought_level) %>%
  slice(1) #slice selects the first row in each group, a group being fips and valid_date. therefore removes unnecessary rows
```
## download the county/shire data from the packagen urbnmapr
```
data("county.fips") # the fips data which we can associate to county

# clean up the polyname variable
county.fips <- county.fips %>%
  mutate(region = word(polyname,1, sep = ","),
         subregion = word(polyname, 2, sep = ","))%>%
  mutate(subregion = word(subregion, 1, sep = ":"))#only keep words prior to the colon

head(county.fips)

# get the USA map coordinate points and join with fips
map_us <- map_data("county")

map_us <- map_us %>%
  left_join(county.fips) %>%
  select(!polyname) #removes polyname from the final dataframe
```

## Join our cleaned temp file to the map file and factor recode our drought categories
* This is a rather sizeable dataframe with ~8.5 million rows. 
```
map_us <- map_us %>%
  left_join(temp)

# recode the drought levels
map_us <- map_us %>%
  mutate(drought_level = fct_recode(drought_level,"Abnormally dry" = "D0",
                                    "Moderate" = "D1",
                                    "Severe" = "D2",
                                    "Extreme" = "D3",
                                    "Catastrophic" = "D4"))
                                    
str(map_us) # just want to have a squiz at the dataframe
```
## Now we get to the fin plotting side of the practical
* I have gone with  a colour scheme from white through to red. I think this is a very good colour scheme given what the data is representing.
```
colours <- scales::seq_gradient_pal("white", "red")(seq(0,1,length.out = 6))

#NAs within the dataframe, remove. 
map_us <- map_us[!is.na(map_us),]

anim <- map_us %>%
  ungroup() %>%
  mutate(valid_start = as.Date(valid_start)) %>%
  filter(valid_start > "2019-10-01") %>%

#plotting
  ggplot() +
  geom_polygon(aes(long, lat, group = group,fill = fct_rev(drought_level))) +
  borders("county", colour = "grey") +
  borders("state", colour = "black") + 
  
  coord_map() +
  theme_minimal() +
  scale_fill_manual(values = colours, name = "Drought Category") +

#set the transition from week to week
  transition_manual(frames = valid_start) +

#plot labels
  labs(title = "Drought level across each county",
       subtitle = "Week starting: {current_frame}",
       caption = "#TidyTuesday | @calumwatt12 | 25-07-2021")
```
## Now we save the .gif animation
* This will take a fair while, particularly for the ~2.5 years of data I have used. 
* If you want to avoid a long wait to see if your code has worked (like me waiting 20 minutes) I would filter the date in the previous code section to the last four weeks or so. This will give you a good idea of whether your code actually does it's thing!!
```
animate(anim, height = 5, width = 7, units = "in", res = 1000, renderer = gifski_renderer("USA droughts by shire.gif"))
```

