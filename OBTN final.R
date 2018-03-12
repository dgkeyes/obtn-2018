# PACKAGES AND THEMES ----------------------------------------------------------------

library(tidyverse) 
library(ggplot2)
library(ggrepel)
library(readxl)
library(stringr)
library(ggmap)
library(maps)
library(scales)
library(gganimate)
library(googlesheets)
library(extrafont)


# loadfonts()


source("~/Google Drive/Work/R/TFFF themes.R")


# DK FUNCTIONS ------------------------------------------------------------

dk_save_plot <- function(plot.category, plotwidth, plotheight) {
     ggsave(filename = paste("plots/final/by county/",
                             oregon.counties[i],
                             "/",
                             plot.category,
                             " - ",
                             oregon.counties[i],
                             ".pdf", sep=""),
            device = cairo_pdf,
            height = plotheight,
            width = plotwidth, 
            units = "in")
     
}

dk_save_plot_png <- function(plot.category, plotwidth, plotheight) {
     ggsave(filename = paste("plots/final/by county/",
                             oregon.counties[i],
                             "/",
                             plot.category,
                             " - ",
                             oregon.counties[i],
                             ".png", sep=""),
            dpi = 300,
            height = plotheight,
            width = plotwidth, 
            units = "in")
     
}

dk_save_plot_svg <- function(plot.category, plotwidth, plotheight) {
     ggsave(filename = paste("plots/final/by county/",
                             oregon.counties[i],
                             "/",
                             plot.category,
                             " - ",
                             oregon.counties[i],
                             ".svg", sep=""),
            # device = cairo_pdf,
            # dpi = 300,
            height = plotheight,
            width = plotwidth, 
            units = "in")
     
}



dk_save_plot_by_measure <- function(plot.category, plotwidth, plotheight) {
     ggsave(filename = paste("plots/final/temp/",
                             plot.category,
                             "/",
                             oregon.counties[i],
                             ".pdf", sep=""),
            # type = "cairo",
            device = cairo_pdf,
            dpi = 300,
            width = plotwidth,
            height = plotheight)
}

dk_save_plot_by_measure_weird_ones <- function(plot.category, 
                                               plotwidth, 
                                               plotheight) {
     ggsave(filename = paste("plots/final/by measure/",
                             plot.category,
                             "/",
                             urban.rural.oregon[i],
                             ".pdf", sep=""),
            # type = "cairo",
            device = cairo_pdf,
            dpi = 300,
            width = plotwidth,
            height = plotheight)
}



# BY COUNTY INDICATORS ----------------------------------------------------


county.data <- read_excel(path = "data/OBTN final data by county.xlsx", sheet = 2, skip = 1) %>%
     set_names(c("geography",
                 "largest_community",
                 "largest_community_pop",
                 "largest_community_pop_moe",
                 "secondary_community",
                 "secondary_community_pop",
                 "secondary_community_pop_moe",
                 "tertiary_community",
                 "tertiary_community_pop",
                 "tertiary_community_pop_moe",
                 "feature_1",
                 "feature_2",
                 "feature_3",
                 "tribal_services",
                 "employment_1",
                 "employment_2",
                 "employment_3",
                 "median_income",
                 "median_income_moe",
                 "land_area",
                 "public_lands",
                 "population_pct_white",
                 "population_pct_black",
                 "population_pct_native_american",
                 "population_pct_asian",
                 "population_pct_hpi",
                 "population_pct_other",
                 "population_pct_multi",
                 "population_pct_latino",
                 "males_0_4",
                 "males_5_9",
                 "males_10_14",
                 "males_15_19",
                 "males_20_24",
                 "males_25_29",
                 "males_30_34",
                 "males_35_39",
                 "males_40_44",
                 "males_45_49",
                 "males_50_54",
                 "males_55_59",
                 "males_60_64",
                 "males_65_69",
                 "males_70_74",
                 "males_75_79",
                 "males_80_85",
                 "males_85_plus",
                 "females_0_4",
                 "females_5_9",
                 "females_10_14",
                 "females_15_19",
                 "females_20_24",
                 "females_25_29",
                 "females_30_34",
                 "females_35_39",
                 "females_40_44",
                 "females_45_49",
                 "females_50_54",
                 "females_55_59",
                 "females_60_64",
                 "females_65_69",
                 "females_70_74",
                 "females_75_79",
                 "females_80_85",
                 "females_85_plus",
                 "total_pop",
                 "population_trend",
                 "rural_population_pct",
                 "migration")) %>%
     mutate(geography = str_replace(geography, " County, Oregon", "")) %>%
     mutate(geography = str_replace(geography, "Rural Oregon", "Rural")) %>%
     mutate(geography = str_replace(geography, "Urban Oregon", "Urban")) %>%
     mutate(geography = str_to_lower(geography))

county.data.filtered <- county.data %>%
     filter(geography != "rural") %>%
     filter(geography != "urban") %>%
     filter(geography != "oregon")


# BY MEASURE INDICATORS ---------------------------------------------------



# DEFINE OREGON COUNTIES VARIABLES AND CREATE DIRECTORIES  --------------------------------------

# Create Oregon Counties variable

oregon.counties <- county.data$geography
oregon.counties <- oregon.counties[4:39] # Remove urban, rural, and Oregon

oregon.total.population <- county.data$total_pop[3]



# Create directories for each count to save final plots -------------------

 

dir.create("plots/final")
dir.create("plots/final/by county")
dir.create("plots/final/by measure")
dir.create("plots/final/by measure/maps")
dir.create("plots/final/by measure/industries")

for (i in 1:36) {
     dir.create(paste("plots/final/by county/", oregon.counties[i], sep =""))
}



# INSET MAPS ------------------------------------------------

for (i in 1:length(oregon.counties)) {
     
     oregon.map.inset <- map_data("county") %>%
          filter(region == "oregon") 
     
     oregon.map.county.inset <- map_data("county") %>%
          filter(region == "oregon") %>%
          filter(subregion == oregon.counties[i])
     
     
     ggplot(data = oregon.map.inset, aes(x = long, y = lat, 
                                         group = group)) +
          geom_polygon(color = tfff.medium.gray,
                       fill = tfff.medium.gray) +
          geom_polygon(data = oregon.map.county.inset, 
                       fill = tfff.light.green) +
          coord_map() +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          tfff.map.theme
     
     dk_save_plot("inset map", 1.2313, 1.1225)
     
}







# COUNTY MAPS --------------------------------------------------------



MAKE DATA FRAMES FOR LARGEST COMMUNITY AND NOTABLE FEATURES

# register_google(key = "AIzaSyBonrLhrEfIT08lG62TA-KOYoaoghonw4Y")
# 
# largest.communities <- county.data %>%
#      select(one_of("largest_community", "geography")) %>%
#      set_names("name", "county") %>%
#      mutate(for_geocode = paste(name, ", Oregon", sep="")) %>%
#      mutate_geocode(for_geocode)
# 
# 
# 
# notable.features <- county.data %>%
#      select(one_of(c("geography", "feature_1", "feature_2", "feature_3"))) %>%
#      set_names(c("county", "feature_1", "feature_2", "feature_3")) %>%
#      gather(key = location, value = feature, -county) %>%
#      select(-location) %>%
#      arrange(county) %>%
#      mutate(for_geocode = paste(feature, ", ", str_to_title(county), " County, Oregon", sep ="")) %>%
#      mutate_geocode(for_geocode)

### Save data frames with new names so I don't have to re-geocode theme

# notable.features.post.geocode <- notable.features
# largest.communities.post.geocode <- largest.communities

### Recreate data frames so I don't have to re-geocode them

notable.features <- notable.features.post.geocode
largest.communities <- largest.communities.post.geocode


### Manual geocoding for places that need it

# Portland in Clackamas 
largest.communities[6,4] <- -122.5889
largest.communities[6,5] <- 45.451662

# Astoria
largest.communities[7,4] <- -123.765416
largest.communities[7,5] <- 46.2


# Wallowa Whitman (Baker)
notable.features[1,4] <- -117.263415
notable.features[1,5] <- 45.007738 

# Hells Canyon (Baker)
notable.features[2,4] <- -116.871924
notable.features[2,5] <- 45.058014 

 


# Oregon Dunes (Coos)

notable.features[16,4] <- -124.212523
notable.features[16,5] <- 43.593794



# Cape Blanco (Curry)

notable.features[23,4] <- -124.5276

# Brookings (Curry)

largest.communities[11,4] <- -124.23

# Umatilla National Forest (Grant)
notable.features[35,4] <- -118.643033
notable.features[35,5] <- 44.985688


# Oregon Dunes (Lane)
notable.features[58,4] <- -124.134574
notable.features[58,5] <- 43.950020

# Lincoln county (everything)

largest.communities[24,4] <- largest.communities[24,4] + .03
notable.features[61,4] <- notable.features[61,4] + .03
notable.features[62,4] <- notable.features[62,4] + .03
notable.features[63,4] <- notable.features[63,4] + .03

# Williamette National Forest (Linn)
notable.features[66,4] <- -122.353905
notable.features[66,5] <- 44.281151

# Umatilla National Forest (Morrow)
notable.features[73,4] <- -119.331740
notable.features[73,5] <- 45.081194

# Mt Hood National Forest (Multnomah)
notable.features[77,4] <- -121.954062 
notable.features[77,5] <- 45.485373

# Mark Hatfield Wilderness (Multnomah)
notable.features[78,4] <- -121.960153
notable.features[78,5] <- 45.604172

# Mt Hood National Forest (Wasco)
notable.features[106,4] <- -121.434747 
notable.features[106,5] <- 45.420526

# Portland (Washington)
largest.communities[37,4] <- -122.767919
largest.communities[37,5] <- 45.471887

# Umatilla National Forest (Wheeler)
notable.features[114,4] <- -119.772718
notable.features[114,5] <- 44.959235 

# Ochoco National Forest (Wheeler)
notable.features[112,4] <- -119.994371
notable.features[112,5] <- 44.457416 

# Mt Hebo (Yamhill)
notable.features[115,5] <- 45.2

## Merge stuff

notable.features <- notable.features %>%
     mutate(type = "notable feature") %>%
     set_names(c("county", "name", "for_geocode", "lon", "lat", "type"))


largest.communities <- largest.communities %>%
     mutate(type = "largest community") %>%
     select(one_of(c("county", "name", "for_geocode", "lon", "lat", "type"))) 


all.places <- rbind(notable.features, largest.communities) %>%
     filter(!is.na(name)) %>%
     mutate(name_wrapped = str_wrap(name, width = 20)) %>%
     mutate(fill = ifelse(type == "notable feature", 
                          "#ffffff", 
                          tfff.dark.green)) %>%
     mutate(color = ifelse(type == "notable feature", 
                           tfff.dark.green, 
                           "#ffffff")) %>%
     mutate(fontweight = ifelse(type == "notable feature", 
                                "plain", 
                                "bold")) %>%
     mutate(shape = "square") %>%
     mutate(shape = ifelse(county == "curry" |
                                county == "harney" |
                                county == "klamath" |
                                county == "lincoln" |
                                county == "malheur" |
                                county == "morrow" |
                                county == "sherman" |
                                county == "tillamook",
                           "tall", 
                           shape)) %>%
     mutate(shape = ifelse(county == "deschutes" |
                                county == "jefferson" |
                                county == "lane" |
                                county == "multnomah" |
                                county == "yamhill",
                           "long", 
                           shape))



## Make county maps

for (i in 1:length(oregon.counties)) {
     
     oregon.map.by.county <- map_data("county") %>%
          filter(region == "oregon") %>%
          filter(subregion == oregon.counties[i])
     
     largest.community.temp <- largest.communities %>%
          filter(county == oregon.counties[i])
     
     notable.features.temp <- notable.features %>%
          filter(county == oregon.counties[i])
     
     all.places.temp <- all.places %>%
          filter(county == oregon.counties[i])
     
     ## Plot map
     
     ggplot() +
          geom_polygon(data = oregon.map.by.county, 
                       aes(x = long, 
                           y = lat, 
                           group = group),
                       fill = tfff.light.green,
                       color = tfff.light.green,
                       size = 1) +
          geom_point(data = notable.features.temp,
                     aes(x = notable.features.temp$lon,
                         y = notable.features.temp$lat),
                     size = 2,
                     alpha = .8,
                     color = tfff.dark.green) +
          geom_point(data = largest.community.temp,
                     aes(x = largest.community.temp$lon,
                         y = largest.community.temp$lat),
                     size = 2,
                     shape = 21,
                     stroke = 1,
                     fill = "white",
                     color = tfff.dark.green) +
  
     coord_map() +
     tfff.map.theme
     

     if (all.places.temp$shape[1] == "square") {
          dk_save_plot("county maps", 4.03, 2.7592)
     } else if (all.places.temp$shape[1] == "tall") {
          dk_save_plot("county maps", 1.8813, 4.325)
     } else if (all.places.temp$shape[1] == "long") {
          dk_save_plot("county maps", 3.9637, 1.998)
     }

     
     
}





# RACE/ETHNICITY ----------------------------------------------------------

race.ethnicity.order <- rev(c("White",
                              "Latino",
                              "African American",
                              "Asian",
                              "Am Indian/Alaska Native",
                              "Native Hawaiian/Pacific Islander",
                              "Multiracial",
                              "Other Race"))

for (i in 1:length(oregon.counties)) {
     
     race.ethnicity <- county.data %>%
          select(geography, population_pct_white:population_pct_latino) %>%
          set_names(c("geography",
                      "White",
                      "African American",
                      "Am Indian/Alaska Native",
                      "Asian",
                      "Native Hawaiian/Pacific Islander",
                      "Other Race",
                      "Multiracial",
                      "Latino")) %>%
          gather(key = "racial.ethnic.group", value = "pct", -geography) %>%
          arrange(geography) %>%
          mutate(racial.ethnic.group = factor(racial.ethnic.group, levels = race.ethnicity.order)) %>%
          mutate(pct = round(pct, digits = 3)) %>%
          filter(geography == oregon.counties[i])
     
     race.ethnicity.white <- race.ethnicity %>%
          filter(racial.ethnic.group == "White")
     
     
     race.ethnicity.all.but.white <- race.ethnicity %>%
          filter(racial.ethnic.group != "White")
     
     ggplot(race.ethnicity, 
            aes(x = racial.ethnic.group, y = pct)) +
          geom_bar(stat = "identity", fill = tfff.dark.green) +
          geom_text(data = race.ethnicity.all.but.white,
                    label = paste(race.ethnicity.all.but.white$racial.ethnic.group, 
                                  percent(race.ethnicity.all.but.white$pct), 
                                  sep =": "),
                    hjust = 0,
                    nudge_y = .02,
                    color = tfff.dark.gray,
                    family = "Calibri") +
          geom_text(data = race.ethnicity.white,
                    label = paste(race.ethnicity.white$racial.ethnic.group, 
                                  percent(race.ethnicity.white$pct), 
                                  sep =": "),
                    hjust = 0,
                    nudge_y = -1 * (race.ethnicity.white$pct / 3),
                    color = "white",
                    family = "Calibri") +
          scale_x_discrete(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          tfff.bar.chart.theme +
          coord_flip()
     
     dk_save_plot("race ethnicity", 3.1, 2.1)
     
     
}


# Urban/rural/Oregon ------------------------------------------------------

urban.rural.oregon <- c("urban", "rural", "oregon")

for (i in 1:length(urban.rural.oregon)) {
     
     race.ethnicity <- county.data %>%
          select(geography, population_pct_white:population_pct_latino) %>%
          set_names(c("geography",
                      "White",
                      "African American",
                      "American Indian or Alaska Native",
                      "Asian",
                      "Native Hawaiian/Pacific Islander",
                      "Other Race",
                      "Multiracial",
                      "Latino")) %>%
          gather(key = "racial.ethnic.group", value = "pct", -geography) %>%
          arrange(geography) %>%
          mutate(racial.ethnic.group = factor(racial.ethnic.group, levels = race.ethnicity.order)) %>%
          mutate(pct = round(pct, digits = 3)) %>%
          filter(geography == urban.rural.oregon[i])
     
     race.ethnicity.white <- race.ethnicity %>%
          filter(racial.ethnic.group == "White")
     
     
     race.ethnicity.all.but.white <- race.ethnicity %>%
          filter(racial.ethnic.group != "White")
     
     ggplot(race.ethnicity, 
            aes(x = racial.ethnic.group, y = pct)) +
          geom_bar(stat = "identity", fill = tfff.dark.green) +
          geom_text(data = race.ethnicity.all.but.white,
                    label = paste(race.ethnicity.all.but.white$racial.ethnic.group, 
                                  percent(race.ethnicity.all.but.white$pct), 
                                  sep =": "),
                    hjust = 0,
                    nudge_y = .02,
                    color = tfff.dark.gray,
                    family = "Calibri") +
          geom_text(data = race.ethnicity.white,
                    label = paste(race.ethnicity.white$racial.ethnic.group, 
                                  percent(race.ethnicity.white$pct), 
                                  sep =": "),
                    hjust = 0,
                    nudge_y = -1 * (race.ethnicity.white$pct / 3),
                    color = "white",
                    family = "Calibri") +
          scale_x_discrete(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0),
                             limits = c(0, .85)) +
          tfff.bar.chart.theme +
          coord_flip()
     
     dk_save_plot_by_measure_weird_ones("race ethnicity", 4, 2.5)
     # dk_save_plot("race ethnicity", 3.1, 2.1)
     
     
}





# MEDIAN INCOME -----------------------------------------------------------

for (i in 1:length(oregon.counties)) {
     
     median.income <- county.data %>%
          select(geography, median_income) %>%
          gather(key = "median_income", value = "amount", -geography) %>%
          arrange(geography) %>%
          filter(geography == oregon.counties[i] | geography == "oregon") %>%
          select(-median_income) %>%
          mutate(geography.factor = factor(geography, 
                                           levels = c("oregon",
                                                      county.data$geography[i + 3])))
     
     
     ggplot(median.income, aes(x = geography.factor, y = amount, 
                               fill = geography.factor)) +
          geom_bar(stat = "identity",
                   width = .75) +
          geom_text(label = dollar(median.income$amount),
                    # size = tfff.base.font.size,
                    color = "white",
                    # fontface = "bold",
                    family = "Calibri",
                    nudge_y = -9500) +
          geom_text(label = str_to_title(median.income$geography.factor),
                    hjust = .1,
                    # size = tfff.base.font.size,
                    color = "white",
                    # fontface = "bold",
                    family = "Calibri",
                    aes(x = median.income$geography.factor, 
                        y = (median.income$amount[1] * .07))) +
          tfff.bar.chart.theme +
          scale_fill_manual(values = c(tfff.medium.gray, tfff.dark.green)) +
          scale_x_discrete(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          coord_flip() 
     
     dk_save_plot("median income", 2.43, .61)
     
     
}



# POPULATION PYRAMID ------------------------------------------------------

# Define age order for later on

age.order <- c("0-4",
               "5-9",
               "10-14",
               "15-19",
               "20-24",
               "25-29",
               "30-34",
               "35-39",
               "40-44",
               "45-49",
               "50-54",
               "55-59",
               "60-64",
               "65-69",
               "70-74",
               "75-79",
               "80-85",
               "85+")  


# Define make population pyramid tibbles function

make_population_pyramid_tibble <- function(dataset) {
     dataset <- dataset %>%
          select(geography, males_0_4:females_85_plus) %>%
          gather(key = "gender_age", value = "amount", -geography) %>%
          arrange(geography) %>%
          mutate(gender = ifelse(str_detect(gender_age, "female"), "women", "men")) %>%
          mutate(gender_age = str_replace(gender_age, "females_", "")) %>%
          mutate(gender_age = str_replace(gender_age, "males_", "")) %>%
          mutate(gender_age = str_replace(gender_age, "_", "-")) %>%
          mutate(gender_age = str_replace(gender_age, "-plus", "+")) %>%
          set_names(c("geography", "age", "pct", "gender")) %>%
          mutate(age = factor(age, levels = age.order)) %>%
          mutate(pct = round(pct, digits = 3)) %>%
          mutate(pct_formatted = ifelse(gender == "women", -pct, pct)) %>%
          mutate(age_labels = age) %>%
          mutate(age_labels = str_replace(age_labels, "^0-4", "  0-4  ")) %>%
          mutate(age_labels = str_replace(age_labels, "^5-9", "  5-9  ")) %>%
          mutate(age_labels = str_replace(age_labels, "^85\\+", "  85+  "))
}

population.pyramid <- make_population_pyramid_tibble(county.data)

for (i in 1:length(oregon.counties)) {
     
     
     population.pyramid.temp <- population.pyramid %>%
          filter(geography == oregon.counties[i])
          
          
     
     
     if (max(population.pyramid.temp$pct) < .04) {
          population.pyramid.labels <- c("4%", "2%", 
                                         "0", 
                                         "2%", "4%")  
          population.pyramid.limit <- .04
          population.pyramid.labels.placement <- .01
     } else if (max(population.pyramid.temp$pct) < .06) {
          population.pyramid.labels <- c("6%", "4%", "2%", 
                                         "0", 
                                         "2%", "4%", "6%")
          population.pyramid.limit <- .06
          population.pyramid.labels.placement <- .01
     } else if (max(population.pyramid.temp$pct) < .08) {
          population.pyramid.labels <- c("8%", "6%", "4%", "2%", 
                                         "0", 
                                         "2%", "4%", "6%", "8%")    
          population.pyramid.limit <- .08
          population.pyramid.labels.placement <- .02
     } else if (max(population.pyramid.temp$pct) < .1) {
          population.pyramid.labels <- c("10%", "8%", "6%", "4%", "2%", 
                                         "0", 
                                         "2%", "4%", "6%", "8%", "10%")  
          population.pyramid.limit <- .1
          population.pyramid.labels.placement <- .03
     }
     
     
     # mutate(age_labels = str_replace(age_labels, "^0-4", 
     #                                 str_pad("0-4", 2, "both"))) %>%
     #      mutate(age_labels = str_replace(age_labels, "^5-9",
     #                                      str_pad("5-9", 2, "both"))) %>%
     #      mutate(age_labels = str_replace(age_labels, "^85\\+",
     #                                      str_pad("85+", 2, "both")))
     
     
     
     
     # Plot population pyramid
     
     ggplot(population.pyramid.temp, aes(x = age, y = pct_formatted, 
                                    fill = gender,
                                    frame = geography)) +
          geom_bar(data = population.pyramid.temp, 
                   stat = "identity",
                   width = .7) +
          geom_label(label = population.pyramid.temp$age_labels, 
                     aes(x = age, y = 0), 
                     # hjust = 0,
                     fill = "white",
                     family = "Calibri",
                     label.size = NA, 
                     # size = tfff.base.font.size,
                     color = tfff.dark.gray) +
          geom_label(aes(x = 17, y = population.pyramid.limit - 
                              population.pyramid.labels.placement),
                     label = "Men",
                     color = "white",
                     family = "Calibri",
                     fill = tfff.dark.green,
                     label.size = 0,
                     label.r = unit(0, "lines"),
                     label.padding = unit(.3, "lines")) +
          geom_label(aes(x = 17, y = (population.pyramid.limit -
                              population.pyramid.labels.placement) * -1),
                     label = "Women",
                     color = "white",
                     family = "Calibri",
                     fill = tfff.light.green,
                     label.size = 0,
                     label.r = unit(0, "lines"),
                     label.padding = unit(.3, "lines")) +
          coord_flip() +
          scale_y_continuous(breaks = seq(population.pyramid.limit * -1, 
                                          population.pyramid.limit, 
                                          by = .02),
                             limits = c(population.pyramid.limit * -1, 
                                        population.pyramid.limit),
                             labels = population.pyramid.labels) +
                             # expand = c(0, 0.001)) +
          # scale_x_discrete(expand = c(0, 0)) +
          scale_fill_manual(values = c(tfff.dark.green, tfff.light.green)) +
          tfff.population.pyramid.theme
     
     dk_save_plot_png("population pyramid", 3.27, 4.0752)
}



# Population pyramid state/urban/rural ------------------------------------

population.pyramid.state.urban.rural <- population.pyramid %>%
     filter(geography == "urban" | geography == "rural" | geography == "oregon")

population_pyramid_plot <- function(filter) {
     
     dataset <- population.pyramid.state.urban.rural %>%
          filter(geography == filter)
     
     ggplot(dataset, aes(x = age, y = pct_formatted, 
                         fill = gender,
                         frame = geography)) +
          geom_bar(data = dataset, 
                   stat = "identity",
                   width = .7) +
          geom_label(label = dataset$age_labels, 
                     aes(x = age, y = 0), 
                     # hjust = 0,
                     fill = "white",
                     family = "Calibri",
                     label.size = NA, 
                     # size = tfff.base.font.size,
                     color = tfff.dark.gray) +
          geom_label(aes(x = 17, y = .04),
                     label = "Men",
                     color = "white",
                     family = "Calibri",
                     fill = tfff.dark.green,
                     label.size = 0,
                     label.r = unit(0, "lines"),
                     label.padding = unit(.3, "lines")) +
          geom_label(aes(x = 17, y = -.04),
                     label = "Women",
                     color = "white",
                     family = "Calibri",
                     fill = tfff.light.green,
                     label.size = 0,
                     label.r = unit(0, "lines"),
                     label.padding = unit(.3, "lines")) +
          coord_flip() +
          scale_y_continuous(breaks = seq(-.05,
                                          .05,
                                          by = .01),
                             limits = c(-.05,
                                        .05),
                             labels = c("5%", "4%", "3%", "2%", "1%", 
                                       "0", 
                                       "1%", "2%", "3%", "4%", "5%")) +
                             # expand = c(0, 0.001)) +
          scale_x_discrete(expand = c(0, 0)) +
          scale_fill_manual(values = c(tfff.dark.green, tfff.light.green)) +
          tfff.population.pyramid.theme
     
     ggsave(filename = paste("plots/final/by measure/population pyramid/",
                             filter,
                             ".png", 
                             sep=""),
            dpi = 300,
            width = 3.27,
            height = 4.0752,
            units = "in")
     
}


save_pop_pyramid <- function(name) {
     ggsave(filename = paste("plots/final/by measure/population pyramid/",
                             name,
                             ".png", 
                             sep=""),
            dpi = 300,
            width = 3,
            height = 4,
            units = "in")
}


population_pyramid_plot("oregon")
save_pop_pyramid("oregon")

population_pyramid_plot("rural")
save_pop_pyramid("rural")

population_pyramid_plot("urban")
save_pop_pyramid("urban")


# SINGLE MEASURE CHOROPLETH MAPS ------------------------------------------

single.measures.list <- excel_sheets("data/OBTN final data by measure.xlsx")




## Get first sheet
single.measures.data <- read_excel(path = "data/OBTN final data by measure.xlsx", 
                                   sheet = single.measures.list[2]) %>%
     select(-3) %>%
     set_names(c(single.measures.list[2], 
                 "geography")) %>%
     mutate(geography = str_replace(geography, " County, Oregon", "")) %>%
     mutate(geography = str_replace(geography, "Rural Oregon", "Rural")) %>%
     mutate(geography = str_replace(geography, "Urban Oregon", "Urban")) %>%
     mutate(geography = str_to_lower(geography))


## Get later sheets and add them to first one

for (i in 3:length(single.measures.list)) {
     single.measures.data.temp <- read_excel(path = "data/OBTN final data by measure.xlsx", 
                                             sheet = i) %>%
          select(-3) %>%
          set_names(c(single.measures.list[i], 
                      "geography")) %>%
          mutate(geography = str_replace(geography, " County, Oregon", "")) %>%
          mutate(geography = str_replace(geography, "Rural Oregon", "Rural")) %>%
          mutate(geography = str_replace(geography, "Urban Oregon", "Urban")) %>%
          mutate(geography = str_to_lower(geography))
     
     single.measures.data <<- left_join(single.measures.data, 
                                        single.measures.data.temp, 
                                        by = "geography")
}

## Add in mobile home data (not sure why it's not getting imported, but whatevs)
mobile.home.data <- read_excel(path = "data/OBTN final data by measure.xlsx", 
                               sheet = 30) %>%
     select(one_of("Rank", "County")) %>%
     set_names(c(single.measures.list[30], 
                 "geography")) %>%
     mutate(geography = str_replace(geography, " County, Oregon", "")) %>%
     mutate(geography = str_replace(geography, "Rural Oregon", "Rural")) %>%
     mutate(geography = str_replace(geography, "Urban Oregon", "Urban")) %>%
     mutate(geography = str_to_lower(geography))

single.measures.data <<- left_join(single.measures.data, 
                                   mobile.home.data, 
                                   by = "geography")

# Reorder, putting county first

single.measures.data <- single.measures.data[, c(2, 1, 3:length(single.measures.data))] %>%
     filter(geography != "oregon")

# Create tertiles and make data tidy 

single.measures.data <- single.measures.data %>%
     gather(key = measure, value = rank, -geography) %>%
     group_by(measure) %>%
     mutate(tertile = ntile(rank, 3)) %>%
     arrange(measure, tertile) %>%
     ungroup() %>% 
     mutate(tertile = as.character(tertile)) %>%
     mutate(tertile = str_replace(tertile, "1", "Higher")) %>%
     mutate(tertile = str_replace(tertile, "2", "Middle")) %>%
     mutate(tertile = str_replace(tertile, "3", "Lower")) %>%
     mutate(tertile = str_replace_na(tertile)) %>%
     mutate(tertile = str_replace(tertile, "NA", "Not available")) %>%
     mutate(tertile = factor(tertile, levels = c("Higher", 
                                                 "Middle",
                                                 "Lower",
                                                 "Not available")))




oregon.map.single.measure <- map_data("county") %>%
     filter(region == "oregon") %>%
     left_join(single.measures.data, by = c("subregion" = "geography"))




for (i in 2:30) {
     oregon.map.single.measure.temp <- oregon.map.single.measure %>%
          filter(measure == single.measures.list[i])
     
     
     ggplot(data = oregon.map.single.measure.temp, aes(x = long, y = lat, 
                                                       group = group,
                                                       fill = factor(tertile))) +
          # labs(title = single.measures.list[2]) +
          geom_polygon(color = "#ffffff") +
          scale_fill_manual(values = rev(c("#dddddd", 
                                           "#B5CC8E", 
                                           "#6E8F68", 
                                           "#265142"))) +
          coord_map() +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          tfff.map.theme 
     
     ggsave(filename = paste("plots/final/by measure/maps/pdf/", 
                             single.measures.list[i],
                             ".pdf", 
                             sep=""),
            device = cairo_pdf,
            width = 3.89,
            height = 2.79,
            units = "in",
            dpi = 300)
     
}



# Single measures for InDesign --------------------------------------------



## Get first sheet
single.measures.data.ind <- read_excel(path = "data/OBTN final data by measure.xlsx", 
                                   sheet = single.measures.list[2]) %>%
     # select(-3) %>%
     set_names(c(single.measures.list[2], 
                 "geography", 
                 "amount")) %>%
     mutate(geography = str_replace(geography, " County, Oregon", "")) %>%
     mutate(geography = str_replace(geography, "Rural Oregon", "Rural")) %>%
     mutate(geography = str_replace(geography, "Urban Oregon", "Urban")) %>%
     mutate(geography = str_to_lower(geography))


## Get later sheets and add them to first one

for (i in 3:length(single.measures.list)) {
     single.measures.data.temp <- read_excel(path = "data/OBTN final data by measure.xlsx", 
                                             sheet = i) %>%
          select(1:3) %>%
          set_names(c(single.measures.list[i], 
                      "geography",
                      "amount")) %>%
          mutate(geography = str_replace(geography, " County, Oregon", "")) %>%
          mutate(geography = str_replace(geography, "Rural Oregon", "Rural")) %>%
          mutate(geography = str_replace(geography, "Urban Oregon", "Urban")) %>%
          mutate(geography = str_to_lower(geography))
     
     View(single.measures.data.ind) <- left_join(single.measures.data.ind, 
                                        single.measures.data.temp, 
                                        by = "geography")
}

## Add in mobile home data (not sure why it's not getting imported, but whatevs)
# mobile.home.data.ind <- read_excel(path = "data/OBTN final data by measure.xlsx", 
#                                sheet = 30) %>%
#      select(one_of("Rank", "County", "Mobile Homes")) %>%
#      set_names(c(single.measures.list[30], 
#                  "geography",
#                  "amount")) %>%
#      mutate(geography = str_replace(geography, " County, Oregon", "")) %>%
#      mutate(geography = str_replace(geography, "Rural Oregon", "Rural")) %>%
#      mutate(geography = str_replace(geography, "Urban Oregon", "Urban")) %>%
#      mutate(geography = str_to_lower(geography))
# 
# single.measures.data.ind <- left_join(single.measures.data.ind, 
#                                    mobile.home.data.ind, 
#                                    by = "geography")

# single.measures.data.ind <- single.measures.data.ind %>%
#      select(starts_with("amount"))

write.csv(single.measures.data.ind, "temp.csv")

# EMPLOYMENT CATEGORIES ---------------------------------------------------

employment.categories <- county.data %>%
     select(contains("employment")) %>%
     gather() %>%
     filter(!is.na(value)) %>%
     distinct(value)

top.industries <- county.data %>%
     select(c(geography, employment_1:employment_3)) %>%
     filter(geography != "rural" & geography != "urban" & geography != "oregon") %>%
     gather(key = "key", value = "value", -geography) %>%
     select(-key) %>%
     # arrange(geography) %>%
     set_names(c("county", "industry")) %>%
     arrange(industry) 


oregon.map.industries <- map_data("county") %>%
     filter(region == "oregon") %>%
     left_join(top.industries, by = c("subregion" = "county"))

for (i in 1:length(employment.categories$value)) {
     oregon.map.industries.temp <- oregon.map.industries %>%
          filter(industry == employment.categories$value[i])
     
     ggplot() +
          geom_polygon(data = oregon.map, aes(x = long, y = lat, 
                                              group = group),
                       fill = tfff.light.gray,
                       color = "white") +
          geom_polygon(data = oregon.map.industries.temp, aes(x = long, y = lat, 
                                                              group = group),
                       fill = tfff.dark.green,
                       color = "white") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          coord_map() +
          tfff.map.theme 
     
     ggsave(filename = paste("plots/final/by measure/industries/", 
                             employment.categories$value[i],
                             ".pdf", 
                             sep=""),
            device = cairo_pdf,
            width = 2,
            height = 2*.75,
            unit = "in",
            dpi = 300)
}


# employment.categories.sheet <- gs_title("OBTN employment categories and icons")
# 
# gs_edit_cells(employment.categories.sheet, 
#               ws = 1,
#               input = employment.categories, 
#               anchor = "A1", 
#               trim = TRUE, 
#               col_names = TRUE,
#               verbose = TRUE)


# Tribes ------------------------------------------------------------------

tribes <- read_excel(path = "data/OBTN final data by county.xlsx", 
                     sheet = "Tribes") %>%
     gather(key = "tribe", value = "present", -COUNTY) %>%
     filter(!is.na(present)) %>%
     select(-present) %>%
     set_names(c("county", "tribe")) %>%
     mutate(county = str_to_lower(county))

oregon.map.tribes <- map_data("county") %>%
     filter(region == "oregon") %>%
     left_join(tribes, by = c("subregion" = "county"))

tribes.list <- read_excel(path = "data/OBTN final data by county.xlsx", 
                          sheet = "Tribes") %>%
     select(-COUNTY) 
     
tribes.list <- colnames(tribes.list)

for (i in 1:length(tribes.list)) {
     temp <- oregon.map.tribes %>%
          filter(tribe == tribes.list[i])
     
     ggplot() +
          geom_polygon(data = oregon.map, aes(x = long, 
                                              y = lat, 
                                              group = group),
                       fill = tfff.light.gray,
                       color = "white") +
          geom_polygon(data = temp, aes(x = long, 
                                        y = lat, 
                                        group = group),
                       fill = tfff.dark.green,
                       color = "white") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          coord_map() +
          tfff.map.theme 
     
     ggsave(filename = paste("plots/final/by measure/tribes/", 
                             tribes.list[i],
                             ".pdf", 
                             sep=""),
            device = cairo_pdf,
            width = 2,
            height = 2*.75,
            unit = "in",
            dpi = 300)
}


# STATEWIDE MAPS ----------------------------------------------------------

## Notable features

oregon.map.by.county.all <- map_data("county") %>%
     filter(region == "oregon")

notable.features.statewide <- notable.features %>%
     filter(!is.na(feature)) %>%
     arrange(feature) %>%
     distinct(feature, .keep_all = TRUE) %>%
     mutate(label_number = row_number())


ggplot() +
     geom_polygon(data = oregon.map.by.county.all, 
                  aes(x = long, 
                      y = lat, 
                      group = group),
                  fill = tfff.light.green,
                  color = "white",
                  size = 1) +
     geom_label_repel(data = notable.features.statewide,
                      label = notable.features.statewide$label_number, 
                      aes(x = notable.features.statewide$lon,
                          y = notable.features.statewide$lat),
                      color = "white",
                      fill = tfff.dark.green,
                      family = "Calibri",
                      segment.color = tfff.dark.green) +
     coord_map() +
     tfff.map.theme

ggsave("misc/notable features.png")
write.csv(notable.features.statewide, "misc/notable features.csv")  

## Largest community in county

largest.community.by.county <- largest.communities %>%
     filter(!is.na(name)) %>%
     filter(county != "oregon") %>%
     arrange(name) %>%
     mutate(label_number = row_number(name))

write.csv(largest.community.by.county, "misc/largest communities.csv")    

largest.community.by.county.pdx.salem <- largest.community.by.county %>%
     filter(name == "Portland" | name == "Salem")

largest.community.by.county <- largest.community.by.county %>%
     filter(name != "Portland" & name != "Salem")

ggplot() +
     geom_polygon(data = oregon.map.by.county.all, 
                  aes(x = long, 
                      y = lat, 
                      group = group),
                  fill = tfff.light.green,
                  color = "white",
                  size = 1) +
     geom_label(data = largest.community.by.county,
                label = largest.community.by.county$label_number, 
                aes(x = largest.community.by.county$lon,
                    y = largest.community.by.county$lat),
                color = tfff.dark.green,
                family = "Calibri") +
     geom_label_repel(data = largest.community.by.county.pdx.salem,
                      label = largest.community.by.county.pdx.salem$label_number, 
                      aes(x = largest.community.by.county.pdx.salem$lon,
                          y = largest.community.by.county.pdx.salem$lat),
                      color = tfff.dark.green,
                      family = "Calibri",
                      segment.color = tfff.dark.green) +
     coord_map() +
     tfff.map.theme

ggsave("misc/largest communities.png")

