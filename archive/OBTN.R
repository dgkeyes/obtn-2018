#### Packages ####

library(tidyverse) 
library(ggplot2)
library(readxl)
library(stringr)
library(XML)
library(reshape2)
library(ggmap)
library(maps)
library(scales)
library(rgdal)
library(gridExtra)
library(grid)
library(googlesheets)
library(kableExtra)
library(knitr)
library(lettercase)
library(ggrepel)
library(tigris)

#### Get data ####

source("~/Google Drive/Work/R/TFFF themes.R")
dc.data <- read_excel(path = "data/dc header indicators.xlsx", sheet = 2)



#### Population pyramid ####

population.pyramid <- dc.data %>%
     filter(Geography == "Douglas County, Oregon") %>%
     gather(`Males 0-4`:`Females 85+`, key = "age", value = "pct") %>%
     mutate(number = pct * 107194) %>%
     select(one_of(c("Geography", "age", "pct", "number"))) 

population.pyramid$age <- str_replace(population.pyramid$age, "Males ", "")
population.pyramid$age <- str_replace(population.pyramid$age, "Females ", "")

population.pyramid$gender[1:18] <- "men"
population.pyramid$gender[19:36] <- "women"

total.population <- sum(population.pyramid$number, na.rm = TRUE)
population.pyramid$number[population.pyramid$gender == "women"] <- population.pyramid$number * -1
population.pyramid$pct.formatted <- population.pyramid$pct
population.pyramid$pct.formatted[population.pyramid$gender == "women"] <- population.pyramid$pct * -1

age.order <- population.pyramid$age[1:18]

population.pyramid$age <- factor(population.pyramid$age, levels = age.order)

# population.pyramid$age <- str_replace(population.pyramid$age, "0-4", "  0-4  ")
# 
# 
# population.pyramid.women <- population.pyramid %>%
#      filter(gender == "women")
# 
# population.pyramid.men <- population.pyramid %>%
#      filter(gender == "men")


ggplot(population.pyramid, aes(x = age, y = pct.formatted, fill = gender)) +
     geom_bar(data = population.pyramid, 
              stat = "identity",
              width = .7) +
     geom_label(label = population.pyramid$age, 
                aes(x = age, y = 0), 
                fill = "white", 
                label.size = 0, 
                size = tfff.base.font.size,
               color = tfff.dark.gray) +
     geom_label(aes(x = 17, y = .045),
                label = "Men",
                color = "white",
                fill = tfff.dark.green,
                size = tfff.base.font.size,
                label.size = 0,
                label.padding = unit(.5, "lines")) +
     geom_label(aes(x = 17, y = -.045),
                label = "Women",
                color = "white",
                fill = tfff.light.green,
                size = tfff.base.font.size,
                label.size = 0,
                label.padding = unit(.5, "lines")) +
     coord_flip() +
     scale_y_continuous(breaks = seq(-.05, .05, by = .01),
                        limits = c(-.05, .05),
                        labels = c("5%", "4%", "3%", "2%", "1%", "0", "1%", "2%", "3%", "4%", "5%")) +
     scale_fill_manual(values = c(tfff.dark.green, tfff.light.green)) +
     
     tfff.population.pyramid.theme


ggsave("plots/douglas population pyramid.png")



#### Race/ethnicity ####

race.ethnicity <- dc.data %>%
     filter(Geography == "Douglas County, Oregon") %>%
     select(contains("Percentage of Population")) %>%
     set_names(c("White",
                 "African American", 
                 "American Indian or Alaska Native", 
                 "Asian",
                 "Native Hawaiian/Pacific Islander", 
                 "Other Race", 
                 "Multiracial", 
                 "Latino")) %>%
     gather() %>%
     set_names(c("racial.ethnic.group", "pct")) %>%
     mutate(pct = round(pct, digits = 4)) %>%
     mutate(location = "Douglas")


race.ethnicity.all.but.white <- race.ethnicity %>%
     filter(racial.ethnic.group != "White")

race.ethnicity.white <- race.ethnicity %>%
     filter(racial.ethnic.group == "White")


figure.population.race <- ggplot(race.ethnicity, aes(x = reorder(racial.ethnic.group, pct), y = pct)) +
     # geom_bar(stat = "identity", position = "fill") +
     geom_bar(stat = "identity", fill = tfff.dark.green) +
     geom_text(data = race.ethnicity.all.but.white,
               label = paste(race.ethnicity.all.but.white$racial.ethnic.group, percent(race.ethnicity.all.but.white$pct), sep =": "), 
               hjust = 0,
               nudge_y = .02,
               color = tfff.dark.gray,
               fontface = "bold") +
     geom_text(data = race.ethnicity.white,
               label = paste(race.ethnicity.white$racial.ethnic.group, percent(race.ethnicity.white$pct), sep =": "), 
               hjust = 0,
               nudge_y = -.18,
               color = "white",
               fontface = "bold") +
     # scale_fill_manual(values = c(tfff.light.green, tfff.dark.green, tfff.yellow, tfff.orange, tfff.blue, tfff.red, tfff.brown, tfff.medium.gray)) +
     tfff.100.bar.chart.theme +
     coord_flip()


ggsave("plots/race ethnicity bar chart.eps", height = 3)     



#### Notable features ####


notable.features <- dc.data %>%
     filter(Geography == "Douglas County, Oregon") %>%
     select(one_of(c("Largest Comm.", "Features (1)", "Features (2)", "Features (3)"))) %>%
     gather() %>%
     # select(value) %>%
     set_names("type", "feature") %>%
     mutate_geocode(feature)

notable.features$type <- str_replace(notable.features$type, "Largest Comm.", "Largest community")
notable.features$type <- str_replace(notable.features$type, "Features \\(1\\)", "Notable feature")
notable.features$type <- str_replace(notable.features$type, "Features \\(2\\)", "Notable feature")
notable.features$type <- str_replace(notable.features$type, "Features \\(3\\)", "Notable feature")
notable.features$feature <- str_replace(notable.features$feature, "Umpqua National Forest", "Umpqua \nNational Forest")
notable.features$feature <- str_replace(notable.features$feature, "Oregon Dunes National Recreation Area", "Oregon Dunes")


dc.map <- map_data('county') %>%
     filter(region == "oregon") %>%
     filter(subregion == "douglas")

## Map of DC location in Oregon

ggplot (oregon.map, aes(x = long, y = lat, group=subregion)) +
     geom_polygon(fill = tfff.light.gray, color = tfff.light.gray) +
     geom_polygon(data = oregon.map.douglas,
                  aes(x = long, y = lat, group = subregion),
                  fill = tfff.light.green) +
     tfff.map.theme
     
## Map of notable features in DC

figure.notable.features <- ggplot () +
     geom_polygon(data = dc.map,
                  aes(x = long, y = lat, group = subregion),
                  fill = tfff.light.green) +
     geom_point(data = notable.features,
                aes(x = notable.features$lon, y = notable.features$lat),
                color = tfff.dark.green,
                size = tfff.base.font.size * .7) +
    
     geom_point(data = subset(notable.features, type == "Largest community"),
                aes(x = lon, y = lat),
                color = "white",
                size = tfff.base.font.size * .3) +
     
     geom_text(data = notable.features,
               label = notable.features$feature, 
               aes(x = notable.features$lon, y = notable.features$lat,
                   lineheight = .9),
               color = tfff.dark.green,
               size = tfff.base.font.size * .9,
               hjust = 0,
               nudge_x = .05) +
     # scale_x_discrete(labels = function(feature) str_wrap(feature, width = 30)) +
     tfff.map.theme


ggsave("plots/dc notable features map.png", dpi = 300)


#### Median income ####

median.income <- dc.data %>%
     filter(Geography == "Douglas County, Oregon" | Geography == "Oregon") %>%
     select(one_of(c("Geography", "Median Income"))) %>%
     set_names(c("location", "income"))

median.income$location <- str_replace(median.income$location, "Douglas County, Oregon", "Douglas")

figure.median.income <- ggplot(median.income, aes(x = location, y = income, fill = factor(location))) +
     geom_bar(stat = "identity",
              width = 1) +
     geom_text(label = dollar(median.income$income),
               size = tfff.base.font.size,
               color = "white",
               fontface = "bold",
               nudge_y = -8000) +
     geom_text(label = median.income$location,
               hjust = .1,
               size = tfff.base.font.size,
               color = "white",
               fontface = "bold",
               aes(x = median.income$location, y = 4000)) +
     tfff.bar.chart.theme +
     scale_fill_manual(values = c(tfff.dark.green, tfff.medium.gray)) +
     # scale_color_manual(values = c("white", "black")) +
     coord_flip()

ggsave("plots/median income.png")


#### Grid Extra ####

title <- textGrob("test")
land.text <- textGrob("landinfo")
population.text <- textGrob("populationinfo")
     
top.row <- grid.arrange(title, 
                        land.text,
                        ncol = 2,
                        heights=1:2, widths=1:2)

second.row <- grid.arrange(population.text,
                           figure.notable.features,
                           ncol = 2, 
                           heights=1:2, widths=1:2)

grid.arrange(top.row, second.row)

grid.arrange(title,
             land.text,
             population.text,
             figure.notable.features, 
             figure.population.pyramid, 
             figure.population.race, 
             figure.median.income, 
             ncol = 2)


#### Single measure pages data ####

obtn.data.by.measure.sheet <- gs_title("OBTN data by measure")


obtn.data.by.measure <- gs_read(obtn.data.by.measure.sheet, ws = "all") %>%
     filter(county != "oregon") %>%
     filter(county != "rural") %>%
     filter(county != "urban") %>%
     filter(!is.na(number)) %>%
     mutate(rank = min_rank((number))) %>%
     mutate(ordering = order((number))) %>%
     mutate(county = factor(county)) %>%
     mutate(county = fct_reorder(county, -number))
     

obtn.data.by.measure.comparisons <- gs_read(obtn.data.by.measure.sheet, ws = "all") %>%
     filter(county == "oregon" | county == "rural" | county == "urban")

obtn.data.by.measure.nas <- gs_read(obtn.data.by.measure.sheet, ws = "all") %>%
     filter(is.na(number))

ggplot(obtn.data.by.measure, aes(x = county, y = number)) +
     # geom_bar(stat = "identity") +
     geom_point(size = 15,
                fill = "white",
                color = "black",
                alpha = 0.5) +
     # geom_segment(aes(x = ))
     geom_text(label = percent(obtn.data.by.measure$number)) +
     # annotate("segment",
     #          y = obtn.data.by.measure.comparisons$number,
     #          yend = obtn.data.by.measure.comparisons$number,
     #          x = 0,
     #          xend = max(obtn.data.by.measure$ordering) + 1,
     #          alpha = 0.5) +
     geom_hline(yintercept=obtn.data.by.measure.comparisons$number,
                linetype = "dashed") +
     # geom_label_repel(data = obtn.data.by.measure.comparisons, 
     #                 aes(x = max(obtn.data.by.measure$ordering) + 1, y = number),
     #                 label = percent(obtn.data.by.measure.comparisons$number),
     #                 segment.color = "transparent",
     #                 nudge_x = 0.1) +
     # annotate("text",
     #          y = obtn.data.by.measure.comparisons$number,
     #          x = 36,
     #          label = obtn.data.by.measure.comparisons$county) +
     scale_y_continuous(limits = c(0, .11),
                        labels = scales::percent) +
     scale_x_discrete(
          # limits = c(0, 37),
          labels = str_title_case(rev(obtn.data.by.measure$county))) +
          # labels = paste(str_title_case(rev(obtn.data.by.measure$county)),
          #                " (",
          #                percent(rev(obtn.data.by.measure$number)),
          #                ")",
          #                sep="")) +
     coord_flip() 
     # theme_minimal() + tfff.bar.chart.with.benchmark.theme


obtn.data.by.measure %>%
     kable("html") %>%
     kable_styling(bootstrap_options = c("striped", "hover")) 


# Single measure map ------------------------------------------------------

obtn.data.by.measure <- obtn.data.by.measure %>%
     mutate(tertile = ntile(number, 3))

oregon.map.single.measure <- map_data("county") %>%
     filter(region == "oregon") %>%
     left_join(obtn.data.by.measure, by = c("subregion" = "county"))





ggplot(data = oregon.map.single.measure, aes(x = long, y = lat, 
                                             group = group,
                                             fill = factor(tertile))) +
     geom_polygon(color = "#ffffff") +
     scale_fill_manual(values = rev(c("#B5CC8E", "#6E8F68", "#265142"))) +
     coord_map() +
     tfff.map.theme +
     theme(legend.position = "bottom")

ggsave("low birthweight.png")


