
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
          geom_point(data = all.places.temp,
                     aes(x = all.places.temp$lon,
                         y = all.places.temp$lat),
                     size = 2,
                     # shape = 21,
                     # stroke = 1,
                     alpha = .8,
                     color = tfff.dark.green) +
     geom_text_repel(data = all.places.temp,
                     aes(label = name_wrapped,
                         x = all.places.temp$lon,
                         y = all.places.temp$lat),
                     color = ifelse(all.places.temp$type == "notable feature",
                                    tfff.dark.gray,
                                    tfff.dark.green),
                     family = "Calibri",
                     lineheight = .65,
                     point.padding = 1,
                     segment.alpha = 0.5,
                     box.padding = 1.5,
                     force = 2) +
     
     coord_map() +
     # scale_x_continuous(expand = c(0, 0)) +
     # scale_y_continuous(expand = c(0, 0)) +
     tfff.map.theme


if (all.places.temp$shape[1] == "square") {
     dk_save_plot_by_measure("county maps", 4.03, 2.7592)
} else if (all.places.temp$shape[1] == "tall") {
     dk_save_plot_by_measure("county maps", 1.8813, 4.325)
} else if (all.places.temp$shape[1] == "long") {
     dk_save_plot_by_measure("county maps", 3.9637, 1.998)
}

}



#### 


backside.indicators.sheet <- gs_title("OBTN backside indicators")
backside.indicators.data <- gs_read(backside.indicators.sheet, ws = "Newer") 




ggplot(population.pyramid, aes(x = age, y = pct.formatted, fill = gender)) +
     geom_bar(data = population.pyramid, 
              stat = "identity") +
     #  geom_bar(data = subset(population.pyramid, gender == "men"), 
     #          stat = "identity") +
     # geom_bar(data = subset(population.pyramid, gender == "women"), 
     #          stat = "identity") +
     geom_label(label = population.pyramid$age, 
                aes(x = age, y = 0), 
                fill = "white", 
                label.size = 0, 
                size = tfff.base.font.size,
                color = tfff.dark.gray) +
     coord_flip() +
     # scale_y_continuous(breaks = seq(-1, 1, by = .1)) +
     # scale_y_continuous(breaks = seq(-.5, .5, by = .1),
     # labels = c("50%", "40%", "30%", "20%", "10%", "0", "10%", "20%", "30%", "40%", "50%")) +
     scale_fill_manual(values = c(tfff.dark.green, tfff.light.green)) +
     
     tfff.bar.chart.theme


if (all.places.temp$shape[1] == "square") {
     dk_save_plot_by_measure("county maps", 4.03, 2.7592)
} else if (all.places.temp$shape[1] == "tall") {
     dk_save_plot_by_measure("county maps", 1.8813, 4.325)
} else if (all.places.temp$shape[1] == "long") {
     dk_save_plot_by_measure("county maps", 3.9637, 1.998)
}

#### Backside stuff ####


backside.indicators.sheet <- gs_title("OBTN backside indicators")
backside.indicators.data <- gs_read(backside.indicators.sheet, ws = 1)

backside.indicators.data.dc.oregon <- backside.indicators.data %>%
     filter(year == 2014) %>%
     filter(location == "douglas" | location == "oregon")

ggplot(backside.indicators.data.dc.oregon, aes(x = pct, y = 1, fill = factor(location))) +
     geom_point(size = 10) +
     scale_x_continuous(limits = c(0, .5)) +
     scale_y_continuous(limits = c(1, 1)) +
     scale_fill_manual(values = c("#000000", "#eeeeee")) +
     theme_void() + theme(panel.grid.major.y = element_line(color = "#eeeeee"))



ggplot(race.ethnicity, aes(x = reorder(racial.ethnic.group, pct), y = pct)) +
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


ggplot(race.ethnicity, aes(x = reorder(racial.ethnic.group, -pct), y = pct)) +
     # geom_bar(stat = "identity", position = "fill") +
     geom_bar(stat = "identity", fill = tfff.dark.green) +
     geom_text(label = percent(race.ethnicity$pct),
               # hjust = 0,
               nudge_y = .04,
               color = tfff.dark.gray,
               fontface = "bold") +
     scale_y_discrete(labels = function(pct) str_wrap(pct, width = 100)) +
     # scale_fill_manual(values = c(tfff.light.green, tfff.dark.green, tfff.yellow, tfff.orange, tfff.blue, tfff.red, tfff.brown, tfff.medium.gray)) +
     tfff.column.chart.theme



ggplot(race.ethnicity, aes(x = location, y = pct, fill = racial.ethnic.group)) +
     geom_bar(stat = "identity", position = "fill") +
     scale_fill_manual(values = c(tfff.light.green, tfff.dark.green, tfff.yellow, tfff.orange, tfff.blue, tfff.red, tfff.brown, tfff.medium.gray)) +
     tfff.100.bar.chart.theme +
     coord_flip()

ggsave("plots/race ethnicity horizontal.eps", height = 2)   


ggplot(median.income, aes(x = location, y = income, fill = factor(location))) +
     geom_bar(stat = "identity", width = 1) +
     geom_text(label = dollar(median.income$income),
               color = "#ffffff",
               nudge_y = -4000) +
     geom_text(label = median.income$location,
               color = "#ffffff",
               # hjust = 0,
               aes(x = median.income$location, y = 4000)) +
     tfff.bar.chart.theme +
     scale_fill_manual(values = c(tfff.dark.green, tfff.light.green)) 




largest.community <- dc.data %>%
     filter(Geography == "Douglas County, Oregon") %>%
     select(one_of(c("Largest Comm.", "Largest Comm. Pop."))) %>%
     set_names(c("community", "population")) %>%
     mutate_geocode(community) 
# mutate(community = paste(community, population, sep = " (population ")) %>%
# mutate(community = paste(community, ")", sep = ""))
# 

