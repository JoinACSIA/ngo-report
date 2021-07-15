### NGO Sector Report R Code
### Zachary Hadley, ACSIA 15 JUN 2021


## Load packages into R session
library(knitr)
library(kableExtra)
library(tidyverse)
library(lubridate)
library(scales)
library(formattable)
library(shiny)
library(reactable)
library(DT)
library(viridis)
library(hrbrthemes)
library(plotly)



# Summary of mapped NGO by sector table ------------------

ngo_tbl <- data.frame(
   "NGO Sector" = c("Agriculture", "Animal Welfare", "Capacity Strengthening Organization (CSO)",
                    "Communications/Technology", "Conflict Prevention and Resolution/Peace and Security",
                    "Construction", "Debt Relief", "Disaster Prevention and Preparedness", 
                    "Economic Recovery and Development", "Education", "Energy", "Environment", "Fishing",
                    "Food Aid", "Forestry", "Gender", "Health", "Human Rights, Democracy, and Governance",
                    "Humanitarian Aid", "Mining and Extractive Resources", "Other", "Protection", "Refugee
            Resettlement", "Shelter and Housing", "Social Services", "Trade", "Transport/Infrastructure",
                    "Water Sanitation and Hygiene"),
   "Total NGOs Mapped" = c(
      "3", "1", "29", "16", "5", "2", "0", "4", "18", "82", 
      "0", "28", "1", "6", "1", "17", "35", "40", "11", "1", 
      "21", "18", "7", "1", "30", "1", "0", "4"),
   check.names = F
)

unit.scale = function(x) (x - min(x)) / (max(x) - min(x)) #unit.scale function

formattable(ngo_tbl,
            align = c("l", "l"),
            list(`Indicator Name` = formattable::formatter("span", 
                                                           style = ~ style(color = "grey", font.weight = "bold")),
                 `Total NGOs Mapped` = color_bar("#B1CBEB", fun = unit.scale))) %>%
   as.datatable(escape = FALSE, 
                options = list(dom = 't',
                               order = list(list(2, "desc"))
                ))




## Sector summary stat tables ---------------------------


### Environmental NGO summary stats
top_env_ngo <- data.frame(
   "Sector" = "environmental",
   "NGO" = c("Greenpeace France", "PETA France", "WWF-France", "Wildlife Angel", "CCFD-Terre Solidaire"),
   'Total interactions' = c(1390000, 1250000, 573657, 130913, 78364),
   'Interaction rate' = percent(c(.00366, .02785, .00291, .01656, .00126)),
   'Avg. posts per day' = c(1.25, 1.1, 0.63, 0.88, 0.99),
   'Page followers' = c(869145, 117628, 899401, 27648, 174468),
   "Growth" = percent(c(.0983, .1246, .0956, .2850, .0289)),
   check.names = F)

formattable(top_env_ngo, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "NGO" = formatter("span", 
                                 style = ~ formattable::style(color = "grey",font.weight = "bold")), 
               "Total interactions" = color_bar("#B1CBEB"),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Page followers" = color_bar("#B1CBEB"),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))

env_compare <- data.frame(
   "Year" = ymd(c(20200101, 20190101, 20180101)),
   "Total interactions" = number(c(3660000, 4120000, 3950000), big.mark = ","),
   "Interaction rate" = percent(c(0.0804, .01116, .01293)),
   "Avg. posts per day" = c(13.63, 12.15, 11.83),
   "Page followers" = number(c(2550000, 2330000, 1980000), big.mark = ","),
   "Growth" = percent(c(.0918, .1784, .2323)),
   check.names = F
)

formattable(env_compare, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "Year" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))

### Human Rights NGO summary stats
top_hum_ngo <- data.frame(
   "Sector" = "human_rights",
   "NGO" = c("ATTAC France", "Amnesty International France", "Ligue des droits de l’Homme", 
             "Survival International", "La Cimade"),
   `Total interactions` = c(1230000, 930742, 394572, 170334, 59025),
   `Interaction rate` = percent(c(.00793, .00379, .00116, .00038, .00387)),
   `Avg. posts per day` = c(3.08, 1.11, 6.31, 3.92, 0.52),
   `Page followers` = c(148876, 616159, 153958, 312339, 86134),
   `Growth` = percent(c(.1776, .0429, .0986, .0065, .1455)),
   check.names = F
)

formattable(top_hum_ngo, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "NGO" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")), 
               "Total interactions" = color_bar("#B1CBEB", fun = unit.scale),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Page followers" = color_bar("#B1CBEB", fun = unit.scale),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))

hum_compare <- data.frame(
   `Year` = ymd(c(20200101, 20190101, 20180101)),
   `Total interactions` = c(3270000, 2490000, 1970000),
   `Interaction rate` = percent(c(.0039, .00399, .00324)),
   `Avg. posts per day` = c(37.13, 29.39, 29.74),
   `Page followers` = c(2460000, 2330000, 2230000),
   `Growth` = percent(c(.0561, .0431, .0571)),
   check.names = F
)

formattable(hum_compare, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "Year" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))


### Health NGO summary stats

top_health_ngo <- data.frame(
   "Sector" = "health",
   "NGO" = c("Médecins Sans Frontieres", "Croix-Rouge Française", "Médecins du Monde France",
             "European Society of Cardiology", "AIDES"),
   `Total interactions` = c(747658, 346901, 82930, 62356, 46113),
   `Interaction rate` = percent(c(.00216, .00246, .00063, .00078, .00160)),
   `Avg. posts per day` = c(1.59, 0.72, 0.86, .95, 1.17), 
   `Page followers` = c(610129, 549852, 426132, 239150, 69289),
   `Growth` = percent(c(.0585, .0616, .0361, .0947, .0743)),
   check.names =  F
)

formattable(top_health_ngo, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "NGO" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")), 
               "Total interactions" = color_bar("#B1CBEB", fun = unit.scale),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Page followers" = color_bar("#B1CBEB", fun = unit.scale),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))

health_compare <- data.frame(
   `Year` = ymd(c(20200101, 20190101, 20180101)),
   `Total interactions` = c(1490000, 878611, 832203),
   `Interaction rate` = percent(c(.00359, .00263, .00281)),
   `Avg. posts per day` = c(16.67, 14.44, 13.33),
   `Page followers` = c(2370000, 2210000, 2130000),
   `Growth` = percent(c(.0611, .0377, .0589)),
   check.names = F
)

formattable(health_compare, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "Year" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))


### Education NGO Summary stats
top_education_ngo <- data.frame(
   `Sector` = "Education",
   `NGO` = c("Organisation internationale de Francophonie (OIF)", "Institut du Monde Arabe",
             "Fondation Napoleon", "Enfants du Mekong", 
             "Jeux de la Francophonie"),
   `Total interactions` = c(117533, 97735, 86317, 48085, 47539),
   `Interaction rate` = percent(c(.00137, .00095, .00655, .00215, .02003)),
   `Avg. posts per day` = c(.08, 1.7, 2.58, 0.96, 0.44), 
   `Page followers` = c(297035, 174870, 15188, 64432, 15912),
   `Growth` = percent(c(.0132, .1286, .2069, 0.0374, .1581)),
   check.names = F
)

formattable(top_education_ngo, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "NGO" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")), 
               "Total interactions" = color_bar("#B1CBEB", fun = unit.scale),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Page followers" = color_bar("#B1CBEB", fun = unit.scale),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))

education_compare <- data.frame(
   `Year` = ymd(c(20200101, 20190101, 20180101)),
   `Total interactions` = c(1490000, 828332, 799252),
   `Interaction rate` = percent(c(.00242, .00258, .00277)),
   `Avg. posts per day` = c(43.83, 41.88, 40.81),
   `Page followers` = c(1960000, 1720000, 1570000),
   `Growth` = percent(c(.1073, .0940, .1543)),
   check.names = F
)

formattable(education_compare, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "Year" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))


### Humanitarian Aid NGO summary stats
top_aid_ngo <- data.frame(
   `Sector` = "humanitarian_aid",
   `NGO` = c("Secours Catholique-Caritas France (SC-CF)", "Action contre la Faim",
             "Secours Islamique France", "The Alliance for International Medical Action (ALIMA)",
             "ACTED"),
   `Total interactions` = c(55978, 35156, 24232, 23820, 18912),
   `Interaction rate` = percent(c(.00795, .0008, .00025, .00408, .00327)),
   `Avg. posts per day` = c(0.51, 0.43, 0.81, 0.78, 0.36), 
   `Page followers` = c(40859, 281558, 362737, 22770, 48424),
   `Growth` = percent(c(.2078, .0242, .3152, .2736, .2366)),
   check.names = F
)

formattable(top_aid_ngo, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "NGO" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")), 
               "Total interactions" = color_bar("#B1CBEB", fun = unit.scale),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Page followers" = color_bar("#B1CBEB", fun = unit.scale),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))

aid_compare <- data.frame(
   `Year` = ymd(c(20200101, 20190101, 20180101)),
   `Total interactions` = c(543524, 517249, 571176),
   `Interaction rate` = percent(c(.00225, .00233, .00268)),
   `Avg. posts per day` = c(5.37, 5.6, 5.79),
   `Page followers` = c(1350000, 1200000, 1100000),
   `Growth` = percent(c(.1277, .0828, .0745)),
   check.names = F
)

formattable(aid_compare, align = c("l", "l", "c", "c", "l", "c"), 
            list(
               "Year" = formatter("span", style = ~ formattable::style(color = "grey",font.weight = "bold")),
               "Interaction rate" = color_tile("#DeF7E9", "#71CA97"),
               "Avg. posts per day" = color_tile("#DeF7E9", "#71CA97"),
               "Growth" = color_tile("#DeF7E9", "#71CA97")))


### Bubble Plots -------------------
### Top 5 NGOs by Sector interaction rate for 2020-2021

top_env_ngo <- data.frame(
   "Sector" = "environmental",
   "NGO" = c("Greenpeace France", "PETA France", "WWF-France", "Wildlife Angel", "CCFD-Terre Solidaire"),
   'Total interactions' = c(1390000, 1250000, 573657, 130913, 78364),
   'Interaction rate' = percent(c(.00366, .02785, .00291, .01656, .00126)),
   'Avg. posts per day' = c(1.25, 1.1, 0.63, 0.88, 0.99),
   'Page followers' = c(869145, 117628, 899401, 27648, 174468),
   "Growth" = percent(c(.0983, .1246, .0956, .2850, .0289)),
   check.names = F)

top_hum_ngo <- data.frame(
   "Sector" = "human_rights",
   "NGO" = c("ATTAC France", "Amnesty International France", "Ligue des droits de l’Homme", 
             "Survival International", "La Cimade"),
   `Total interactions` = c(1230000, 930742, 394572, 170334, 59025),
   `Interaction rate` = percent(c(.00793, .00379, .00116, .00038, .00387)),
   `Avg. posts per day` = c(3.08, 1.11, 6.31, 3.92, 0.52),
   `Page followers` = c(148876, 616159, 153958, 312339, 86134),
   `Growth` = percent(c(.1776, .0429, .0986, .0065, .1455)),
   check.names = F
)

top_health_ngo <- data.frame(
   "Sector" = "health",
   "NGO" = c("Médecins Sans Frontieres", "Croix-Rouge Française", "Médecins du Monde France",
             "European Society of Cardiology", "AIDES"),
   `Total interactions` = c(747658, 346901, 82930, 62356, 46113),
   `Interaction rate` = percent(c(.00216, .00246, .00063, .00078, .00160)),
   `Avg. posts per day` = c(1.59, 0.72, 0.86, .95, 1.17), 
   `Page followers` = c(610129, 549852, 426132, 239150, 69289),
   `Growth` = percent(c(.0585, .0616, .0361, .0947, .0743)),
   check.names =  F
)

top_education_ngo <- data.frame(
   `Sector` = "Education",
   `NGO` = c("Organisation internationale de Francophonie (OIF)", "Institut du Monde Arabe",
             "Fondation Napoleon", "Enfants du Mekong", 
             "Jeux de la Francophonie"),
   `Total interactions` = c(117533, 97735, 86317, 48085, 47539),
   `Interaction rate` = percent(c(.00137, .00095, .00655, .00215, .02003)),
   `Avg. posts per day` = c(.08, 1.7, 2.58, 0.96, 0.44), 
   `Page followers` = c(297035, 174870, 15188, 64432, 15912),
   `Growth` = percent(c(.0132, .1286, .2069, 0.0374, .1581)),
   check.names = F
)

education_compare <- data.frame(
   `Year` = ymd(c(20200101, 20190101, 20180101)),
   `Total interactions` = c(1490000, 828332, 799252),
   `Interaction rate` = percent(c(.00242, .00258, .00277)),
   `Avg. posts per day` = c(43.83, 41.88, 40.81),
   `Page followers` = c(1960000, 1720000, 1570000),
   `Growth` = percent(c(.1073, .0940, .1543)),
   check.names = F
)

top_aid_ngo <- data.frame(
   `Sector` = "humanitarian_aid",
   `NGO` = c("Secours Catholique-Caritas France (SC-CF)", "Action contre la Faim",
             "Secours Islamique France", "The Alliance for International Medical Action (ALIMA)",
             "ACTED"),
   `Total interactions` = c(55978, 35156, 24232, 23820, 18912),
   `Interaction rate` = percent(c(.00795, .0008, .00025, .00408, .00327)),
   `Avg. posts per day` = c(0.51, 0.43, 0.81, 0.78, 0.36), 
   `Page followers` = c(40859, 281558, 362737, 22770, 48424),
   `Growth` = percent(c(.2078, .0242, .3152, .2736, .2366)),
   check.names = F
)

# merge dataframes
top_5_sectors <- bind_rows(top_env_ngo, top_aid_ngo, top_education_ngo, top_health_ngo, top_hum_ngo)

is.numeric(top_5_sectors$`Interaction rate`)

# Bubble plot for Top 5 NGO sectors 2020-2021
top_5_sectors %>%
   arrange(desc(`Page followers`)) %>%
   mutate(Sector = factor(NGO, NGO)) %>%
   ggplot(aes(x = `Avg. posts per day`, y = `Interaction rate`, size = `Page followers`,
              fill = Sector)) +
   geom_point(alpha = 0.5, shape = 21, color = "black") +
   scale_size(range = c(.5, 24), name = "Page followers") +
   scale_fill_viridis(discrete=TRUE, guide = FALSE, option = "A") +
   scale_y_continuous(labels = percent_format()) +
   theme(legend.position = "bottom") +
   theme_bw() +
   ylab("Interaction Rate") +
   xlab("Avg. posts per day") +
   theme(legend.position = "none")


### Bubble plot for all Sectors 2020-2021
all_ngo_sectors2020 <- X2021_06_14_11_38_59_EDT_facebook_list_2020_01_01_00_00_00_ET_2021_01_01_23_59_59_ET %>%
   select(List, `All Interaction Rate`, `Page Growth %`, `Page Followers`, `Posts Per Day`) %>%
   filter(!List %in% c("Paris NGOs", "International NGOs", "French NGOs", "Regional NGOs", "Animal Welfare",
                    "Debt Relief", "Trade", "Transport/Infrastructure", "Energy", "Averages"))

all_ngo_sectors2020$Year <- as.Date("2020-01-01")

all_ngo_sectors2020$`Page Growth %` <- as.character(all_ngo_sectors2020$`Page Growth %`)

all_ngo_sectors2020_plot <- all_ngo_sectors2020 %>%
   arrange(desc(`Page Followers`)) %>%
   mutate(Sector = factor(List, List)) %>%
   ggplot(aes(x = `Posts Per Day`, y = `All Interaction Rate`, size = `Page Followers`,
              fill = `Sector`)) +
   geom_point(alpha = 0.5, shape = 21, color = "black") +
   scale_size(range = c(.5, 24), name = "Page followers") +
   scale_fill_viridis(discrete=TRUE, guide = FALSE, option = "A") +
   scale_y_continuous(labels = percent_format(accuracy = .1, scale = 1, )) +
   theme(legend.position = "top") +
   theme_bw() +
   ylab("Interaction Rate") +
   xlab("Avg. posts per day") +
   theme(legend.position = "none")

all_ngo_sectors2020_plot # final plot

# 2018 Bubble plot for all NGO sectors
all_ngo_sectors2018 <- X2021_06_15_07_59_05_EDT_facebook_list_2018_01_01_00_00_00_ET_2019_01_01_23_59_59_ET %>%
   select(List, `All Interaction Rate`, `Page Growth %`, `Page Followers`, `Posts Per Day`) %>%
   filter(!List %in% c("Paris NGOs", "International NGOs", "French NGOs", "Regional NGOs", "Animal Welfare",
                       "Debt Relief", "Trade", "Transport/Infrastructure", "Energy", "Averages"))

all_ngo_sectors2018$Year <- as.Date("2018-01-01") # create date column

all_ngo_sectors2018

all_ngo_sectors2018_plot <- all_ngo_sectors2018 %>%
   arrange(desc(`Page Followers`)) %>%
   mutate(Sector = factor(List, List)) %>%
   ggplot(aes(x = `Posts Per Day`, y = `All Interaction Rate`, size = `Page Followers`,
              fill = `Sector`)) +
   geom_point(alpha = 0.5, shape = 21, color = "black") +
   scale_size(range = c(.5, 24), name = "Page followers") +
   scale_fill_viridis(discrete=TRUE, guide = FALSE, option = "A") +
   scale_y_continuous(labels = percent_format(accuracy = .1, scale = 1, )) +
   theme(legend.position = "top") +
   theme_bw() +
   ylab("Interaction Rate") +
   xlab("Avg. posts per day") +
   theme(legend.position = "none")

all_ngo_sectors2018_plot # 2018 bubble plot


####-----------------------
#### Create 2019 Bubble Plot
all_ngo_sectors2019 <- facebook_list_2019_01_01_00_00_00_ET_2020_01_01_23_59_59_ET %>%
   select(List, `All Interaction Rate`, `Page Growth %`, `Page Followers`, `Posts Per Day`) %>%
   filter(!List %in% c("Paris NGOs", "International NGOs", "French NGOs", "Regional NGOs", "Animal Welfare",
                       "Debt Relief", "Trade", "Transport/Infrastructure", "Energy", "Averages")) 

all_ngo_sectors2019$Year <- as.Date("2019-01-01")

all_ngo_sectors2019_plot <- all_ngo_sectors2019 %>%
   arrange(desc(`Page Followers`)) %>%
   mutate(Sector = factor(List, List)) %>%
   ggplot(aes(x = `Posts Per Day`, y = `All Interaction Rate`, size = `Page Followers`,
              fill = `Sector`)) +
   geom_point(alpha = 0.5, shape = 21, color = "black") +
   scale_size(range = c(.5, 24), name = "Page followers") +
   scale_fill_viridis(discrete=TRUE, guide = FALSE, option = "A") +
   scale_y_continuous(labels = percent_format(accuracy = .1, scale = 1, )) +
   theme(legend.position = "top") +
   theme_bw() +
   ylab("Interaction Rate") +
   xlab("Avg. posts per day") +
   theme(legend.position = "none")

all_ngo_sectors2019_plot # 2019 Bubble Plot


####-----------------------------
### Create 2021 bubble plot 
all_ngo_sectors2021 <- facebook_list_2021_01_01_00_00_00_ET_2021_06_01_23_59_59_ET %>%
   select(List, `All Interaction Rate`, `Page Growth %`, `Page Followers`, `Posts Per Day`) %>%
   filter(!List %in% c("Paris NGOs", "International NGOs", "French NGOs", "Regional NGOs", "Animal Welfare",
                       "Debt Relief", "Trade", "Transport/Infrastructure", "Energy", "Averages"))

all_ngo_sectors2021$Year <- as.Date("2021-01-01") # create date column

all_ngo_sectors2021_plot <- all_ngo_sectors2021 %>%
   arrange(desc(`Page Followers`)) %>%
   mutate(Sector = factor(List, List)) %>%
   ggplot(aes(x = `Posts Per Day`, y = `All Interaction Rate`, size = `Page Followers`,
              fill = `Sector`)) +
   geom_point(alpha = 0.5, shape = 21, color = "black") +
   scale_size(range = c(.5, 24), name = "Page followers") +
   scale_fill_viridis(discrete=TRUE, guide = FALSE, option = "A") +
   scale_y_continuous(labels = percent_format(accuracy = .1, scale = 1, )) +
   theme(legend.position = "top") +
   theme_bw() +
   ylab("Interaction Rate") +
   xlab("Avg. posts per day") +
   theme(legend.position = "none")


####-----------------------------
### Create 2017 bubble plot 
all_ngo_sectors2017 <- facebook_list_2017_01_01_00_00_00_ET_2018_01_01_23_59_59_ET %>%
   select(List, `All Interaction Rate`, `Page Growth %`, `Page Followers`, `Posts Per Day`) %>%
   filter(!List %in% c("Paris NGOs", "International NGOs", "French NGOs", "Regional NGOs", "Animal Welfare",
                       "Debt Relief", "Trade", "Transport/Infrastructure", "Energy", "Averages"))

all_ngo_sectors2017$Year <- as.Date("2017-01-01") # create date column

all_ngo_sectors2017_plot <- all_ngo_sectors2017 %>%
   arrange(desc(`Page Followers`)) %>%
   mutate(Sector = factor(List, List)) %>%
   ggplot(aes(x = `Posts Per Day`, y = `All Interaction Rate`, size = `Page Followers`,
              fill = `Sector`)) +
   geom_point(alpha = 0.5, shape = 21, color = "black") +
   scale_size(range = c(.5, 24), name = "Page followers") +
   scale_fill_viridis(discrete=TRUE, guide = FALSE, option = "A") +
   scale_y_continuous(labels = percent_format(accuracy = .1, scale = 1, )) +
   theme(legend.position = "top") +
   theme_bw() +
   ylab("Interaction Rate") +
   xlab("Avg. posts per day") +
   theme(legend.position = "none")

all_ngo_sectors20182020 <- bind_rows(all_ngo_sectors2017, all_ngo_sectors2018, all_ngo_sectors2019, all_ngo_sectors2020, all_ngo_sectors2021) # combine data frames
all_ngo_sectors20182020 <- rename(all_ngo_sectors20182020, Sector = `List`)

## Create 2017-2021 bubble plot
pplot <- all_ngo_sectors20182020 %>%
   arrange(desc(`Page Followers`)) %>%
   ggplot(aes(x = `Posts Per Day`, y = `All Interaction Rate`, size = `Page Followers`,
              fill = `Sector`, label = `Sector`)) +
   geom_point(alpha = 0.5, shape = 21, color = "black") +
   geom_text(size = 3) +
   scale_size(range = c(.5, 24), name = "Page followers") +
   scale_fill_viridis(discrete=TRUE, guide = FALSE, option = "A") +
   scale_y_continuous(labels = percent_format(accuracy = .1, scale = 1, )) +
   theme(legend.position = "top") +
   theme_bw() +
   ylab("Interaction rate") +
   xlab("Avg. posts per day by sector") +
   theme(legend.position = "none") +
   labs(title = "Year: {format(frame_time, '%Y')} NGO social media trends and public engagement", 
        x = "Avg. posts per day by sector", y = "Interaction rate") +
   transition_time(Year) +
   ease_aes()

animate(pplot, height = 400, width = 600, # animate the bubble plot
        end_pause = 30, renderer = gifski_renderer())

   anim_save("ngo_sector.gif", pplot) #save as .gif
   
   
   
 

   
