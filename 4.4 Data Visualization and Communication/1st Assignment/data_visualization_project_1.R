##################################    1. Libraries    ########################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

rm(list = ls())
Sys.setlocale("LC_TIME", "C")
options(scipen=10000)

if ("ggplot2" %in% rownames(installed.packages()) == F){
  install.packages("ggplot2")}
require(ggplot2)
if ("dplyr" %in% rownames(installed.packages()) == F){
  install.packages("dplyr")}
require(dplyr)
if ("readxl" %in% rownames(installed.packages()) == F){
  install.packages("readxl")}
require(readxl)
if ("RColorBrewer" %in% rownames(installed.packages()) == F){
  install.packages("RColorBrewer")}
require(RColorBrewer)
if ("reshape2" %in% rownames(installed.packages()) == F){
  install.packages("reshape2")}
require(reshape2)
if ("maps" %in% rownames(installed.packages()) == F){
  install.packages("maps")}
require(maps)
if ("tidyverse" %in% rownames(installed.packages()) == F){
  install.packages("tidyverse")}
require(tidyverse)


##################################    2. Load Data    ########################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

# Set working directory
setwd("C:/Users/sofia.baltzi/OneDrive - Accenture/Desktop/Data Visualization")

# Read data
data = read_excel("COVID-19-geographic-disbtribution-worldwide.xlsx")

##################################    3. Data Manipulation    ################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

# Add weekdays/months to data, treat dates
data = data %>% 
  dplyr::mutate(weekday = weekdays(dateRep),
                month = months(dateRep),
                dateRep = as.Date(dateRep)) %>% 
  data.frame()

##################################    4. Plot    #############################################
#                                                                                            #
#                                                                                            #
#                                                                                            #

#####----------- Figure 1 ----------------------------------------------------------------####

# Group data by date
group_date = data %>% 
  dplyr::group_by(dateRep) %>% 
  dplyr::summarise(total_cases = sum(cases),
                   total_deaths = sum(deaths)) %>% 
  data.frame()

cols = c("Steelblue", "Red2")

ggplot(data=group_date, aes(x=dateRep)) +
  theme(legend.position = "bottom",
        legend.margin=margin(-5,0,0,0),
        axis.text.y.right=element_text(colour=cols[2]),
        axis.ticks.y.right=element_line(colour=cols[2]),
        axis.title.y.right=element_text(colour=cols[2]),
        axis.text.y=element_text(colour=cols[1]),
        axis.ticks.y=element_line(colour=cols[1]),
        axis.title.y=element_text(colour=cols[1]))+
  labs(title="Total Daily Cases/Deaths - Worldwide")+
  xlab("Time") + 
  ylab("Cases (In thousands)") +
  geom_line(aes(y=total_cases/1000), size=0.9, colour=cols[1]) +
  geom_line(aes(y=total_deaths/100), size=0.9, colour=cols[2]) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Deaths (In hundreds)")) +
  scale_color_manual(name = "Legend", labels = c("Cases", "Deaths")) +
  scale_x_date(date_labels="%b", date_breaks="1 month")


#####----------- Figures 2, 3 ------------------------------------------------------------####

# Group data by date, continent and country 
# - aggregate total cases / deaths and avg population per country
group_cont_cntr = data %>% 
  dplyr::group_by(dateRep, continentExp, countriesAndTerritories) %>% 
  dplyr::summarise(total_cases = sum(cases),
                   total_deaths = sum(deaths),
                   avg_pop = mean(popData2019)) %>% 
  dplyr::mutate(dateRep = as.Date(dateRep)) %>% 
  data.frame()

# Group group_cont_cntr by date, continent 
# - aggregate total cases / deaths and avg population per continent
group_cont = group_cont_cntr %>%
  dplyr::filter(continentExp!="Other") %>% 
    dplyr::group_by(dateRep, continentExp) %>% 
  dplyr::summarise(total_cases_cnt = sum(total_cases),
                   total_deaths_cnt = sum(total_deaths),
                   pop_cnt = sum(avg_pop)) %>% 
  dplyr::mutate(dateRep = as.Date(dateRep),
                cases_over_pop=total_cases_cnt*100/pop_cnt,
                deaths_over_pop=total_deaths_cnt*100/pop_cnt) %>% 
  data.frame()


ggplot(data=group_cont, aes(x=dateRep, y=cases_over_pop, group=continentExp, color=factor(continentExp))) +
  labs(title = "Total Daily Cases per Continent",
       subtitle = "As percentage of continents' population",
       color="Continent")+
  xlab("Time") + 
  ylab("Cases") +
  geom_line(size=0.7) +
  scale_color_brewer(palette="Set1") +
  scale_x_date(date_labels="%b", date_breaks="1 month")

ggplot(data=group_cont, aes(x=dateRep, y=deaths_over_pop, group=continentExp, color=factor(continentExp))) +
  labs(title = "Total Daily Deaths per Continent",
       subtitle = "As percentage of continents' population",
       color="Continent")+
  xlab("Time") + 
  ylab("Deaths") +
  ylim(0, NA) +
  geom_line(size=0.7) +
  scale_color_brewer(palette="Set1") +
  scale_x_date(date_labels="%b", date_breaks="1 month")

#####----------- Figure 4 ----------------------------------------------------------------####

# Europe's cases and deaths per month
group_europe_c = data %>% 
  dplyr::filter(continentExp=="Europe") %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(value = sum(cases)) %>% 
  dplyr::mutate(variable="Cases") %>% 
  data.frame()

group_europe_d = data %>% 
  dplyr::filter(continentExp=="Europe") %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(value = sum(deaths)) %>% 
  dplyr::mutate(variable="Deaths") %>% 
  data.frame()

group_europe = rbind(group_europe_c, group_europe_d)

group_europe = group_europe %>% 
  dplyr::filter(month!="December") %>%
  data.frame()


ggplot(group_europe, aes(x = month, y = value, fill = factor(variable))) +
  labs(title = "Total Cases/Deaths in Europe per Month",
       subtitle = bquote("Data was available for up to " *21^st*" of November"))+
  geom_col(position = "dodge") +
  scale_fill_manual("Legend", values = c("Cases" = "Sky Blue 4", "Deaths" = "Red")) +
  geom_text(aes(label=scales::comma(round(value), accuracy=1)), position=position_dodge(width=0.9), vjust=-0.25, size=2.8) +
  xlim("February","March","April","May","June","July","August","September","October","November") +
  xlab("Months") +
  ylab("Total Cases/Deaths")

#####----------- Figure 5 ----------------------------------------------------------------####

# Load coordinates
world = map_data("world")

# Total deaths in each of Europe's countries
europe_group = data %>% 
  dplyr::filter(continentExp=="Europe", dateRep>"2020-10-22") %>% 
  dplyr::group_by(countriesAndTerritories) %>% 
  dplyr::summarise(tot_cases = sum(cases),
                   tot_deaths = sum(deaths),
                   avg_pop = mean(popData2019)) %>% 
  dplyr::mutate(Cases = tot_cases*100/avg_pop,
                Deaths = tot_deaths*100/avg_pop) %>% 
  data.frame()

# Labels for map
# Cases
labels = europe_group %>% 
  dplyr::filter(Cases>1.5|Cases<0.4|countriesAndTerritories=="Greece") %>% 
  dplyr::select(countriesAndTerritories) %>% 
  dplyr::distinct() %>% 
  data.frame()

labels = labels %>% 
  dplyr::rename(region=countriesAndTerritories) %>%
  dplyr::mutate(region=dplyr::case_when( region=="United_Kingdom"~"UK",
                                         region=="North_Macedonia"~"Macedonia",
                                         region=="Czechia"~"Czech Republic",
                                         region=="Bosnia_and_Herzegovina"~"Bosnia and Herzegovina",
                                         TRUE~region)) %>% 
  dplyr::left_join(world, by = "region") %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarise(long=mean(long),
                   lat=mean(lat)) %>% 
  dplyr::filter(!is.na(lat), 
                !region %in% c("Guernsey", "Jersey", "Liechtenstein")) %>% 
  data.frame()

# Deaths
labels_d = europe_group %>% 
  dplyr::filter(Deaths>0.03|Deaths<0.005|countriesAndTerritories=="Greece") %>% 
  dplyr::select(countriesAndTerritories) %>% 
  dplyr::distinct() %>% 
  data.frame()

labels_d = labels_d %>% 
  dplyr::rename(region=countriesAndTerritories) %>%
  dplyr::mutate(region=dplyr::case_when( region=="United_Kingdom"~"UK",
                                         region=="North_Macedonia"~"Macedonia",
                                         region=="Czechia"~"Czech Republic",
                                         region=="Bosnia_and_Herzegovina"~"Bosnia and Herzegovina",
                                         TRUE~region)) %>% 
  dplyr::left_join(world, by = "region") %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarise(long=mean(long),
                   lat=mean(lat)) %>% 
  dplyr::filter(!is.na(lat), 
                !region %in% c("Guernsey", "Jersey", "Liechtenstein")) %>% 
  data.frame()
  


# Join europe data with coordinates
europe_group = europe_group %>% 
  dplyr::rename(region=countriesAndTerritories) %>%
  dplyr::mutate(region=dplyr::case_when( region=="United_Kingdom"~"UK",
                                         region=="North_Macedonia"~"Macedonia",
                                         region=="Czechia"~"Czech Republic",
                                         region=="Bosnia_and_Herzegovina"~"Bosnia and Herzegovina",
                                         TRUE~region)) %>% 
  dplyr::left_join(world, by = "region") %>% 
  dplyr::filter(region!="Andorra") %>% 
  dplyr::select(region, group, long, lat, Cases, tot_cases, Deaths, tot_deaths) %>% 
  data.frame()

# Cases
ggplot(europe_group, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = Cases))+
  labs(title = "Total Cases per Country the month of November",
       subtitle = "As percentage of country population")+
  geom_text(aes(label = region), data = labels,  size = 3)+
  scale_fill_distiller(palette = "Spectral")+
  ylim(35, 75) +
  xlim(-25, 50) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text =element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# Deaths
ggplot(europe_group, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = Deaths))+
  labs(title = "Total Deaths per Country the month of November",
       subtitle = "As percentage of country population")+
  geom_text(aes(label = region), data = labels_d,  size = 3)+
  scale_fill_distiller(palette = "Spectral")+
  ylim(35, 75) +
  xlim(-25, 50) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text =element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())



#####----------- Figures 6,7 ---------------------------------------------------------------####

# Rank Countries
# Cases
group_european_countries = data %>% 
  dplyr::filter(continentExp=="Europe",
                countriesAndTerritories %in% c("Austria","Italy","Belgium","Latvia","Bulgaria","Lithuania",
                                               "Croatia","Luxembourg","Cyprus","Malta","Czechia","Netherlands",
                                               "Denmark","Poland","Estonia","Portugal","Finland","Romania",
                                               "France","Slovakia","Germany","Slovenia","Greece","Spain",
                                               "Hungary","Sweden","Ireland"	)) %>% 
  dplyr::group_by(countriesAndTerritories) %>% 
  dplyr::summarise(tot_cases = sum(cases)) %>% 
  dplyr::mutate(countriesAndTerritories=dplyr::case_when( countriesAndTerritories=="United_Kingdom"~"UK",
                                                          countriesAndTerritories=="North_Macedonia"~"Macedonia",
                                                          countriesAndTerritories=="Czechia"~"Czech Republic",
                                                          countriesAndTerritories=="San_Marino"~"San Marino",
                                                          countriesAndTerritories=="Holy_See"~"Holy See",
                                                          countriesAndTerritories=="Isle_of_Man"~"Isle of Man",
                                                          countriesAndTerritories=="Faroe_Islands"~"Faroe Islands",
                                                          countriesAndTerritories=="Bosnia_and_Herzegovina"~"Bosnia and Herzegovina",
                                                          TRUE~countriesAndTerritories)) %>% 
  dplyr::arrange(-tot_cases) %>% 
  dplyr::mutate(countriesAndTerritories=paste(countriesAndTerritories, "- Rank:", row_number())) %>% 
  data.frame()


ggplot(group_european_countries, aes(x=reorder(countriesAndTerritories, tot_cases), y=tot_cases, 
                                     fill=factor(ifelse(countriesAndTerritories=="Greece - Rank: 17","Highlighted","Normal")))) +
  labs(title = "Total Cases per Country, European Union")+
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_manual(name = "countriesAndTerritories", values=c("orange","Sky Blue 4")) +
  xlab("Countries") + 
  ylab("Cases") +
  scale_y_continuous(n.breaks = 10, labels = comma) +
  coord_flip()

# Deaths
group_european_countries = data %>% 
  dplyr::filter(continentExp=="Europe",
                countriesAndTerritories %in% c("Austria","Italy","Belgium","Latvia","Bulgaria","Lithuania",
                                               "Croatia","Luxembourg","Cyprus","Malta","Czechia","Netherlands",
                                               "Denmark","Poland","Estonia","Portugal","Finland","Romania",
                                               "France","Slovakia","Germany","Slovenia","Greece","Spain",
                                               "Hungary","Sweden","Ireland"	)) %>% 
  dplyr::group_by(countriesAndTerritories) %>% 
  dplyr::summarise(tot_deaths = sum(deaths)) %>% 
  dplyr::mutate(countriesAndTerritories=dplyr::case_when( countriesAndTerritories=="United_Kingdom"~"UK",
                                                          countriesAndTerritories=="North_Macedonia"~"Macedonia",
                                                          countriesAndTerritories=="Czechia"~"Czech Republic",
                                                          countriesAndTerritories=="San_Marino"~"San Marino",
                                                          countriesAndTerritories=="Holy_See"~"Holy See",
                                                          countriesAndTerritories=="Isle_of_Man"~"Isle of Man",
                                                          countriesAndTerritories=="Faroe_Islands"~"Faroe Islands",
                                                          countriesAndTerritories=="Bosnia_and_Herzegovina"~"Bosnia and Herzegovina",
                                                          TRUE~countriesAndTerritories)) %>% 
  dplyr::arrange(-tot_deaths) %>% 
  dplyr::mutate(countriesAndTerritories=paste(countriesAndTerritories, "- Rank:", row_number())) %>% 
  data.frame()


ggplot(group_european_countries, aes(x=reorder(countriesAndTerritories, tot_deaths), y=tot_deaths, 
                                     fill=factor(ifelse(countriesAndTerritories=="Greece - Rank: 16","Highlighted","Normal")))) +
  labs(title = "Total Deaths per Country, European Union")+
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_manual(name = "countriesAndTerritories", values=c("orange","Red")) +
  xlab("Countries") + 
  ylab("Deaths") +
  scale_y_continuous(n.breaks = 10, labels = comma) +
  coord_flip()


#####----------- Figures 8,9 -------------------------------------------------------------####

# An average of Europe's population (without Greece)
eur_pop = (data %>%
  dplyr::filter(continentExp=="Europe",
                countriesAndTerritories!="Greece") %>% 
  dplyr::group_by(countriesAndTerritories) %>% 
  dplyr::summarise(pop=mean(popData2019)) %>% 
  dplyr::select(pop) %>% 
  dplyr::summarise(eur_avg_pop=mean(pop)) %>% 
  data.frame())[1,1]

# An average of Greece's population
greece_pop = (data %>%
                dplyr::filter(countriesAndTerritories=="Greece") %>% 
                dplyr::group_by(countriesAndTerritories) %>% 
                dplyr::summarise(pop=mean(popData2019)) %>% 
                data.frame())[1,2]

# Filter Europe countries
group_europe = data %>%
  dplyr::filter(continentExp=="Europe") %>% 
  dplyr::mutate(target=dplyr::case_when(countriesAndTerritories=="Greece"~"Greece",
                                        countriesAndTerritories!="Greece"~"Europe")) %>% 
  dplyr::mutate(cs_ovr_pop=dplyr::case_when(target=="Greece"~cases*100/greece_pop,
                                            target=="Europe"~cases*100/eur_pop)) %>% 
  dplyr::mutate(dths_ovr_pop=dplyr::case_when(target=="Greece"~deaths*100/greece_pop,
                                              target=="Europe"~deaths*100/eur_pop)) %>% 
  dplyr::group_by(dateRep,target) %>% 
  dplyr::summarise(total_cases=mean(cs_ovr_pop),
                   total_deaths=mean(dths_ovr_pop)) %>% 
  data.frame()

ggplot(data=group_europe, aes(x=dateRep, y=total_cases, group=target, color=factor(target))) +
  labs(title="Total Daily Cases in Greece VS Rest of Europe",
       subtitle="As percentage of Greece's/Europe's population, respectively",
       color="Area")+
  xlab("Time") + 
  ylab("Cases") +
  ylim(0, NA) +
  geom_line(size=0.8) +
  scale_color_brewer(palette="Set2") +
  scale_x_date(date_labels="%b", date_breaks="1 month")

ggplot(data=group_europe, aes(x=dateRep, y=total_deaths, group=target, color=factor(target))) +
  labs(title="Total Daily Deaths in Greece VS Rest of Europe",
       subtitle="As percentage of Greece's/Europe's population, respectively",
       color="Area")+
  xlab("Time") + 
  ylab("Deaths") +
  ylim(0, NA) +
  geom_line(size=0.8) +
  scale_color_brewer(palette="Set2") +
  scale_x_date(date_labels="%b", date_breaks="1 month")

#####----------- Figure 10 ----------------------------------------------------------------####

# Europe's cases and deaths per month
group_greece_c = data %>% 
  dplyr::filter(countriesAndTerritories=="Greece") %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(value = sum(cases)) %>% 
  dplyr::mutate(variable="Cases") %>% 
  data.frame()

group_greece_d = data %>% 
  dplyr::filter(countriesAndTerritories=="Greece") %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarise(value = sum(deaths)) %>% 
  dplyr::mutate(variable="Deaths") %>% 
  data.frame()

group_greece = rbind(group_greece_c, group_greece_d)

group_greece = group_greece %>% 
  dplyr::filter(month!="December",
                month!="January") %>%
  data.frame()


ggplot(group_greece, aes(x = month, y = value, fill = factor(variable))) +
  labs(title = "Total Cases/Deaths in Greece per Month",
       subtitle = bquote("Data was available for up to " *21^st*" of November"))+
  geom_col(position = "dodge") +
  scale_fill_manual("Legend", values = c("Cases" = "Sky Blue 4", "Deaths" = "Red")) +
  geom_text(aes(label=scales::comma(round(value), accuracy=1)), position=position_dodge(width=0.9), vjust=-0.25, size=2.8) +
  xlim("February","March","April","May","June","July","August","September","October","November") +
  xlab("Months") +
  ylab("Total Cases/Deaths")

#####----------- Figures 11,12 --------------------------------------------------------------####

# Percentage difference
monthseq = c("March","April","May","June","July","August","September","October","November")
perc_diff = data.frame()

for(i in c(1:8)){
  s = group_greece_c[group_greece_c$month==monthseq[i],"value"]
  e = group_greece_c[group_greece_c$month==monthseq[i+1],"value"]
  diff = (e-s)*100/s
  sd = group_greece_d[group_greece_d$month==monthseq[i],"value"]
  ed = group_greece_d[group_greece_d$month==monthseq[i+1],"value"]
  diffd = (ed-sd)*100/sd
  perc_diff=rbind(perc_diff, c(monthseq[i+1], diff, diffd))
}
colnames(perc_diff)=c("Months","Cases Percentage Difference", "Deaths Percentage Difference")
perc_diff$`Cases Percentage Difference` = as.numeric(perc_diff$`Cases Percentage Difference`)
perc_diff$`Deaths Percentage Difference` = as.numeric(perc_diff$`Deaths Percentage Difference`)

# Cases
ggplot(data=perc_diff, aes(x=Months, y=`Cases Percentage Difference`, group=1)) +
  labs(title = "Percentage Difference of Cases Between Months for Greece")+
  xlab("Months") + 
  ylab("Percentage") +
  geom_hline(yintercept=0, linetype="dashed", color="green", size=0.9)+
  geom_line(size=0.9, color="Sky Blue 4") +
  geom_point(size=4, shape=21, color = 'Sky Blue 4', fill='Sky Blue 4', stroke = 1) +
  geom_text(aes(label = paste(round(`Cases Percentage Difference`, 1),"%")),
            position=position_dodge(width=0.9), vjust=-0.7,
            show.legend = FALSE) +
  scale_color_brewer(palette="Set1") +
  xlim("April","May","June","July","August","September","October","November")


#Deaths
ggplot(data=perc_diff, aes(x=Months, y=`Deaths Percentage Difference`, group=1)) +
  labs(title = "Percentage Difference of Deaths Between Months for Greece")+
  xlab("Months") + 
  ylab("Percentage") +
  geom_hline(yintercept=0, linetype="dashed", color="green", size=0.9)+
  geom_line(size=0.9, color="red") +
  geom_point(size=4, shape=21, color = 'red', fill='red', stroke = 1) +
  geom_text(aes(label = paste(round(`Deaths Percentage Difference`, 1),"%")),
            position=position_dodge(width=0.9), vjust=-0.7,
            show.legend = FALSE) +
  scale_color_brewer(palette="Set1") +
  xlim("April","May","June","July","August","September","October","November")



#####----------- Bonus Figure -------------------------------------------------------------####

Legend = c("Thank", "you")
value = c(70, 30)
thanks = data.frame(cbind(class, value))

ggplot(thanks, aes(x = "", y = value, fill = Legend)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(title = "Useless Plot",
       subtitle = "This took more time than it should have")+
  coord_polar("y", start = 0)+
  theme_void()
