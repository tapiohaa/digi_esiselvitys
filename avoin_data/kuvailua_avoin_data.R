
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script kuvailua_avoin_data.R        ###
###                  Tapio Haaga                  ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Some descriptive analysis based on publicly available data.
rm(list=ls())

# Make sure the following packages are installed (version in parentheses):
library(here)           # A Simpler Way to Find Your Files (1.0.1)
library(jsonlite)       # A JSON Parser and Generator for R (1.8.0)
library(data.table)     # Extension of 'data.frame' (1.14.2)
library(ggplot2)        # Visualizations Using the Grammar of Graphics (3.3.6)
library(patchwork)      # The Composer of Plots (1.1.1)


# Inputs:
inpt_meh.dashboard = here::here('avoin_data', 'data', 'mehiläinen_conctacts_daily.json')
inpt_ter.dashboard = here::here('avoin_data', 'data', 'terveystalo_dashboard.csv')
input_population = here::here('avoin_data', 'data', 'vaesto_harjun_terveys_2021.csv')

# Outputs:
otpt_plot.by.wday = here::here('avoin_data', 'kuviot', 'käynnit_viikonpäivittäin_avoin_data.pdf')
otpt_plot.over.time = here::here('avoin_data', 'kuviot', 'käynnit_ajassa_avoin_data.pdf')
otpt_plot.over.time.ter = here::here('avoin_data', 'kuviot', 'käynnit_ajassa_terveystalo_avoin_data.pdf')
output_harju = here::here('avoin_data', 'kuviot', 'kayttajat_harjun_terveys_2021.pdf')


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 1) Load and tidy the data from Mehiläinen and Terveystalo: ####
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Parse the Mehiläinen JSON data (chat-based visits) and create a data.table:
data.meh = jsonlite::fromJSON(inpt_meh.dashboard)
df.meh = as.data.table(data.meh$series$data[[1]])
colnames(df.meh) = c("datetime", "visits")

# Tidy the Mehiläinen data table:
df.meh[, ':=' (visits = as.integer(visits),
               date = as.Date(substr(datetime, 1, 10)),
               source = 'Mehiläinen')]
df.meh[, datetime := NULL]


# Load and tidy the Terveystalo data (remote visits: calls, chat, video):

df.ter = as.data.table(read.csv(inpt_ter.dashboard, sep=';', na.strings = ''))

df.ter[!is.na(X2020), X2020 := paste(X2020, 2020, sep='')]
df.ter[!is.na(X2021), X2021 := paste(X2021, 2021, sep='')]
df.ter[!is.na(X2022), X2022 := paste(X2022, 2022, sep='')]
df.ter[!is.na(X2023), X2023 := paste(X2023, 2023, sep='')]

df.ter[, ':=' (X2020 = as.Date(X2020, format='%d.%m.%Y'),
               X2021 = as.Date(X2021, format='%d.%m.%Y'),
               X2022 = as.Date(X2022, format='%d.%m.%Y'),
               X2023 = as.Date(X2023, format='%d.%m.%Y'))]

df.ter = data.table(
  date = c(df.ter$X2020, df.ter$X2021, df.ter$X2022, df.ter$X2023),
  visits = c(df.ter$X2020_kaynteja, df.ter$X2021_kaynteja, 
             df.ter$X2022_kaynteja, df.ter$X2023_kaynteja),
  source = 'Terveystalo'
)

df.ter = df.ter[!is.na(date)]


# Combine the datasets and map dates to weekday numbers:

df = rbind(df.meh, df.ter, fill=TRUE)

# Define a function to map weekday names to numbers
weekday_to_number = function(weekday) {
  switch(weekday,
         "Monday" = 1,
         "Tuesday" = 2,
         "Wednesday" = 3,
         "Thursday" = 4,
         "Friday" = 5,
         "Saturday" = 6,
         "Sunday" = 7
  )
}

# Add a new column with weekday numbers:
df[, weekday_no := sapply(weekdays(date), weekday_to_number)]


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 2) Plot visits by weekday and provider. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #

# 2.5.2022 - 10.9.2023
# Mehiläinen: "digiklinikka", chat-based visits
# Terveystalo: "etävastaanotot", calls, chat, video, 
#               in occupational care and for private clients

# Aggregate visits by weekday and provider:
data = df[date >= as.Date('2022-05-02') & date <= as.Date('2023-09-10'), 
          .(visits = mean(visits)), by=c('weekday_no', 'source')]

# The total number of visits by provider:
sum.ter = data[source=='Terveystalo', sum(visits)]
sum.meh = data[source=='Mehiläinen', sum(visits)]

# Compute shares of how many visits are on a given weekday:
data[source=='Terveystalo', 
     visits.share := round(100 * visits / sum.ter, digits=0), by='source']
data[source=='Mehiläinen', 
     visits.share := round(100 * visits / sum.meh, digits=0), by='source']
data[, visits.share := paste(visits.share, '%')]


# Loop over providers and plot:

providers = c('Mehiläinen', 'Terveystalo')

plots.by.wday = lapply(providers, function(provider) {
  
  data.plt = data[source==provider]
  
  if(provider == 'Mehiläinen') {
    p.title = 'Mehiläisen digiklinikka'
    p.x.lab = 'Yhteydenotot / vrk (ka.)'
    
  } else if (provider == 'Terveystalo') {
    p.title = 'Terveystalon etävastaanotot'
    p.x.lab = 'Etävastaanotot / vrk (ka.)' }
  
  
  p = ggplot2::ggplot(data=data.plt, aes(x=weekday_no, y=visits)) +
    geom_bar(stat= 'identity', fill = 'grey', color = 'black') +
    geom_text(aes(label = round(visits, digits=0)), vjust = -0.5, size = 3) +
    geom_text(aes(label = visits.share), vjust = 1.5, size = 3) +
    geom_hline(yintercept = data.plt[, mean(visits)]) + 
    annotate('text', x = 6.5, y = data.plt[, mean(visits)],
             label = paste('Ka. ', data.plt[, round(mean(visits), digits=0)],
                           ' / vrk ja ', 
                           round(365*data.plt[, mean(visits)] / 1000000, digits=1), 
                           ' m / v', sep=''), 
             size = 3, vjust = -0.5) +
    scale_x_continuous(breaks = 1:7,
                       labels = c('Ma', 'Ti', 'Ke', 'To', 'Pe', 'La', 'Su')) +
    labs(title = p.title, y = p.x.lab, x = 'Viikonpäivä') +
    ylim(0, 1.1 * max(data.plt$visits)) + 
    theme(text = element_text(size=20),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
  
  return(p)

})
names(plots.by.wday) = providers


# Save:
plot.by.wday = plots.by.wday$Mehiläinen + plots.by.wday$Terveystalo 
ggsave(otpt_plot.by.wday, plot.by.wday, width = 15, height = 6)


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 3) Plot visits by time. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Both: 2.5.2022 - 10.9.2023

data = df[date >= as.Date('2022-05-02') & date <= as.Date('2023-09-10')]
data[, visits.moving.avr := frollmean(visits, n=14, align = 'right'), by='source']

# Loop over providers and plot:

providers = c('Mehiläinen', 'Terveystalo')

plots.over.time = lapply(providers, function(provider) {
  
  data.plt = data[source==provider]
  
  if(provider == 'Mehiläinen') {
    p.title = 'Mehiläisen digiklinikka'
    p.x.lab = 'Yhteydenotot / vrk (liukuva 14 vrk)'
    
  } else if (provider == 'Terveystalo') {
    p.title = 'Terveystalon etävastaanotot'
    p.x.lab = 'Etävastaanotot / vrk (liukuva 14 vrk)' }
  
  
  p = ggplot2::ggplot(data=data.plt, aes(x=date, y=visits.moving.avr)) +
    geom_line(size=1.5) +
    labs(title = p.title, y = p.x.lab, x = 'Päivämäärä') +
    ylim(0, 1.1 * max(data.plt$visits.moving.avr)) + 
    scale_x_date(breaks = c(as.Date('2022-07-01'),
                            as.Date('2022-10-01'),
                            as.Date('2023-01-01'),
                            as.Date('2023-04-01'),
                            as.Date('2023-07-01')),
                 labels = c('7/2022', '10/2022', '1/2023', '4/2023', '7/2023')) + 
    theme(text = element_text(size=20),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
  
  return(p)
  
})
names(plots.over.time) = providers

# Save:
plot.over.time = plots.over.time$Mehiläinen + plots.over.time$Terveystalo
ggsave(otpt_plot.over.time, plot.over.time, width = 15, height = 6)


# Terveystalo: 1.3.2020 - 10.9.2023

data = df[date >= as.Date('2020-03-01') & date <= as.Date('2023-09-10') &
            source == 'Terveystalo']
data[, visits.moving.avr := frollmean(visits, n=14, align = 'right'), by='source']

plot.over.time.ter = ggplot2::ggplot(data=data, aes(x=date, y=visits.moving.avr)) +
  geom_line() +
  labs(title = 'Terveystalon etävastaanotot', 
       y = 'Etävastaanotot / vrk (liukuva 14 vrk)', 
       x = 'Päivämäärä') +
  ylim(0, 1.1 * max(data$visits.moving.avr)) + 
  scale_x_date(breaks = c(as.Date('2020-01-01'),
                          as.Date('2021-01-01'),
                          as.Date('2022-01-01'),
                          as.Date('2023-01-01')),
               labels = c('1/2020', '1/2021', '1/2022', '1/2023')) + 
  theme(text = element_text(size=20),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                        colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                        colour = "lightgrey"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))


# Save:
ggsave(otpt_plot.over.time.ter, plot.over.time.ter, width = 7.5, height = 6)


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 4) Plot the share of digital clinic users (Harjun terveys). ####
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Source: https://harjunterveys.fi/wp-content/uploads/2022/06/202206_EHMA_Paijat_Sote_Digital-solutions-1.pdf

#  Create the datasets that we will plot:

gender = data.table(
  dimension = 'Sukupuoli',
  label = c('Naiset', 'Miehet'),
  order = 1:2,
  users = c(4783, 2253),
  population = c(63540, 67291)
)

age = data.table(
  dimension = 'Ikäryhmä',
  label = c('0-9', '10-19', '20-29', '30-39', '40-49', 
            '50-59', '60-69', '70-79', '80-89', '90-99'),
  order = 4:13,
  users = c(1229, 537, 1479, 1271, 906, 680, 646, 298, 50, 3)
)

age.pop = setDT(read.csv(input_population, sep=';'))
age.pop = age.pop[, .(population = sum(vaesto)), by='label']
age = merge(age, age.pop, by='label', all.x = TRUE)

dt = rbind(gender, age)
dt[, users.per.capita := 100 * users / population]


# Plot:
p = ggplot(dt, aes(x=order, y=users.per.capita)) +
  geom_bar(stat= 'identity', fill = 'grey', color = 'black') + 
  geom_text(aes(label = paste(round(users.per.capita, digits=1), '%')), 
            vjust = -0.5, size = 3) +
  annotate('text', x = 13, y = 5.263, label = 'Ka. 5.3 %', size = 3, vjust = -0.5) +
  scale_x_continuous(breaks = dt$order, labels = dt$label) +
  geom_hline(yintercept = 5.263) +
  ylim(0, 1.1 * dt[, max(users.per.capita)]) + 
  labs(y = 'Käyttäjiä väestöstä (%)') +
  theme(text = element_text(size=20),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                        colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                        colour = "lightgrey"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

# Save:
ggsave(output_harju, p, width = 12, height = 6)

# End.
