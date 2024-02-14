
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###    r-script 1_power_analysis_parameters.R     ###
###                Replication file               ###        
###               2024 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Compute parameters used in power calculations.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # Mutating and aggregating data.
library(ggplot2)          # Data visualizations.
library(fixest)           # Fast fixed-effects estimation.

# Inputs:
input_folk = here::here('data', 'interim', 'folk_clean.rds')
input_visits = here::here('data', 'interim', 'visits_clean_20') # visits_clean_20XX.rds, XX in 20:22

# Outputs:
output_share.clients = here::here('VOIMALASKELMAT', 'figures', 'kayttajat_lasnakaynnit_suomi.pdf')

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read, aggregate, and merge the datasets. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read the datasets:

folk = readRDS(input_folk)

years = 20:22 # 1.1.2020-15.6.2022

phc = lapply(years, function(yr) {
  source = paste(input_visits, yr, ".rds", sep="")
  data = readRDS(source)
})

phc = rbindlist(phc)


# Extract curative outpatient (T11) visits (R10) for doctors and nurses in
# public providers or health stations:
phc = phc[kaynti_palvelumuoto=='T11' & kaynti_yhteystapa=='R10' & 
            ammattiluokka %in% c(1, 2) & kaynti_luonne_sh==1 &
            (julkinen==1 | terveysasema==1),
          .(shnro, ammattiluokka, kaynti_alkoi)]

# Take visits from 1.6.2020-31.5.2022
phc[, kaynti_alkoi := as.Date(kaynti_alkoi)]
phc = phc[kaynti_alkoi >= as.Date('2020-06-01') &
            kaynti_alkoi <= as.Date('2022-05-31')]

phc[kaynti_alkoi >= as.Date('2020-06-01') &
      kaynti_alkoi <= as.Date('2021-05-31'), vuosi := 'kaynnit_ennen']
phc[kaynti_alkoi >= as.Date('2021-06-01') &
      kaynti_alkoi <= as.Date('2022-05-31'), vuosi := 'kaynnit_jalkeen']

# Sum at the patient-by-date level:
phc = phc[, .(visits = .N), by=c('shnro', 'kaynti_alkoi', 'vuosi')]

# Count the number of dates on which the patient had an appointment:
phc = phc[visits > 0, .N, by=c('shnro', 'vuosi')]
phc = phc[order(shnro)]

# Pivot wider:
phc = dcast(phc, shnro ~ vuosi, value.var = 'N')

# Merge PHC data to FOLK:
dt = merge(folk, phc, by='shnro', all.x = TRUE)
dt[is.na(kaynnit_ennen), kaynnit_ennen := 0]
dt[is.na(kaynnit_jalkeen), kaynnit_jalkeen := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Mean and standard deviation of the outcome. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# 6/2021-5/2022: curative outpatient primary care visit dates
# (doctors or nurses) per capita in public primary care in Finland
#   mean   variance    sd
# 1.174122 8.820297 2.969898
dt[, .(mean = mean(kaynnit_jalkeen),
       variance = var(kaynnit_jalkeen),
       sd = sd(kaynnit_jalkeen))]

# Mean number of visit days for those who visited at least once:
#   mean
# 2.845686
dt[kaynnit_jalkeen > 0, .(mean = mean(kaynnit_jalkeen))]
nrow(dt[kaynnit_jalkeen > 0])

# Compute a weighted average of visits by using the share of each age group
# out of all patients in the digital clinic of Harjun terveys (the young 
# receive a larger weight):

# First, label age groups:
dt[ika %in% c(0:9), label := '0-9']
dt[ika %in% c(10:19), label := '10-19']
dt[ika %in% c(20:29), label := '20-29']
dt[ika %in% c(30:39), label := '30-39']
dt[ika %in% c(40:49), label := '40-49']
dt[ika %in% c(50:59), label := '50-59']
dt[ika %in% c(60:69), label := '60-69']
dt[ika %in% c(70:79), label := '70-79']
dt[ika %in% c(80:89), label := '80-89']
dt[ika %in% c(90:99), label := '90-99']


# Then, compute the share of each age group out of all patients in Harjun 
# terveys (digiklinikka) in 2021. The number of users is publicly available data 
# released by Harjun terveys (presentation at the EHMA conference):
age = data.table(
  label = c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', 
            '70-79', '80-89', '90-99'),
  users = c(1229, 537, 1479, 1271, 906, 680, 646, 298, 50, 3)
)
users.sum = age[, sum(users)]
age[, weight.age := users / users.sum]
dt = merge(dt, age, by='label', all.x = TRUE)

# 6/2021-5/2022: curative outpatient primary care visit dates
# (doctors or nurses) per capita in public primary care in Finland
# mean: 0.9667927
dt[!is.na(weight.age), .(mean = mean(kaynnit_jalkeen), 
                         weight.age = unique(weight.age)), by='label'
   ][, .(mean = weighted.mean(mean, w=weight.age))]

# Mean number of visit days for those who visited at least once:
#   mean
# mean: 2.541915
dt[!is.na(weight.age) & kaynnit_jalkeen > 0, 
   .(mean = mean(kaynnit_jalkeen), 
     weight.age = unique(weight.age)), by='label'
   ][, .(mean = weighted.mean(mean, w=weight.age))]


# The share of those who had at least 1 visit date:
# 0.4125972
dt[, mean(kaynnit_jalkeen > 0)]
# The share of those who had 1 visit date conditional on having >0 visit dates:
# 0.4343454
dt[kaynnit_jalkeen > 0, mean(kaynnit_jalkeen == 1)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Plot the share of clients by gender and age. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# The share of patients in the population:
population.mean = 100 * dt[, mean(kaynnit_jalkeen > 0)]

# The share of clients by gender:

gender = data.table(
  label = c('Naiset', 'Miehet'),
  order = 1:2,
  users.per.capita =
    c(100 * dt[nainen==1, mean(kaynnit_jalkeen > 0)],
      100 * dt[nainen==0, mean(kaynnit_jalkeen > 0)])
)

# The share of clients by age group:

# Compute the share of users per age group:
age = dt[, .(users.per.capita = 100 * mean(kaynnit_jalkeen > 0)), by='label'
         ][order(label)
           ][, order := 4:14
             ][order %in% c(4:13)]

dt.share = rbind(gender, age)

# Plot:
p = ggplot(data=dt.share, aes(x = order, y = users.per.capita)) +
  geom_bar(stat = 'identity', fill = 'grey', color = 'black') +
  geom_text(aes(label = paste(round(users.per.capita, digits=1), '%')),
            vjust = -0.5, size = 3) +
  annotate('text', x=13, y = population.mean,
           label = paste('Ka.', round(population.mean, digits=1), '%'), 
           size = 3, vjust = -0.5) +
  scale_x_continuous(breaks = dt.share$order, labels = dt.share$label) +
  geom_hline(yintercept = population.mean) +
  ylim(0, 1.1 * dt.share[, max(users.per.capita)]) +
  labs(y = 'Käyttäjiä väestössä (%)') +
  theme(text = element_text(size=20),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(size=0.25, linetype = 'solid',
                                        color = 'lightgrey'),
        panel.grid.minor = element_line(size=0.25, linetype = 'solid',
                                        color = 'lightgrey'),
        panel.border = element_rect(colour='black', fill=NA, size=0.5))

# Save:
ggsave(output_share.clients, p, width = 12, height = 6)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) r2yw. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Estimate the proportion of variation in Y left unexplained by Z that is 
# explained by W.


# First, add income percentiles to the data:
dt[, kturaha_persentiili := 
     cut(kturaha_ekv,
         breaks = quantile(kturaha_ekv, probs = seq(0, 1, 0.01), na.rm = TRUE),
         labels = FALSE)]
dt[kturaha_ekv==0, kturaha_persentiili := 0]

# Leading PHC use appears to predict lagging PHC use much better than age, 
# income percentile or gender, measured by adjusted R2:
r2(fixest::feols(kaynnit_jalkeen ~ 1 | kaynnit_ennen, data= dt), 'ar2')
r2(fixest::feols(kaynnit_jalkeen ~ 1 | ika, data= dt), 'ar2')
r2(fixest::feols(kaynnit_jalkeen ~ 1 | kturaha_persentiili, data= dt), 'ar2')
r2(fixest::feols(kaynnit_jalkeen ~ 1 | nainen, data= dt), 'ar2')

# Leading PHC use predicts well. Adding age and income bring only minor
# improvements in adjusted R2. For sparsity, we will use only leading PHC use.
r2(fixest::feols(kaynnit_jalkeen ~ 1 | kaynnit_ennen + ika, data= dt), 'ar2')
r2(fixest::feols(kaynnit_jalkeen ~ 1 | kaynnit_ennen + ika + 
                   kturaha_persentiili, data= dt), 'ar2')


# To be conservative, we use Bayesian bootstrap to construct 95% CIs for the 
# adj. R2 and use the lower bound in power calculations:

set.seed(12345)
R = 500 # the number of replications

bayes_boot = replicate(R, {
  
  # Generate gamma random variables:
  weights.btsrp = rgamma(nrow(dt), 1, 1)
  
  # Normalize to get Dirichlet weights:
  weights.btsrp = weights.btsrp / sum(weights.btsrp)
  
  # Compute the adj. R2 from the weighted regression:
  ar2 = r2(fixest::feols(kaynnit_jalkeen ~ 1 | kaynnit_ennen, 
                         data= dt, weights = weights.btsrp), 'ar2')
  ar2 = unname(ar2)
  
})

# Mean and CI over bootstrap runs:
summary_bayes_boot = list(
  mean = mean(bayes_boot),
  quantiles = quantile(bayes_boot, c(0.025, 0.975))
)
print(summary_bayes_boot) # mean 0.3560111; CI 0.3422858, 0.3707106

# End.

