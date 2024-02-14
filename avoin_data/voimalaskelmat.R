
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script voimalaskelmat.R         ###
###                  Tapio Haaga                  ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Some initial preliminary power analysis.
rm(list=ls())

# Make sure the following packages are installed (version in parentheses):
library(here)           # A Simpler Way to Find Your Files (1.0.1)
library(data.table)     # Extension of 'data.frame' (1.14.2)
library(powerLATE)      # Generalized Power Analysis for LATE (0.1.1)
library(ggplot2)        # Visualizations Using the Grammar of Graphics (3.3.6)
library(patchwork)      # The Composer of Plots (1.1.1)
library(pwr)            # Basic function for power analysis (ITT) (1.3-0)
library(stargazer)      # Save tables as tex (5.2.2)

# Outputs:
output_plot_pwr80 = here::here('avoin_data', 'kuviot', 'resurssit_histogrammi_voima_80.pdf')
output_plot_pwr90 = here::here('avoin_data', 'kuviot', 'resurssit_histogrammi_voima_90.pdf')
output_plot_pwr80_cov = here::here('avoin_data', 'kuviot', 'resurssit_histogrammi_voima_80_kontrollit.pdf')
output_plot_pwr90_cov = here::here('avoin_data', 'kuviot', 'resurssit_histogrammi_voima_90_kontrollit.pdf')
ouput_table_nudge = here::here('avoin_data', 'taulukot', 'resurssit_tuuppaus.tex')


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 1) Power analysis for LATE. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #

# The proposed intervention is to offer an opportunity to use
# a digital clinic (telemedicine visits) in public PHC.


# Error tolerance parameters:
power = c(0.8, 0.9)                     
sig.level = 0.05

# Choose with the help of publicly available data:
ann.price.per.client = 178 
complience = 0.053 * c(0.6, 0.8, 1, 1.20, 1.4)
visits.per.client = 4.39
sd.visits = 2.97

# Set share.treated to limit spillovers that are meaningful at the system level:
share.treated = c(0.20, 0.25, 0.30)

# Control variables can increase power:
r2dw = 0
r2yw = c(0, 0.34)

# Baseline values:
bsln = data.table(
  baseline=TRUE,
  share.treated = 0.25,
  complience = 0.053,
  sig.level = 0.05,
  ann.price.per.client = 178, 
  visits.per.client = 4.39,
  sd.visits = 2.97
)


# Our primary outcome is the number of PHC visit dates:

grid.d = data.table::CJ(
  outcome = 'phc_visits',
  effect.subs = c(0.16, 0.18, 0.20, 0.22, 0.24),
  share.treated = share.treated,
  sig.level = sig.level, power = power, complience = complience,
  ann.price.per.client = ann.price.per.client, 
  visits.per.client = visits.per.client, sd.visits = sd.visits,
  r2dw = r2dw, r2yw = r2yw
)

bsln.d = cbind(bsln, data.table(outcome = 'phc_visits', effect.subs = 0.20))

grid.d = merge(grid.d, bsln.d, 
               by=intersect(names(grid.d), names(bsln.d)), all.x = TRUE)
grid.d[, effect := 'd']


# Another grid is used with Cohen's d:
cohen.ds = c(0.15, 0.20, 0.25)

grid.cohen = unique(
  grid.d[, mget(setdiff(colnames(grid.d), c('effect.subs')))]
)

grid.cohen =
  grid.cohen[, .(outcome = rep(outcome, each = length(cohen.ds)),
                 share.treated = rep(share.treated, each = length(cohen.ds)),
                 sig.level = rep(sig.level, each = length(cohen.ds)),
                 complience = rep(complience, each = length(cohen.ds)),
                 ann.price.per.client = rep(ann.price.per.client, each = length(cohen.ds)),
                 power = rep(power, each = length(cohen.ds)),
                 baseline = rep(baseline, each = length(cohen.ds)),
                 visits.per.client = rep(visits.per.client, each = length(cohen.ds)),
                 sd.visits = rep(sd.visits, each = length(cohen.ds)),
                 r2dw = rep(r2dw, each = length(cohen.ds)),
                 r2yw = rep(r2yw, each = length(cohen.ds)),
                 cohen.d = rep(cohen.ds, times = nrow(grid.cohen)))]

grid.cohen[, effect := 'cohen.d']
grid.cohen[cohen.d != 0.2, baseline := NA] # baseline is d=0.20


# We can also have a binary outcome such as having any curative PHC visits:
# (coming up with reasonable effect sizes seems harder)

grid.b = data.table::CJ(
  outcome = 'phc_visits',
  baseline.mean = 0.41,
  effect.abs = c(0.03, 0.05, 0.07, 0.09, 0.11), # ??
  share.treated = share.treated,
  sig.level = sig.level, power = power, complience = complience,
  ann.price.per.client = ann.price.per.client, 
  visits.per.client = visits.per.client, sd.visits = sd.visits,
  r2dw = r2dw, r2yw = r2yw
)

bsln.b = cbind(
  bsln, 
  data.table(outcome = 'phc_visits', baseline.mean = 0.41, effect.abs = 0.07)
)

grid.b = merge(grid.b, bsln.b, 
               by=intersect(names(grid.b), names(bsln.b)), all.x = TRUE)
grid.b[, effect := 'binary']


# Combine the grids containing absolute effects and effect sizes:
grid = rbind(grid.d, grid.cohen, grid.b, fill=TRUE)


# Loop over all combinations of parameters:

results = lapply(1:nrow(grid), function(i) {
  
  # Compute the conservative upper bound for N:
  
  if(grid[i, effect] == 'd') {
    
    pwr = powerLATE::powerLATE.cov(
      pZ = grid[i, share.treated],
      pi = grid[i, complience],
      sig.level = grid[i, sig.level],
      power = grid[i, power],
      kappa = grid[i, effect.subs] * grid[i, visits.per.client] / 
        grid[i, sd.visits],
      r2dw = grid[i, r2dw],
      r2yw = grid[i, r2yw],
      verbose = FALSE
    )
    
  } else if (grid[i, effect] == 'cohen.d') {
    
    pwr = powerLATE::powerLATE.cov(
      pZ = grid[i, share.treated],
      pi = grid[i, complience],
      sig.level = grid[i, sig.level],
      power = grid[i, power],
      kappa = grid[i, cohen.d],
      r2dw = grid[i, r2dw],
      r2yw = grid[i, r2yw],
      verbose = FALSE
    )
    
  } else if (grid[i, effect] == 'binary') {
    
    pwr = powerLATE::powerLATE.cov(
      pZ = grid[i, share.treated],
      pi = grid[i, complience],
      effect.size = FALSE,
      sig.level = grid[i, sig.level],
      power = grid[i, power],
      tau = grid[i, effect.abs],
      # omega = sqrt(var) = sqrt(p*(1-p))
      omega = sqrt(grid[i, baseline.mean] * (1 - grid[i, baseline.mean])),
      r2dw = grid[i, r2dw],
      r2yw = grid[i, r2yw],
      verbose = FALSE
    )
    
  }
 
  
  # Collect all parameter values and the N:
  pwr.data = c(unlist(pwr$input.parameter), unlist(pwr$output.parameter))
  pwr.data = data.table(t(pwr.data))
  
  pwr.data[, ':=' (outcome = grid[i, outcome],
                   effect = grid[i, effect],
                   baseline = grid[i, baseline],
                   ann.price.per.client = grid[i, ann.price.per.client],
                   visits.per.client = grid[i, visits.per.client],
                   sd.visits = grid[i, sd.visits],
                   effect.subs = grid[i, effect.subs])]
  
  return(pwr.data)
  
})

results = rbindlist(results, fill=TRUE)


# Compute the number of treated and the cost of the trial:
results[, ':=' (N.treated = N * pZ,
                N.clients = N * pZ * pi)] 
results[, costs.millions := ann.price.per.client * N.clients / 1000000]
 

### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 2) Visualize sample sizes for LATE. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #


### A plot that shows histograms of N and the costs over the 
# previously specified grid. ###

# Tidy data for plotting:

dt.p = results[, .(N.treated, N, costs.millions, 
                   effect, baseline, outcome, power, r2yw)]
dt.p[, ':=' (N.treated = N.treated / 1000,
             N = N / 1000,
             baseline = as.integer(!is.na(baseline)))]

columns.new = c(
  'Koeryhmä (tuhansia)', 'Väestöpohja (tuhansia)', 'Kustannukset (miljoonia)', 
  'effect', 'baseline', 'outcome', 'power', 'r2yw')

setnames(dt.p, old = colnames(dt.p), new = columns.new)

dt.p = melt(
  dt.p, id.vars = c('baseline', 'outcome', 'power', 'effect', 'r2yw'),
  variable.name = 'category', value.name = 'value'
)
  

# Plot (loop over power, outcomes, and effect type):

power_levels = c(0.8, 0.9)
outcomes = c('phc_visits')
effect.classes = c('d', 'cohen.d')
r2yws = c(0, 0.34)

p = lapply(power_levels, function(pwr) {
  pp = lapply(outcomes, function(otc) {
    ppp = lapply(effect.classes, function(effect.type) {
      pppp = lapply(r2yws, function(r2y) {
        
        # Extract the right data:
        df = dt.p[power== pwr & outcome==otc & effect==effect.type & r2yw==r2y]
        
        # Plot:
        ggplot2::ggplot(df, aes(x = value)) +
          facet_wrap(~ category, ncol = 1, scales = "free") +
          geom_histogram(color = 'black') +
          geom_vline(data=df[baseline==1], aes(xintercept=value), color='red') +
          geom_label(data=df[baseline==1],
                     aes(x = value, y = 0,    
                         label = round(value, digits=2)), fill = "red") + 
          labs(y = "Esiintymiskerrat") +
          theme(axis.title.x = element_blank(),
                text = element_text(size=20),
                panel.background = element_rect(fill = "white", colour = "white"),
                panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                                colour = "lightgrey"),
                panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                                colour = "lightgrey"),
                panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
        
      })
      names(pppp) = paste0('r2yw.', r2yws)
      return(pppp)
      
    })
    names(ppp) = paste0('effect.', effect.classes)
    return(ppp)
    
  })
  names(pp) = outcomes
  return(pp)
  
})
names(p) = paste0('power.', power_levels)



# Power 0.8, no covariates:
plt.80 =
  p$power.0.8$phc_visits$effect.d$r2yw.0 + 
  ggtitle('Vaikutuskokona valistunut veikkaus') + 
  p$power.0.8$phc_visits$effect.cohen.d$r2yw.0 + 
  ggtitle('Vaikutuskokona Cohenin d')

# Power 0.9, no covariates:
plt.90 =
  p$power.0.9$phc_visits$effect.d$r2yw.0 + 
  ggtitle('Vaikutuskokona valistunut veikkaus') + 
  p$power.0.9$phc_visits$effect.cohen.d$r2yw.0 + 
  ggtitle('Vaikutuskokona Cohenin d') 

# Power 0.8, with covariates:
plt.80.cov =
  p$power.0.8$phc_visits$effect.d$r2yw.0.34 + 
  ggtitle('Vaikutuskokona valistunut veikkaus') + 
  p$power.0.8$phc_visits$effect.cohen.d$r2yw.0.34 + 
  ggtitle('Vaikutuskokona Cohenin d')

# Power 0.9, with covariates:
plt.90.cov =
  p$power.0.9$phc_visits$effect.d$r2yw.0.34 + 
  ggtitle('Vaikutuskokona valistunut veikkaus') + 
  p$power.0.9$phc_visits$effect.cohen.d$r2yw.0.34 + 
  ggtitle('Vaikutuskokona Cohenin d') 

# Save:
ggsave(output_plot_pwr80, plt.80, width = 15, height = 13)
ggsave(output_plot_pwr90, plt.90, width = 15, height = 13)
ggsave(output_plot_pwr80_cov, plt.80.cov, width = 15, height = 13)
ggsave(output_plot_pwr90_cov, plt.90.cov, width = 15, height = 13)


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 3) Power analysis for ITT. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #

# The proposed intervention is a nugde to increase the share
# of digital clinic users. The outcome is binary (has / has not
# used the digital clinic in the follow-up).

# We have two active treatment arms (T1, T2) and one control arm (T0).
# We compare T1+T2 vs. T0 and T1 vs. T2.

# Cohen's h (proportion test for two samples): difference of proportion 
# power calculation for binomial distribution (arcsine transformation) 


# Error tolerance parameters:
power = c(0.8, 0.9)                     
sig.level = 0.05

# Cohen's h (minimum detectable effect size): the average 
# impact in Nudge Unit trials (see DellaVigna & Linos, 2022).
h.main = 0.036

# T1+T2 vs. T0: 50% of the average impact in Nudge Unit trials.
h.main.scaler = 0.50

# T1 vs. T2: 20% of the average impact in Nudge Unit trials.
h.arms.scaler = 0.20

h.scaler = c(h.main.scaler, h.arms.scaler)

# The cost per treated in euros:
cost.per.treated = 0.75


# The grid of specifications:
grid = data.table::CJ(
  outcome = 'has_visited',
  h.main = h.main,
  h.scaler = h.scaler,
  power = power,
  sig.level = sig.level
)
grid[, h := h.main * h.scaler]


# Loop over all combinations of parameters:

results = lapply(1:nrow(grid), function(i) {
  
  # Compute the sample size:
  pwr = pwr.2p.test(
    h = grid[i, h], 
    power = grid[i, power],
    sig.level = grid[i, sig.level], 
    alternative = "two.sided"
  )
  
  # Collect and return the parameters:
  data.table(
    outcome = grid[i, outcome],
    h.main = grid[i, h.main],
    h.scaler = grid[i, h.scaler],
    h = pwr$h,
    sig.level = pwr$sig.level,
    power = pwr$power,
    N.treated = pwr$n,
    N = 2 * pwr$n,
    costs.thousands = pwr$n * cost.per.treated / 1000
  )
  
})
results = rbindlist(results)


# Create a tidy table:
table = results[, .(h, power, N.treated, N, costs.thousands)]
table[h==0.50*0.036, Vertailu := 'T1+T2 vs. T0']
table[h==0.20*0.036, Vertailu := 'T1 vs. T2']
table = table[order(power, -h)]
table[, ':=' ('Voima' = 100 *power,
              'Cohenin h' = h,
              'Koeryhmä (tuhansia)' = ceiling(N.treated / 1000),
              'Väestöpohja (tuhansia)' = ceiling(N / 1000),
              'Kustannukset (tuhansia)' = ceiling(costs.thousands))]
table[, ':=' (N.treated=NULL, N=NULL, costs.thousands=NULL, power=NULL, h=NULL)]

# Save as tex:
stargazer::stargazer(
  table, out = ouput_table_nudge,
  title = 'Kevyt interventio digiklinikan asiakasmäärien lisäämiseksi: tarvittavat resurssit',
  label = 'tab:resurssit_tuuppaus',
  summary = FALSE, header = FALSE, rownames = FALSE
)


# Effect Sizes:
abs(2 * asin(sqrt(0.1733+0.0139)) - 2 * asin(sqrt(0.1733))) # DellaVigna & Linos (2022); +8%
abs(2 * asin(sqrt(0.053+0.0084)) - 2 * asin(sqrt(0.053))) # Harjun terveys (2021); +16%
abs(2 * asin(sqrt(0.34+0.064)) - 2 * asin(sqrt(0.34))) # Sääksvuori et al. (2022)
abs(2 * asin(sqrt(0.308+0.009)) - 2 * asin(sqrt(0.308))) # Hirvonen et al. (2023)
abs(2 * asin(sqrt(0.2559+0.00091)) - 2 * asin(sqrt(0.2559))) # Haaga et al. (2023)

# End
