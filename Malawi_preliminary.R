source('possible.R')
# TODO: Probably not necessary to load all of the JSR metrics; can make
# this more modular

###############################################################################
# Reasonable limits on fertility changes
###############################################################################
tfr_all <- read_tsv('IFs_exports/tfr_all.txt',
                    col_names=c('country','year','value')) %>%
  filter(year <= 2015)
find_improvements(tfr_all,'Malawi',2015,increase = FALSE)
find_improvements(tfr_all,'Malawi',2015,increase = TRUE)

###############################################################################
# Reasonable limits on education improvements
###############################################################################

### Primary enrollment is already close to 100%, but survival needs some work
pri_all <- read_tsv('IFs_exports/pri_survival_all.txt',
                    col_names=c('country','year','value')) %>%
  filter(year <= 2015) %>%
  na.omit %>%
  mutate(country=sub(', Total','',country))

find_improvements(pri_all,'Malawi',2015,increase=TRUE)
# lots of candidates -- problem is that a lot of these are just volatile, not
# sustained improvements

### Secondary
sec_all <- read_tsv('IFs_exports/sec_enroll_net_all.txt',
                    col_names=c('country','year','value')) %>%
  filter(year <= 2015) %>%
  mutate(country=sub(', Total','',country))

find_improvements(sec_all,'Malawi',2015,increase = TRUE)
# South Korea took 24 years to get from where Malawi is now to near-universal enrollment
# Qatar had some setbacks in the 2000s, but was on track to get there in about 35 years
# If Bhutan keeps up the rate of improvement they had from 1998 to 2014, it will take them 
# about 25 years.
# South Korea and Qatar both had near-universal primary (like Malawi) when their secondary
# started to improve; Bhutan has been improving both at the same time.

### Tertiary
tert_all <- read_tsv('IFs_exports/gross_enroll_tert_all.txt',
                    col_names=c('country','year','value')) %>%
  filter(year <= 2015) %>%
  mutate(country=sub(', Total','',country))

find_improvements(tert_all,'Malawi',2015,increase = TRUE)

###############################################################################
# Governance JSR plots
###############################################################################
j2sr_plot <- function(cname,vname) {
  c_series <- all_jsr %>% 
    filter(country==cname, varstr==vname, year >= 1960) %>%
    select(year,value,country)
  low_series <- all_jsr %>%
    filter(varstr==vname, year >= 1960, country %in% low_income) %>%
    group_by(year) %>%
    summarize(value=mean(value)) %>%
    mutate(country='Low-income')
  lmic_series <- all_jsr %>%
    filter(varstr==vname, year >= 1960, country %in% lmic) %>%
    group_by(year) %>%
    summarize(value=mean(value)) %>%

        mutate(country='LMIC')
  umic_series <- all_jsr %>%
    filter(varstr==vname, year >= 1960, country %in% umic) %>%
    group_by(year) %>%
    summarize(value=mean(value)) %>%
    mutate(country='UMIC')
  rbind(c_series,low_series,lmic_series,umic_series) %>%
    mutate(country=factor(country,levels=c('Malawi','UMIC','LMIC','Low-income'))) %>%
    ggplot(aes(x=year,y=value,group=country,color=country)) +
    geom_line(size=1) +
    theme_USAID + colors_USAID +
    xlab('Year') + ylab(vname) +
    theme(legend.title=element_blank())
}

j2sr_plot('Malawi','Liberal Democracy') +
  labs(title='Liberal Democracy index',caption='Source: Varieties of Democracy') +
  scale_x_continuous(breaks=c(1960,1980,2000,2016))

all_jsr %>% filter(varstr=='Liberal Democracy') %>% arrange(value) %>% head
all_jsr %>% filter(varstr=='Liberal Democracy') %>% arrange(value) %>% tail

j2sr_plot('Malawi','Group Equality') +
  labs(title='Group Equality index',caption='Source: Varieties of Democracy') +
  scale_x_continuous(breaks=c(1960,1980,2000,2016))

j2sr_plot('Malawi','Diagonal Accountability') +
  labs(title='Media & Civil Society Capacity',caption='Source: Varieties of Democracy') +
  scale_x_continuous(breaks=c(1960,1980,2000,2016))

j2sr_plot('Malawi','Government Effectiveness') +
  labs(title='Government Effectiveness index',caption='Source: World Bank') +
  scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2016))

j2sr_plot('Malawi','Safety & Security') +
  labs(title='Safety & Security index',caption='Source: Legatum Institute') +
  scale_x_continuous(breaks=c(2007,2009,2011,2013,2015,2017))

all_jsr %>% filter(varstr=='Safety & Security',year==2007,value > 64, value < 65)
all_jsr %>% filter(varstr=='Safety & Security',year==2017,value > 58, value < 59)
