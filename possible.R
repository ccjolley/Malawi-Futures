wd <- getwd()
setwd('../JSR/')
source('utils.R')
source('extract_indices.R')
setwd(wd)
source('IFs_plots.R')

###############################################################################
# Function to find improvements
###############################################################################
find_improvements <- function(input,country_,year_,time_span=10,num_results=10,
                              tol=0.1,increase=TRUE) {
  input <- na.omit(input)
  target <- (input %>% filter(country==country_,year==year_))$value 
  lo <- min(target*c(1-tol,1+tol))
  hi <- max(target*c(1-tol,1+tol))
  compare <- input %>% 
    filter(value>lo,value<hi,country != country_)
  if (nrow(compare)==0) { return('Nothing found')}
  get_span <- function(d) {
    yrs <- (input %>% filter(country==d$country))$year %>% unique %>% sort
    shift_yr <- max(yrs[yrs <= (d$year+10)]) # deal with missing values
    newval <- (input %>% filter(country==d$country,year==shift_yr))$value
    diff <- newval - d$value
    out <- d
    out$shift_yr <- shift_yr
    out$newval <- newval
    out$diff <- diff
    out
  }
  dir <- ifelse(increase,-1,1)
  plyr::ldply(1:nrow(compare),function(i) get_span(compare[i,])) %>%
    arrange(dir*diff) %>%
    filter(year != shift_yr) %>%
    head(num_results)
}

