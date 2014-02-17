########################
# ANALYTICAL FUNCTIONS #
########################
double  <-  function(x) {
  x * 2
}

average  <-  function(x) {
  sum(x) / length(x)
}

variance  <-  function(x) {
  x <- na.omit(x)
  if(length(x) == 0){
    stop("Can't compute the variance of no numbers")
  }
  (1 / (length(x) - 1)) * sum((x-mean(x))^2)
}

rescale  <-  function(x, range) {
  p  <-  (x - min(x)) / (max(x) - min(x))
  range[[1]] + p * (range[[2]] - range[[1]])
}

get.n.countries  <-  function(x) {
  length(unique(x$country))
}

get.total.pop  <-  function(x) {
  sum(x$pop)
}

get.countries  <-  function(x) {
  unique(x$country)
}

model  <-  function(x) {
  fit  <-  lm(lifeExp ~ log10(gdpPercap), data=x)
  c(n=length(x$lifeExp), r2=summary(fit)$r.squared, a=coef(fit)[[1]], b=coef(fit)[[2]])
}

######################
# PLOTTING FUNCTIONS #
######################
colour.by.category <- function(things, table) {
  unname(table[things])
}

add.trend.line <- function(x, y, d, ...) {
  lx <- log10(d[[x]])
  fit <- lm(d[[y]] ~ lx)
  xr <- range(lx)
  lines(10^xr, predict(fit, list(lx=xr)), ...)
}

add.continent.trend.line <- function(x, y, d, continent, col.table, ...) {
  add.trend.line(x, y, d[d$continent == continent,], col=col.table[continent], ...)
}

f <- function(continent) {
  add.continent.trend.line("gdpPercap", "lifeExp", dat.1982, continent, col.table)
}

##########################
# ANALYSES FORM LESSON 1 #
##########################
dat <- read.csv("gapminder-FiveYearData.csv", stringsAsFactors=FALSE)

double(pi)
double(2)

dat.1982  <-  dat[dat$year == 1982, ]
average(dat.1982$gdpPercap) #same as: sum(dat.1982$gdpPercap) / length(dat.1982$gdpPercap)
variance(dat.1982$pop) #try with var(dat.1982$pop)

##########################
# ANALYSES FORM LESSON 2 #
##########################
library(plyr)
get.n.countries(dat)

n.countries  <-  integer(0)
for(continent in unique(dat$continent)) {
  n.countries[[continent]]  <-  get.n.countries(dat[dat$continent == continent, ])    
}
n.countries

#USING PLYR 
ddply(dat, .(continent), get.n.countries)
ddply(dat, .(continent, year), get.n.countries)
ddply(dat, .("continent", "year"), get.n.countries)
ddply(dat, ~continent*year, get.n.countries)
get.total.pop(dat)
ddply(dat, .(continent, year), get.total.pop)
ddply(dat, .(continent, year), function(x)sum(x$pop))
lmPerContAndYear  <-  dlply(dat, .(continent, year), model)

######################
# MAKE PLOTS BY YEAR #
######################
col.table  <-  c(Asia='tomato',
                 Europe='chocolate4',
                 Africa='dodgerblue2',
                 Americas='darkgoldenrod1',
                 Oceania='green4')

col <- colour.by.category(dat.1982$continent, col.table)
cex <- rescale(sqrt(dat.1982$pop), c(0.2, 10))

plot(lifeExp ~ gdpPercap, dat.1982, log="x", cex=cex, col=col, pch=21)
f("Africa")
f("Asia")
f("Europe")
f("Americas")
f("Oceania")
