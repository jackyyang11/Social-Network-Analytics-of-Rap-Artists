
library(sna)
library(RSiena)
library(data.table)

#dt2012 <- fread('2012.csv')
#dt2013 <- fread('2013.csv')
#dt2014 <- fread('2014.csv')
dt2015 <- fread('2015.csv')[-1, -1]
dt2016 <- fread('2016.csv')[-1, -1]
dt2017 <- fread('2017.csv')[-1, -1]
dt2018 <- fread('2018.csv')[-1, -1]

colnames <- c('album', 'from', 'to')
colnames(dt2015) <- colnames
colnames(dt2016) <- colnames
colnames(dt2017) <- colnames
colnames(dt2018) <- colnames

v2015 <- sort(unique(unlist(dt2015[, -1])))
v2016 <- sort(unique(unlist(dt2016[, -1])))
v2017 <- sort(unique(unlist(dt2017[, -1])))
v2018 <- sort(unique(unlist(dt2018[, -1])))

intersect(intersect(v2015, v2016), intersect(v2017, v2018))

