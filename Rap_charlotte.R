#rap data


library('igraph')
library('data.table')
library('reshape2')
library('MASS')
library('purrr')
library('dplyr')
library('pglm')
library('plm')
library('scatterplot3d')
library('rgl')
library('nnet')

#import data

rappers_edges <- read.csv('C:/Users/chigg/Documents/MSBA/Social Network Analytics/2016_rap_network_analysis-master/data/artist_pair_counts.csv')
