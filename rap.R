library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(MASS)
library(proxy)
library(igraph)
library(scales)
library(plm)
library(pglm)
library(gridExtra)
library(grid)

#setwd("C:/Users/Jacky Yang/Desktop/Social Network/final project/2016_rap_network_analysis-master/data")

top<-read.csv('top.csv')
albumrank<-read.csv('billboard_rap_chart_albums_cleaned.csv')
artist<-read.csv('artist_pair_counts.csv')

song_d<-data.frame()
for (row in 1:nrow(top)){
  if (row %% 3 == 0){
    song<-as.data.frame(as.character(top[row,1]))
    song_d<-rbind(song_d,song)
  }
}
colnames(song_d)<-c("artist")
song_d$top<-1

twoway<-as.data.frame(cbind(as.character(artist$target),as.character(artist$source),artist$weight,as.character(artist$type)))
colnames(twoway)<-c("source","target","weight","type")
artist<-rbind(artist,twoway)

g<-graph.data.frame(artist,directed = FALSE)
g<-set_edge_attr(g,"weight",value = artist$weight)

plot(g,vertex.label=NA)

#join album
join<-left_join(artist,albumrank, by = c("source"="Artist"))
join<-left_join(join,song_d, by = c("source"="artist"))

# Function 1 : For ploting missing value
plot_missing <- function(data, title = NULL, ggtheme = theme_gray(), theme_config = list("legend.position" = c("bottom"))) {
  ## Declare variable first to pass R CMD check
  feature <- num_missing <- pct_missing <- group <- NULL
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) data <- data.table(data)
  ## Extract missing value distribution
  missing_value <- data.table(
    "feature" = names(data),
    "num_missing" = sapply(data, function(x) {sum(is.na(x))})
  )
  missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  missing_value[, pct_missing := num_missing / nrow(data)]
  missing_value[pct_missing < 0.05, group := "Good"]
  missing_value[pct_missing >= 0.05 & pct_missing < 0.4, group := "OK"]
  missing_value[pct_missing >= 0.4 & pct_missing < 0.8, group := "Bad"]
  missing_value[pct_missing >= 0.8, group := "Remove"][]
  ## Set data class back to original
  if (!is_data_table) class(missing_value) <- data_class
  ## Create ggplot object
  output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "group")) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(100 * pct_missing, 2), "%"))) +
    scale_fill_manual("Group", values = c("Good" = "#1a9641", "OK" = "#a6d96a", "Bad" = "#fdae61", "Remove" = "#d7191c"), breaks = c("Good", "OK", "Bad", "Remove")) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    xlab("Features") + ylab("Number of missing rows") +
    ggtitle(title) +
    ggtheme + theme_linedraw()+
    do.call(theme, theme_config)
  ## Print plot
  print(output)
  ## Set return object
  return(invisible(missing_value))
}
plot_missing(join)

join[is.na(join)]<-0

#degree
degree<-as.data.frame(degree(g))
#get the name of 
id=V(g)$name
degree<-as.data.table(list(artist=id,eigen = degree))
#betweenness
betweenness<-as.data.frame(betweenness(g))
#get the name 
id=V(g)$name
between<-as.data.table(list(artist=id,between = betweenness))
#close
close<-as.data.frame(closeness(g))
#get the name 
id=V(g)$name
close<-as.data.table(list(artist=id,close = close))
#calculate eigen vector
eigen<-as.data.table(eigen_centrality(g)$vector)
#get the name of the investor
id=V(g)$name
eigen<-as.data.table(list(artist=id,eigen = eigen))

unique(artist$source)
#get the highest closeness
close[which.max(close$close)]
#distance
distance<-shortest.paths(g,mode="all")
distance[!is.finite(distance)]<-1034
min(rowMeans(distance))
mean(distance["ScHoolboy Q",])#verify if the artist with highest closeness also have the lowest path

#join centrality tables
merge1<-left_join(join,degree, by = c("source"="artist"))
merge1<-left_join(merge1,between, by = c("source"="artist"))
merge1<-left_join(merge1,close, by = c("source"="artist"))
merge1<-left_join(merge1,eigen, by = c("source"="artist"))
colnames(merge1)<-c("source","target","weight","type","Album","top_album","top_song","degree","between","close","eigen")

#regression
# glm1<-glm(top_song~degree+between+close+eigen,data=merge1,family = "binomial")
# summary(glm1)
# 
# glm2<-glm(top_album~degree+between+close+eigen,data=merge1,family = "binomial")
# summary(glm2)
#top album list
par(mfrow=c(1,1))

glm1<-glm(top_album~degree,data=merge1,family = "binomial")
glm2<-glm(top_album~between,data=merge1,family = "binomial")
glm3<-glm(top_album~close,data=merge1,family = "binomial")
glm4<-glm(top_album~eigen,data=merge1,family = "binomial")
plot(glm1)

merge1$album_predict_degree<-predict(glm1,merge1, type="response")
merge1$album_predict_between<-predict(glm2,merge1, type="response")
merge1$album_predict_close<-predict(glm3,merge1, type="response")
merge1$album_predict_eigen<-predict(glm4,merge1, type="response")

(p1<-ggplot(data=merge1,aes(y=merge1$album_predict_degree,x=degree))+geom_point()+
  geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Degree"))
(p2<-ggplot(data=merge1,aes(y=merge1$album_predict_between,x=between))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Betweenness"))
(p3<-ggplot(data=merge1,aes(y=merge1$album_predict_close,x=close))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Closeness"))
#(p4<-ggplot(data=merge1,aes(y=merge1$album_predict_eigen,x=eigen))+geom_point()+
#    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE))
grid.arrange(p1,p2,p3,nrow=2,top = textGrob("Top Album Models",gp=gpar(fontsize=15,font=3)))

#top song list
glm1<-glm(top_song~degree,data=merge1,family = "binomial")
glm2<-glm(top_song~between,data=merge1,family = "binomial")
glm3<-glm(top_song~close,data=merge1,family = "binomial")
glm4<-glm(top_song~eigen,data=merge1,family = "binomial")

merge1$song_predict_degree<-predict(glm1,merge1, type="response")
merge1$song_predict_between<-predict(glm2,merge1, type="response")
merge1$song_predict_close<-predict(glm3,merge1, type="response")
merge1$song_predict_eigen<-predict(glm4,merge1, type="response")

(p1<-ggplot(data=merge1,aes(y=merge1$song_predict_degree,x=degree))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Degree"))
(p2<-ggplot(data=merge1,aes(y=merge1$song_predict_between,x=between))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Betweenness"))
(p3<-ggplot(data=merge1,aes(y=merge1$song_predict_close,x=close))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Closeness"))
#(p4<-ggplot(data=merge1,aes(y=merge1$song_predict_eigen,x=eigen))+geom_point()+
#    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE))
grid.arrange(p1,p2,p3,nrow=2,top = textGrob("Top Song Models",gp=gpar(fontsize=15,font=3)))

#correlation analysis
cor(merge1$top_song,merge1$degree)
cor(merge1$top_song,merge1$between)
cor(merge1$top_song,merge1$close)
cor(merge1$top_song,merge1$eigen)

cor(merge1$top_album,merge1$degree)#high correlation
cor(merge1$top_album,merge1$between)#high correlation
cor(merge1$top_album,merge1$close)
cor(merge1$top_album,merge1$eigen)

plot(merge1$top_album,merge1$degree)






