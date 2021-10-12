## SKRYPT MICHALA

library(tidyverse)
library(moments)
mode <- function(data) {
  uniqv <- unique(data)
  uniqv[which.max(tabulate(match(data, uniqv)))]
}

outliers <- function(data) {
  uniqv <- unique(data)
  uniqv[which.max(tabulate(match(data, uniqv)))]
  iqr <- IQR(selected, na.rm = TRUE)
  q1 <- quantile(selected, 0.25, na.rm = TRUE)
  q3 <- quantile(selected, 0.75, na.rm = TRUE)
  message("Pierwszy kwantyl",q1)
  message("Trzeci kwantyl",q3)
  
  
}

desc_stats <- function(data) {
  message("Ilosc probek ",length(data))
  message("Minimum ",min(data))
  message("Maximum ",max(data))
  message(" ")
  message("Miary tendencji centralnej")
  message("Srednia ",mean(data))
  message("Mediana ",median(data))
  message("Moda ",mode(data))
  message(" ")
  message("Wskazniki Rozproszenia")
  message("Skosnosc", skewness(data))
  message("Odchylenie standardowe ",sd(data))
  message("Wariancja ",var(data))
  message("Dolny kwartyl ",quantile(data, 0.25, na.rm = TRUE))
  message("Gorny kwartyl ",quantile(data, 0.75, na.rm = TRUE))
  message("Rozstep ",max(data)-min(data))
  
}

remove_outliers <- function(data) {
  selected <- data
  iqr <- IQR(data, na.rm = TRUE)
  q1 <- quantile(data, 0.25, na.rm = TRUE)
  q3 <- quantile(data, 0.75, na.rm = TRUE)
  cat("Upper outliers: ", sum(data > (q3+1.5*iqr)), "\n")
  cat("Bottom outliers: ", sum(data < (q1-1.5*iqr)), "\n")
  data[data < (q1-1.5*iqr)] <- median(data, na.rm = TRUE)
  data[data > (q3+1.5*iqr)] <- median(data, na.rm = TRUE)
  return (data)
}

remove_na <- function(data)
{
  data[is.na(data)] <- median(data, na.rm = TRUE)
  return(data)
}

get_boxplot <- function(data,y,quality,step,label,y_label,color) {
  ggplot(data = dane,aes(x = factor(0),y=y)) +stat_boxplot(geom="errorbar")+
    geom_boxplot(aes(fill=y),outlier.color="darkred",color="grey25",coef=1.5,fill=color) +
    #  facet_wrap( quality)+ 
    # scale_y_continuous(breaks=seq(min(y,na.rm=TRUE),max(y,na.rm=TRUE),step)) +
    scale_x_discrete(breaks = NULL) + 
    labs(x = "",y=y_label,title=label)+
    theme(strip.placement = "outside")
  ggsave(paste(label,"histogram.png"))
}

get_histogram <- function(data,x,quality,x_step,label,x_label,x_fill,color) {
  ggplot(data=data,aes(x=x,fill=x_label))+
    geom_histogram(bins=1+3.322*(log(length(x))), fill=color,color="black",closed="left",alpha=.5)+
    # facet_wrap( quality,dir="v", scales="free")+ 
    labs(x = x_label,title=label,fill = x_fill)
  ggsave(paste(label,"histogram.png"))
}

###KONIEC FUNKCJI, CZAS NA DANE

dane <- read.csv("world-happiness-report-2021.csv")

# dane$city.mpg<-remove_outliers(dane$city.mpg)
dane$Ladder.score<-remove_na(dane$Ladder.score)

message("city.mpg")
message("")

desc_stats(dane$Ladder.score)
desc_stats(dane$Social.support)
desc_stats(dane$Logged.GDP.per.capita)
desc_stats(dane$Freedom.to.make.life.choices)
desc_stats(dane$Healthy.life.expectancy)

get_histogram(dane,dane$city.mpg,~sex,3,"Histogram city.mpg","city.mpg","","seagreen2")

get_boxplot(dane,dane$city.mpg,~sex,3,"Ramka wasy city.mpg","city.mpg","seagreen2")

# dane$curb.weight<-remove_outliers(dane$curb.weight)
dane$curb.weight<-remove_na(dane$curb.weight)

message("curb.weight")
message("")

desc_stats(dane$curb.weight)

get_histogram(dane,dane$curb.weight,~sex,7,"Histogram curb.weight","curb.weight","","firebrick2")

get_boxplot(dane,dane$Ladder.score,7,"Ramka wasy curb.weight","curb.weight","firebrick2")
