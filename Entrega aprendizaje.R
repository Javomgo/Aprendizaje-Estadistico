library(dplyr)
video.df = read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
video.df2 = mutate(video.df[,-c(11:16)]) %>% na.omit
str(video.df2)
asdf