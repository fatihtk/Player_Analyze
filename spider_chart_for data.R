defence<-subset(all_players,all_players$Position=="Defence" & all_players$Nationality=="Azerbaijan")
midfield<-subset(all_players,all_players$Position=="Midfield" & all_players$Nationality=="Azerbaijan")
forward<-subset(all_players,all_players$Position=="Forward" & all_players$Nationality=="Azerbaijan")
goalie<-subset(all_players,all_players$Position=="Goalie" & all_players$Nationality=="Azerbaijan")

tdefence<-subset(all_players,all_players$Position=="Defence" & all_players$Nationality=="Turkey")
tmidfield<-subset(all_players,all_players$Position=="Midfield" & all_players$Nationality=="Turkey")
tforward<-subset(all_players,all_players$Position=="Forward" & all_players$Nationality=="Turkey")
tgoalie<-subset(all_players,all_players$Position=="Goalie" & all_players$Nationality=="Turkey")


library(plotly)

p <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = c(nrow(defence), nrow(midfield), nrow(forward), nrow(goalie),mean(defence$Weight),mean(defence$Height)),
    theta = c('Defence','Midfield','Forward', 'Goalie', 'mean of weight', 'mean of Height'),
    name = 'Azerbaijan'
  ) %>%
  add_trace(
    r = c(nrow(tdefence), nrow(tmidfield), nrow(tforward), nrow(tgoalie),mean(tdefence$Weight),mean(tdefence$Height)),
    theta = c('Defence','Midfield','Forward', 'Goalie', 'mean of weight', 'mean of Height'),
    name = 'Turkey'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,50)
      )
    )
  )

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link <- api_create(p, filename = "radar-multiple")
chart_link

#r = c(nrow(defence), nrow(midfield), nrow(forward), nrow(goalie),mean(defence$Weight),mean(defence$Height))
#r2 = c(nrow(tdefence), nrow(tmidfield), nrow(tforward), nrow(tgoalie),mean(tdefence$Weight),mean(tdefence$Height))



# Library
library(fmsb)
 
# Create data: for Azerbaijan:
data=as.data.frame(matrix( sample( 2:20 , 6 , replace=T) , ncol=6))
colnames(data)=c("defence" , "midfield" , "forward" , "goalie" , "Weight", "Height" )
 
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(20,10) , rep(0,10) , data)
 
# The default radar chart proposed by the library:
radarchart(data)
 
# Custom the radarChart !
radarchart( data  , axistype=1 , 
                             
                               #custom polygon
                               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                             
                               #custom the grid
                               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                             
                               #custom labels
                               vlcex=0.8 
                 )




# Library
library(fmsb)
 
# Create data: for Turkey:
data=as.data.frame(matrix( sample( 2:20 , 5 , replace=T) , ncol=6))
Warning message:
In matrix(sample(2:20, 5, replace = T), ncol = 6) :
data length [5] is not a sub-multiple or multiple of the number of columns [6]
colnames(data)=c("defence" , "midfield" , "forward" , "goalie" , "Weight", "Height" )
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(20,10) , rep(0,10) , data)
# The default radar chart proposed by the library:
radarchart(data)
# Custom the radarChart !
radarchart( data  , axistype=1 , 
                             
                               #custom polygon
                               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                             
                               #custom the grid
                               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                             
                               #custom labels
                               vlcex=0.8 
                 )


