library(rvest)
library(purrr)
library(XML)
library(httr)

#player
dflist1 <- map(.x = 1:121, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=35&teamId=0&season=2018&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('.wisbb_playerContainer') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)
#team
dflist2 <- map(.x = 1:121, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=35&teamId=0&season=2018&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('.wisbb_fixedColumn+ td') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)
#position
dflist3 <- map(.x = 1:121, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=35&teamId=0&season=2018&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('.wisbb_priorityColumn') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)
#height
dflist4 <- map(.x = 1:121, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=35&teamId=0&season=2018&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('.wisbb_priorityColumn+ td') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)
#weight
dflist5 <- map(.x = 1:121, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=35&teamId=0&season=2018&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('td:nth-child(5)') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)
#birthdate
dflist6 <- map(.x = 1:121, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=35&teamId=0&season=2018&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('td:nth-child(6)') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)
dflist7 <- map(.x = 1:121, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=35&teamId=0&season=2018&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('td:nth-child(7)') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)

players<-data.frame(dflist1,dflist2,dflist3,dflist4,dflist5,dflist6,dflist7)
colnames(players) <- c("Player","Team","Position","Height","Weight","BirthDate","Nationality")

#Data Manipulation
#Changing Tables Coloumn names and implementing null values
colnames(players) <- c("Player","Team","Position","Height","Weight","BirthDate","Nationality") 
players$Weight<-gsub("-",NA,players$Weight)
players$Height<-gsub("-",NA,players$Height)

#Converting feet and inches into centimeters.
players$Height<-gsub("'","-",players$Height)
players$Height<-gsub("\"","",players$Height)

players$Height<- sapply(strsplit(as.character(players$Height),"-"),function(x){30.48*as.numeric(x[1]) + 2.54*as.numeric(x[2])})

#Factor level editing for Position attribute.
levels(players$Position) <- list(Defence="D",Forward="F",Goalie="G",Midfield="M")

players$Player<-as.character(players$Player)
players$Team<-as.character(players$Team)
players$Weight<-as.numeric(players$Weight)
players$BirthDate<-as.Date(players$BirthDate, "%m/%d/%Y")
players$Nationality<-as.character(players$Nationality)

#Adding New Coloums as Longtitude and Latitude for location

colnames(players) <- c("Player","Team","Position","Height","Weight","BirthDate","Nationality")
players["Longitude"] <- NA
players["Latitude"]<-NA


#Adding coordinates for every country to use map 
library(tidyverse)    # for ggplot2 and `%>%`
library(ggmap)
library(ggrepel)    # for geom_text_repel, though adjust overlaps manually if you prefer

#players2 <- data_frame(city = c(players$Nationality)) %>%    # start data.frame
# mutate_geocode(city)    # use ggmap function to add lon/lat columns
#players2$city <- NULL
#players2

#players<- cbind(players,players2)



players$Longitude[players$Nationality == "Azerbaijan"] <- 47.57693
players$Longitude[players$Nationality == "Norway"] <- 8.468946
players$Longitude[players$Nationality == "Kazakhstan"] <- 66.92368
players$Longitude[players$Nationality == "Macedonia"] <- 21.74527
players$Longitude[players$Nationality == "Israel"] <- 34.85161
players$Longitude[players$Nationality == "England"] <- -1.17432
players$Longitude[players$Nationality == "Albania"] <- 20.16833
players$Longitude[players$Nationality == "Georgia"] <- 43.35689
players$Longitude[players$Nationality == "Italy"] <- 2.56738
players$Longitude[players$Nationality == "Romania"] <- 24.96676
players$Longitude[players$Nationality == "Hungary"] <- 19.5033
players$Longitude[players$Nationality == "Spain"] <- -3.74922
players$Longitude[players$Nationality == "Netherlands"] <- 5.291266
players$Longitude[players$Nationality == "Denmark"] <- 9.501785
players$Longitude[players$Nationality == "Malta"] <- 14.37542
players$Longitude[players$Nationality == "Armenia"] <- 45.03819
players$Longitude[players$Nationality == "Sweden"] <- 18.6435

players$Latitude[players$Nationality == "Azerbaijan"] <- 40.1431
players$Latitude[players$Nationality == "Norway"] <- 60.47202
players$Latitude[players$Nationality == "Kazakhstan"] <- 48.01957
players$Latitude[players$Nationality == "Macedonia"] <- 41.60863
players$Latitude[players$Nationality == "Israel"] <- 31.04605
players$Latitude[players$Nationality == "England"] <- 52.35552
players$Latitude[players$Nationality == "Albania"] <- 41.15333
players$Latitude[players$Nationality == "Georgia"] <- 42.31541
players$Latitude[players$Nationality == "Italy"] <- 41.87194
players$Latitude[players$Nationality == "Romania"] <- 45.94316
players$Latitude[players$Nationality == "Hungary"] <- 47.16249
players$Latitude[players$Nationality == "Spain"] <- 40.46367
players$Latitude[players$Nationality == "Netherlands"] <- 52.13263
players$Latitude[players$Nationality == "Denmark"] <- 56.26392
players$Latitude[players$Nationality == "Malta"] <- 35.9375
players$Latitude[players$Nationality == "Armenia"] <- 40.0691
players$Latitude[players$Nationality == "Sweden"] <- 60.12816


mean(players$Weight,na.rm=TRUE)
players$Weight[is.na(players$Weight)]<-170
mean(players$Height,na.rm=TRUE)
players$Height[is.na(players$Height)]<-185
players$Nationality<-gsub("-",NA,players$Nationality)
players$Nationality[is.na(players$Nationality)]<-median(players$Nationality,na.rm=TRUE)
players["l"]<-(players$Weight)/50


#UEFA Champions League Players


#Getting tables from every page that we want with a function and getting together
df1 <- map(.x = 1:106, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=7&teamId=0&season=2017&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('.wisbb_playerContainer') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)

df2 <- map(.x = 1:106, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=7&teamId=0&season=2017&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('.wisbb_fixedColumn+ td') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)

df3 <- map(.x = 1:106, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=7&teamId=0&season=2017&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('.wisbb_priorityColumn') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)

df4 <- map(.x = 1:106, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=7&teamId=0&season=2017&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('.wisbb_priorityColumn+ td') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)

df5 <- map(.x = 1:106, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=7&teamId=0&season=2017&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('td:nth-child(5)') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)

df6 <- map(.x = 1:106, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=7&teamId=0&season=2017&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('td:nth-child(6)') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)

df7 <- map(.x = 1:106, .f = function(x) {
  url <- paste0("https://www.foxsports.com/soccer/players?competition=7&teamId=0&season=2017&position=0&page=",x,"&country=0&grouping=0&weightclass=0")
  read_html(url) %>%
    html_nodes('td:nth-child(7)') %>%
    html_text() %>%
    as.data.frame()
}) %>% do.call(rbind, .)

players3<-data.frame(df1,df2,df3,df4,df5,df6,df7)
colnames(players3) <- c("Player","Team","Position","Height","Weight","BirthDate","Nationality")

#Data Manipulation
#Changing Tables Coloumn names and implementing null values
colnames(players3) <- c("Player","Team","Position","Height","Weight","BirthDate","Nationality") 
players3$Weight<-gsub("-",NA,players3$Weight)
players3$Height<-gsub("-",NA,players3$Height)
players3$Position<-gsub("-",NA,players3$Position)
players3$BirthDate<-gsub("-",NA,players3$BirthDate)
players3$Nationality<-gsub("-",NA,players3$Nationality)
#Converting feet and inches into centimeters.
players3$Height<-gsub("'","-",players3$Height)
players3$Height<-gsub("\"","",players3$Height)

players3$Height<- sapply(strsplit(as.character(players3$Height),"-"),function(x){30.48*as.numeric(x[1]) + 2.54*as.numeric(x[2])})

#Factor level editing for Position attribute.
levels(players3$Position) <- list(Defence="D",Forward="F",Goalie="G",Midfield="M")

players3$Player<-as.character(players3$Player)
players3$Team<-as.character(players3$Team)
players3$Weight<-as.numeric(players3$Weight)
players3$BirthDate<-as.Date(players3$BirthDate, "%m/%d/%Y")
players3$Nationality<-as.character(players3$Nationality)

#Adding New Coloums as Longtitude and Latitude for location

colnames(players3) <- c("Player","Team","Position","Height","Weight","BirthDate","Nationality")
players3["Longitude"] <- NA
players3["Latitude"]<-NA


players3$Weight[is.na(players3$Weight)]<-mean(players3$Weight,na.rm=TRUE)
players3$Team[is.na(players3$Team)]<-median(players3$Team,na.rm=TRUE)
players3$Height[is.na(players3$Height)]<-mean(players$Height,na.rm=TRUE)
players3$Position<-as.character(players3$Position)
players3$Position[is.na(players3$Position)]<-"Forward"
players3$Position<-as.factor(players3$Position)
levels(players3$Position) <- list(Defence="D",Forward="F",Goalie="G",Midfield="M")
players3$Nationality<-gsub("-",NA,players3$Nationality)
#players3$Nationality[is.na(players3$Nationality)]<-median(players$Nationality,na.rm=TRUE)
players3$BirthDate[is.na(players3$BirthDate)]<-"2000-03-20"


library(tidyverse)    # for ggplot2 and `%>%`
library(ggmap)
library(ggrepel)

players3$Longitude[players3$Nationality == "Azerbaijan"] <- 47.57693
players3$Longitude[players3$Nationality == "Norway"] <- 8.468946
players3$Longitude[players3$Nationality == "Kazakhstan"] <- 66.92368
players3$Longitude[players3$Nationality == "Macedonia"] <- 21.74527
players3$Longitude[players3$Nationality == "Israel"] <- 34.85161
players3$Longitude[players3$Nationality == "England"] <- -1.17432
players3$Longitude[players3$Nationality == "Albania"] <- 20.16833
players3$Longitude[players3$Nationality == "Georgia"] <- 43.35689
players3$Longitude[players3$Nationality == "Italy"] <- 2.56738
players3$Longitude[players3$Nationality == "Romania"] <- 24.96676
players3$Longitude[players3$Nationality == "Hungary"] <- 19.5033
players3$Longitude[players3$Nationality == "Spain"] <- -3.74922
players3$Longitude[players3$Nationality == "Netherlands"] <- 5.291266
players3$Longitude[players3$Nationality == "Denmark"] <- 9.501785
players3$Longitude[players3$Nationality == "Malta"] <- 14.37542
players3$Longitude[players3$Nationality == "Armenia"] <- 45.03819
players3$Longitude[players3$Nationality == "Sweden"] <- 18.6435
players3$Longitude[players3$Nationality == "Faroe Island"] <- -6.911806
players3$Longitude[players3$Nationality == "Madagascar"] <- 46.86911
players3$Longitude[players3$Nationality == "Cameroon"] <- 12.35472
players3$Longitude[players3$Nationality == "Germany"] <- 10.45153
players3$Longitude[players3$Nationality == "Morocco"] <- -7.09262
players3$Longitude[players3$Nationality == "Serbia"] <- 21.00586
players3$Longitude[players3$Nationality == "Argentina"] <- -63.61667
players3$Longitude[players3$Nationality == "Togo"] <- 0.824782
players3$Longitude[players3$Nationality == "Nigeria"] <- 8.675277
players3$Longitude[players3$Nationality == "France"] <- 2.213749
players3$Longitude[players3$Nationality == "Cyprus"] <- 33.42986
players3$Longitude[players3$Nationality == "Brazil"] <- -51.92528
players3$Longitude[players3$Nationality == "Switzerland"] <- 8.227512
players3$Longitude[players3$Nationality == "Croatia"] <- 5.2 



players3$Latitude[players3$Nationality == "Azerbaijan"] <- 40.1431
players3$Latitude[players3$Nationality == "Norway"] <- 60.47202
players3$Latitude[players3$Nationality == "Kazakhstan"] <- 48.01957
players3$Latitude[players3$Nationality == "Macedonia"] <- 41.60863
players3$Latitude[players3$Nationality == "Israel"] <- 31.04605
players3$Latitude[players3$Nationality == "England"] <- 52.35552
players3$Latitude[players3$Nationality == "Albania"] <- 41.15333
players3$Latitude[players3$Nationality == "Georgia"] <- 42.31541
players3$Latitude[players3$Nationality == "Italy"] <- 41.87194
players3$Latitude[players3$Nationality == "Romania"] <- 45.94316
players3$Latitude[players3$Nationality == "Hungary"] <- 47.16249
players3$Latitude[players3$Nationality == "Spain"] <- 40.46367
players3$Latitude[players3$Nationality == "Netherlands"] <- 52.13263
players3$Latitude[players3$Nationality == "Denmark"] <- 56.26392
players3$Latitude[players3$Nationality == "Malta"] <- 35.9375
players3$Latitude[players3$Nationality == "Armenia"] <- 40.0691
players3$Latitude[players3$Nationality == "Sweden"] <- 60.12816
players3$Latitude[players3$Nationality == "Faroe Island"] <- 61.89264
players3$Latitude[players3$Nationality == "Madagascar"] <- -18.76695
players3$Latitude[players3$Nationality == "Cameroon"] <- 7.369722
players3$Latitude[players3$Nationality == "Germany"] <- 51.16569
players3$Latitude[players3$Nationality == "Morocco"] <- 31.7917
players3$Latitude[players3$Nationality == "Serbia"] <- 44.01652
players3$Latitude[players3$Nationality == "Argentina"] <- -38.4161
players3$Latitude[players3$Nationality == "Togo"] <- 8.619543
players3$Latitude[players3$Nationality == "Nigeria"] <- 9.081999
players3$Latitude[players3$Nationality == "France"] <- 46.22764
players3$Latitude[players3$Nationality == "Cyprus"] <- 35.12641
players3$Latitude[players3$Nationality == "Brazil"] <- -14.235
players3$Latitude[players3$Nationality == "Switzerland"] <- 46.81819
players3$Latitude[players3$Nationality == "Croatia"] <- 45.1


players3$Weight[is.na(players3$Weight)]<-mean(players3$Weight,na.rm=TRUE)

players3$Height[is.na(players3$Height)]<-mean(players3$Height,na.rm=TRUE)
players3$Longitude[is.na(players3$Longitude)]<-median(players3$Longitude,na.rm=TRUE)
players3$Latitude[is.na(players3$Latitude)]<-median(players3$Latitude,na.rm=TRUE)
players3["l"]<-(players3$Weight)/50

all_players<-rbind(players,players3)
all_players$Weight<-all_players$Weight/2.2046
all_players["Sustain"]<-NA
all_players$Sustain<-all_players$Weight/all_players$Height


all_players$BirthDate[is.na(all_players$BirthDate)]<-"2000-03-20"



#all_players$Nationality[is.na(all_players$Nationality)]<-median(all_players$Nationality,na.rm=TRUE)


all_players$Longitude[all_players$Nationality == "Poland"] <- 19.14514
all_players$Longitude[all_players$Nationality == "Switzerland"] <- 8.227512
all_players$Longitude[all_players$Nationality == "Slovakia"] <- 19.69902
all_players$Longitude[all_players$Nationality == "Ukraine"] <- 31.16558
all_players$Longitude[all_players$Nationality == "Montenegro"] <- 19.37439
all_players$Longitude[all_players$Nationality == "Bulgaria"] <- 25.48583
all_players$Longitude[all_players$Nationality == "Latvia"] <- 24.60319
all_players$Longitude[all_players$Nationality == "Serbia"] <- 21.00586
all_players$Longitude[all_players$Nationality == "Germany"] <- 10.45153
all_players$Longitude[all_players$Nationality == "Italy"] <- 12.5673
all_players$Longitude[all_players$Nationality == "Moldova"] <- 28.36985
all_players$Longitude[all_players$Nationality == "Kosovo"] <- 20.90298
all_players$Longitude[all_players$Nationality == "Czech Republic"] <- 15.47296
all_players$Longitude[all_players$Nationality == "Turkey"] <- 35.24332
all_players$Longitude[all_players$Nationality == "Belarus"] <- 27.95339
all_players$Longitude[all_players$Nationality == "Gibrallar"] <- -5.353585
all_players$Longitude[all_players$Nationality == "Ireland"] <- -7.692054
all_players$Longitude[all_players$Nationality == "Wales"] <- -3.783712
all_players$Longitude[all_players$Nationality == "Scotland"] <- -4.202646
all_players$Longitude[all_players$Nationality == "Slovenia"] <- 14.55546
all_players$Longitude[all_players$Nationality == "Luxemborg"] <- 6.125583
all_players$Longitude[all_players$Nationality == "Croatia"] <- 15.2
all_players$Longitude[all_players$Nationality == "San Marina"] <- 12.45778
all_players$Longitude[all_players$Nationality == "Finland"] <- 25.74815
all_players$Longitude[all_players$Nationality == "Portugal"] <- -8.224454
all_players$Longitude[all_players$Nationality == "Faroe Islands"] <- -6.911806
all_players$Longitude[all_players$Nationality == "Estonia"] <- 25.01361
all_players$Longitude[all_players$Nationality == "Greece"] <- 21.82431
all_players$Longitude[all_players$Nationality == "Bosnia"] <- 17.67508
all_players$Longitude[all_players$Nationality == "Iceland"] <- -19.02084
all_players$Longitude[all_players$Nationality == "Northern Ireland"] <- -6.492314
all_players$Longitude[all_players$Nationality == "Austria"] <- 14.55007
all_players$Longitude[all_players$Nationality == "Cyprus"] <- 33.42986
all_players$Longitude[all_players$Nationality == "Andorra"] <- 1.521801
all_players$Longitude[all_players$Nationality == "France"] <- 2.213745
all_players$Longitude[all_players$Nationality == "United States"] <- -55.71289
all_players$Longitude[all_players$Nationality == "Lithuania"] <- 23.88127
all_players$Longitude[all_players$Nationality == "Bosnia and Herzegovina"] <- 17.67908
all_players$Longitude[all_players$Nationality == "Nigeria"] <- 8.675277
all_players$Longitude[all_players$Nationality == "Zimbabwe"] <- 29.15486
all_players$Longitude[all_players$Nationality == "Zambia"] <- 27.84933
all_players$Longitude[all_players$Nationality == "Yugoslavia"] <- 20.16711
all_players$Longitude[all_players$Nationality == "Virgin Islands"] <- -64.89633
all_players$Longitude[all_players$Nationality == "Venezuela"] <- -66.58973
all_players$Longitude[all_players$Nationality == "Uzbekistan"] <- 64.58526
all_players$Longitude[all_players$Nationality == "USSR"] <- -97.78849
all_players$Longitude[all_players$Nationality == "Uruguay"] <- -55.76584
all_players$Longitude[all_players$Nationality == "Turkmenistan"] <- 59.55628
all_players$Longitude[all_players$Nationality == "Tunisia"] <- 9.537499
all_players$Longitude[all_players$Nationality == "Togo"] <- 0.824782
all_players$Longitude[all_players$Nationality == "The Democratic Republic of Congo"] <- 21.75866
all_players$Longitude[all_players$Nationality == "South Korea"] <- 127.7669
all_players$Longitude[all_players$Nationality == "South Africa"] <- 22.93751
all_players$Longitude[all_players$Nationality == "Senegal"] <- -14.45236
all_players$Longitude[all_players$Nationality == "Russia"] <- 105.3188
all_players$Longitude[all_players$Nationality == "Peru"] <- -75.01515
all_players$Longitude[all_players$Nationality == "Paraguay"] <- -58.44383
all_players$Longitude[all_players$Nationality == "New Zealand"] <- 174.886
all_players$Longitude[all_players$Nationality == "Mexico"] <- -102.5528
all_players$Longitude[all_players$Nationality == "Mali"] <- -3.996166
all_players$Longitude[all_players$Nationality == "Liechtenstein"] <- 9.555373
all_players$Longitude[all_players$Nationality == "Kenya"] <- 37.90619
all_players$Longitude[all_players$Nationality == "Japan"] <- 138.2529
all_players$Longitude[all_players$Nationality == "Iran"] <- 53.68805
all_players$Longitude[all_players$Nationality == "Honduras"] <- -86.24191
all_players$Longitude[all_players$Nationality == "Denmark"] <- 9.501785
all_players$Longitude[all_players$Nationality == "Belgium"] <- 4.469936
all_players$Longitude[all_players$Nationality == "Brazil"] <- -51.92528
all_players$Longitude[all_players$Nationality == "Gibraltar"] <- -5.353585
all_players$Longitude[all_players$Nationality == "San Marino"] <- 12.45778


all_players$Latitude[all_players$Nationality == "Poland"] <- 51.91944
all_players$Latitude[all_players$Nationality == "Switzerland"] <- 46.89819
all_players$Latitude[all_players$Nationality == "Slovakia"] <- 48.66903
all_players$Latitude[all_players$Nationality == "Ukraine"] <- 48.37943
all_players$Latitude[all_players$Nationality == "Montenegro"] <- 42.70868
all_players$Latitude[all_players$Nationality == "Bulgaria"] <- 42.73388
all_players$Latitude[all_players$Nationality == "Latvia"] <- 56.8796
all_players$Latitude[all_players$Nationality == "Serbia"] <- 21.00586
all_players$Latitude[all_players$Nationality == "Germany"] <- 51.16569
all_players$Latitude[all_players$Nationality == "Italy"] <- 41.87194
all_players$Latitude[all_players$Nationality == "Moldova"] <- 47.41163
all_players$Latitude[all_players$Nationality == "Kosovo"] <- 42.60264
all_players$Latitude[all_players$Nationality == "Czech Republic"] <- 49.81745
all_players$Latitude[all_players$Nationality == "Turkey"] <- 38.96375
all_players$Latitude[all_players$Nationality == "Belarus"] <- 53.70981
all_players$Latitude[all_players$Nationality == "Gibrallar"] <- 36.14075
all_players$Latitude[all_players$Nationality == "Ireland"] <- 53.14237
all_players$Latitude[all_players$Nationality == "Wales"] <- 52.13066
all_players$Latitude[all_players$Nationality == "Scotland"] <- 56.49067
all_players$Latitude[all_players$Nationality == "Slovenia"] <- 46.15124
all_players$Latitude[all_players$Nationality == "Luxemborg"] <- -49.81527
all_players$Latitude[all_players$Nationality == "Croatia"] <- 45.1
all_players$Latitude[all_players$Nationality == "San Marino"] <- 43.94236
all_players$Latitude[all_players$Nationality == "Finland"] <- 61.52411
all_players$Latitude[all_players$Nationality == "Portugal"] <- 39.39987
all_players$Latitude[all_players$Nationality == "Faroe Islands"] <- 61.89264
all_players$Latitude[all_players$Nationality == "Estonia"] <- 58.59527
all_players$Latitude[all_players$Nationality == "Greece"] <- 39.07421
all_players$Latitude[all_players$Nationality == "Bosnia"] <- 43.91589
all_players$Latitude[all_players$Nationality == "Iceland"] <- 64.96305
all_players$Latitude[all_players$Nationality == "Northern Ireland"] <- 54.78771
all_players$Latitude[all_players$Nationality == "Austria"] <- 47.51623
all_players$Latitude[all_players$Nationality == "Cyprus"] <- 35.12641
all_players$Latitude[all_players$Nationality == "Andorra"] <- 42.50628
all_players$Latitude[all_players$Nationality == "France"] <- 46.22764
all_players$Latitude[all_players$Nationality == "United States"] <- 37.09024
all_players$Latitude[all_players$Nationality == "Lithuania"] <- 55.169944
all_players$Latitude[all_players$Nationality == "Bosnia and Herzegovina"] <- 43.91589
all_players$Latitude[all_players$Nationality == "Nigeria"] <- 9.081999
all_players$Latitude[all_players$Nationality == "Zimbabwe"] <- -19.01544
all_players$Latitude[all_players$Nationality == "Zambia"] <- -13.1339
all_players$Latitude[all_players$Nationality == "Yugoslavia"] <- 44.98891
all_players$Latitude[all_players$Nationality == "Virgin Islands"] <- 18.33576
all_players$Latitude[all_players$Nationality == "Venezuela"] <- 6.42375
all_players$Latitude[all_players$Nationality == "Uzbekistan"] <- 41.37749
all_players$Latitude[all_players$Nationality == "USSR"] <- 30.52131
all_players$Latitude[all_players$Nationality == "Uruguay"] <- -32.52278
all_players$Latitude[all_players$Nationality == "Turkmenistan"] <- 38.96972
all_players$Latitude[all_players$Nationality == "Tunisia"] <- 33.88692
all_players$Latitude[all_players$Nationality == "Togo"] <- 8.619543
all_players$Latitude[all_players$Nationality == "The Democratic Republic of Congo"] <- -4.038333
all_players$Latitude[all_players$Nationality == "South Korea"] <- 35.90776
all_players$Latitude[all_players$Nationality == "South Africa"] <- -30.55948
all_players$Latitude[all_players$Nationality == "Senegal"] <-  14.4974
all_players$Latitude[all_players$Nationality == "Russia"] <- 61.52401
all_players$Latitude[all_players$Nationality == "Peru"] <- -9.189967
all_players$Latitude[all_players$Nationality == "Paraguay"] <- -23.4425
all_players$Latitude[all_players$Nationality == "New Zealand"] <- -40.90056
all_players$Latitude[all_players$Nationality == "Mexico"] <- 23.6345
all_players$Latitude[all_players$Nationality == "Mali"] <- 17.57069
all_players$Latitude[all_players$Nationality == "Liechtenstein"] <- 47.166
all_players$Latitude[all_players$Nationality == "Kenya"] <- -0.023559
all_players$Latitude[all_players$Nationality == "Japan"] <- 36.20482
all_players$Latitude[all_players$Nationality == "Iran"] <- 32.42791
all_players$Latitude[all_players$Nationality == "Honduras"] <- 15.2
all_players$Latitude[all_players$Nationality == "Denmark"] <- 56.26392
all_players$Latitude[all_players$Nationality == "Belgium"] <- 50.50389
all_players$Latitude[all_players$Nationality == "Brazil"] <- -14.235
all_players$Latitude[all_players$Nationality == "Gibraltar"] <- 36.14075

all_players<-na.omit(all_players)
#Plots

#md1
#position distribution chart of turkish soccer players according to teams
newdata<-all_players[which(all_players$Nationality=='Turkey'),]
library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(newdata, aes(Team))
g + geom_bar(aes(fill=Position), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Team across Player Position") 

#md2
#Players who from Romania and has midfield position histogram by Weight and Height
newdata2<-all_players[ which(all_players$Nationality =="Romania" & all_players$Position == 'Midfield'),]
ggplot(data = newdata2) + 
  geom_point(mapping = aes(x = Team, y = Weight))

#md3
#Players  who has defence position and under 65kg weight histogram by Nation
newdata3<-all_players[ which(all_players$Position == "Defence" & all_players$Weight < 65),]
ggplot(data = newdata3) + 
  geom_point(mapping = aes(x = Team, y = Weight, color = Nationality))


#md4


library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(all_players, aes(Nationality))
g + geom_bar(aes(fill=Team), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Player numbers categorized by nationality and  teams") 


