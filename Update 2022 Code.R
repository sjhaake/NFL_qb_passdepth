## import libraries (you don't actually need all these)

{
  library(airball)
  library(hoopR)
  library(nbastatR)
  library(nbaTools)
  library(ggrepel)
  library(ggimage)
  library(jpeg)
  library(grid)
  library(rjson)
  library(RCurl)
  library(gganimate)
  library(ggthemes)
  library(pracma)
  library(hrbrthemes)
  library(ggbiplot)
  library(factoextra)
  library(Rcpp)
  library(RcppParallel)
  library(caret)
  library(magick)
  library(jsonlite)
  library(rvest)
  library(httr)
  library(FactoMineR)
  library(Hmisc)
  library(zoo)
  library(janitor)
  library(gt)
  library(readxl)
  library(tidyverse)
}

#get NBA Team information (logos,colors,etc)

nbateams <- nbastatR::nba_teams()
nbateams <- nbateams %>% filter(isNonNBATeam == 0)

nbateams <- nbateams %>% mutate(
  Conference = case_when(
    idConference == 1 ~ "East",
    TRUE ~ "West"
  )
)

#Bring in travel data and change it to fit nba.com data (slugteam)
travel2022 <- travel2022 %>% filter(Season == "2021-22")

travel2022 <- travel2022 %>%
  mutate(
    nameTeam = case_when(
      Team == "Atlanta Hawks" ~ "ATL",
      Team == "Boston Celtics" ~ "BOS",
      Team == "Brooklyn Nets" ~ "BKN",
      Team == "Charlotte Hornets" ~ "CHA",
      Team == "Chicago Bulls" ~ "CHI",
      Team == "Cleveland Cavaliers" ~ "CLE",
      Team == "Dallas Mavericks" ~ "DAL",
      Team == "Denver Nuggets" ~ "DEN",
      Team == "Detroit Pistons" ~ "DET",
      Team == "Golden State Warriors" ~ "GSW",
      Team == "Houston Rockets" ~ "HOU",
      Team == "Indiana Pacers" ~ "IND",
      Team == "Los Angeles Clippers" ~ "LAC",
      Team == "Los Angeles Lakers" ~ "LAL",
      Team == "Memphis Grizzlies" ~ "MEM",
      Team == "Miami Heat" ~ "MIA",
      Team == "Milwaukee Bucks" ~ "MIL",
      Team == "Minnesota Timberwolves" ~ "MIN",
      Team == "New Orleans Pelicans" ~ "NOP",
      Team == "New York Knicks" ~ "NYK",
      Team == "Oklahoma City Thunder" ~ "OKC",
      Team == "Orlando Magic" ~ "ORL",
      Team == "Philadelphia 76ers" ~ "PHI",
      Team == "Phoenix Suns" ~ "PHO",
      Team == "Portland Trail Blazers" ~ "POR",
      Team == "Sacramento Kings" ~ "SAC",
      Team == "San Antonio Spurs" ~ "SAS",
      Team == "Toronto Raptors" ~ "TOR",
      Team == "Utah Jazz" ~ "UTA",
      Team == "Washington Wizards" ~ "WAS",
      TRUE ~ "NULL"
    ),
    nameTeam_opp = case_when(
      Opponent =="Atlanta Hawks" ~ "ATL",
      Opponent == "Boston Celtics" ~ "BOS",
      Opponent == "Brooklyn Nets" ~ "BKN",
      Opponent == "Charlotte Hornets" ~ "CHA",
      Opponent == "Chicago Bulls" ~ "CHI",
      Opponent == "Cleveland Cavaliers" ~ "CLE",
      Opponent == "Dallas Mavericks" ~ "DAL",
      Opponent == "Denver Nuggets" ~ "DEN",
      Opponent == "Detroit Pistons" ~ "DET",
      Opponent == "Golden State Warriors" ~ "GSW",
      Opponent == "Houston Rockets" ~ "HOU",
      Opponent == "Indiana Pacers" ~ "IND",
      Opponent == "Los Angeles Clippers" ~ "LAC",
      Opponent == "Los Angeles Lakers" ~ "LAL",
      Opponent == "Memphis Grizzlies" ~ "MEM",
      Opponent == "Miami Heat" ~ "MIA",
      Opponent == "Milwaukee Bucks" ~ "MIL",
      Opponent == "Minnesota Timberwolves" ~ "MIN",
      Opponent == "New Orleans Pelicans" ~ "NOP",
      Opponent == "New York Knicks" ~ "NYK",
      Opponent == "Oklahoma City Thunder" ~ "OKC",
      Opponent == "Orlando Magic" ~ "ORL",
      Opponent == "Philadelphia 76ers" ~ "PHI",
      Opponent == "Phoenix Suns" ~ "PHO",
      Opponent == "Portland Trail Blazers" ~ "POR",
      Opponent == "Sacramento Kings" ~ "SAC",
      Opponent == "San Antonio Spurs" ~ "SAS",
      Opponent == "Toronto Raptors" ~ "TOR",
      Opponent == "Utah Jazz" ~ "UTA",
      Opponent == "Washington Wizards" ~ "WAS",
      TRUE ~ "NULL"
    )
  )

travel2022 <- travel2022 %>% mutate(
  Rest = case_when(
    Rest >= 7 ~ 7,
    TRUE ~ Rest
  )
)

### DOWNLOAD 2022 GAMES ###

games2022 <- nbastatR::game_logs(seasons = 2022, result_types = "team",box_score_types = "Four Factors")

#determine which games are new to the database
unique(games2022$idGame) %>% sort()

#extract boxscore data for each day
nbastatR::box_scores(c("22100001":"22100002"),result_types = "team")
games_101921 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100003":"22100013"),result_types = "team")
games_102021 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100014":"22100016"),result_types = "team")
games_102122 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100017":"22100026"),result_types = "team")
games_102222 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100027":"22100034"),result_types = "team")
games_102322 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100035":"22100040"),result_types = "team")
games_102422 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100041":"22100049"),result_types = "team")
games_102522 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100050":"22100054"),result_types = "team")
games_102622 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100055":"22100064"),result_types = "team")
games_102722 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100065":"22100070"),result_types = "team")
games_102822 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100071":"22100077"),result_types = "team")
games_102922 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100078":"22100088"),result_types = "team")
games_103022 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100089":"22100093"),result_types = "team")
games_103122 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100094":"22100102"),result_types = "team")
games_110122 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100103":"22100107"),result_types = "team")
games_110222 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100108":"22100118"),result_types = "team")
games_110322 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100119":"22100123"),result_types = "team")
games_110422 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100124":"22100132"),result_types = "team")
games_110522 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100133":"22100138"),result_types = "team")
games_110622 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100139":"22100146"),result_types = "team")
games_110722 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100147":"22100154"),result_types = "team")
games_110822 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100155":"22100157"),result_types = "team")
games_110922 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100158":"22100170"),result_types = "team")
games_111022 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100171":"22100181"),result_types = "team")
games_111122 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100182":"22100184"),result_types = "team")
games_111222 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100185":"22100191"),result_types = "team")
games_111322 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100192":"22100198"),result_types = "team")
games_111422 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100199":"22100209"),result_types = "team")
games_111522 <- dataBoxScoreTeamNBA

nbastatR::box_scores(c("22100210":"22100212"),result_types = "team")
games_111622 <- dataBoxScoreTeamNBA

#bind all games into database
db2022 <- bind_rows(
  games_101921,
  games_102021,
  games_102122,
  games_102222,
  games_102322,
  games_102422,
  games_102522,
  games_102622,
  games_102722,
  games_102822,
  games_102922,
  games_103022,
  games_103122,
  games_110122,
  games_110222,
  games_110322,
  games_110422,
  games_110522,
  games_110622,
  games_110722,
  games_110822,
  games_110922,
  games_111022,
  games_111122,
  games_111222,
  games_111322,
  games_111422,
  games_111522,
  games_111622
)

#calculate Four Factors (SFF) along with wins/losses
db2022 <- db2022 %>% mutate(
  SFF = x*pctEFG + y*pctOREB - z*(pctTOVTeam/100) + q*rateFTA,
  SFF_opp = x*pctEFGOpponent + y*pctOREBOpponent - z*(pctTOVOpponent) + q*rateFTAOpponent,
  Net_SFF = SFF-SFF_opp,
  W_L = case_when(
    plusminus > 0 ~ "W",
    TRUE ~ "L"
  ),
  win = case_when(
    W_L == "W" ~ 1,
    TRUE ~ 0
  )
) %>% select(idGame,slugTeam,W_L,SFF,SFF_opp,Net_SFF,everything())

#add dates to games (WHY ARENT THEY DATED??)
db2022 <- left_join(db2022,games2022[,c("dateGame","idGame")],by="idGame")

#only 1 copy of each game/team (this is to correct a mistake I made somewhere)
db2022 <- db2022 %>% group_by(idGame,slugTeam) %>% slice(1)

#add in travel data
db2022 <- left_join(db2022,travel2022[,c("Date","nameTeam","nameTeam_opp","G","Location","Rest","Distance")],by=c("slugTeam" = "nameTeam","dateGame" = "Date"))

#determine homecourt advantage
db2022 <- db2022 %>% mutate(
  hca = case_when(
    Location == "Home" ~ 1,
    Location == "Away" ~ -1,
    TRUE ~ 0
  )
)

#add weeks to db2022
db2022 <- db2022 %>% mutate(week = lubridate::week(dateGame)) %>% select(week,everything()) %>% mutate(week = case_when(week <= 52 ~ week-41,week >=1 ~ week+12))

#add team colors,logos,conference
db2022 <- left_join(db2022,nbateams[,c("slugTeam","urlThumbnailTeam","colorsTeam","Conference")],by="slugTeam")

#correct for strange color format
db2022 <- db2022 %>% mutate(
  colorsTeam = gsub("\\,,*.*","",colorsTeam)
)

#create SIMON RATING (S_RTG) which takes into account FF and ORTG/DRTG
db2022 <- db2022 %>% 
  mutate(S_O_RTG = SFF*ortg, S_D_RTG = SFF_opp*drtg, S_RTG = S_O_RTG - S_D_RTG)

#Isolate most recent game for charting purposes
lastgame <- db2022 %>% group_by(slugTeam) %>% slice_tail()



#CHART CONFERENCE FF STANDINGS including most recent game as logo
db2022 %>%
  ggplot(aes(reorder(slugTeam,S_RTG),S_RTG))+
  geom_hline(yintercept=mean(db2022$S_RTG),linetype = "dashed",color="red",alpha=.5)+
  geom_point(aes(alpha=G,size=3),color=db2022$colorsTeam)+
  coord_flip()+
  facet_wrap(~Conference,scales="free")+
  geom_text(aes(label=G),color="white",size=3)+
  geom_image(data=lastgame,aes(image=lastgame$urlThumbnailTeam),size=.0725)+
  labs(x="",y="Net Rating",title="NBA 2022 Conference Net Ratings",subtitle="Net Four Factors Through 11-16 \nMost Recent Game Highlighted",caption="Data: NBA.com thru nbafastR \nViz: @simonhaake")+
  theme_reach()+
  theme(plot.caption = element_text(face="bold",colour = "red"),strip.background = element_rect(fill="grey"),strip.text = element_text(face="bold",size=20),plot.subtitle = element_text(size=15))




#Create SFF Overall Rankings Summary
nba_sff_rankings_22 <- db2022 %>%
  group_by(slugTeam) %>%
  summarize(n=n(),wins = sum(win),losses = n-wins, record = paste0(wins,"-",losses), SFF = mean(SFF),SFF_opp = mean(SFF_opp),NET_SFF = mean(Net_SFF),ORTG = mean(ortg),DRTG = mean(drtg),NET_RTG = ORTG-DRTG,S_RTG = NET_RTG+(NET_SFF*10),pm = mean(plusminus),wp=wins/n)

#bring back in the colors/logos
nba_sff_rankings_22 <- left_join(nba_sff_rankings_22,nbateams[,c("slugTeam","urlThumbnailTeam","colorsTeam","Conference")],by="slugTeam")

#correct for stupid color issue
nba_sff_rankings_22 <- nba_sff_rankings_22 %>% mutate(
  colorsTeam = gsub("\\,,*.*","",colorsTeam)
)

#create S Ratings
nba_sff_rankings_22 <- nba_sff_rankings_22 %>% 
  mutate(S_O_RTG = SFF*ORTG, S_D_RTG = SFF_opp*DRTG)



## PLOT current Offensive/Defensive S Ratings
nba_sff_rankings_22 %>% 
  ggplot(aes(S_O_RTG,-S_D_RTG))+
  geom_hline(yintercept = -mean(nba_sff_rankings_22$S_D_RTG),linetype = "dotted",color="red",size=1,alpha=.4)+
  geom_vline(xintercept = mean(nba_sff_rankings_22$S_O_RTG),linetype = "dotted",color="red",size=1,alpha=.4)+
  geom_abline(slope=-1,intercept = c(8,5,2,-1,-4,-7,-10),size=.5,alpha=.2)+
  geom_image(image=nba_sff_rankings$urlThumbnailTeam,size=.0675,image_fun = transparent)+
  geom_text(aes(label=record),vjust = 3,fontface = "bold",alpha=.6)+
  labs(x="Offensive Rating*",y="Defensive Rating*",title="NBA Offensive/Defensive Adjusted Four Factors", subtitle = "2022 Regular Season (Through 11/15)",caption="viz: @simonhaake \ndata: NBA.com via nbastatR")+
  theme_reach()+
  theme(plot.caption = element_text(face="bold",colour = "red"))+
  geom_curve(aes(x=27.4,y=-25,xend=28.1,yend=-25.9),arrow=arrow(length=unit(0.03,"npc")))+
  geom_text(aes(x=27.4,y=-25,label="*Facepalm*"))


#adjusted ff vs win percentage
nba_sff_rankings_22 %>% 
  ggplot(aes(wp,S_RTG))+
  geom_smooth(method="lm",se=F,linetype = "dashed",alpha=.8)+
  geom_hline(yintercept = mean(nba_sff_rankings_22$S_RTG),linetype = "dotted",color="red",size=1,alpha=.4)+
  geom_vline(xintercept = mean(nba_sff_rankings_22$wp),linetype = "dotted",color="red",size=1,alpha=.4)+
  geom_image(image=nba_sff_rankings$urlThumbnailTeam,size=.0675,image_fun = transparent)+
  labs(x="Win Percentage",y="Net Offensive/Defensive Rating",title="NBA Win Percentage vs Net Rating", subtitle = "2022 Regular Season (Through 11/15)",caption="viz: @simonhaake \ndata: NBA.com via nbastatR")+
  theme_reach()+
  theme(plot.caption = element_text(face="bold",colour = "red"))+
  geom_text(aes(x=.24,y=4,label="Losing More than \nthey should"))+
  geom_text(aes(x=.75,y=-3.8,label="Winning More than \nthey should"))+
  geom_curve(aes(x=.75,y=-2.8,xend=.7,yend=-2),arrow=arrow(length=unit(0.03,"npc")))+
  geom_curve(aes(x=.24,y=3,xend=.3,yend=1.8),arrow=arrow(length=unit(0.03,"npc")))



### GT Table with Conference Ratings ###
nba_sff_rankings_22 %>% group_by(Conference) %>% arrange(-S_RTG) %>% slice(1:8) %>% mutate(S_RTG = round(S_RTG,1), rank = round(rank(-S_RTG),0)) %>% select(rank,slugTeam,urlThumbnailTeam,record,S_RTG,Conference) %>%
  gt(groupname_col = "Conference") %>%
  cols_label(rank = "",slugTeam = "Team",record = "W-L", S_RTG = "Net Rtg Through 11/16") %>%
  tab_header(title = md("NBA Four Factors Net Rating (Top 8)")) %>%
  tab_source_note(source_note = "Data: NBA.com through nbastatR, Table: @simonhaake") %>%
  tab_source_note(source_note = "*updated through 11/16/21") %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(3)),
      cell_text(weight = "bold",size = "small")
    )
  ) %>%
  text_transform(
    locations = cells_body(c(urlThumbnailTeam)),
    fn = function(x) {
      web_image(
        url = x,
        height = 30
      )
    }
  ) %>%
  gt::cols_align(c(urlThumbnailTeam,slugTeam),align = "center") %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  cols_width(c(urlThumbnailTeam) ~ px(50)) %>%
  cols_width(c(slugTeam) ~ px(60)) %>%
  cols_label(urlThumbnailTeam = "") %>%
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "black",
    table.border.top.color = "snow",
    table.border.bottom.color = "snow",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    heading.align = "left",
    row_group.background.color = "grey",
    row_group.font.weight = "bold"
  )