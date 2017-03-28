library(XML)
library(dplyr)     # please install - replaces plyr
library(reshape2)  # please install - replaces reshape
library(gdata)


setwd("C:/Analysis/Huddersfield/Data")   # directory containing f24, f7 & csv files 

evNames <- read.csv("Opta event__ids.csv")
quNames <- read.csv("qualifiers.csv")

#utility function
grabAll <- function(XML.parsed, field){
  parse.field <- xpathSApply(XML.parsed, paste("//", field, "[@*]", sep=""))
  results <- t(sapply(parse.field, function(x) xmlAttrs(x)))
  if(typeof(results)=="list"){
    bind_rows(lapply(lapply(results, t), data.frame, stringsAsFactors=F))
  } else {
    as.data.frame(results, stringsAsFactors=F)
  }
}

get_teamInfo <- function(gameParse) {
  
  teamParse <- xpathSApply(gameParse, "//TeamData")
  teamParse2 <- xpathSApply(gameParse, "//Team/Name")
  
  teamInfo <- data.frame(
    team_id = sapply(teamParse, function(x) xmlGetAttr(node=x, "TeamRef"))    # team_id
    , team_side = sapply(teamParse, function(x) xmlGetAttr(node=x, "Side"))   # Home/Away
    , team_name = sapply(teamParse2, function(x) xmlValue(x))                 # Team Name
    , stringsAsFactors=F
  )
  
  # Rev-01 add opposition team name
  teamInfo$team_name.OPP <- NA
  teamInfo$team_name.OPP[1] <- teamInfo$team_name[2]
  teamInfo$team_name.OPP[2] <- teamInfo$team_name[1]
  return(teamInfo)
}

get_playerInfo <- function(gameParse) {
  
  playerParse <- xpathSApply(gameParse, "//Team/Player")
  lineupParse <- xpathSApply(gameParse, "//Team")
  
  NPlayers <- sapply(lineupParse, function(x) sum(names(xmlChildren(x)) == "Player"))
  
  playerInfo <- data.frame(
    player_id = sapply(playerParse, function(x) xmlGetAttr(node=x, "uID"))
    , team_id = c(rep(teamInfo$team_id[1], NPlayers[1]), rep(teamInfo$team_id[2], NPlayers[2]))
    , position = sapply(playerParse, function(x) xmlGetAttr(node=x, "Position"))
    , first_name = sapply(playerParse, function(x) xmlValue(xmlChildren(xmlChildren(x)$PersonName)$First))
    , last_name = sapply(playerParse, function(x) xmlValue(xmlChildren(xmlChildren(x)$PersonName)$Last))
  )
  playerInfo$player_id <- gsub('p', '', playerInfo$player_id)
 
   return(playerInfo)  
  
}


#===========================================================================================
f24.files <- list.files(pattern= "f24-.*-eventdetails.xml")  # retrieve event files

#NB: This routine assumes there is a corresponding f7 file for each f24 file
#    conforming to the assumed naming convention
#    f24 name = f24-X-Y-Z-eventdetails.xml, f7 name = srml-X-Y-fZ-matchresults.xml
#    if not, the routine will fail

for (f in f24.files) { 
  
  match.id <- unlist(strsplit(f, split= "-", fixed=TRUE))
  
  f24 <- paste("f24", match.id[2], match.id[3], match.id[4], "eventdetails.xml", sep="-")
  f7  <- paste0("srml-", match.id[2], "-", match.id[3], "-f", match.id[4],
                "-matchresults.xml")
  outfile <- paste0(paste("Events",match.id[2], match.id[3], match.id[4], sep="-"), ".RDS")
  
  
  print(paste("Processing file", f24))
  
  
  #team parsing
  gameParse <- xmlInternalTreeParse(f7)
  teamInfo <- get_teamInfo(gameParse)
  playerInfo <- get_playerInfo(gameParse)
  
  #Play-by-Play Parsing
  pbpParse <- xmlInternalTreeParse(f24)
  eventInfo <- grabAll(pbpParse, "Event")
  eventParse <- xpathSApply(pbpParse, "//Event")
  NInfo <- sapply(eventParse, function(x) sum(names(xmlChildren(x)) == "Q"))
  QInfo <- grabAll(pbpParse, "Q")
  EventsExpanded <- as.data.frame(lapply(eventInfo[,1:2], function(x) rep(x, NInfo)), stringsAsFactors=F)
  QInfo <- cbind(EventsExpanded, QInfo)
  names(QInfo)[c(1,3)] <- c("Eid", "Qid")
  QInfo$value <- ifelse(is.na(QInfo$value), 1, QInfo$value)
  Qual <- dcast(QInfo, Eid ~ qualifier_id)
  
  #comment the following loop if you have commented the xls files loading at the beginning
  for(i in names(Qual)[-1]){
    
    ii <- as.integer(i)
    if ( ii %in% quNames$id) {
      
      txt <- quNames[which(quNames$id==ii), "name"]
      txt <- gsub('[[:space:]]+$', '', txt)
      lbl <- tolower(gsub("-", "_", gsub(" ", "_", txt, fixed=T), fixed=T))
      names(Qual)[which(names(Qual)==i)] <- lbl
    } else {
      names(Qual)[which(names(Qual)==i)] <- paste0("qualifier_", i)
      print(paste("Warning. Please add definition for qualifier", i, "to qualifiers.csv."))
    }
    
  }
  
  #final data set
  events <- merge(eventInfo, Qual, by.x="id", by.y="Eid", all.x=T, suffixes=c("", "Q"))
  
  #adjustment of variables
  events$TimeStamp <- as.POSIXct(events$timestamp, format="%Y-%m-%dT%H:%M:%S")
  events$timestamp <- NULL
  events$last_modified <- as.POSIXct(events$last_modified, format="%Y-%m-%dT%H:%M:%S")
  events$team_id <- paste("t", events$team_id, sep="")
  
  # convert to numerics where possible 
  #[NB some events may be missing eg injury, so need to check
  
  
  for (var in names(events)) {
    
    if (class(events[, var])[1] == "character") {
      events[, var] <- type.convert(events[, var])
    } 
  }
  
  
  
  events <- merge(events, teamInfo)
  events <- merge(events, playerInfo)
  eventstouch <- events[events$type_id %in% c(1,2,3,4,7,8,9,10,11,12,13,14,15,16,41,42,49,50,52,54,61), ]
  
  eventstouch <- eventstouch[order(eventstouch$min, eventstouch$sec, eventstouch$player_id, eventstouch$team_id, eventstouch$event_id),]
  
  eventstouch <- subset(eventstouch, eventstouch$type_id!=43)
  
  eventstouch$disx <- eventstouch$x
  eventstouch$disy <- eventstouch$y
  
  library(dplyr)
  
  eventstouch <- eventstouch %>% 
    mutate(previousx = if_else(lag(type_id)==1, lag(pass_end_x), lag(x)))
  #
  eventstouch <- eventstouch %>% 
    mutate(previousy = if_else(lag(type_id)==1, lag(pass_end_y), lag(y)))
  
  
  attach(eventstouch)
  ydiff <- abs(previousy - disy)
  xdiff <- abs(previousx - disx)
  eventstouch$distance <- sqrt(xdiff^2 + ydiff^2)
  
  eventstouch$pteam_id <- lag(eventstouch$team_id)
  
  eventstouch$pplayerid <- lag(eventstouch$player_id)
  
  eventstouch <- eventstouch %>% 
    mutate(distance = if_else(lag(outcome)==0, 0, distance))
  
  eventstouch <- eventstouch %>% 
    mutate(distance = if_else(lag(type_id)== 12, 0, distance))
  
  eventstouch <- eventstouch %>%
    mutate(distance = if_else(lag(type_id)== 8 & player_id != pplayerid, 0, distance))
  
  eventstouch <- eventstouch %>%
    mutate(distance = if_else(lag(type_id)== 7 & player_id != pplayerid, 0, distance))
  
  eventstouch <- eventstouch %>%
    mutate(distance = if_else(lag(type_id)== 4 & player_id != pplayerid, 0, distance))
  
  
  eventstouch$carrydistance <- ifelse(eventstouch$team_id == eventstouch$pteam_id, eventstouch$distance, 0)
  eventstouch$touch <- 1
  
  previousxdiff <- abs(eventstouch$previousx- 100)
  previousydiff <- abs(eventstouch$previousy- 50)
  eventstouch$predisG <- sqrt(previousxdiff^2 + previousydiff^2)
  
  disxdiff <- abs(eventstouch$disx - 100)
  disydiff <- abs(eventstouch$disy - 50)
  eventstouch$disG <- sqrt(disxdiff^2 + disydiff^2)
  
  eventstouch$directdistance <- eventstouch$predisG - eventstouch$disG
  
  eventstouch$directdistance <- ifelse(eventstouch$carrydistance == 0, 0, eventstouch$directdistance)
  
  #PPlayer For Expected Assists
  eventstouch$pplayerid <- ifelse(eventstouch$assisted == 1, eventstouch$pplayerid, 0)
  
  saveRDS(eventstouch, outfile)        ## amended
  print(paste("File", outfile, "saved to RDS"))
}

require(data.table)
files = list.files(pattern = 'Events-.*.RDS')
dat_list = lapply(files, function (x) data.table(readRDS(x)))
dat9 = rbindlist(dat_list, fill = TRUE)



