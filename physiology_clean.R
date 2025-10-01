##### Raw skin conductance and skin temperature munging script ######
##### Scott Bannister 3/4/19 ########################################
#####################################################################

##### Script provides an overall skin temperature dataset for all participants, and also exports skin conductance 
##### data for further processing in Ledalab (noramlisation will come after extracting phasic component!)

##### LOAD IN SKIN CONDUCTANCE/SKIN TEMPERATURE
##### NOTE: Participants 1 - 11 do not have data for Behind the Door stimulus; 3 instead of 4 total stimuli
##### NOTE: No data for participants 7, 9, or 10
##### NOTE: Participant 16 only has data for one piece of music (Prayer); trigger box battery had depleted
##### NOTE: No film data for participant 13, no music data for participant 15

setwd('Directory Here')

## CLEAN MEMORY AND IMPORT PACKAGES/SCRIPTS
rm(list=ls(all=TRUE))
source('config/libraries.R')
source('read_nexus_export.R') # Script to help take physiological data from NeXuS BioTrace wearables and software

sc <- list.files(path = 'data', pattern = '*.txt') ## List of scl files

### First 11 participants; separate because we do not have the fourth stimulus data (Behind the Door)
st <- NULL
for (k in 1:length(sc)){
  tst <- sc[k]
  nme <- unlist(strsplit(tst, split = '_'))
  par <- unlist(strsplit(nme[1], split = 'par'))
  par <- as.numeric(par[2])
  qual<- substr(nme[2], start = 1, stop = 2)
  d1 <- read_nexus_export(tst,32)
  id <- which(d1$Events == 'Main Trigger')
  if (par < 12){
    if (qual == '1A'){
      for (n in 1) {
        model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
        d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
        model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
        d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
        
        t1 <- d1[(id[1]-320):(id[1]+10720),]  
        t2 <- d1[(id[2]-320):(id[2]+9600),]   
        t3 <- d1[(id[3]-320):(id[3]+12096),]
        
        t1 <- t1 %>% select(-TIME)
        temp1 <- t1 %>% select(Sensor.F.Temp.)
        t1 <- t1 %>% select(-Sensor.F.Temp.)
        t2 <- t2 %>% select(-TIME)
        temp2 <- t2 %>% select(Sensor.F.Temp.)
        t2 <- t2 %>% select(-Sensor.F.Temp.)
        t3 <- t3 %>% select(-TIME)
        temp3 <- t3 %>% select(Sensor.F.Temp.)
        t3 <- t3 %>% select(-Sensor.F.Temp.)
        
        t1$Events <- as.character(t1$Events)
        t1[t1$Events == "Main Trigger",]$Events = 2
        t2$Events <- as.character(t2$Events)
        t2[t2$Events == "Main Trigger",]$Events = 2
        t3$Events <- as.character(t3$Events)
        t3[t3$Events == "Main Trigger",]$Events = 2
        
        t1$sec <- t1$sec-min(t1$sec)
        t2$sec <- t2$sec-min(t2$sec)
        t3$sec <- t3$sec-min(t3$sec)
        
        temp1$piece <- rep(c('pines_empathy'))
        temp1$category <- rep(c('music'))
        temp1$condition <- rep(c('empathy'))
        temp2$piece <- rep(c('prayer_structure'))
        temp2$category <- rep(c('music'))
        temp2$condition <- rep(c('structure'))
        temp3$piece <- rep(c('glosoli_empathy'))
        temp3$category <- rep(c('video'))
        temp3$condition <- rep(c('empathy'))
        
        tmp <- rbind(temp1,temp2,temp3)
        tmp$participant <- par
        st <- rbind(st,tmp)
      }
    }
    if (qual == '2A'){
      for (n in 1) {
        model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
        d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
        model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
        d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
        
        t1 <- d1[(id[1]-320):(id[1]+10720),]
        t2 <- d1[(id[2]-320):(id[2]+9600),]
        t3 <- d1[(id[3]-320):(id[3]+12800),]
        
        t1 <- t1 %>% select(-TIME)
        temp1 <- t1 %>% select(Sensor.F.Temp.)
        t1 <- t1 %>% select(-Sensor.F.Temp.)
        
        t2 <- t2 %>% select(-TIME)
        temp2 <- t2 %>% select(Sensor.F.Temp.)
        t2 <- t2 %>% select(-Sensor.F.Temp.)
        
        t3 <- t3 %>% select(-TIME)
        temp3 <- t3 %>% select(Sensor.F.Temp.)
        t3 <- t3 %>% select(-Sensor.F.Temp.)
        
        t1$Events <- as.character(t1$Events)
        t1[t1$Events == "Main Trigger",]$Events = 2
        t2$Events <- as.character(t2$Events)
        t2[t2$Events == "Main Trigger",]$Events = 2
        t3$Events <- as.character(t3$Events)
        t3[t3$Events == "Main Trigger",]$Events = 2
        
        t1$sec <- t1$sec-min(t1$sec)
        t2$sec <- t2$sec-min(t2$sec)
        t3$sec <- t3$sec-min(t3$sec)

        temp1$piece <- rep(c('pines_structure'))
        temp1$category <- rep(c('music'))
        temp1$condition <- rep(c('structure'))
        temp2$piece <- rep(c('prayer_empathy'))
        temp2$category <- rep(c('music'))
        temp2$condition <- rep(c('empathy'))
        temp3$piece <- rep(c('glosoli_structure'))
        temp3$category <- rep(c('video'))
        temp3$condition <- rep(c('structure'))
        
        tmp <- rbind(temp1,temp2,temp3)
        tmp$participant <- par
        st <- rbind(st,tmp)
      }
    }
  }
  if (par > 11){
    if (qual == '1A'){
      if (nme[3] == 'music.txt'){
        for (n in 1) {
          model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
          d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
          model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
          d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
          
          t1 <- d1[(id[1]-320):(id[1]+10720),]
          t2 <- d1[(id[2]-320):(id[2]+9600),]
          
          t1 <- t1 %>% select(-TIME)
          temp1 <- t1 %>% select(Sensor.F.Temp.)
          t1 <- t1 %>% select(-Sensor.F.Temp.)
          
          t2 <- t2 %>% select(-TIME)
          temp2 <- t2 %>% select(Sensor.F.Temp.)
          t2 <- t2 %>% select(-Sensor.F.Temp.)
          
          t1$Events <- as.character(t1$Events)
          t1[t1$Events == "Main Trigger",]$Events = 2
          t2$Events <- as.character(t2$Events)
          t2[t2$Events == "Main Trigger",]$Events = 2
          
          t1$sec <- t1$sec-min(t1$sec)
          t2$sec <- t2$sec-min(t2$sec)

          temp1$piece <- rep(c('pines_empathy'))
          temp1$category <- rep(c('music'))
          temp1$condition <- rep(c('empathy'))
          temp2$piece <- rep(c('prayer_structure'))
          temp2$category <- rep(c('music'))
          temp2$condition <- rep(c('structure'))
          
          tmp <- rbind(temp1,temp2)
          tmp$participant <- par
          st <- rbind(st,tmp)
        }
      }
      if (nme[3] == 'film.txt'){
        for (n in 1) {
          model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
          d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
          model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
          d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
          
          t1 <- d1[(id[1]-320):(id[1]+12096),]
          t2 <- d1[(id[2]-320):(id[2]+4256),]
          
          t1 <- t1 %>% select(-TIME)
          temp1 <- t1 %>% select(Sensor.F.Temp.)
          t1 <- t1 %>% select(-Sensor.F.Temp.)
          
          t2 <- t2 %>% select(-TIME)
          temp2 <- t2 %>% select(Sensor.F.Temp.)
          t2 <- t2 %>% select(-Sensor.F.Temp.)
          
          t1$Events <- as.character(t1$Events)
          t1[t1$Events == "Main Trigger",]$Events = 2
          t2$Events <- as.character(t2$Events)
          t2[t2$Events == "Main Trigger",]$Events = 2
          
          t1$sec <- t1$sec-min(t1$sec)
          t2$sec <- t2$sec-min(t2$sec)

          temp1$piece <- rep(c('glosoli_empathy'))
          temp1$category <- rep(c('video'))
          temp1$condition <- rep(c('empathy'))
          temp2$piece <- rep(c('door_structure'))
          temp2$category <- rep(c('video'))
          temp2$condition <- rep(c('structure'))
          
          tmp <- rbind(temp1,temp2)
          tmp$participant <- par
          st <- rbind(st,tmp)
        }
      }
    }
    if (qual == '1B'){
      if (par == 23){
        model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
        d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
        model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
        d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
        
        t2 <- d1[(id[1]-320):(id[1]+9600),]
        t2 <- t2 %>% select(-TIME)
        temp2 <- t2 %>% select(Sensor.F.Temp.)
        t2 <- t2 %>% select(-Sensor.F.Temp.)
        t2$Events <- as.character(t2$Events)
        t2[t2$Events == "Main Trigger",]$Events = 2
        t2$sec <- t2$sec-min(t2$sec)
        
        temp2$piece <- rep(c('prayer_structure'))
        temp2$category <- rep(c('music'))
        temp2$condition <- rep(c('structure'))
        temp2$participant <- par
        st <- rbind(st,temp2)
      } else {
        if (nme[3] == 'music.txt'){
          for (n in 1) {
            model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
            d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
            model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
            d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
            
            t1 <- d1[(id[2]-320):(id[2]+10720),]
            t2 <- d1[(id[1]-320):(id[1]+9600),]
            
            t1 <- t1 %>% select(-TIME)
            temp1 <- t1 %>% select(Sensor.F.Temp.)
            t1 <- t1 %>% select(-Sensor.F.Temp.)
            
            t2 <- t2 %>% select(-TIME)
            temp2 <- t2 %>% select(Sensor.F.Temp.)
            t2 <- t2 %>% select(-Sensor.F.Temp.)
            
            t1$Events <- as.character(t1$Events)
            t1[t1$Events == "Main Trigger",]$Events = 2
            t2$Events <- as.character(t2$Events)
            t2[t2$Events == "Main Trigger",]$Events = 2
            
            t1$sec <- t1$sec-min(t1$sec)
            t2$sec <- t2$sec-min(t2$sec)

            temp1$piece <- rep(c('pines_empathy'))
            temp1$category <- rep(c('music'))
            temp1$condition <- rep(c('empathy'))
            temp2$piece <- rep(c('prayer_structure'))
            temp2$category <- rep(c('music'))
            temp2$condition <- rep(c('structure'))
            
            tmp <- rbind(temp1,temp2)
            tmp$participant <- par
            st <- rbind(st,tmp)
          }
        }
      }
      if (nme[3] == 'film.txt'){
        for (n in 1) {
          model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
          d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
          model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
          d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
          
          t1 <- d1[(id[2]-320):(id[2]+12096),]
          t2 <- d1[(id[1]-320):(id[1]+4256),]
          
          t1 <- t1 %>% select(-TIME)
          temp1 <- t1 %>% select(Sensor.F.Temp.)
          t1 <- t1 %>% select(-Sensor.F.Temp.)
          
          t2 <- t2 %>% select(-TIME)
          temp2 <- t2 %>% select(Sensor.F.Temp.)
          t2 <- t2 %>% select(-Sensor.F.Temp.)
          
          t1$Events <- as.character(t1$Events)
          t1[t1$Events == "Main Trigger",]$Events = 2
          t2$Events <- as.character(t2$Events)
          t2[t2$Events == "Main Trigger",]$Events = 2
          
          t1$sec <- t1$sec-min(t1$sec)
          t2$sec <- t2$sec-min(t2$sec)

          temp1$piece <- rep(c('glosoli_empathy'))
          temp1$category <- rep(c('video'))
          temp1$condition <- rep(c('empathy'))
          temp2$piece <- rep(c('door_structure'))
          temp2$category <- rep(c('video'))
          temp2$condition <- rep(c('structure'))
          
          tmp <- rbind(temp1,temp2)
          tmp$participant <- par
          st <- rbind(st,tmp)
        }
      }
    }
    if (qual == '2A'){
      if (nme[3] == 'music.txt'){
        for (n in 1) {
          model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
          d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
          model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
          d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
          
          t1 <- d1[(id[1]-320):(id[1]+10720),]
          t2 <- d1[(id[2]-320):(id[2]+9600),]
          
          t1 <- t1 %>% select(-TIME)
          temp1 <- t1 %>% select(Sensor.F.Temp.)
          t1 <- t1 %>% select(-Sensor.F.Temp.)
          
          t2 <- t2 %>% select(-TIME)
          temp2 <- t2 %>% select(Sensor.F.Temp.)
          t2 <- t2 %>% select(-Sensor.F.Temp.)
          
          t1$Events <- as.character(t1$Events)
          t1[t1$Events == "Main Trigger",]$Events = 2
          t2$Events <- as.character(t2$Events)
          t2[t2$Events == "Main Trigger",]$Events = 2
          
          t1$sec <- t1$sec-min(t1$sec)
          t2$sec <- t2$sec-min(t2$sec)

          temp1$piece <- rep(c('pines_structure'))
          temp1$category <- rep(c('music'))
          temp1$condition <- rep(c('structure'))
          temp2$piece <- rep(c('prayer_empathy'))
          temp2$category <- rep(c('music'))
          temp2$condition <- rep(c('empathy'))
          
          tmp <- rbind(temp1,temp2)
          tmp$participant <- par
          st <- rbind(st,tmp)
        }
      }
      if (nme[3] == 'film.txt'){
        for (n in 1) {
          model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
          d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
          model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
          d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
          
          t1 <- d1[(id[1]-320):(id[1]+12800),]
          t2 <- d1[(id[2]-320):(id[2]+4000),]
          
          t1 <- t1 %>% select(-TIME)
          temp1 <- t1 %>% select(Sensor.F.Temp.)
          t1 <- t1 %>% select(-Sensor.F.Temp.)
          
          t2 <- t2 %>% select(-TIME)
          temp2 <- t2 %>% select(Sensor.F.Temp.)
          t2 <- t2 %>% select(-Sensor.F.Temp.)
          
          t1$Events <- as.character(t1$Events)
          t1[t1$Events == "Main Trigger",]$Events = 2
          t2$Events <- as.character(t2$Events)
          t2[t2$Events == "Main Trigger",]$Events = 2
          
          t1$sec <- t1$sec-min(t1$sec)
          t2$sec <- t2$sec-min(t2$sec)

          temp1$piece <- rep(c('glosoli_structure'))
          temp1$category <- rep(c('video'))
          temp1$condition <- rep(c('structure'))
          temp2$piece <- rep(c('door_empathy'))
          temp2$category <- rep(c('video'))
          temp2$condition <- rep(c('empathy'))
          
          tmp <- rbind(temp1,temp2)
          tmp$participant <- par
          st <- rbind(st,tmp)
        }
      }
    }
    if (qual == '2B'){
      if (par == 16){
        model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
        d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
        model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
        d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
        
        t2 <- d1[(id[1]-320):(id[1]+9600),]
        
        t2 <- t2 %>% select(-TIME)
        temp2 <- t2 %>% select(Sensor.F.Temp.)
        t2 <- t2 %>% select(-Sensor.F.Temp.)
        
        t2$Events <- as.character(t2$Events)
        t2[t2$Events == "Main Trigger",]$Events = 2
        t2$sec <- t2$sec-min(t2$sec)
        
        temp2$piece <- rep(c('prayer_empathy'))
        temp2$category <- rep(c('music'))
        temp2$condition <- rep(c('empathy'))
        temp2$participant <- par
        st <- rbind(st,temp2)
      } else {
        if (nme[3] == 'music.txt'){
          for (n in 1) {
            model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
            d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
            model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
            d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
            
            t1 <- d1[(id[2]-320):(id[2]+10720),]
            t2 <- d1[(id[1]-320):(id[1]+9600),]
            
            t1 <- t1 %>% select(-TIME)
            temp1 <- t1 %>% select(Sensor.F.Temp.)
            t1 <- t1 %>% select(-Sensor.F.Temp.)
            
            t2 <- t2 %>% select(-TIME)
            temp2 <- t2 %>% select(Sensor.F.Temp.)
            t2 <- t2 %>% select(-Sensor.F.Temp.)
            
            t1$Events <- as.character(t1$Events)
            t1[t1$Events == "Main Trigger",]$Events = 2
            t2$Events <- as.character(t2$Events)
            t2[t2$Events == "Main Trigger",]$Events = 2
            
            t1$sec <- t1$sec-min(t1$sec)
            t2$sec <- t2$sec-min(t2$sec)

            temp1$piece <- rep(c('pines_structure'))
            temp1$category <- rep(c('music'))
            temp1$condition <- rep(c('structure'))
            temp2$piece <- rep(c('prayer_empathy'))
            temp2$category <- rep(c('music'))
            temp2$condition <- rep(c('empathy'))
            
            tmp <- rbind(temp1,temp2)
            tmp$participant <- par
            st <- rbind(st,tmp)
          }
        }
      }
      if (nme[3] == 'film.txt'){
        for (n in 1) {
          model <- lm(d1$Sensor.E.SC.GSR~I(1:length(d1$Sensor.E.SC.GSR)))
          d1$Sensor.E.SC.GSR <- d1$Sensor.E.SC.GSR - predict(model)
          model <- lm(d1$Sensor.F.Temp.~I(1:length(d1$Sensor.F.Temp.)))
          d1$Sensor.F.Temp. <- d1$Sensor.F.Temp. - predict(model)
          
          t1 <- d1[(id[2]-320):(id[2]+12800),]
          t2 <- d1[(id[1]-320):(id[1]+4000),]
          
          t1 <- t1 %>% select(-TIME)
          temp1 <- t1 %>% select(Sensor.F.Temp.)
          t1 <- t1 %>% select(-Sensor.F.Temp.)
          
          t2 <- t2 %>% select(-TIME)
          temp2 <- t2 %>% select(Sensor.F.Temp.)
          t2 <- t2 %>% select(-Sensor.F.Temp.)
          t1$Events <- as.character(t1$Events)
          
          t1[t1$Events == "Main Trigger",]$Events = 2
          t2$Events <- as.character(t2$Events)
          t2[t2$Events == "Main Trigger",]$Events = 2
          
          t1$sec <- t1$sec-min(t1$sec)
          t2$sec <- t2$sec-min(t2$sec)

          temp1$piece <- rep(c('glosoli_structure'))
          temp1$category <- rep(c('video'))
          temp1$condition <- rep(c('structure'))
          temp2$piece <- rep(c('door_empathy'))
          temp2$category <- rep(c('video'))
          temp2$condition <- rep(c('empathy'))
          tmp <- rbind(temp1,temp2)
          tmp$participant <- par
          st <- rbind(st,tmp)
        }
      }
    }
  }
}

rm(d1,model,t1,t2,t3,temp1,temp2,temp3,tmp)
