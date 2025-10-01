
dire <- list.files(path = 'matlab/tonic_export', pattern = '*.csv')
dire2 <- list.files(path = 'matlab/phasic_baseline', pattern = '*.csv')

### Bring together phasic driver and skin temperature data to same dataframe (needs check, one warning message...)

bin <- NULL   # This will be long dataframe, with st + scr time-series for each participant
for (k in 1:8) {
  tst <- dire[k]
  tmp <- read.csv(paste('matlab/tonic_export','/',dire[k],sep = ""),col.names = c('participant','scr'), fill = T, header = F)
  phas <-read.csv(paste('matlab/phasic_baseline','/',dire2[k],sep = ""),col.names = c('participant','scr'), fill = T, header = F)
  l <- unique(st$participant)
  tst <- unlist(strsplit(tst, split = '_'))
  tst <- paste(tst[1],tst[2])
  if (tst == 'door emp'){
    for (x in 1:length(l)) {
      par <- tmp[tmp$participant == l[x],]
      par3 <- phas[phas$participant == l[x],]
      par2 <- st[st$participant == l[x],]
      par2 <- par2[par2$piece == 'door_empathy',]
      if (nrow(par) == 0){
        if (nrow(par2) == 0){
          next
        } else {
          z <- nrow(par2)
          par <- as.data.frame(rep(c(NA), times = z)) ## Populate scr with NA, no participant, but we do have ST, dont lose it
          par3 <- as.data.frame(rep(c(NA), times = z))
          tmp2 <- cbind(participant = par2$participant, scr = par$`rep(c(NA), times = z)`,st = par2$Sensor.F.Temp., phasic = par3$`rep(c(NA), times = z)`, piece = par2$piece)
          bin <- rbind(bin,tmp2)
        }
      } else {
        tmp2 <- cbind(par,st = par2$Sensor.F.Temp., phasic = par3$scr, piece = par2$piece)
        bin <- rbind(bin,tmp2)
      }
    }
  }
  if (tst == 'door struc'){
    for (x in 1:length(l)) {
      par <- tmp[tmp$participant == l[x],]
      par3 <- phas[phas$participant == l[x],]
      par2 <- st[st$participant == l[x],]
      par2 <- par2[par2$piece == 'door_structure',]
      if (nrow(par) == 0){
        if (nrow(par2) == 0){
          next
        } else {
          z <- nrow(par2)
          par <- as.data.frame(rep(c(NA), times = z)) ## Populate scr with NA, no participant, but we do have ST, dont lose it
          par3 <- as.data.frame(rep(c(NA), times = z))
          tmp2 <- cbind(participant = par2$participant, scr = par$`rep(c(NA), times = z)`,st = par2$Sensor.F.Temp., 
                        phasic = par3$`rep(c(NA), times = z)`, piece = par2$piece)
          bin <- rbind(bin,tmp2)
        }
      } else {
        tmp2 <- cbind(par,st = par2$Sensor.F.Temp., phasic = par3$scr, piece = par2$piece)
        bin <- rbind(bin,tmp2)
      }
    }
  }  
  if (tst == 'glos emp'){
    for (x in 1:length(l)) {
      par <- tmp[tmp$participant == l[x],]
      par3 <- phas[phas$participant == l[x],]
      par2 <- st[st$participant == l[x],]
      par2 <- par2[par2$piece == 'glosoli_empathy',]
      if (nrow(par) == 0){
        if (nrow(par2) == 0){
          next
        } else {
          z <- nrow(par2)
          par <- as.data.frame(rep(c(NA), times = z)) ## Populate scr with NA, no participant, but we do have ST, dont lose it
          par3 <- as.data.frame(rep(c(NA), times = z))
          tmp2 <- cbind(participant = par2$participant, scr = par$`rep(c(NA), times = z)`,st = par2$Sensor.F.Temp., 
                        phasic = par3$`rep(c(NA), times = z)`, piece = par2$piece)
          bin <- rbind(bin,tmp2)
        }
      } else {
        tmp2 <- cbind(par,st = par2$Sensor.F.Temp., phasic = par3$scr, piece = par2$piece)
        bin <- rbind(bin,tmp2)
      }
    }
  }  
  if (tst == 'glos struc'){
    for (x in 1:length(l)) {
      par <- tmp[tmp$participant == l[x],]
      par3 <- phas[phas$participant == l[x],]
      par2 <- st[st$participant == l[x],]
      par2 <- par2[par2$piece == 'glosoli_structure',]
      if (nrow(par) == 0){
        if (nrow(par2) == 0){
          next
        } else {
          z <- nrow(par2)
          par <- as.data.frame(rep(c(NA), times = z)) ## Populate scr with NA, no participant, but we do have ST, dont lose it
          par3 <- as.data.frame(rep(c(NA), times = z))
          tmp2 <- cbind(participant = par2$participant, scr = par$`rep(c(NA), times = z)`,st = par2$Sensor.F.Temp., 
                        phasic = par3$`rep(c(NA), times = z)`, piece = par2$piece)
          bin <- rbind(bin,tmp2)
        }
      } else {
        tmp2 <- cbind(par,st = par2$Sensor.F.Temp., phasic = par3$scr, piece = par2$piece)
        bin <- rbind(bin,tmp2)
      }
    }
  }  
  if (tst == 'pines emp'){
    for (x in 1:length(l)) {
      par <- tmp[tmp$participant == l[x],]
      par3 <- phas[phas$participant == l[x],]
      par2 <- st[st$participant == l[x],]
      par2 <- par2[par2$piece == 'pines_empathy',]
      if (nrow(par) == 0){
        if (nrow(par2) == 0){
          next
        } else {
          z <- nrow(par2)
          par <- as.data.frame(rep(c(NA), times = z)) ## Populate scr with NA, no participant, but we do have ST, dont lose it
          par3 <- as.data.frame(rep(c(NA), times = z))
          tmp2 <- cbind(participant = par2$participant, scr = par$`rep(c(NA), times = z)`,st = par2$Sensor.F.Temp., 
                        phasic = par3$`rep(c(NA), times = z)`, piece = par2$piece)
          bin <- rbind(bin,tmp2)
        }
      } else {
        tmp2 <- cbind(par,st = par2$Sensor.F.Temp., phasic = par3$scr, piece = par2$piece)
        bin <- rbind(bin,tmp2)
      }
    }
  }  
  if (tst == 'pines struc'){
    for (x in 1:length(l)) {
      par <- tmp[tmp$participant == l[x],]
      par3 <- phas[phas$participant == l[x],]
      par2 <- st[st$participant == l[x],]
      par2 <- par2[par2$piece == 'pines_structure',]
      if (nrow(par) == 0){
        if (nrow(par2) == 0){
          next
        } else {
          z <- nrow(par2)
          par <- as.data.frame(rep(c(NA), times = z)) ## Populate scr with NA, no participant, but we do have ST, dont lose it
          par3 <- as.data.frame(rep(c(NA), times = z))
          tmp2 <- cbind(participant = par2$participant, scr = par$`rep(c(NA), times = z)`,st = par2$Sensor.F.Temp., 
                        phasic = par3$`rep(c(NA), times = z)`, piece = par2$piece)
          bin <- rbind(bin,tmp2)
        }
      } else {
        tmp2 <- cbind(par,st = par2$Sensor.F.Temp., phasic = par3$scr, piece = par2$piece)
        bin <- rbind(bin,tmp2)
      }
    }
  }  
  if (tst == 'prayer emp'){
    for (x in 1:length(l)) {
      par <- tmp[tmp$participant == l[x],]
      par3 <- phas[phas$participant == l[x],]
      par2 <- st[st$participant == l[x],]
      par2 <- par2[par2$piece == 'prayer_empathy',]
      if (nrow(par) == 0){
        if (nrow(par2) == 0){
          next
        } else {
          z <- nrow(par2)
          par <- as.data.frame(rep(c(NA), times = z)) ## Populate scr with NA, no participant, but we do have ST, dont lose it
          par3 <- as.data.frame(rep(c(NA), times = z))
          tmp2 <- cbind(participant = par2$participant, scr = par$`rep(c(NA), times = z)`,st = par2$Sensor.F.Temp., 
                        phasic = par3$`rep(c(NA), times = z)`, piece = par2$piece)
          bin <- rbind(bin,tmp2)
        }
      } else {
        tmp2 <- cbind(par,st = par2$Sensor.F.Temp., phasic = par3$scr, piece = par2$piece)
        bin <- rbind(bin,tmp2)
      }
    }
  } 
  if (tst == 'prayer struc'){
    for (x in 1:length(l)) {
      par <- tmp[tmp$participant == l[x],]
      par3 <- phas[phas$participant == l[x],]
      par2 <- st[st$participant == l[x],]
      par2 <- par2[par2$piece == 'prayer_structure',]
      if (nrow(par) == 0){
        if (nrow(par2) == 0){
          next
        } else {
          z <- nrow(par2)
          par <- as.data.frame(rep(c(NA), times = z)) ## Populate scr with NA, no participant, but we do have ST, dont lose it
          par3 <- as.data.frame(rep(c(NA), times = z))
          tmp2 <- cbind(participant = par2$participant, scr = par$`rep(c(NA), times = z)`,st = par2$Sensor.F.Temp., 
                        phasic = par3$`rep(c(NA), times = z)`, piece = par2$piece)
          bin <- rbind(bin,tmp2)
        }
      } else {
        tmp2 <- cbind(par,st = par2$Sensor.F.Temp., phasic = par3$scr, piece = par2$piece)
        bin <- rbind(bin,tmp2)
      }
    }
  }
}

########################################################################################################################
##### Take button press data to physiology data (glosoli structure some removed, due to past end point)
########################################################################################################################

comp <- NULL
but <- NULL
series <- NULL
l <- unique(tme$participant)
bin$scr <- as.numeric(as.character(bin$scr))
bin$st <- as.numeric(as.character(bin$st))
bin$phasic <- as.numeric(as.character(bin$phasic))
for (k in 1:length(l)) {
  cat(k, '\n')
  tmp <- tme[tme$participant == l[k],]  ## button press data by participant
  tmp3 <- bin[bin$participant == l[k],] ## physiological data by participant
  idx <- tot2[tot2$participant == l[k],]
  if (nrow(tmp3) == 0){ ## If there is no physiological data...
    cat('next','\n')
    next
  }
  nme <- as.character(unique(tmp$v1))
  tmp3$scr <- scale(tmp3$scr) ### normalise scores within each participant
  tmp3$st <- scale(tmp3$st)
  tmp3$phasic <- scale(tmp3$phasic)
  for (x in 1:length(nme)) {
    cat('2nd loop','\n')
    tmp2 <- tmp[tmp$v1 == nme[x],]
    tmp4 <- tmp3[tmp3$piece == nme[x],]
    idx2 <- idx[idx$condition == nme[x],]
    if (nrow(idx2) == 0){
      idx3 <- NA
    } else {
      idx3 <- idx2$type
    }
    if (nrow(tmp4) == 0){
      next
    }
    prs <- as.numeric(unique(tmp2$v2))
    prs <- as.data.frame(prs)

    if (nrow(prs) < 3){ ## This basically removes really low response rates as well...
      next
    }
    for (z in 1:length(prs)) {
      prs <- as.vector(prs$prs)
      press <- prs[z]
      tot <- tmp2[tmp2$v2 == prs[z],]
      mark <- press * 32
      mark2 <- mark + 128
      mark3 <- mark - 128
      val <- nrow(tmp4) / 32
      val2 <- mark2 / 32
      if (val2 > val){ ## This checks whether the button press is too late in the piece to get physiology means
        next
      }
      if (mark3 < 1){  ## This checks whether pre-chill epoch goes before the start of music
        next
      }
      rng2 <- tmp4[mark3:mark,]
      max2 <- max(rng2$phasic)
      mark3 <- tmp4[1:320,]
      mark3 <- mark3[sample(nrow(mark3),129),]  ## take random 4 sec from the 10 sec epoch before stimulus onset
      rng <- tmp4[mark:mark2,]
      mark4 <- mark - 128
      if (mark4 > 0){
        rng3 <- tmp4[mark4:mark2,]
        rng3$type <- idx3
        series <- rbind(series,rng3)
      }
      max1 <- max(rng$phasic)
      max3 <- max(mark3$phasic)
      rng$tonic <- rng$scr - rng2$scr ## this is localised change in signal
      rng$temp <- rng$st - rng2$st
      rng$tonic_base <- rng$scr - mark3$scr ## this is change from baseline in signal
      rng$temp_base <- rng$st - mark3$st

      rng$phasic <- rng$phasic - rng2$phasic  ## This just follows average values for phasic!
      rng$phasic_base <- rng$phasic - mark3$phasic
      rng$type <- idx3
      tot$tonic <- mean(rng$tonic, na.rm = T)
      tot$temp <- mean(rng$temp, na.rm = T)
      tot$tonic_base <- mean(rng$tonic_base, na.rm = T)
      tot$temp_base <- mean(rng$temp_base, na.rm = T)
      tot$phasic <- mean(rng$phasic, na.rm = T)
      tot$phasic_base <- mean(rng$phasic_base, na.rm = T)
      comp <- rbind(comp,rng)
      but <- rbind(but,tot)
    }
  }
}

##### add SCR and ST averages to overall dataframe

df <- NULL  ## This will be main full dataframe, including self-reports and mean physiology for button presses
z <- length(unique(tot2$participant))
for (k in 1:z) {
  tmp <-  comp[comp$participant == k,]
  tmp2 <- tot2[tot2$participant == k,]
  if (nrow(tmp) == 0){
    tmp2$tonic <- NA
    tmp2$temp <- NA
    tmp2$tonic_base <- NA
    tmp2$temp_base <- NA
    tmp2$phasic <- NA
    tmp2$phasic_base <- NA
    df <- rbind(df,tmp2)
    next
  }
  nme <- unique(tmp2$condition)
  for (x in 1:length(nme)) {
    tmp3 <- tmp[tmp$piece == nme[x],]
    if (nrow(tmp3) == 0){
      tmp4 <- tmp2[tmp2$condition == nme[x],]
      tmp4$tonic <- NA
      tmp4$temp <- NA
      tmp4$tonic_base <- NA
      tmp4$temp_base <- NA
      tmp4$phasic <- NA
      tmp4$phasic_base <- NA
      df <- rbind(df,tmp4)
      next
    }
    tmp4 <- tmp2[tmp2$condition == nme[x],]
    tmp4$tonic <- mean(tmp3$tonic, na.rm = T)
    tmp4$temp <- mean(tmp3$temp, na.rm = T)
    tmp4$tonic_base <- mean(tmp3$tonic_base, na.rm = T)
    tmp4$temp_base <- mean(tmp3$temp_base, na.rm = T)
    tmp4$phasic <- mean(tmp3$phasic, na.rm = T)
    tmp4$phasic_base <- mean(tmp3$phasic_base, na.rm = T)
    df <- rbind(df,tmp4)
  }
}

### Statistical test of full 4 sec epoch phasic and temp changes from baseline

comp2 <- na.omit(comp)

## Check data before model construction
hist(comp2$tonic) ## positive skew, non-normal; consider transform
comp2$tonicL <- log10(comp2$tonic + 0.45)
hist(comp2$tonicL)  ## Much more normal, but kurtosis is high!
hist(comp2$temp) ## not far off normal, but high kurtosis; consider transform
comp2$tempL <- log10(comp2$temp + 0.8)
hist(comp2$tempL) ## Little better normality, but kurtosis is high
hist(comp2$phasic) ## positive skew, high kurtosis; consider transform; transform does not work so well..

hist(comp2$tonic_base) ## Normal distribution
hist(comp2$temp_base) ## Normal distribution
hist(comp2$phasic_base) ## positive skew

boxplot(tonicL ~ participant, comp2)

s <- summarise(group_by(comp2,type), M = mean(tonic, na.rm = T), SE=sd(tonic,na.rm = T)/sqrt(length(dim(comp2)[1])))
s2 <- summarise(group_by(df,type), M = mean(tonic, na.rm = T), SE=sd(tonic,na.rm = T)/sqrt(length(dim(df)[1])))

## Visualise normalised scores?
comp3 <- comp2
comp3$tonic <- scale(comp3$tonic)
comp3$temp <- scale(comp3$temp)
comp3$tonic_base <- scale(comp3$tonic_base)
comp3$temp_base <- scale(comp3$temp_base)
comp3$phasic <- scale(comp3$phasic)
comp3$phasic_base <- scale(comp3$phasic_base)

m <- melt(comp3, id.vars = c('participant','piece','type'), measure.vars = c('tonic','phasic','temp'))
s <- summarise(group_by(m,type,variable), M = mean(value, na.rm = T), SE=sd(value,na.rm = T)/sqrt(length(dim(m)[1])))
g1 <- ggplot(s, aes(x = type, y = M)) +
  geom_bar(aes(fill = type), stat = "identity", position = 'dodge', colour = 'black') +
  facet_grid(.~variable) +
  theme_bw()
g1

m <- melt(comp3, id.vars = c('participant','piece','type'), measure.vars = c('temp'))
s <- summarise(group_by(m,type,participant,piece,variable), M = mean(value, na.rm = T))

## Choose analysis
## For tonic and temp (phasic requires reshape before test, since we extract only 1 value from each epoch)

tst <- lmer(temp ~ type + (1|participant) + (1|piece), comp3, control = lmerControl(optimizer = 'bobyqa'), REML = T)
reduc <- lmer(temp ~ 1 + (1|participant) + (1|piece), comp3, control = lmerControl(optimizer = 'bobyqa'), REML = T)
anova(tst,reduc)
anova(tst)
table(comp3$type)
contr <- c(-1, 0, 1)
summary(glht(tst, linfct = mcp(type = contr)), test = adjusted('holm'))
summary(glht(tst, linfct = mcp(type = 'Tukey')))

## Standardised beta co-efficients for the model? Assess magnitude of effect, significance appears to carry little power?
MuMIn::r.squaredGLMM(tst, partial.sd = T)

############################################################################
#### Time-series visualisation
############################################################################

tmp <- series[series$type == 'awe',]
tonic <- NULL
phasic <- NULL
temp <- NULL
for (p in 1) {
  for (k in 1:length(unique(tmp$participant))) {
    idx <- unique(tmp$participant)
    par <- tmp[tmp$participant == idx[k],]
    numb <- (nrow(par) / 257)
    for (m in 1:numb) {
      tmp2 <- par[1:257,]
      par <- par[-c(1:257),]
      tonic <- as.data.frame(cbind(tonic,tmp2$scr))
      phasic <- as.data.frame(cbind(phasic,tmp2$phasic))
      temp <- as.data.frame(cbind(temp,tmp2$st))
    }
  }
  tonic$M <- rowMeans(tonic, na.rm = T)
  phasic$M <- rowMeans(phasic, na.rm = T)
  temp$M <- rowMeans(temp, na.rm = T)
  tonic$type <- rep(c('Awe'))
  phasic$type <- rep(c('Awe'))
  temp$type <- rep(c('Awe'))
}

tmp <- series[series$type == 'balanced',]
tonic3 <- NULL
phasic3 <- NULL
temp3 <- NULL
for (p in 1) {
  for (k in 1:length(unique(tmp$participant))) {
    idx <- unique(tmp$participant)
    par <- tmp[tmp$participant == idx[k],]
    numb <- (nrow(par) / 257)
    for (m in 1:numb) {
      tmp2 <- par[1:257,]
      par <- par[-c(1:257),]
      tonic3 <- as.data.frame(cbind(tonic3,tmp2$scr))
      phasic3 <- as.data.frame(cbind(phasic3,tmp2$phasic))
      temp3 <- as.data.frame(cbind(temp3,tmp2$st))
    }
  }
  tonic3$M <- rowMeans(tonic3, na.rm = T)
  phasic3$M <- rowMeans(phasic3, na.rm = T)
  temp3$M <- rowMeans(temp3, na.rm = T)
  tonic3$type <- rep(c('Balanced'))
  phasic3$type <- rep(c('Balanced'))
  temp3$type <- rep(c('Balanced'))
}

tmp <- series[series$type == 'kama muta',]
tonic2 <- NULL
phasic2 <- NULL
temp2 <- NULL
for (p in 1) {
  for (k in 1:length(unique(tmp$participant))) {
    idx <- unique(tmp$participant)
    par <- tmp[tmp$participant == idx[k],]
    numb <- (nrow(par) / 257)
    for (m in 1:numb) {
      tmp2 <- par[1:257,]
      par <- par[-c(1:257),]
      tonic2 <- as.data.frame(cbind(tonic2,tmp2$scr))
      phasic2 <- as.data.frame(cbind(phasic2,tmp2$phasic))
      temp2 <- as.data.frame(cbind(temp2,tmp2$st))
    }
  }
  tonic2$M <- rowMeans(tonic2, na.rm = T)
  phasic2$M <- rowMeans(phasic2, na.rm = T)
  temp2$M <- rowMeans(temp2, na.rm = T)
  tonic2$type <- rep(c('Being Moved'))
  phasic2$type <- rep(c('Being Moved'))
  temp2$type <- rep(c('Being Moved'))
}

tmp <- as.matrix(tonic[,1:28])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
tonic <- as.data.frame(cbind(M = tonic$M, SE = stan, type = tonic$type))
tmp <- as.matrix(tonic2[,1:24])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
tonic2 <- as.data.frame(cbind(M = tonic2$M, SE = stan, type = tonic2$type))
tmp <- as.matrix(tonic3[,1:24])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
tonic3 <- as.data.frame(cbind(M = tonic3$M, SE = stan, type = tonic3$type))

tonic$time <- seq(0,(nrow(tonic)-1)/32,by=1/32)
tonic2$time <- seq(0,(nrow(tonic)-1)/32,by=1/32)
tonic3$time <- seq(0,(nrow(tonic)-1)/32,by=1/32)
tonic$M <- as.numeric(as.character(tonic$M))
tonic$M <- tonic$M - 0.0386053228175457   ## Originate physiology at point zero for visualisation!
tonic2$M <- as.numeric(as.character(tonic2$M))
tonic2$M <- tonic2$M + 0.520505958486398
tonic3$M <- as.numeric(as.character(tonic3$M))
tonic3$M <- tonic3$M + 0.2086082
tonic <- rbind(tonic,tonic2,tonic3)
tonic$time <- tonic$time - 4
tonic$SE <- as.numeric(as.character(tonic$SE))

tmp <- as.matrix(phasic[,1:28])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
phasic <- as.data.frame(cbind(M = phasic$M, SE = stan, type = phasic$type))
tmp <- as.matrix(phasic2[,1:24])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
phasic2 <- as.data.frame(cbind(M = phasic2$M, SE = stan, type = phasic2$type))
tmp <- as.matrix(phasic3[,1:23])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
phasic3 <- as.data.frame(cbind(M = phasic3$M, SE = stan, type = phasic3$type))

phasic$time <- seq(0,(nrow(phasic)-1)/32,by=1/32)
phasic2$time <- seq(0,(nrow(phasic)-1)/32,by=1/32)
phasic3$time <- seq(0,(nrow(phasic)-1)/32,by=1/32)
phasic$M <- as.numeric(as.character(phasic$M))
phasic$M <- phasic$M - 0.2217274997   ## Originate physiology at point zero for visualisation!
phasic2$M <- as.numeric(as.character(phasic2$M))
phasic2$M <- phasic2$M - 0.3111246114
phasic3$M <- as.numeric(as.character(phasic3$M))
phasic3$M <- phasic3$M - 0.25261435

phasic <- rbind(phasic,phasic2,phasic3)
phasic$time <- phasic$time - 4
phasic$SE <- as.numeric(as.character(phasic$SE))

tmp <- as.matrix(temp[,1:28])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
temp <- as.data.frame(cbind(M = temp$M, SE = stan, type = temp$type))
tmp <- as.matrix(temp2[,1:24])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
temp2 <- as.data.frame(cbind(M = temp2$M, SE = stan, type = temp2$type))
tmp <- as.matrix(temp3[,1:24])
stan <- matrixStats::rowSds(tmp, na.rm = T)/sqrt(length(tmp))
temp3 <- as.data.frame(cbind(M = temp3$M, SE = stan, type = temp3$type))

temp$time <- seq(0,(nrow(temp)-1)/32,by=1/32)
temp2$time <- seq(0,(nrow(temp)-1)/32,by=1/32)
temp3$time <- seq(0,(nrow(temp)-1)/32,by=1/32)
temp$M <- as.numeric(as.character(temp$M))
temp$M <- temp$M - 0.3457096   ## Originate physiology at point zero for visualisation!
temp2$M <- as.numeric(as.character(temp2$M))
temp2$M <- temp2$M - 0.2759438
temp3$M <- as.numeric(as.character(temp3$M))
temp3$M <- temp3$M - 0.08420715

temp <- rbind(temp,temp2,temp3)
temp$time <- temp$time - 4
temp$SE <- as.numeric(as.character(temp$SE))

ggplot(tonic, aes(x = time, y = M, color = type)) +
  geom_line(size = 2, stat = 'identity') +
  geom_ribbon(aes(ymin = M - SE, ymax = M + SE, color = NULL, fill = type), alpha = 0.2) +
  labs(x = 'Time Relative to Chills (s)', y = 'Tonic SCL (+- SE)', color = 'Chills Type') +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  scale_color_manual(labels = c('Awe','Being Moved','Balanced'), values = c('royalblue1','firebrick1','goldenrod2')) +
  scale_fill_manual(labels = c('Awe','Being Moved','Balanced'), values = c('royalblue1','firebrick1','goldenrod2')) +
  theme_bw() +
  theme(legend.title = element_text(face = 'bold',size = 20), axis.title.x = element_text(face = 'bold',size = 20), axis.title.y = element_text(face = 'bold',size = 20), legend.text = element_text(face = 'bold'), axis.text.x = element_text(face = 'bold', size = 18),axis.text.y = element_text(face = 'bold', size = 18), title = element_text(face = 'bold', size = 20), legend.position = 'bottom', legend.background = element_rect(fill = 'grey80'))

ggplot(phasic, aes(x = time, y = M, color = type)) +
  geom_line(size = 2, stat = 'identity') +
  geom_ribbon(aes(ymin = M - SE, ymax = M + SE, color = NULL, fill = type), alpha = 0.2) +
  labs(x = 'Time Relative to Chills (s)', y = 'Phasic SCR (+- SE)', color = 'Chills Type') +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  scale_color_manual(labels = c('Awe','Being Moved','Balanced'), values = c('royalblue1','firebrick1','goldenrod2')) +
  scale_fill_manual(labels = c('Awe','Being Moved','Balanced'), values = c('royalblue1','firebrick1','goldenrod2')) +
  theme_bw() +
  theme(legend.title = element_text(face = 'bold',size = 20), axis.title.x = element_text(face = 'bold',size = 20), axis.title.y = element_text(face = 'bold',size = 20), legend.text = element_text(face = 'bold'), axis.text.x = element_text(face = 'bold', size = 18),axis.text.y = element_text(face = 'bold', size = 18), title = element_text(face = 'bold', size = 20), legend.position = 'bottom', legend.background = element_rect(fill = 'grey80'))

ggplot(temp, aes(x = time, y = M, color = type)) +
  geom_line(size = 2, stat = 'identity') +
  geom_ribbon(aes(ymin = M - SE, ymax = M + SE, color = NULL, fill = type), alpha = 0.2) +
  labs(x = 'Time Relative to Chills (s)', y = 'Skin Temperature (+- SE)', color = 'Chills Type') +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', size = 1) +
  scale_color_manual(labels = c('Awe','Being Moved','Balanced'), values = c('royalblue1','firebrick1','goldenrod2')) +
  scale_fill_manual(labels = c('Awe','Being Moved','Balanced'), values = c('royalblue1','firebrick1','goldenrod2')) +
  theme_bw() +
  theme(legend.title = element_text(face = 'bold',size = 20), axis.title.x = element_text(face = 'bold',size = 20), axis.title.y = element_text(face = 'bold',size = 20), legend.text = element_text(face = 'bold'), axis.text.x = element_text(face = 'bold', size = 18),axis.text.y = element_text(face = 'bold', size = 18), title = element_text(face = 'bold', size = 20), legend.position = 'bottom', legend.background = element_rect(fill = 'grey80'))

