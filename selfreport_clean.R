##### opensesame_munge
##### Scott Bannister, 28/03/19
##### LOAD IN SELF-REPORTS FROM OPENSESAME
##### MUNGE SELF-REPORT DATA

dire <- list.files(path = 'data/selfreports', pattern = '*.csv')  # list of all self-report files (current N = 34)
emp <- read.csv('data/eqsq2.csv')  # EQ and SQ scores imported through separate csv file (row by participant)

##### First, sort out the empathising-systemising data (scoring, arranging)

################################################################################
##### Empathising Quotient (two loops, for positive-negative scoring method)
################################################################################

eq <- c(1,6,19,22,25,26,35,36,37,38,41,42,43,44,52,54,55,57,58,59,60) # Choose all non-distractor questions
for (k in 1:length(eq)) {
  nme <- paste('EQ_',eq[k], sep = '')
  emp[nme][emp[nme] == 1] <- 10 ## This is an extra step, to make sure we dont change values to 2, then change 2s to 1...
  emp[nme][emp[nme] == 2] <- 20
  emp[nme][emp[nme] == 3] <- 0
  emp[nme][emp[nme] == 4] <- 0
  emp[nme][emp[nme] == 10] <- 2
  emp[nme][emp[nme] == 20] <- 1
}
eq <- c(4,8,10,11,12,14,15,18,21,27,28,29,32,34,39,46,48,49,50) # Separate loop for different scoring procedure
for (k in 1:length(eq)) {
  nme <- paste('EQ_',eq[k], sep = '')
  emp[nme][emp[nme] == 1] <- 0
  emp[nme][emp[nme] == 2] <- 0
  emp[nme][emp[nme] == 3] <- 10
  emp[nme][emp[nme] == 4] <- 20
  emp[nme][emp[nme] == 10] <- 1
  emp[nme][emp[nme] == 20] <- 2
}
eq <- c(2,3,5,7,9,13,16,17,20,23,24,30,31,33,40,45,47,51,53,56) # Remove distractor questions
for (k in 1:length(eq)) {
  nme <- paste('EQ_',eq[k], sep = '')
  emp[nme] <- NULL
}

##### Systemising Quotient (two main loops for different scoring)

sq <- c(1,2,4,5,7,9,11,12,13,14,16,18,19,20,21,23,25,27,29,30,32,36,38,41,42,43,46,50,53,55,60,61,62,66,68,69,72,74,75)
for (k in 1:length(sq)) {
  nme <- paste('SQ_',sq[k], sep = '')
  emp[nme][emp[nme] == 1] <- 10 ## This is an extra step, to make sure we dont change values to 2, then change 2s to 1...
  emp[nme][emp[nme] == 2] <- 20
  emp[nme][emp[nme] == 3] <- 0
  emp[nme][emp[nme] == 4] <- 0
  emp[nme][emp[nme] == 10] <- 2
  emp[nme][emp[nme] == 20] <- 1
}

sq <- c(3,6,8,10,15,17,22,24,26,28,31,33,34,35,37,39,40,44,45,47,48,49,51,52,54,56,57,58,59,63,64,65,67,70,71,73)
for (k in 1:length(sq)) {
  nme <- paste('SQ_',sq[k], sep = '')
  emp[nme][emp[nme] == 1] <- 0 ## This is an extra step, to make sure we dont change values to 2, then change 2s to 1...
  emp[nme][emp[nme] == 2] <- 0
  emp[nme][emp[nme] == 3] <- 10
  emp[nme][emp[nme] == 4] <- 20
  emp[nme][emp[nme] == 10] <- 1
  emp[nme][emp[nme] == 20] <- 2
}

## Get total scores; note that size of instruments is different (i.e. totals are not comparable); likely worth normalising
emp$eq <- rowSums(emp[2:41], na.rm = T)
emp$sq <- rowSums(emp[42:116], na.rm = T)
emp <- emp[,-c(1:116)]

#######################################
## Munge self-report experiment data
#######################################

tot2 <- NULL
tme <- NULL
for (k in 1:length(dire)) {
  tst <- dire[k]  ## String object; name of current self-report datafile
  tmp <- read.csv(paste('data/selfreports','/',dire[k],sep = ""),col.names = paste('v',1:28, sep = ""), fill = T, header = F)
  tmp <- tmp[-c(1,2),]  ## remove useless first two rows
  dem <- tmp[1,]  ## Take out demographics row
  dem <- dem[,1:6]  ## Filter demographics to main columns
  names(dem) <- c('age','gender','student','instrument','msi','chill')  ## Rename demographic variables
  tmp <- tmp[-c(1),]  ## Remove demographic row from main self-reports
  lst <- as.character(unique(tmp$v1)) ## What pieces were involved for the participant?
  nme <- unlist(strsplit(tst, split = '_')) ## Split name of datafile
  par <- unlist(strsplit(nme[1], split = 'par'))  ## Get participant number for later assignment
  for (n in 1:length(lst)) {
    txt <- unlist(strsplit(lst[n], '_'))  ## split datafile name by '_'
    ok <- tmp[tmp$v1 == lst[n],]  ## Filter datafile by piece in question
    if (nrow(ok) == 1){ ## Basically if no chills were reported run this part of loop
      names(ok) <- c('condition','familiarity','enjoyment','intensity','jaw','gasp','eyes','tears','chest','throat','grand','self','greatness','larger','vast','mental','comprehend','understand','struggle','magnitude','bond','close','love','welcome','care','hug','nice','commit')
      ok[ok == 'None'] <- NA  ## lack of emotion rating = NA
      for (t in 2:28) {
        ok[,t] <- as.numeric(as.character(ok[,t]))  ## Convert all column types to numeric
      }
      chill <- as.data.frame(0) ## no chills were reported, so input 0.
      names(chill) <- c('chills_freq')
      tot <- cbind(dem,ok,chill)## bind to 'tot' object; this cbinds demographic data, self-reports, and chills frequency
    }
    if (nrow(ok) > 1){  ## If at least one chill was reported run this loop instead
      ok2 <- ok[grepl('time =',ok$v2),] ## Filter to all button press data
      ok2 <- ok2 %>% distinct(v2, .keep_all = T)  ## Remove duplicate values (too fast button press)
      p <- nrow(ok2)  ## definer for next loop (number of button presses)
      ok <- ok %>% distinct(v2, .keep_all = T)  ## Remove duplicate values
      for (m in 1:p) {
        ok <- ok[-c(grepl('time =',ok$v2)),]  ## Get self-report data only
      }
      names(ok) <- c('condition','familiarity','enjoyment','intensity','jaw','gasp','eyes','tears','chest','throat','grand','self','greatness','larger','vast','mental','comprehend','understand','struggle','magnitude','bond','close','love','welcome','care','hug','nice','commit')
      ok[ok == 'None'] <- NA ## Lack of response = NA
      for (t in 2:28) {
        ok[,t] <- as.numeric(as.character(ok[,t]))  # Convert column type
      }
      ok3 <- ok2
      ok3 <- ok3[-c(3:28)]  ## Object focused only on button press, remove all other columns
      ok3$v2 <- gsub('.* = ',"",ok3$v2) ## Make button press time data just a number
      ok3$v2 <- as.numeric(as.character(ok3$v2))
      ok3 <- ok3[!ok3$v2 > 400,]  ## Any higher than this causes trouble for physiological measures; filter out
      chk <- ok3$v2
      for (y in 1:length(chk)) {  ## This says: if a button press follows another by less than 6.9 secs, remove!
        val <- chk[y]
        val2 <- chk[y] + 6.9
        x <- with(ok3, v2 < val2 & v2 > val)
        for (v in 1:length(x)){
          if (x[v] == TRUE){
            ok3 <- ok3[-c(v),]
          }
        }
      }
      chill <- as.data.frame(nrow(ok3)) ## How many button presses, value becomes 'chill'
      names(chill) <- c('chills_freq')
      tot <- cbind(dem,ok,chill)  ## Main object; note demographics are duplicated, but should be fine
      ok3$participant <- par[2]
      tme <- rbind(tme,ok3) ## This is working towards building independent button press dataset, for later physiology
    }
    tot$participant <- par[2]
    bint <- txt[1]
    if (bint == 'pines'){
      tot$modality <- c('audio')
    }
    if (bint == 'glos'){
      tot$modality <- c('visual')
    }
    if (bint == 'prayer'){
      tot$modality <- c('audio')
    }
    if (bint == 'door'){
      tot$modality <- c('visual')
    }
    tot$category <- c(txt[2])
    tot$awe <- rowMeans(tot[,c(11:13,17:26)])
    tot$awe_phys <- rowMeans(tot[,c(11:13)])
    tot$awe_feel <- rowMeans(tot[,c(17:26)])
    tot$kama <- rowMeans(tot[,c(14:16,27:34)])
    tot$kama_phys <- rowMeans(tot[,c(14:16)])
    tot$kama_feel <- rowMeans(tot[,c(27:34)])
    num <- as.numeric(par[2])
    eqsq <- emp[c(num),]
    tot$eq <- eqsq$eq
    tot$sq <- eqsq$sq
    tot2 <- rbind(tot,tot2)
  }
}

##########################################################################################
## sort out button press data, for eventual bringing in of skin conductance and temperature in later script
##########################################################################################

tme$v2 <- gsub('.* = ',"",tme$v2)
tme$v1 <- as.character(tme$v1)
tot2$condition <- as.character(tot2$condition)
tme$v1[tme$v1 == 'glos_empathy'] <- 'glosoli_empathy' ## Keep naming conventions consistent for later
tme$v1[tme$v1 == 'glos_structure'] <- 'glosoli_structure'
tot2$condition[tot2$condition == 'glos_empathy'] <- 'glosoli_empathy'
tot2$condition[tot2$condition == 'glos_structure'] <- 'glosoli_structure'
tme$v2 <- as.numeric(as.character(tme$v2))
tme <- tme[!tme$v2 > 400,]    ## To deal with glosoli issues; button press beyond actual time-series data range...

tot2$awe <- scale(tot2$awe)
tot2$kama <- scale(tot2$kama)

## within participants normalisation of score/type

tot2$exp <- tot2[,39] - tot2[,42]
hist(tot2$exp)
quans <- quantile(tot2$exp, probs = c(0,0.33,0.67,1),na.rm = T)
tot <- NULL
for (k in 1:nrow(tot2)) {
  tmp <- tot2[k,]
  if (is.na(tmp$exp) == TRUE){
    next
  }
  if (tmp$exp < quans[2]){
    tmp$type <- 'kama muta'
  }
  if (tmp$exp >= quans[2]){
    if (tmp$exp <= quans[3]){
      tmp$type <- 'balanced'
    }
  }
  if (tmp$exp > quans[3]){
    tmp$type <- 'awe'
  }
  tot <- rbind(tmp,tot)
}
tot2 <- tot
####################################

rm(emp,tmp,dire,k,lst,m,n,p,t,chill,dem,ok,ok2,txt,bint,eq,nme,num,par,sq,tst,eqsq)
