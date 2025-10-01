# read NeXus-II export data with emphasis on getting the timing from the Line Trigger markers 

read_nexus_export <- function(filename,fs = 32, channels = FALSE, ...){

# read_nexus_export('tmp.csv',fs=32)

#system("awk '{if (FNR>13) {print}}' tmp.csv > tmp2.csv")

d <- read.csv(paste('data','/',filename,sep = ""),skip = 11,header = T, sep = "\t", row.names = NULL)
#print(head(d))
D<-d[1:nrow(d)-1,1:4] # throw away useless columns and last line (might require change)
#colnames(D)<-c("time","value","line")

#fs <- 32 # Sampling rate
sec <- seq(0,(nrow(D)-1)/fs,by=1/fs)
D <- data.frame(sec,D)
#head(D)

# write small summary

# length of the file, seconds, samples
#print(paste(nrow(D),' samples'))
#print(paste(as.character(D$TIME[nrow(D)]),' total duration'))
# number of triggers
o <- D$sec[D$Events=="Main Trigger"]
#print(paste(as.character(length(o))," Triggers"))
return(D)
}


