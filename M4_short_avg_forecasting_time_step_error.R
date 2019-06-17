#load necessary libraries
library(sqldf);library(caret);library(Hmisc);library(abind);library(data.table);library(xlsx)

#set plot colors
cols <- c('green','deepskyblue3','firebrick','gold4','darkorchid','grey70','lightblue4','mediumblue','gray41','tan2')
# place here all the point submissions (M4 public dataset)
working_directory <- 'd:\\M4-Competition\\Predictions\\Point Forecasts'
# place here the top10methods.txt file
working_directory_top_10 <- 'd:\\M4-Competition\\Predictions'
# place here the IDtoMethodType.csv file
working_directory_share_data <- 'C:\\Users\\pante\\Dropbox\\m4meta\\data'
# place here the future points (testset)
working_directory_share_data_test_set <- 'C:\\Users\\pante\\Dropbox\\m4meta\\data\\M-test-set'

# reset par settings
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

setwd(working_directory)
#get the list of files from working directory 
#(TRUE=TOP_10),(FALSE=ALL METHODS)
top_10 <- TRUE
if (top_10){
  setwd(working_directory_top_10)
  files <- as.character(unlist(c(matrix(read.table('top10methods.txt' )))))
  tmp_files<- NULL
  for (i in 1:length(files)){if(nchar(files[i])==2){tmp_files<-rbind(tmp_files,paste('submission-0',files[i],'.csv',sep = ''))}
    else{tmp_files<- rbind(tmp_files,paste('submission-',files[i],'.csv',sep = ''))}}
  files <-as.character(tmp_files)
  rm(tmp_files)
  setwd(working_directory)
} else {
  setwd(working_directory)
  files <- list.files()
}

# Function to replace all NA values to zero 
zero.replace <- function(x) { replace(x, is.na(x), 0) }
#store predictions in a list
preds<-list()
#load the candidate predictions replace with zero NA values
for (i in 1:length(files)){
  preds[[i]] <- zero.replace(read.csv(files[i]))
}

#store method's id
methods <- sapply(strsplit(sapply(strsplit(files, "-"), "[", 2), ".csv"), "[", 1)

#change working directory
setwd(working_directory_share_data)

#load file that connects id and method type
idToMethod <- (((read.csv('IDtoMethodType.csv'))))

#concatenate method id and method type
methods.plus <- NULL
for(i in 1:length(methods)){
  strQ <- paste("select * from idToMethod where id=='",methods[i],"'",sep = '') 
  tmpQ <-  sqldf(strQ)
  methods.plus <- rbind(methods.plus,paste(tmpQ[1,c(1)],'-',tmpQ[1,c(2)]))
}
rm(strQ,tmpQ)

##### FOR EVERY NEXT TYPE OF PREDICTION --- RUN FROM THIS POINT --- #####

#set working directory for true Values (test dataset)
setwd(working_directory_share_data_test_set)

#codify the type of predictions
type <- list(Daily='D',Hourly='H',Monthly='M',Quarterly='Q',Weekly='W',Yearly='Y',D='Daily',H='Hourly',M='Monthly',Q='Quarterly',W='Weekly',Y='Yearly')

#enumerate the type of predictions
# Daily     : D
# Hourly    : H
# Monthly   : M
# Quarterly : Q
# Weekly    : W
# Yearly    : Y

# place an appropriate type for filter e.g. Hourly
filter <- as.character(type['Daily'])

#load the true values
true_values <- read.csv(paste(type[filter],'-test.csv',sep=''))

#calculate the number of series exist in a prediction type
n.series <- 1:nrow(true_values)

#calculate and create the error lists
errors <- list()
for (i in 1:length(preds)){
  tmp_pred <- data.frame(preds[i])
  tmp <- sqldf(paste('select * from tmp_pred where id like "%',filter,'%"',sep = ''))
  errors[[i]] <-  (cbind(id=true_values[n.series,c(1)], (true_values[n.series,c(2:ncol(true_values))] - tmp[n.series,c(2:ncol(true_values))])/true_values[n.series,c(2:ncol(true_values))] ))
}
rm(tmp,tmp_pred)

#create the error series list per method
series <- list()
for (i in 1:length(errors)){
  # get current method error list
  tmp <- (errors[[i]])
  # remove first row
  tmp <- tmp[-1]
  # create a list to store the series in a row
  inner.series <- list()
  for (j in 1:nrow(tmp)){
    inner.series[[j]] <- tmp[j,c(1:ncol(tmp))]
  }
  series[[i]] <- inner.series
}
rm(tmp)

#create the compare series list ()
compare.series <- list()
for (i in n.series){
  compare.series[[i]] <- (sapply(series,'[[',i))}

# calculate the average error per method
# M4 short fig 2
ts_avg<-NULL
for(j in n.series){
  # get current j series, (ts table stores the error per series and method)
  ts_i <- matrix( as.double(unlist((compare.series[[j]]))),ncol=length(preds),nrow=ncol(true_values)-1)
  ts_j <- matrix( as.double(unlist((compare.series[[j+1]]))),ncol=length(preds),nrow=ncol(true_values)-1)
  if (j==1) {
    ts_avg <- (ts_i + ts_j)/2
  } else {
    tmp_avg <-(ts_i + ts_j)/2
    ts_avg <- (ts_avg+tmp_avg)/2
  }
  cat("error avg", j,' from ', length(n.series), "\n")
  if (j==(length(n.series)-1)){break}
}
rm(ts_i,ts_j,tmp_avg)

ts<-ts(ts_avg)
par(mar=c(0.1, 1.5 ,0.5 , 0.5), mfrow=c(1,1), oma = c(2, 2, 0.1, 0.1)) #four in a row - bottom/left/up/right
# plot the average values
x.labs <-  seq(0,round(nrow(ts),0),2)
y.labs <- c(0,round(seq((min(ts)-0.1) ,(max(ts)+0.2),((max(ts)+0.2)-(min(ts)-0.1)/3)),1)) #change min/max adjust values for best fit
colnames(ts)<- methods
plot(ts, plot.type = "single", xaxt = "n", yaxt = "n", lty = 1:2,col = cols,lwd=.5, ylim=c(min(y.labs),max(y.labs)))
axis(1, at=x.labs,x.labs,cex.axis=.85,mgp=c(0,.5,.0))
axis(2, at=y.labs,y.labs,cex.axis=.85,mgp=c(0,.75,.0))
mtext("% Error", side=2, line=2, adj=0.5,cex=1)
