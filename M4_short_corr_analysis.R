library(sqldf);library(caret);library(corrplot);library(Hmisc);library(abind)
#set color for correlation plot
col <- colorRampPalette(c("#85AACE"))
# place here all the point submissions (M4 public dataset)
working_directory <- 'd:\\M4-Competition\\Predictions\\Point Forecasts'
# place here the top10methods.txt file
working_directory_top_10 <- 'd:\\M4-Competition\\Predictions'
# place here the IDtoMethodType.csv file
working_directory_share_data <- 'C:\\Users\\pante\\Dropbox\\m4meta\\data'
# place here the future points (testset)
working_directory_share_data_test_set <- 'C:\\Users\\pante\\Dropbox\\m4meta\\data\\M-test-set'

setwd(working_directory)

# get list of files from working directory
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

# function to replace all NA values to zero 
zero.replace <- function(x) { replace(x, is.na(x), 0) }

preds<-list()
# load the candidate Predictions and zero NA values
for (i in 1:length(files)){
  preds[[i]] <- zero.replace(read.csv(files[i]))
}

methods <- sapply(strsplit(sapply(strsplit(files, "-"), "[", 2), ".csv"), "[", 1)

# load method types
setwd(working_directory_share_data)
idToMethod <- (((read.csv('IDtoMethodType.csv'))))
methods.plus <- NULL
for(i in 1:length(methods)){
  strQ <- paste("select * from idToMethod where id=='",methods[i],"'",sep = '') 
  tmpQ <-  sqldf(strQ)
  methods.plus <- rbind(methods.plus,paste(tmpQ[1,c(1)],'-',tmpQ[1,c(2)]))
}
rm(strQ,tmpQ)


# Set Working Directory for True Values
setwd(working_directory_share_data_test_set)
# codify the type of predictions
type <- list(Daily='D',Hourly='H',Monthly='M',Quarterly='Q',Weekly='W',Yearly='Y',
             D='Daily',H='Hourly',M='Monthly',Q='Quarterly',W='Weekly',Y='Yearly')

# Select type of predictions to filter e.g. H for hourly
# Daily     : D
# Hourly    : H
# Monthly   : M
# Quarterly : Q
# Weekly    : W
# Yearly    : Y

# Set appropriate type for filter value e.g. Hourly
filter <- as.character(type['Daily'])

# Load the true values
true_values <-read.csv(paste(type[filter],'-test.csv',sep=''))

# Set the number of series to calculate
n.series <-1:nrow(true_values)

# calculate and create the error lists
errors <- list()
for (i in 1:length(preds)){
  tmp_pred <- data.frame(preds[i])
  tmp <- sqldf(paste('select * from tmp_pred where id like "%',filter,'%"',sep = ''))
  errors[[i]] <-  (cbind(id=true_values[n.series,c(1)], (true_values[n.series,c(2:ncol(true_values))]
                                                         - tmp[n.series,c(2:ncol(true_values))])/true_values[n.series,c(2:ncol(true_values))]   ))
  cat(" errors of method ", i,' from ', length(preds), "\n")
}
rm(tmp,tmp_pred)# preds

# create the error series list per method
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

# create the compare series list
compare.series <- list()
for (i in n.series){
  compare.series[[i]] <- (sapply(series,'[[',i))
}

#loop through compare series and calculate avg corr-matrix
for (j in  seq(1,nrow(true_values)-1,by=2)){
  # get current j series
  ts <- (matrix( unlist((compare.series[[j]])),ncol=length(preds),nrow=ncol(true_values)-1))
  corrMatrix <- cor(ts, method = "pearson", use = "complete.obs")
  if(any(is.na(corrMatrix) == TRUE ,na.rm = FALSE)){next}
  
  # get series j+1
  ts_ <- (matrix( unlist((compare.series[[j+1]])),ncol=length(preds),nrow=ncol(true_values)-1))
  corrMatrix_ <- cor(ts_, method = "pearson", use = "complete.obs")
  if(any(is.na(corrMatrix_) == TRUE ,na.rm = FALSE)){next}
  # calculate avg matrix
  avg_ <-  apply(abind(corrMatrix,corrMatrix_, along=3),  1:2, mean)
  if(j==1){corrMatrix_avg <-avg_} else {
    corrMatrix_avg <-apply(abind(corrMatrix_avg,avg_, along=3),  1:2, mean)
  }
  cat("series", j,' from ', nrow(true_values), "\n")
}


#top 10 authors
authors<-c('Smyl','Manso','Pawlikowski','Jaganathan','Fiorucci','Petropoulos','Shaub','Zampeta','Doornik','Pedregal')
methods.rank <- c(1:10)

methods.plus.author<-NULL
for (k in 1:length(methods)){
  methods.plus.author<-rbind(methods.plus.author,paste(methods.rank[k],'-',authors[k],sep=''))
}

rownames(corrMatrix_avg)<- methods.plus.author
colnames(corrMatrix_avg)<- methods.plus.author

corrplot(corrMatrix_avg, method="pie", col=col(1),  cl.pos = "n",
         type="upper", number.cex = .6,
         addCoef.col = "grey20", 
         tl.srt=45, #Text label color and rotation
         tl.col = "black",cl.lim = c(0,1),
         diag=TRUE)
