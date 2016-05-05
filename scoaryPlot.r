if (!require("hexbin")){
  install.packages("hexbin",dependencies = TRUE)
  library('hexbin')
}
if (!require("tm")){
  install.packages("tm",dependencies = TRUE)
  library('tm')
}
if (!require("SnowballC")){
  install.packages("SnowballC",dependencies = TRUE)
  library('SnowballC')
}
if (!require("wordcloud")){
  install.packages("wordcloud",dependencies = TRUE)
  library('wordcloud')
}

#read chosen file
file = file.choose()
filename = basename(file)

data=read.csv(file, header=TRUE, sep=';')

#select data by benjamin corrected p-value
data.df=data.frame(data)
#names(data.df)
data.df.sub=subset(data.df,Benjamini_H_p<0.05)

#Select data
positive_present_in=data.df.sub[,4]
negative_present_in=data.df.sub[,5]

#save plot
png(paste(file,"_scatterplot_gray.png"), width=480, height=480)

#create plot

cols <- colorRampPalette(c("darkblue", "red"))
bANDw <- colorRampPalette(c("lightgray","black"))

#plot - gray scale
bin<-hexbin(negative_present_in,positive_present_in)
#cols <- colorRampPalette(c("lightgray","black"))
plot(bin, main=filename,colorcut = seq(0,1,length.out=15),
     colramp = function(n) bANDw(15))
dev.off()

#plot - colours
png(paste(file,"_scatterplot_colour.png"), width=480, height=480)
#cols <- colorRampPalette(c("darkblue","red"))
plot(hexbin(negative_present_in,positive_present_in), main = filename ,
     colorcut = seq(0,1,length.out=15),
     colramp = function(n) cols(15))
dev.off()

#plot - control - all data
positive_present_in_alldata=data.df[,4]
negative_present_in_alldata=data.df[,5]
png(paste(file,"_alldata.png"), width=480, height=480)
#cols <- colorRampPalette(c("darkblue","red"))
plot(hexbin(negative_present_in_alldata,positive_present_in_alldata), main = filename ,
     colorcut = seq(0,1,length.out=15),
     colramp = function(n) cols(15))
dev.off()


#Exclusive accessory genome - file and wordcloud
max.Number_pos_present_in=max(positive_present_in)
subset.exclusive=subset(data.df,Number_pos_present_in==max.Number_pos_present_in & Number_neg_present_in == 0)
if (length(row.names(subset.exclusive)) != 0){
  write.table(subset.exclusive, file=paste(file,"exclusive_present.csv"), sep=";", row.names = FALSE)
  #TODO
  jeopCorpus <- Corpus(VectorSource(subset.exclusive$Annotation))
  jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
  jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
  wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)
  
  } else{
  print(paste("No value to print in ", file, "exclusive present"))
  }
max.Number_neg_present_in=max(negative_present_in)
subset.absent=subset(data.df,Number_pos_present_in==0 & max.Number_neg_present_in == 0)
if (length(row.names(subset.absent)) != 0){
  write.table(subset.absent, file=paste(file,"exclusive_absent.csv"), sep=";", row.names = FALSE)
  } else{
  
  print(paste("No value to print in ", file, "exclusive absent"))
  }

