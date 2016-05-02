if (!require("hexbin")){
  install.packages("hexbin",dependencies = TRUE)
  library('hexbin')
}

#setwd("C:/Users/cimendes/Dropbox/Tese/Scoary_Debug")

#lala=list.files()

#read chosen file
file = file.choose()
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

cols <- colorRampPalette(c("darkorchid4","darkblue","green","yellow", "red"))

#gray scale
bin<-hexbin(negative_present_in,positive_present_in)
plot(bin, main=file)
dev.off()

#colours
png(paste(file,"_scatterplot_colour.png"), width=480, height=480)
cols <- colorRampPalette(c("darkorchid4","darkblue","green","yellow", "red"))
plot(hexbin(negative_present_in,positive_present_in), main = file ,
     colorcut = seq(0,1,length.out=24),
     colramp = function(n) cols(24))
dev.off()

#control - all data
positive_present_in_alldata=data.df[,4]
negative_present_in_alldata=data.df[,5]
png(paste(file,"_alldata.png"), width=480, height=480)
cols <- colorRampPalette(c("darkorchid4","darkblue","green","yellow", "red"))
plot(hexbin(negative_present_in_alldata,positive_present_in_alldata), main = file ,
     colorcut = seq(0,1,length.out=24),
     colramp = function(n) cols(24))
dev.off()
