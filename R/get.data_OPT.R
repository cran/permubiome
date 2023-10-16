get.data <-
function()
{
permubiomenv <- new.env(parent = emptyenv())
#Getting input from user
DATA<-readline("Type the name of your data set : ")
if (substr(DATA, 1, 1) == ""){
tb<-read.table(system.file("extdat", "DATA_2", package="permubiome"), header=T, sep="\t")
print(paste("As you declare no input file, the permubiome test data was loaded" ))
permubiomenv$data<-tb
}
else
{
#Evaluating data format
FORMAT<-readline("Type the format of your data set (PERMUBIOME or COLUMN): ")
if(FORMAT == "PERMUBIOME"){
tb<-read.table(DATA, header=T, sep="\t")
permubiomenv$data<-tb
}
else
{
biom<-read.table(DATA, sep="\t")
tb<-t(biom)
colnames(tb)<-tb[1,]
rownames(tb)<-NULL
tb = tb[-1,]
labels<-colnames(tb)
tb<-as.data.frame(tb)
for (i in 3:length(labels)){
tb[,i]<-as.numeric(as.character(tb[,i]))
}
permubiomenv$data<-tb
}
}
df<-as.data.frame(tb)
classes<-levels(as.factor(df$Class))
samples<-nrow(df)
#Parsing reference and cases samples
print(paste("Your data file contains:", samples, "samples" ))
print(paste("The classes in your data file are:", classes[1], "and", classes[2]))
REFERENCE <- readline("Which one of the above classes detected is your CONTROL/REFERENCE group: ")
print(paste("The number of different categories to compare are:", (ncol(tb)-2) ))
}
