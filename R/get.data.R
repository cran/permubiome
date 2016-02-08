get.data <-
function()
{
DATA <- readline("Type the name of your data set : ")
if (DATA == ""){
tb<-read.table(system.file("extdat", "DATA_1", package="permubiome"), header=T, sep="\t")
print(paste("As you declare no input file, the permubiome test data was loaded" ))
}
else
{
tb<-read.table(DATA, header=T, sep="\t")
}
df<-as.data.frame(tb)
classes<-levels(df$Class)
samples<-nrow(df)
print(paste("Your data file contains:", samples, "samples" ))
print(paste("The classes in your data file are:", classes[1], "and", classes[2]))
print(paste("The number of different categories to compare are:", (ncol(tb)-2) ))
save(df, file = "permubiome.RData")
}
