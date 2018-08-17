plots <-
function ()
{
Class<-NULL
loadNamespace("ggplot2")
load("permubiome.RData")
df_norm <- df_norm
category <- readline("Type the category you want to plot : ")
if (category == ""){
category <- colnames(df_norm[3])
print(paste("As you declared no categories, the very first one of your dataset is plotted!"))
}
df_to_plot <- df_norm[,c("Sample","Class",category)]
classes<-levels(df_to_plot$Class)
if (REFERENCE == ""){
REFERENCE <- classes[1]
} else if (REFERENCE == classes[2]){
classes[2] <- classes[1]
classes[1] <- REFERENCE
}
df_to_plot$ref <- factor(df_to_plot$Class, levels=(c(classes[1],classes[2])))
output <- readline("Do you want an output file (yes/no)? : ")
if (substr(output, 1, 1) == "y"){
extension <- readline("What extension do you prefer for the output plot (ps, pdf, jpeg, tiff, png, bmp )? : ")
p1<-(ggplot(df_to_plot, aes(df_to_plot$ref, df_to_plot[,category]), environment = environment())+geom_boxplot(notch=F, outlier.colour="blue", outlier.shape=1, outlier.size=3)+ggtitle(category)+theme(plot.title=element_text(size=24, face="bold"))+ylab("Normalized read proportion")+xlab("Classes")+theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"))+geom_jitter(position=position_jitter(width=0, height=0)))
ggsave(filename = paste(category, extension, sep = "."), plot = p1, path = NULL, scale = 1, units = c("cm"), dpi = 300, limitsize = TRUE)
} 
else
{
p1<-(ggplot(df_to_plot, aes(df_to_plot$ref, df_to_plot[,category]), environment = environment())+geom_boxplot(notch=F, outlier.colour="blue", outlier.shape=1, outlier.size=3)+ggtitle(category)+theme(plot.title=element_text(size=24, face="bold"))+ylab("Normalized read proportion")+xlab("Classes")+theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"))+geom_jitter(position=position_jitter(width=0, height=0)))
plot(p1)
}
save(df, df_norm, df_to_plot, REFERENCE, classes, file="permubiome.RData")
}
