size.effect <-
function (category = "", replicates = 5000, n.seed = 12345, paired = F, plot.file = "tiff")
{
Class<-NULL
ref<-NULL
loadNamespace("dabestr")
load("permubiome.RData")
df_norm <- df_norm
if (category == ""){
category <- colnames(df_norm[3])
print(paste("As you declared no categories, the very first one of your dataset will be processed!"))
}
class_to_estimate <- df_norm[,c("Sample","Class",category)]
classes<-levels(class_to_estimate$Class)
if (REFERENCE == ""){
REFERENCE <- classes[1]
} else if (REFERENCE == classes[2]){
classes[2] <- classes[1]
classes[1] <- REFERENCE
}
names(class_to_estimate)[3]<-"category"
class_to_estimate$ref <- factor(class_to_estimate$Class, levels=(c(classes[1],classes[2])))
estimation.stats <- dabest(class_to_estimate, ref, category, paired = paired, ci = 95, reps = replicates, func = median, seed = n.seed, idx=c(classes[1], classes[2]))
print(estimation.stats)
e_plot<-plot(estimation.stats, group.summaries = "median_quartiles", palette = "Set1", rawplot.ylabel = paste(category, "normalized reads", sep = " "), tick.fontsize = 14, axes.title.fontsize = 22)
tiff(filename=paste(category, "estimation_plot.tiff", sep = "."), width=650, height=600, res=100, units="px")
e_plot
dev.off()
print(e_plot)
save(df, df_norm, class_to_estimate, REFERENCE, classes, file="permubiome.RData")
}
