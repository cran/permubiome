size.effect <-
function (category = "", replicates = 5000, n.seed = 12345, paired = FALSE, plot.file = "tiff", id.pairs = NULL)
{
Class<-NULL
ref<-NULL
loadNamespace("dabestr")
loadNamespace("rlang")
load("permubiome.RData")
df_norm <- df_norm
if (paired == TRUE){
print(paste("You declared paired data, be sure to include the correct -id.column- argument to parse the identity of the datapoint!"))
}
classes<-levels(df_norm$Class)
if (REFERENCE == ""){
REFERENCE <- classes[1]
} else if (REFERENCE == classes[2]){
classes[2] <- classes[1]
classes[1] <- REFERENCE
}
prepare.stats <- dabest(df_norm, Class, category, paired = paired, idx=c(classes[1], classes[2]), id.column = id.pairs)
prepare.stats$y<-quo_set_expr(prepare.stats$y, as.symbol(category))
print(prepare.stats)
if (category == ""){
category <- colnames(df_norm[3])
print(paste("As you declared no categories, the very first one of your dataset will be processed!"))
}
estimation.stats<-median_diff(prepare.stats, ci = 95, reps = replicates, seed = n.seed)
e_plot<-plot(estimation.stats, group.summaries = "median_quartiles", palette = "Set1", rawplot.ylabel = paste(category, "normalized reads", sep = " "), tick.fontsize = 12, axes.title.fontsize = 18)
tiff(filename=paste(category, "estimation", plot.file, sep = "."), width=650, height=600, res=100, units="px")
e_plot
dev.off()
print(e_plot)
save(df, df_norm, REFERENCE, classes, file="permubiome.RData")
}
