normalize <-
function (prevalence = 0.3, method = 1)
{

#Loading data
load("permubiome.RData")
df_norm<-df
REFERENCE<-REFERENCE

#Method selection and normalization
if (method == 1){
y <- array(, nrow(df_norm))
for (j in 1:nrow(df_norm)){
y[j]<-sum(df_norm[j,3:ncol(df_norm)])
}
for (l in 3:ncol(df_norm)){
for (m in 1:nrow(df_norm)){
df_norm[m,l]<-round((df_norm[m,l]/y[m])*1000000, digits=0)
}
}
for (i in ncol(df_norm):3) {
if (sum(df_norm[,i] == "0") >= (nrow(df_norm) * (1 - prevalence))) {
df_norm[,i]<-NULL
}
}
} else if (method == 2){
for (i in ncol(df_norm):3) {
if (sum(df_norm[,i] == 0) >= (nrow(df_norm) * (1 - prevalence))) {
df_norm[,i]<-NULL
}
}
sfactor_matrix<-matrix(, ncol = ncol(df_norm)-2, nrow = nrow(df_norm))
y <- array(, nrow(df_norm))
for (m in 1:nrow(df_norm)){
for (l in 3:ncol(df_norm)){
sfactor_matrix[m,l-2] <- signif((df_norm[m,l]/mean(df_norm[,l])), digits=3)
}
y[m]<-median(sfactor_matrix[m,1:ncol(sfactor_matrix)])
}
for (a in 3:ncol(df_norm)){
for (b in 1:nrow(df_norm)){
df_norm[b,a] <- round((df_norm[b,a]*y[b]), digits=0)
}
}
} else if (method == 0){
head(df_norm)
print(paste("Your dataset was not normalized according to method option: 0"))
} else { print(paste("Select and appropiate method for normalization: 1 ('proportions'), 2 ('anders'), or 0 ('none')"))
}
print(paste("Your normalized data now contains:", ncol(df_norm)-2, "normalize categories ready to analize" ))
save(df, df_norm, REFERENCE, file="permubiome.RData")
}
