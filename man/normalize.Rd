\name{normalize}
\alias{normalize}
\title{
Normalize the microbiome dataset prior to perform the permutation test.
}
\description{
A critical aspect when working with microbiome data is to achieve a proper normalization to the retrieved counts, thus overpassing the variability in terms of sequencing efforts or coverage. There are several ways to do normalization, and we have implemented two well-known methods whose choice will depend on the research question investigated and the researcher's preference. Optionally, if you don't feel comfortable with normalization methods implemented in this package or if your data are already normalized, you have the option of performing no normalization on your data (\emph{method}=0).
}
\usage{
normalize(prevalence = 0.3, method = 1)
}
\arguments{
  \item{prevalence}{
This controls the prevalence of microbiome features across samples in order to keep those with higher occurrence in the cohort of samples under survey. If you have 20 samples and declare a \emph{prevalence} = 0.3 (default), the algorithm will remove those categories with less prevalence than 6 samples. Although the permutation test deals fairly well with zeros, we recommend setting a restricted value in order to improve the statistics for the biomarker discovery (i.e \emph{prevalence} => 0.3).
}
  \item{method}{
Describes the normalization method to be used. We implemented two different strategies to normalize the microbiome data: (1) corresponds to the relative proportion of counts to the features. After retrieving the relative abundance for every feature in very sample the normalization process generate the number of reads corresponding to the features per million reads; (2) corresponds with normalization method described by Anders & Huber (2010), which uses a size factor to correct differences in sequencing coverage. If the user decides not to perform normalization, it must declare \emph{method} = 0.
}
}
\references{
Benitez-Paez A. 2020. Permubiome: an R package to perform permutation based test for biomarker discovery in microbiome analyses. [https://cran.r-project.org].
Benitez-Paez A, et al. mSystems. 2020;5:e00857-19. doi: 10.1128/mSystems.00857-19.
}
\author{
Alfonso Benitez-Paez
}
\examples{
## The function is currently defined as
function (prevalence = 0.3, method = 1) 
{
    load("permubiome.RData")
    df_norm <- df
    if (method == 1) {
        y <- array(, nrow(df_norm))
        for (j in 1:nrow(df_norm)) {
            y[j] <- sum(df_norm[j, 3:ncol(df_norm)])
        }
        for (l in 3:ncol(df_norm)) {
            for (m in 1:nrow(df_norm)) {
                df_norm[m, l] <- round((df_norm[m, l]/y[m]) * 
                  1e+06, digits = 0)
            }
        }
        for (i in ncol(df_norm):3) {
            if (sum(df_norm[, i] == "0") >= (nrow(df_norm) * 
                1 - prevalence)) {
                df_norm[, i] <- NULL
            }
        }
    }
    else if (method == 2) {
        for (i in ncol(df_norm):3) {
            if (sum(df_norm[, i] == 0) >= (nrow(df_norm) * 1 - prevalence)) {
                df_norm[, i] <- NULL
            }
        }
        sfactor_matrix <- matrix(, ncol = ncol(df_norm) - 2, 
            nrow = nrow(df_norm))
        y <- array(, nrow(df_norm))
        for (m in 1:nrow(df_norm)) {
            for (l in 3:ncol(df_norm)) {
                sfactor_matrix[m, l - 2] <- signif((df_norm[m, 
                  l]/mean(df_norm[, l])), digits = 3)
            }
            y[m] <- median(sfactor_matrix[m, 1:ncol(sfactor_matrix)])
        }
        for (a in 3:ncol(df_norm)) {
            for (b in 1:nrow(df_norm)) {
                df_norm[b, a] <- round((df_norm[b, a] * y[b]), 
                  digits = 0)
            }
        }
    }
    else if (method == 0) {
        head(df_norm)
        print(paste("Your dataset was not normalized according to method option: 0"))
    }
    else {
        print(paste("Select and appropiate method for normalization: 1 ('proportions'), 
        2 ('anders'), or 0 ('none')"))
    }
    print(paste("Your normalized data now contains:", ncol(df_norm) - 
        2, "normalize categories ready to analize"))
    save(df_norm, file = "permubiome.RData")
  }
}
\keyword{ normalization }
\keyword{ normalize }
