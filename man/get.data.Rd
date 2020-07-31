\name{get.data}
\alias{get.data}
\title{
Parsing the data file.
}
\description{
This function prompts for the file contained all the data needed to process. You only have to 
execute this function in the working directory where your file is stored properly formatted as requested.

The input file is a tab-delimited text matrix as follows:

\preformatted{
Sample     Class      feature(1)      feature(2)      feature(n)      ...
sampleA    classX     counts(A1)      counts(A2)      counts(An)      ...
sampleB    classY     counts(B1)      counts(B2)      counts(Bn)      ...
sampleC    classX     counts(C1)      counts(C2)      counts(Cn)      ...
sampleD    classY     counts(D1)      counts(D2)      counts(Dn)      ...
}

From the version 1.1 on you will be able to load your data as COLUMN format, 
just adding the "Class" information in the second row as follows:

\preformatted{
Sample        sampleA        sampleB        sampleC        sampleD        ...
Class         classX         classY         classX         classY         ...
feature(1)    counts(A1)     counts(B1)     counts(C1)     counts(D1)     ...
feature(2)    counts(A2)     counts(B2)     counts(C2)     counts(D2)     ...
feature(3)    counts(A3)     counts(B3)     counts(C3)     counts(D3)     ...
feature(4)    counts(A4)     counts(B4)     counts(C4)     counts(D4)     ...
feature(n)    counts(An)     counts(Bn)     counts(Cn)     counts(Dn)     ...
}

 
}
\usage{
get.data()
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
function () 
{
    DATA <- readline("Type the name of your data set : ")
    if (DATA == "") {
        tb <- read.table(system.file("extdat", "DATA_2", package = "permubiome"), 
            header = T, sep = "\t")
        print(paste("As you declare no input file, the permubiome test data was loaded"))
        save(tb, file = "permubiome.RData")
    }
    else {
        FORMAT <- readline("Type the format of your data set (PERMUBIOME or COLUMN): ")
        if (FORMAT == "PERMUBIOME") {
            tb <- read.table(DATA, header = T, sep = "\t")
            save(tb, file = "permubiome.RData")
        }
        else {
            biom <- read.table(DATA, sep = "\t")
            tb <- t(biom)
            colnames(tb) <- tb[1, ]
            rownames(tb) <- NULL
            tb = tb[-1, ]
            labels <- colnames(tb)
            tb <- as.data.frame(tb)
            for (i in 3:length(labels)) {
                tb[, i] <- as.numeric(as.character(tb[, i]))
            }
            save(tb, file = "permubiome.RData")
        }
    }
    load("permubiome.RData")
    df <- as.data.frame(tb)
    classes <- levels(df$Class)
    samples <- nrow(df)
    print(paste("Your data file contains:", samples, "samples"))
    print(paste("The classes in your data file are:", classes[1], 
        "and", classes[2]))
    print(paste("The number of different categories to compare are:", 
        (ncol(tb) - 2)))
    save(DATA, df, REFERENCE, classes, file = "permubiome.RData")
  }
}
\keyword{ input }
\keyword{ dataset }
