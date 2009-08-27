\name{EEG-package}
\Rdversion{1.1}
\alias{EEG-package}
\alias{EEG}
\docType{package}
\title{
Package for the analysis of Evoked/Event Related Potentials in R
}
\description{
This package reads in single trial EEG (currently only ascii-exported preprocessed and trial segmented in Brain Vision Analyzer), computes averages (i.e., ERP's) and stores ERP's from multiple data sets in a data.frame like object, such that statistical analysis (linear model, (M)ANOVA) can be done using the familiar R modeling frame work. The philosophy of the package is to make ERP analysis in R resemble standard linear modeling in R as close as possible.
}
\details{
\tabular{ll}{
Package: \tab EEG\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-07-29\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Raoul Grasman

Maintainer: Raoul Grasman <rgrasman@uva.nl>
}
\references{
%%~~ Literature or other references for background information ~~
}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
\code{\link{names2factors}}, \code{\link{importBVA}}, \code{\link{batchImportBVA}}, 
\code{\link{eeg.plot}}, \code{\link{mbvadata.frame}}, \code{\link[EEG:rbind.mbva.data.frame]{rbind}}
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
\dontrun{
pp01prdec = importBVA("./bva_export/PP01_MA_Pi_RDE_correct.vhdr")
plot(pp01prdec[,1:10,]) # ts.plot of average accross trials (the ERP) of 10 channels
plot(pp01prdec[,c('Fz', 'Cz', 'Pz', 'Oz'), 20:30]) # plot of average across trials 20...30
pp01select = pp01prdec[1:86, , 40:60] # samples 1..86, trials 40..60
eeg.plot(pp01select, axes=TRUE) # topographical layout of average of selection

m.pp01prdec = mean(pp01prdec) # compute ERP 
eeg.plot(m.pp01prdec) # topographically layed out ERP plot

# t-test comparison of two ERP's

##
## MULTIPLE DATA SETS
##

eegs.c = batchImportBVA("./bva_export", "correct\\.vhdr$", "(.+)\\.vhdr$") # import all data sets in directory
erps.c = lapply(eegs.c, mean) # compute ERPs

# make mbvadata.frame object from ERPs
## generate design data.frame from filenames
nams = gsub("[-.]", "_", names(erps.c)) # convert - and . to seperator _
design = names2factors(nams,  ~ subj+initials+task+stim:level+resp+ext, sep="_")
df.erp.c = mbvadata.frame(erp = erps.c, design) 

eegs.e = batchImportBVA("./bva_export", "error\\.vhdr$", "(.+)\\.vhdr$") # import all data sets in directory
erps.e = lapply(eegs.e, mean) # compute ERPs
nams = gsub("[-.]", "_", names(erps.c)) # convert - and . to seperator _
design = names2factors(nams,  ~ subj+initials+task+stim:level+resp+ext, sep="_")
mbva.erp.e = mbvadata.frame(erp = erps.e, design) 

# bind two mbvadata.frames together
mbva.erp = rbind(mbva.erp.c, mbva.erp.e)


## MANOVA on segments of the ERPs
# convert to regular data.frame 
df = as.data.frame(mbva.erp, from=85, to=115, chan='Cz') # from and to in data set time units (msec, sec) 
head(df)
}
}
