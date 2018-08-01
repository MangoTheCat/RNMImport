# RNMImport

NONMEM is a FORTRAN application for fitting mixed effects models. 
NONMEM licenses are available from ICON Development Solutions.
Models are specified as plain text files called control files. 
The control file contains blocks corresponding to components of the model.
Model results are presented as plain text files. These input 
and output files can be imported into R by **RNMImport** to 
facilitate model modification, model diagnostics, model interpretation
and reporting.

```{r}
# install devtools for devtools::install_github
install.packages("devtools")
library(devtools)
# install RNMImport
install_github("MangoTheCat/RNMImport")
library(RNMImport)
```

A NONMEM execution including both input files and results files can
be read using `importNm`.
The imported object is S4 class 'NMRun'. 
Various methods are available for extracting the model information.

```{r}
theo1 <- importNm(conFile = "theoph.con", 
    path = system.file(package = "RNMImport", "examples", "theoph"))
slotNames(theo1)
```

The dataset can be extracted using `nmData`.
This contains the input data, and the model residuals and other 
records requested when defining the model.

```{r}
head(nmData(theo1))
```

Model parameters can be extracted using methods corresponding
to the parameter type.

```{r}
getThetas(theo1)
getOmegas(theo1)
getSigmas(theo1)
```
