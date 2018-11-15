# shiny
This repos host most of our shiny apps

## Contributing

Make sure you one folder for each App you wish to contribute.

Inside that folder we will need a `requirements.R` file which will have to include the installation of all required libraries.

Example of a `requirements.R` file:

```
install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'), repos=c('https://cloud.r-project.org/'), dependencies=TRUE )"
devtools::install_github('IRkernel/IRkernel')
source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
```
