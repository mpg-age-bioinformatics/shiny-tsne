# shiny
This repo hosts most of our shiny apps

## Contributing

Make sure you have one folder for each App you wish to contribute.

Inside that folder we will need a `requirements.R` file which will have to include the installation of all required libraries.

Example of a `requirements.R` file:

```
install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'), \
repos=c('https://cloud.r-project.org/'), \
dependencies=TRUE )"
devtools::install_github('IRkernel/IRkernel')
source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
```

Please develop your apps in the same enviroment where they will be hosted.

Build the respective shiny server and rstudio server:
```bash
docker build -t shiny .
```
Run the image with:
```
docker run --rm -p 3838:3838 -p 8787:8787 \
-v </path/to/shiny/apps/folder>/:/srv/shiny-server/ \
-v </path/to/shiny/apps/log/folder/>:/var/log/shiny-server/ \
--name shiny shiny
```

You can now access your apps with your host's browser over http://localhost:3838. 

For starting the R-Studio server you can enter the container by:
```
docker exec -i -t shiny /bin/bash
```
An then 
```
sudo rstudio-server start
```
User/Password: `mpiage/bioinf`

The RStudio server will now be accessible over http://localhost:8787.

For stopping the server use:
```
sudo rstudio-server stop
```

You can stop and remove the container with `docker stop shiny ; docker rm shiny`.

The image can be removed with `docker rmi shiny`.