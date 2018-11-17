# shiny
This repo hosts most of our shiny apps

## Running the shiny server locally

You can start the server by:

```bash
cd ~/
mkdir -p shinylogs
git clone https://github.com/mpg-age-bioinformatics/shiny.git
cd shiny
docker build -t shiny .
docker run --rm -p 3838:3838 -p 8787:8787 \
-v ~/shiny:/srv/shiny-server/ \
-v ~/shinylogs:/var/log/shiny-server/ \
--name shiny shiny
```
List the Apps by:
```bash
cd ~/shiny
ls -d */
```
And now access the Apps by directing your browser to http://localhost:3838/AppName
(replacing AppName by the name of the respective App).

The server can be stopped and the container removed with:
```bash
docker stop shiny && docker rm shiny
``` 
The build image can be removed with:
```bash
docker rmi shiny
```

## Contributing

Make sure you have one folder for each App you wish to contribute.

### Apps

Please check the [VennDiagram App](https://github.com/mpg-age-bioinformatics/shiny/tree/master/VennDiagram) for a complete running example.

Inside that folder we will need a `requirements.R` file which will have to include the installation of all required libraries.

Example of a `requirements.R` file:

```
.libPaths("/srv/shiny-server/exampleApp/libs")
install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'), \
repos=c('https://cloud.r-project.org/'), \
dependencies=TRUE )"
devtools::install_github('IRkernel/IRkernel')
source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
```

Always add a small documentation to your `ui.R` eg.:

```
    mainPanel(
      plotOutput("histogram", height = "500px", width = "500px"),
      downloadButton('downloadPlot', 'Download Plot'),
      br(),br(),
      p("This App uses R's ", code('hist'), " function. For more information read the respective documentation in ",
        a("rdocumentation.org", href = "https://www.rdocumentation.org/packages/graphics/versions/3.5.1/topics/hist"),
        "and wikipedia's entry for ", a("histogram.",href="https://en.wikipedia.org/wiki/Histogram" )),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
      )
```

Notice the `htmlOutput('appversion')` paragraph which makes use of the `gitversion()` function in `server.R`:
```
gitversion <- function(){ 
  git<-read.csv("/srv/shiny-server/.git/refs/heads/master", header=FALSE)
  git<-git$V1
  git<-toString(git[1])
  git<-substr(git, 1, 7)
  return(git)
}
.
.
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
    }
  )
```
Please make sure that all files downloaded by the user containt the version tag eg.:
```
    # specify the output file name
    filename = function(){
      paste0('Histogram.',gitversion(),'.pdf')
    }
```

### Development environment 

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
Your App will have the address http://localhost:3838/AppName.

For starting the R-Studio server you can enter the container by:
```
docker exec -i -t shiny /bin/bash
```
An then 
```
sudo rstudio-server start
```
User/Password: `mpiage/bioinf`

The RStudio server will now be accessible over http://localhost:8787 and the shiny repo available in `/srv/shiny-server/`.

For stopping the server use:
```
sudo rstudio-server stop
```

`server.R`, `ui.R`, `requirements.R` should all start with `.libPaths("/srv/shiny-server/<AppName>/libs")` for which you should
have a folder called `libs` in your App main folder.

You can stop and remove the container with `docker stop shiny ; docker rm shiny`.

The image can be removed with `docker rmi shiny`.

If **system libraries** are required please add them to the **end** of the `Dockerfile`.