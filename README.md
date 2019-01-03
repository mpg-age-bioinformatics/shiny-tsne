## t-SNE shiny app

This app has been developed to work inside a docker container (Dockerfile file available [here](https://github.com/mpg-age-bioinformatics/shiny)).

To use this app locally you need to build the respective container and clone this repo:

Build the image:
```bash
cd ~/
mkdir -p shinylogs
git clone https://github.com/mpg-age-bioinformatics/shiny.git
cd shiny
docker build -t shiny .
```
Pull the app:
```bash
cd ~/shiny
git submodule init tsne 
git submodule update tsne 
```
Start the container:
```bash
docker run --rm -p 3838:3838 -p 8787:8787 \
-v ~/shiny:/srv/shiny-server/ \
-v ~/shinylogs:/var/log/shiny-server/ \
--name shiny shiny
```
Access the app on your browser over [http://localhost:3838/tsne](http://localhost:3838/tsne).
