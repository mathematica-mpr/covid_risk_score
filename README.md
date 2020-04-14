# 19andMe
An interactive dashboard for users to calculate their individualized risk score of contracting COVID-19 and related adverse health outcomes.

## Contents
* app.R
    * Launch the 19andMe app (https://19andme.shinyapps.io/covid_risk_score/)
* data/
    * Store the input data
* doc/
    * Calculate input from the literature
* src/
    * R Scripts for different modules of the app
    * Google Analytics plug-in
    * ShareThis plug-in
    * CSS

## Authors
* **Cindy Hu** - *back-end lead*
* **George Luo** - *front-end lead*

## Reviewers and Support
* **Margaret Luo** - *Docker guru*
* **George Gallo** - *AWS architect*
* **Kelsey Skvoretz** - *QA reviewer*
* **Fei Xing** - *QA reviewer*

## Acknowledgements
We appreciate additional support towards this project from Alex Bohl, Dave Peters, and Matt Gillingham.

## Contact info
For more information, please contact Cindy Hu at CHu@mathematica-mpr.com and George Luo at covid.risk.score@gmail.com.

# DockerShinyApp

## Installing
This project is built to use Docker and docker-compose to make development easy across all machines and remove host machine configuration as a potential issue.  Use `docker-compose` to get started quickly.  You will need to install docker and docker-compose. 

* [Docker](https://docs.docker.com/install/)
* [Docker Compose](https://docs.docker.com/compose/install/)

Use docker-compose to build the image:
```
docker-compose build
```

Remember that docker images are immutable once built.  Only changes to files in `/home/rstudio` will persist after restarts. 

## Development
To create an RStudio environment preloaded with all dependencies
1. Create a `.env` file that defines the desired RStudio password, e.g.
```
PASSWORD=mystrongpassword
```
There is an example [sample.env](sample.env) you can also use.  Copy it and rename it to `.env`. 

2. Generate username and API key for USPS ZIP CODE CROSSWALK [here](https://www.huduser.gov/portal/dataset/api.html). We will use this to look up the FIPS county code for the zip code submitted by the user. Add API key to doc/HUD_API_KEY.txt

3. Start the environment
`docker-compose up`

4. Visit `http://localhost:8787` and start hacking.

5. Keep shiny app code in `app.R`.  Launch it for development with `shiny::runApp('app.R')`.

## Deployment
TBD
