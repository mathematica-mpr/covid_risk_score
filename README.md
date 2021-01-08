# 19andMe

## Introduction
19andMe is an interactive dashboard for users to calculate their individualized risk score of contracting COVID-19 and related adverse health outcomes. https://19andme.covid19.mathematica.org/

How 19andMe Works?

19andMe takes user input on where you live, who you are, and what you do in the pandemic, uses the best available science to provide a ballpark estimation on the how likely someone with similar characteristics like you may contract COVID-19, and if infected, how likely is the outcome going to be severe. Please see the "Methods" tab in the app for more details on our methodology.

<img src = "https://raw.githubusercontent.com/mathematica-mpr/covid_risk_score/main/app/www/How_19andMe_Works.png" alt = "19andme workflow">


## Contents

* app/
    * R Scripts for different modules of the app
    * Google Analytics plug-in
    * CSS
    * www/
        * Mathematica logo
        * How 19andMe works

## Authors
* **Cindy Hu** - *product owner*
* **Emma Pendl-Robinson** - *full-stack developer* 
* **Erin Lipman** - *back-end developer and data engineer*
* **Jennifer Starling** - *validation lead*
* **Margaret Luo** - *API support and technical consultant*
* **Max Dulieu** - *API tech lead*

## Reviewers and Support
* **George Luo** - *front-end developer*
* **Arpan Bhattacharya, George Gallo** - *AWS architects*
* **Aaron White** - *technical consultant*
* **Sean Kirk** - *DevOps engineer*
* **Kelsey Skvoretz, Fei Xing** - *QA reviewers*
* **Matt Salganik** - *expert reviewer*

## Acknowledgements
We appreciate additional support towards this project from Alex Bohl, Dave Peters, and Matt Gillingham.

## Contact info
For more information, please contact Cindy Hu at CHu@mathematica-mpr.com or at covid.risk.score@gmail.com.

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

2. Email the [Mathematica Communications team](Communications@mathematica-mpr.com ) to get an x-api-key. We will use this to make POST requests for the covid-risk-score-api. Add X_API_KEY to your `.Renviron` file.

3. Start the environment
`docker-compose up`

4. Visit `http://localhost:8787` and start hacking.

5. Keep shiny app code in `app.R`.  Launch it for development with `shiny::runApp('app.R')`.

## Deployment
Utilize the attached Dockerfile for a simple but efficient deployment setup.
