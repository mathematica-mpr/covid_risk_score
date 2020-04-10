# covid_risk_score
An interactive dashboard for users to calculate their individualized risk score of contracting COVID-19

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

2. Start the environment
`docker-compose up`

3. Visit `http://localhost:8787` and start hacking.

4. Keep shiny app code in `app.R`.  Launch it for development with `shiny::runApp('app.R')`.

## Deployment
TBD
