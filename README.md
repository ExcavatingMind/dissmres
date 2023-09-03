# dissertation mres file
This contains a preprint style word document of mres dissertaion, r code, and will be a test for docker files




### Analysis Within a Fully-Reproducible Computational Environment

Resources are provided for the fully computational reproducible environment (R, Rstudio, and package versions) that was used for data wrangling, visualization, statistical modelling, and reporting.

To begin, clone this repository to your local machine. With Docker running in the background, use a terminal (or cmd on Windows) to navigate to the cloned repository and type the following Docker command:

```docker build -t diss-r-attempt ```

Then, type:

```docker run --rm -p 8787:8787 -e PASSWORD=password diss-r-attempt```

Once the container is running, open a web browser and type `localhost:8787` in the address bar. Enter the username `rstudio` and the password `password`. This will generate a fully functioning Rstudio session running from the docker container.
