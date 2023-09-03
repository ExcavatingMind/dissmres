# dissertation MRes file

This contains a preprint style word document of MRes dissertaion, R code, and will be a test for docker files

### Actual dissertation

The Word document file for the dissertation draft provides a preprint style document of the dissertation. If, for example, you have come from the poster presentation, you can look through the dissertation to further examine, for instance, the exploratory analyses touched on within the poster.

#### The experimental repository

If you would like to ascertain how the experiment was run, you can follow the links to the GitLab repository to either run or examine these materials. It further contains all plots used and the PsychoPy file for running the experiment (among other things).
`https://gitlab.pavlovia.org/Strain/exp_size_only`

### Analysis Within a Fully-Reproducible Computational Environment

Resources are provided for the fully computational reproducible environment (R, Rstudio, and package versions) used for data wrangling, visualization, statistical modelling, and reporting.

#### Docker container

To start, you should clone this repository to your local machine. With Docker running in the background, use a terminal (or cmd on Windows) to navigate to the cloned repository and type the following Docker command:

```docker build -t diss-r-attempt ```

Then, type:

```docker run --rm -p 8787:8787 -e PASSWORD=password diss-r-attempt```

Once the container is running, open a web browser and type `localhost:8787` in the address bar. Enter the username `rstudio` and the password `password`. This will generate a fully functioning Rstudio session running from the docker container.


#### Dockerfile

The docker file within this repository contains all the packages, package versions, and Rstudio version/environment required to conduct the exact analysis produced for the dissertation.

#### Other relevant files

The `dissfirstattempt.R` file contains all the code used during analysis for the dissertation. For instance, it contains plots and analyses conducted for the dissertation such as Sitar plots and linear mixed effects model output. 

The `anscombes_quartet.R` file contains the code used to produce the relatively famous Anscombe's (1973) Quartet issue.

The `plot_conditions.R` file contains the code to produce example plots of both the size experiment (from this dissertation) and Strain et al. (2023) contrast plots (from which this dissertation is based on).

The `final_data.csv` file contains the anonymous data used for the analysis.

### References


Anscombe, F. J. (1973). Graphs in Statistical Analysis. The American Statistician, 27(1), 17–21. https://doi.org/10.1080/00031305.1973.10478966

Strain, G., Stewart, A., Warren, P. A., & Jay, C. (2023). The Effects of Contrast on Correlation Perception in Scatterplots. International Journal of Human-Computer Studies, 176, 103040. https://doi.org/10.1016/j.ijhcs.2023.103040
















