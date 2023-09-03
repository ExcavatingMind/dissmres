FROM rocker/rstudio:4.1.1
FROM rocker/verse

ADD dissertation_basics/dissfirstattempt home/rstudio/dissertation_basics
ADD Hackathon/final_data home/rstudio/
ADD dissertation_basics/anscombes_quartet home/rstudio/dissertation_basics
ADD dissertation_basics/plot_conditions home/rstudio/dissertation_basics


RUN R -e "install.packages('devtools')"

RUN R -e "require(devtools)" 

RUN R -e "devtools::install_version('lmerTest', version = '3.1-3', dependencies = T)"
RUN R -e "devtools::install_version('lmtest', version = '0.9-40', dependencies = T)"
RUN R -e "devtools::install_version('ltm', version = '1.2-0', dependencies = T)"
RUN R -e "devtools::install_version('rstatix', version = '0.7.1', dependencies = T)"
RUN R -e "devtools::install_version('emmeans', version = '1.8.2', dependencies = T)"
RUN R -e "devtools::install_version('easystats', version = '0.6.0', dependencies = T)"
RUN R -e "devtools::install_version('lme4', version = '1.1-31', dependencies = T)"
RUN R -e "devtools::install_version('polycor', version = '0.8-1', dependencies = T)"
RUN R -e "devtools::install_version('afex', version = '1.2-0', dependencies = T)"
RUN R -e "devtools::install_version('msm', version = '1.7', dependencies = T)"
RUN R -e "devtools::install_version('MASS', version = '7.3-58.1', dependencies = T)"
RUN R -e "devtools::install_version('buildmer', version = '2.8', dependencies = T)"
RUN R -e "devtools::install_version('patchwork', version = '1.2.1', dependencies = T)"
RUN R -e "devtools::install_version('datasets', version = '4.1.1', dependencies = T)"
RUN R -e "devtools::install_version('ggforce', version = '0.4.1', dependencies = T)"
RUN R -e "devtools::install_version('ggpubr', version = '0.4.0', dependencies = T)"
RUN R -e "devtools::install_version('conflicted', version = '1.2.0', dependencies = T)"
RUN R -e "devtools::install_version('zoo', version = '1.8-11', dependencies = T)"
RUN R -e "devtools::install_version('see', version = '0.8.0', dependencies = T)"
RUN R -e "devtools::install_version('Matrix', version = '1.5-1', dependencies = T)"