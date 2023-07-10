FROM opensciencegrid/osgvo-ubuntu-20.04:latest
RUN apt-get update -y 
RUN apt-get install -y \ 
            build-essential \ 
            cmake \ 
            g++ \ 
            jags \
            r-base-dev 
RUN R -e "install.packages('lme4', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('brokenstick', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('gamlss', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('JMbayes', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('rjags', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('tidyr', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('purrr', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('tibble', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('ggplot2', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('nlme', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readr', dependencies=TRUE, repos='http://cran.rstudio.com/')" 
RUN R -e "install.packages('broom.mixed', dependencies=TRUE, repos='http://cran.rstudio.com/')"  
RUN R -e "install.packages('stringr', dependencies=TRUE, repos='http://cran.rstudio.com/')"
# Testing your container on your laptop: 
# run -it --rm=true username/image:tag /bin/bash
# start R: with “R" and load libraries
# goodgolden555/randy:*







