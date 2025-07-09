FROM rocker/binder:4.3.1
LABEL maintainer='Ben Marwick <benmarwick@gmail.com>'
USER root
COPY . /home/rstudio
RUN chown -R ${NB_USER} /home/rstudio

# Set the user and the working directory
USER ${NB_USER}
WORKDIR /home/rstudio

RUN R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"
