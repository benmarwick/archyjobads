FROM rocker/binder:4.4.1
LABEL maintainer='Ben Marwick <benmarwick@gmail.com>'
USER root
COPY . /home/rstudio/archyjobads
RUN chown -R ${NB_USER} /home/rstudio/archyjobads

# Set the user and the working directory
USER ${NB_USER}
WORKDIR /home/rstudio/archyjobads

RUN R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"
