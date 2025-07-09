FROM rocker/binder:4.3.1
LABEL maintainer='Your Name <your_email@somewhere.com>'
USER root
COPY . /home/rstudio
RUN chown -R ${NB_USER} /home/rstudio
USER ${NB_USER}

RUN wget https://github.com/benmarwick/archyjobads/raw/master/DESCRIPTION && R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"

RUN rm DESCRIPTION.1; exit 0
