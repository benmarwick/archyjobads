# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.5.1

# metadata
LABEL maintainer="Ben Marwick <bmarwick@uw.edu>"

# Define project directory and renv cache
ENV PROJ_DIR /home/rstudio/archyjobads
ENV RENV_PATHS_CACHE=/renv/cache

# Set the working directory to the project directory
WORKDIR $PROJ_DIR

# Copy the project files
COPY . $PROJ_DIR

# Set permissions
RUN chown -R rstudio:rstudio $PROJ_DIR

# Install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Restore the renv environment
RUN R -e "renv::restore()"

# Set permissions
RUN chmod -R 777 /home/

# Render the manuscript, this is only relevant for continuous integration
# not relavant for interactive use
RUN R -e "rmarkdown::render('analysis/paper/article.qmd')"


# To run this container locally:

### STEP 1 ###
# Run on the terminal:
# docker build -t wos .

### STEP 2 ###
# Run on the terminal:
# docker run --rm -it -e ROOT=TRUE -e PASSWORD=rstudio -dp 8787:8787 wos

### STEP 3 ###
# Go to http://localhost:8787/ with your browser. USERID=rstudio, PASSWORD=rstudio

### STEP 4 ####
# Clean and delete containes. Run on the terminal:
# docker ps -aq | xargs docker stop | xargs docker rm

