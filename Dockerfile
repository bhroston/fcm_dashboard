FROM rocker/shiny-verse:latest
LABEL maintainer="bhroston <bhroston@vt.edu>"

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

# install R packages required
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('visNetwork')"
RUN Rscript -e "install.packages('igraph')"
RUN Rscript -e "install.packages('shinyalert')"
RUN Rscript -e "install.packages('tidyr')"

# copy the app to the image
COPY project.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY R /srv/shiny-server/R
COPY data /srv/shiny-server/data

# select port
EXPOSE 3838
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server
# run app
CMD ["/usr/bin/shiny-server.sh"]

#RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
#RUN addgroup --system app \
#    && adduser --system --ingroup app app
#WORKDIR /home/app
#COPY app .
#RUN chown app:app -R /home/app
#USER app
#EXPOSE 3838
#CMD ["R", "-e", "shiny::runApp('/home/app')"]
