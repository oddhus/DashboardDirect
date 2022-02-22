FROM rocker/shiny-verse:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libnlopt-dev \
  libxml2-dev \
  libcairo2-dev \
  libsqlite3-dev \
  libpq-dev \
  libssh2-1-dev \
  unixodbc-dev \
  r-cran-v8 \
  libv8-dev \
  net-tools \
  libprotobuf-dev \
  protobuf-compiler \
  libjq-dev \
  libudunits2-0 \
  libudunits2-dev \
  libgdal-dev \
  libssl-dev \
  curl \
  gnupg2 \
  unixodbc

# Add SQL Server ODBC Driver 17 for Ubuntu 18.04
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
RUN curl https://packages.microsoft.com/config/debian/10/prod.list > /etc/apt/sources.list.d/mssql-release.list
RUN apt-get update
RUN ACCEPT_EULA=Y apt-get install -y msodbcsql17
RUN ACCEPT_EULA=Y apt-get install -y mssql-tools
RUN echo 'export PATH="$PATH:/opt/mssql-tools/bin"' >> ~/.bash_profile
RUN echo 'export PATH="$PATH:/opt/mssql-tools/bin"' >> ~/.bashrc

## update system libraries
RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get clean

# copy necessary files
## renv.lock file
COPY /renv.lock ./renv.lock


# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'


## app folder
#COPY /shiny-app/ ./app
COPY ./shiny-app/app.R /srv/shiny-server/
COPY ./shiny-app/* /srv/shiny-server/shiny-app/
COPY /env.txt ./srv/shiny-server/

# expose port
EXPOSE 3838

# run app on container start
#CMD ["R", "-e", "shiny::runApp('/app')"]
#CMD ["/usr/bin/shiny-server.sh"]
CMD ["/usr/bin/shiny-server"]