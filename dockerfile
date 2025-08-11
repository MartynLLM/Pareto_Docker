FROM rocker/shiny-verse:4.3.0

# Install system dependencies (development packages include runtime libraries)
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server

# Copy application files
COPY app/ ./app/

# Install R packages
RUN cd app && R -e "source('global.R')"

# Create required directories
RUN mkdir -p /srv/shiny-server/data \
             /srv/shiny-server/input \
             /srv/shiny-server/output \
             /srv/shiny-server/support && \
    chown -R shiny:shiny /srv/shiny-server

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
  CMD curl -f http://localhost:3838 || exit 1

# Expose port
EXPOSE 3838

# Start shiny server
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', host='0.0.0.0', port=3838)"]