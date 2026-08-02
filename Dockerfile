# Start with an R image
FROM rocker/tidyverse:latest

# Install Linux system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('plumber','tidymodels','ranger'), repos='https://cloud.r-project.org')"

# Copy files into the container
COPY API.R /home/API.R
COPY water_potability.csv /home/water_potability.csv

# Set working directory
WORKDIR /home

# Expose the Plumber port
EXPOSE 8000

# Run the API
CMD ["R", "-e", "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=8000)"]