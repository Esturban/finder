FROM rocker/shiny-verse:4.2
RUN install2.r shiny forecast jsonlite ggplot2 htmltools rsconnect shinyMobile apexcharter shinyWidgets
WORKDIR /home/shinyusr/
COPY app/ .
RUN ls -la ./*
CMD Rscript deploy.R
