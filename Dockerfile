FROM rocker/shiny:4.0.4
RUN install2.r rsconnect
WORKDIR /home/shinyusr
COPY app/ui ui 
COPY app/modules modules 
COPY app/src src 
COPY app/app.R app.R 
COPY app/deploy.R deploy.R
CMD Rscript deploy.R