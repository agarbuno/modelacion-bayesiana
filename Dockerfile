FROM rocker/verse:4.0.2

ENV WORKON_HOME /opt/virtualenvs
ENV PYTHON_VENV_PATH $WORKON_HOME/stan_env

RUN apt-get update && apt-get install -y libudunits2-dev 
RUN apt-get update && apt-get install -y --no-install-recommends \
	build-essential libssl-dev libffi-dev apt-utils ed libnlopt-dev

# Global site-wide config -- neeeded for building packages
RUN mkdir -p $HOME/.R/ \
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs \n" >> $HOME/.R/Makevars

# Config for rstudio user
USER rstudio
RUN mkdir -p /home/rstudio/.R
RUN touch /home/rstudio/.R/Makevars
RUN r -e 'cat("\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC", "CXX14=g++", file = "/home/rstudio/.R/Makevars", sep = "\n", append = TRUE)'
USER root

# versión de development TODO: cambiar a estables
RUN r -e 'devtools::install_github("stan-dev/cmdstanr")'
RUN r -e 'devtools::install_github("stan-dev/posterior")'
RUN install2.r --error --deps TRUE\
	tidymodels \
	rstan bayesplot loo \
	rstanarm rstantools shinystan
RUN r -e 'install.packages("dplyr", version = "1.0.2")'
USER rstudio
RUN r -e 'cmdstanr::install_cmdstan(cores = 2, release_url = "https://github.com/stan-dev/cmdstan/releases/download/v2.24.1/cmdstan-2.24.1.tar.gz")'
USER root

