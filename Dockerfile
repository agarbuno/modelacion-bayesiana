# Cargamos inicialmente de tidyverse

FROM rocker/tidyverse:4.1.1
MAINTAINER Adrian Tame

# Instalar paquetes de CRAN

RUN install2.r --error \
  posterior \
  bayesplot \
  tidybayes

  # Procedimiento para instalar y poner el directorio de cmdstan

WORKDIR /cmdstan

RUN apt-get update
RUN apt-get install --no-install-recommends -qq wget ca-certificates make g++

RUN wget --progress=dot:mega https://github.com/stan-dev/cmdstan/releases/download/v2.27.0/cmdstan-2.27.0.tar.gz
RUN tar -zxpf cmdstan-2.27.0.tar.gz
RUN ln -s cmdstan-2.27.0 cmdstan
RUN cd cmdstan; make build

RUN cd cmdstan; echo "CmdStan home directory is" $PWD

# Instalar cmdstanr de github

RUN installGithub.r stan-dev/cmdstanr

ENV NAME cmdstan-docker