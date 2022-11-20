FROM rocker/verse:4.2.1
MAINTAINER Alfredo Garbuno Iñigo "alfredo.garbuno@itam.mx"

# Lets declare some user variables =============================================
ENV RSTUDIO_USER rstudio
ENV TARGET_DIR ""
ENV RUNNING_IN_DOCKER true

# Lets declare the work directory ==============================================
RUN adduser $RSTUDIO_USER sudo
WORKDIR /home/$RSTUDIO_USER/

# Instalamos herramientas: htop (monitoreo) y librerias que necesita R para ====
# compilar paquetes
RUN apt-get install --no-install-recommends -qq wget ca-certificates make g++
RUN apt-get update \
    && apt-get install -y libmagick++-dev htop tree \
    && apt-get install -y python3-pip libfontconfig1-dev

RUN pip3 --version \
    && pip3 install radian
    
# Latex ========================================================================

RUN tlmgr install amsmath latex-amsmath-dev iftex kvoptions \
    ltxcmds kvsetkeys etoolbox xcolor geometry fancyvrb framed booktabs \
    auxhook bigintcalc bitset etexcmds gettitlestring hycolor hyperref \
    intcalc kvdefinekeys letltxmacro pdfescape refcount rerunfilecheck \
    stringenc uniquecounter zapfding pdftexcmds infwarerr epstopdf-pkg mdwtools    

# Clean up =====================================================================
RUN apt-get clean all \
    && apt-get purge \
    && rm -rf /tmp/downloaded_packages  \
    && rm -rf /var/lib/apt/lists/*

RUN strip /usr/local/lib/R/site-library/*/libs/*.so

# Setup renv workspace =========================================================
ENV RENV_PATHS_ROOT /home/local/.renv
RUN mkdir -p  $RENV_PATHS_ROOT
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf
RUN install2.r --error rmarkdown httpgd languageserver

RUN wget --progress=dot:mega https://github.com/stan-dev/cmdstan/releases/download/v2.30.1/cmdstan-2.30.1.tar.gz
RUN tar -zxpf cmdstan-2.30.1.tar.gz
RUN ln -s cmdstan-2.30.1 cmdstan
RUN cd cmdstan; make build

RUN R -e "renv::restore()"
RUN rm -rf renv.lock .Rprofile renv

# Aseguramos que podemos trabajar desde ~/rstudio/home =========================
RUN chown -R $RSTUDIO_USER:staff /home/$RSTUDIO_USER/
VOLUME [ "/home/$RSTUDIO_USER/$TARGET_DIR" ]

# Good lookin terminals ========================================================
RUN sh -c "$(wget -O- https://github.com/deluan/zsh-in-docker/releases/download/v1.1.2/zsh-in-docker.sh)" -- \
    -a 'CASE_SENSITIVE="true"' \
    -t robbyrussell \
    -p git
