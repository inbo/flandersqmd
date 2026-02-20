FROM ubuntu:latest

WORKDIR /github/workspace

ENV DEBIAN_FRONTEND=noninteractive

RUN  apt update -qq \
  && apt install --no-install-recommends --yes \
       ca-certificates curl dirmngr ghostscript gnupg  software-properties-common wget \
  && wget -qO- \
       https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | \
       tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc \
  && wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc | \
       tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc \
  && add-apt-repository --yes "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" \
  && echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu $(lsb_release -cs) main" \
       > /etc/apt/sources.list.d/cranapt.list \
  && apt update -qq \
  && apt install --no-install-recommends --yes r-base r-base-dev \
  && echo "Package: *" > /etc/apt/preferences.d/99cranapt \
  && echo "Pin: release o=CRAN-Apt Project" >> /etc/apt/preferences.d/99cranapt \
  && echo "Pin: release l=CRAN-Apt Packages" >> /etc/apt/preferences.d/99cranapt \
  && echo "Pin-Priority: 700"  >> /etc/apt/preferences.d/99cranapt \
  && apt clean

COPY docker/.Rprofile /usr/lib/R/etc/Rprofile.site

RUN  Rscript --no-save --no-restore -e 'install.packages("bspm")' \
  && echo "suppressMessages(bspm::enable())" >> /usr/lib/R/etc/Rprofile.site \
  && echo "options(bspm.version.check=FALSE)" >> /usr/lib/R/etc/Rprofile.site

ENV QUARTO_VERSION=1.8.27

RUN  curl -L -O https://github.com/quarto-dev/quarto-cli/releases/download/v$QUARTO_VERSION/quarto-$QUARTO_VERSION-linux-amd64.deb \
  && dpkg -i quarto-${QUARTO_VERSION}-linux-amd64.deb \
  && quarto install tinytex --update-path \
  && tlmgr install datetime2 emptypage etoolbox fancyhdr fontawesome5 footmisc geometry hyphen-dutch hyphen-french hyphen-german hyperref lastpage multirow parskip pdfpages titlesec tocloft url

## Install fonts
RUN  mkdir -p ${HOME}/.fonts \
  && wget https://www.wfonts.com/download/data/2014/12/12/calibri/calibri.zip \
  && unzip calibri.zip -d ${HOME}/.fonts \
  && rm calibri.zip \
  && rm ${HOME}/.fonts/*.woff \
  && wget -O ${HOME}/.fonts/Inconsolatazi4-Regular.otf http://mirrors.ctan.org/fonts/inconsolata/opentype/Inconsolatazi4-Regular.otf \
  && wget -O ${HOME}/.fonts/Inconsolatazi4-Bold.otf http://mirrors.ctan.org/fonts/inconsolata/opentype/Inconsolatazi4-Bold.otf \
  && fc-cache -fv \
  && updmap-sys

RUN  Rscript --no-save --no-restore -e 'install.packages("pak")' \
  && Rscript --no-save --no-restore -e 'pak::pkg_install(c("assertthat", "checklist", "fs", "gert", "ggplot2", "knitr", "lipsum", "INBOtheme", "pdftools", "quarto", "renv", "yaml"))'

COPY DESCRIPTION /flandersqmd/DESCRIPTION
COPY NAMESPACE /flandersqmd/NAMESPACE
COPY R /flandersqmd/R/
COPY man /flandersqmd/inst/man

RUN Rscript --no-save --no-restore -e 'pak::local_install("/flandersqmd")'

COPY docker/entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
