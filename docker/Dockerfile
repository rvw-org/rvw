## Emacs, make this -*- mode: sh; -*-

FROM r-base:latest

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rvw-org" \
      maintainer="Dirk Eddelbuettel <edd@debian.org>"

RUN apt-get update \
        && apt-get install -y --no-install-recommends \
                r-cran-rcpp \
                r-cran-testthat \
                r-cran-runit \
                r-cran-data.table \
                r-cran-knitr \
                r-cran-rmarkdown \
                libvw-dev \
                vowpal-wabbit \
                libboost-program-options-dev \
        && install.r RApiSerialize \
        && mkdir ~/.R \
        && echo _R_CHECK_FORCE_SUGGESTS_=FALSE > ~/.R/check.Renviron 

CMD ["bash"]
