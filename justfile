analysissrc := "analise/"
talksrc := "fala/"
slidessrc := "slides/"
handoutsrc := "handout/"
releasesrc := "release/"

default:
    just --list

build: all package

all: analysis tex

tex: abstract talk slides handout

analysis:
    cd {{ analysissrc }} && R CMD BATCH dags.R
    cd {{ analysissrc }} && R CMD BATCH glm.R
    cd {{ analysissrc }} && R CMD BATCH plots_tables.R
    cd {{ analysissrc }} && Rscript -e 'rmarkdown::render("main.Rmd")'

talk:
    cd {{ talksrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ talksrc }} && biber --quiet main
    cd {{ talksrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ talksrc }} && lualatex --interaction=batchmode main.tex

slides:
    cd {{ slidessrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ slidessrc }} && biber --quiet main
    cd {{ slidessrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ slidessrc }} && lualatex --interaction=batchmode main.tex

abstract:
    cd {{ abstractsrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ abstractsrc }} && biber --quiet main
    cd {{ abstractsrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ abstractsrc }} && lualatex --interaction=batchmode main.tex

handout:
    cd {{ handoutsrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ handoutsrc }} && biber --quiet main
    cd {{ handoutsrc }} && lualatex --interaction=batchmode --draftmode main.tex
    cd {{ handoutsrc }} && lualatex --interaction=batchmode main.tex

package:
    mkdir -p {{ releasesrc }}/analysis
    cp {{ analysissrc }}/data.csv {{ releasesrc }}/analysis/data.csv
    cp {{ analysissrc }}/glm.R {{ releasesrc }}/analysis/glm.R
    cp {{ analysissrc }}/main.html {{ releasesrc }}analysis.html
    cp {{ talksrc }}/main.pdf {{ releasesrc }}/talk.pdf 
    cp {{ slidessrc }}/main.pdf {{ releasesrc }}/slides.pdf 
    cp {{ handoutsrc }}/main.pdf {{ releasesrc }}/handout.pdf 
    ouch compress release $(cat version).zip -y
    rm -rdf {{ releasesrc }}
