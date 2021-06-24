simulationOne:
	cd simulationOne && Rscript -e "rmarkdown::render('simulationOne.Rmd')"

simulationTwo:
	cd simulationTwo && Rscript -e "rmarkdown::render('simulationTwo.Rmd')"

rtssAnalysis:
	cd simulationTwo && Rscript -e "rmarkdown::render('rtssAnalysis.Rmd')"	

.PHONY: simulationOne simulationTwo rtssAnalysis