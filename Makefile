simulationOne:
	cd simulationOne && Rscript -e "rmarkdown::render('simulationOne.Rmd')"

simulationTwo:
	cd simulationTwo && Rscript -e "rmarkdown::render('simulationTwo.Rmd')"

.PHONY: simulationOne simulationTwo