# tectonicr-app --- Graphical user interface for [tectonicr](https://tobiste.github.io/tectonicr/)

**tectonicr** is a free and open-source **R** package for modeling and analyzing
the direction of the maximum horizontal stress (SHmax) based on the empirical 
link between the direction of intraplate stress and the direction of the 
relative motion of neighboring plates (Wdowinski, 1998; Stephan et al., 2023). 

## Run in browser:
https://tobiste.shinyapps.io/tectonicr-app/

(no need to install R and tectonicr)

## Run in RStudio
```
shiny::runGitHub("tectonicr-app", "tobiste")
```

or download and unzip the project. Then run
```
shiny::runApp()
```


## How to cite
When referencing this package, please cite 

Stephan, T., Enkelmann, E., and Kroner, U. (2023). Analyzing the horizontal 
orientation of the crustal stress adjacent to plate boundaries. 
*Scientific Reports*, *13*(1). DOI: [10.1038/s41598-023-42433-2](https://doi.org/10.1038/s41598-023-42433-2).

and the package DOI: [10.5281/zenodo.10062231](https://doi.org/10.5281/zenodo.10062231).

## Author
Tobias Stephan


## License
GPL-3.0 License
