[![Travis-CI Build Status](https://travis-ci.org/timtrice/Hurricanes.svg?branch=develop-0.1.0)](https://travis-ci.org/timtrice/Hurricanes)

# Hurricanes

`Hurricanes` is a R library that extracts information from available archives on past and current tropical cyclones. Currently, archives date back to 1998. This is considered "raw data" in the sense that information extracted is from then real-time advisories. 

Data can be obtained for cyclones in the north Atlantic (considered the Atlantic Basin) and north-eastern Pacific (the East Pacific Basin from 140&deg;W and eastward, and Central Pacific Basin from 140&deg;W to 180&deg;W). 

## Getting Started

Please view the vignette 'Getting Started':

```r
vignette("getting-started", package = "Hurricanes")
```

### Prerequisites

`Hurricanes` does require an active internet connection as data is extracted from online archives. 

The following R packages are also currently used for data processing:

* data.table (>= 1.9.6), 
* dplyr (>= 0.5.0), 
* lubridate (>= 1.6.0),
* readr (>= 1.0.0),
* rvest (>= 0.3.2), 
* stringr (>= 1.1.0), 
* tibble (>= 1.2), 
* tidyr (>= 0.4.1), 
* xml2 (>= 1.0.0)

Packages `dplyr`, `lubridate`, `readr`, `stringr`, `tibble`, and `tidyr` are also included in the `tidyverse` package which can be used instead.

On future revisions I will work on minimizing package dependencies if it helps increase processing speed.

### Installing

`Hurricanes` is currently only available in GitHub. It can be installed using the `devtools` package:

```r
devtools::install_github("timtrice/Hurricanes")
```

## Built With

* [R 3.4.0](https://www.r-project.org/) - The R Project for Statistical Computing

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/timtrice/f2a4c2a020c87669178dad27e73bfce1) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Tim Trice** - *Initial work* - [timtrice](https://github.com/timtrice)

See also the list of [contributors](https://github.com/timtrice/Hurricanes/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* None yet :)
