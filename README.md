`HMMoce`: an `R` package for improved analysis of marine animal movement data using hidden Markov models
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
**Camrin D. Braun<sup>1,2</sup>\*, Benjamin Galuardi<sup>3,4</sup>, Simon R. Thorrold<sup>2</sup>**

1.  Massachusetts Institute of Technology-Woods Hole Oceanographic Institution Joint Program in Oceanography/Applied Ocean Science and Engineering, Cambridge, MA 02139
2.  Biology Department, Woods Hole Oceanographic Institution, Woods Hole, MA 02543
3.  School of Marine Science and Technology, University of Massachusetts Dartmouth, Fairhaven, MA 02719
4.  Greater Atlantic Regional Fisheries Office, National Marine Fisheries Service, National Oceanic and Atmospheric Administration, Gloucester, MA 01930

Package Summary
---------------

Electronic tagging of marine animals is common throughout the world oceans. Many of these studies have deployed archival tags that rely on light levels and sea-surface temperatures to retrospectively track movements of tagged animals. However, methodological issues associated with light-level geolocation have constrained meaningful inference to species where it is possible to accurately estimate time of sunrise and sunset. Most studies have largely disregarded the oceanographic profiles collected by the tag as a potential way of refining light-level geolocation estimates provided by electronic tags.

Open-source oceanographic measurements and outputs from high-resolution models are increasingly available and accessible. We integrated temperature and depth profiles recorded by electronic tags, with empirical data and model outputs, to construct likelihoods and improve geolocation estimates for marine animals using an existing, but modified, state-space hidden Markov model (HMM). Our model (`HMMoce`) exhibited as much as 6-fold improvement in pointwise error as compared to traditional light-level geolocation approaches and produced the lowest mean error in 3 of 4 cases when compared to the state-of-the-art tag manufacturer's HMM (GPE3). `HMMoce` contained behavior state-switching capability not found in other comparable methods. The use of profile-based likelihood estimates proved useful when we removed data to emulate data returned from species that yield poor quality light data. The results demonstrated the general applicability of the `HMMoce` model to marine animals, particularly those that do not frequent surface waters during crepuscular periods. Our model is available as an open-source `R` package, `HMMoce`, that uses a state-space HMM approach and leverages available tag and oceanographic data to improve position estimates derived from electronic tags.

Package Citation
----------------

TBD

Package Structure
-----------------

The package is structured as follows: \* Load the relevant tag data and establish a study area of interest. \* Get the environmental data to base the likelihood calculations on. \* Calculate the desired likelihoods (e.g. depth-temperature profiles, SST, etc) \* Estimate parameters and run the model. Results are written out along the way. \* Perform model checking and choose a final model.

Installation Instructions
-------------------------

`HMMoce` can be installed from CRAN from within `R` using `install.packages('HMMoce')`. To get the latest developments, get it from GitHub using `devtools::install_github('camrinbraun/HMMoce')`

Examples
--------

For an example use of the package, please see the vignette using `vignette('HMMoce')`.

[![Travis-CI Build Status](https://travis-ci.org/camrinbraun/HMMoce.svg?branch=master)](https://travis-ci.org/camrinbraun/HMMoce)
