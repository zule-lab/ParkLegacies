# Cooling effect provided by urban parks minimally affected by historical legacies



[![DOI](https://zenodo.org/badge/490309243.svg)](https://zenodo.org/doi/10.5281/zenodo.10406627)


## Authors
[Isabella C. Richmond](https://github.com/icrichmond/), Michael A. Paulauskas, Erica Padvaiskas, Laura Carolina Gonzalez Sinisterra, [Kayleigh Hutt-Taylor](https://ca.linkedin.com/in/kayleigh-hutt-taylor-a85981101?trk=public_post_feed-actor-name), [Alec L. Robitaille](https://robitalec.ca/), [Carly D. Ziter](https://www.carlyziter.com/)

## Abstract
Cities are temporally dynamic ecosystems that are developed and redeveloped over time, with development patterns reflecting the power structures and inequities that shape our societies. Understanding the current day functioning of our city’s natural ecosystems is dependent on incorporating the continuing influence of historical conditions. Urban parks provide critical benefits to resident wellbeing, are developed on different land-use types, and contain natural elements that are susceptible to the effects of historical decision making. To plan equitable distribution of park benefits in the future, we must quantify and understand the impact of historical decision-making. We measured neighbourhood socio-demographic composition, forest composition, and the cooling effect of 33 sites in Montreal, each with a past land-use that fell into one of three categories: agricultural, forested, or industrial. We asked: 1) How do surrounding communities differ around parks of each historical land-use type and do these differences indicate inequity? 2) What are the effects of historical land-use types on current park capacity to provide cooling? We find a complex dynamic with inequity at previously industrial sites, where historic environmental racism has resulted in these parks being surrounded by higher levels of immigrants, and lower median incomes than expected. We find little evidence of past land-use type affecting the relative cooling effect of our parks, and we see more similarities than differences in forest composition across past land-use types. We do find that forest composition affects the cooling effect, where tree density strengthens the cooling effect and tree size has a negative relationship with cooling effect. If we want to plan cities that provide critical benefits equitably, we must understand the implications of developing our greenspaces on different types of land. This study provides some evidence that an equitable future is possible on many different types of current land uses.

## Repository Use
This repository is built on a `{targets}` workflow. To run all analyses done for the paper, download and open the project, [install `{targets}`](https://books.ropensci.org/targets/), and run `targets::tar_make()`.

To access data only, raw data can be found in the `input/` folder. The only exception is the Québec census data, which is downloaded in the census-prep.R script.
