# Land-use history causes differences in park nighttime cooling capacity and forest structure


Zenodo: [![DOI](https://zenodo.org/badge/490309243.svg)](https://zenodo.org/doi/10.5281/zenodo.10406627)

Paper (Open-Access): https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.70082 


## Authors
[Isabella C. Richmond](https://github.com/icrichmond/), Michael A. Paulauskas, Erica Padvaiskas, Laura Carolina Gonzalez Sinisterra, [Kayleigh Hutt-Taylor](https://ca.linkedin.com/in/kayleigh-hutt-taylor-a85981101?trk=public_post_feed-actor-name), [Alec L. Robitaille](https://robitalec.ca/), [Carly D. Ziter](https://www.carlyziter.com/)

## Abstract
Cities are temporally dynamic ecosystems that experience continuous redevelopment over time. Urban parks, which provide critical benefits to resident wellbeing, are developed on different land-use types and contain natural elements that are susceptible to the effects of historical decision-making. Thus, understanding the current day functioning of our city's ecosystems and planning for the equitable distribution of park benefits in the future requires incorporation and understanding of the impacts of historical decision-making. We measured neighborhood sociodemographic composition, forest structure, and the cooling effect of 33 sites in parks across Montreal, each with a past land-use in one of three classes: agricultural, forested, or industrial. We asked the following questions: (1) what are the effects of historical land-use on current park forest structure, diversity, and consequently the capacity to provide cooling? (2) how do surrounding communities differ around parks of each historical land-use type, and what are the implications for equitable access to cooling? We found that forest structure and cooling capacity differed across past land-use type, and forest structure has complex relationships with park cooling capacity. Our results provide evidence of historical environmental injustice impacting current day cooling capacity for marginalized groups. Previously industrial parks had less cooling capacity at night, while simultaneously being surrounded by communities with higher proportions of immigrants and lower median incomes than Montreal's average. However, daytime cooling capacity was similar across past land-use type, highlighting the importance of current management decisions to provide a critical ecosystem service, temperature mitigation, regardless of a site's history. Planting areas of small, dense forest stands with trees ≥5 cm dbh within urban parks can help augment daytime cooling benefits in the city but may hinder nighttime cooling. To provide both nighttime and daytime cooling, a mixed management strategy of park trees is required, where small and large trees are incorporated at different densities. Finally, we find evidence of gentrification surrounding all park sites, emphasizing the complex socioecological dynamics of green infrastructure and the need for community-led greening projects paired with social housing policies.

## Repository Use
This repository is built on a `{targets}` workflow and uses a `{renv}` environment. To install all necessary packages, run `renv::restore()` before running the workflow. For any issues related to `{cmdstanr}` installation, refer to their [help page](https://mc-stan.org/cmdstanr/). For any issues related to `{zarg}`, install based on instructions at the [repository](https://github.com/robitalec/zarg).

After all packages have been installed, the entire workflow can be run by opening the R projet and running `targets::tar_make()`. For more details on `{targets}` workflows, check out the `{targets}` [book](https://books.ropensci.org/targets/)

To access data only, raw data and metadata can be found in the `input/` folder. The only exception is the Québec census data, which is downloaded in the census-prep.R script.
