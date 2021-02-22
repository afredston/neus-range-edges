This repository contains all the scripts for the analysis and figures in [Fredston-Hermann *et al.* 2020 *Global Change Biology*](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.15035). Wherever external data must be used, the scripts contain instructions for obtaining the data. Please contact A. Fredston-Hermann (fredston@rutgers.edu) with any questions or for a PDF of the article, and cite the article if using the data or code. 

As of May 2019, these scripts rely heavily on the development version of the `sf` package from `r-spatial`. This is because recently added functions, including `st_crop` and `st_nearest_points`, are extremely useful for our analysis, but not yet in CRAN. Users with older versions will need to install the development version of `sf` which can be a slightly tricky process (see issues on the Github page for that package). Once the CRAN version of `sf` is updated, this issue will be resolved. 

Scripts should be run in the following order: 

1. `get_sst.R`
1. `clean_temp_data.R`
1. `get_trawl_data.R`
1. `clean_trawl_data.R`
1. `coastline_length.R`
1. `calculate_edges.R`
1. `isotherms.R`
1. `analyze_edges.R` (contains all models of range edges)
1. `paper_stats.R` (contains miscellaneous other statistics in the paper)

Once the scripts above have generated the requisite objects in `processed-data`, then any of the figure files can be run as desired. Scripts to generate figures can be found in `neus-range-edges/figures`; the figures will be written out to a directory called `neus-range-edges/results`. Users should create a `neus-range-edges/results` and a `neus-range-edges/data` folder; these are not version controlled due to the size of files involved. 
