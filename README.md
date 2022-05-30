# Replication data for: "TK nyt-mrp-policy-support"

This repository stores code for several multilevel regression and post-stratification (MRP) models that use survey data from the CES and Nationscape polls to estimate state-level opinion on the 2020 presidential election and across multiple policy areas. This was prepared for my TK _New York Times_ Opinion data essay titled "TK", pegged to the July 12th, 2022 release of my book [_Strength in Numbers: How Polls Work and Why We Need Them._](https://gelliottmorris.com/strength_in_numbers/).


## Methods

These MRP models follow the research of [Lax and Phillips (2009)](http://www.columbia.edu/~jhp2121/publications/HowShouldWeEstimateOpinion.pdf), [Ghitza and Gelman (2013)](http://www.stat.columbia.edu/~gelman/research/published/misterp.pdf), [Kuriwaki et al. (2022)](https://osf.io/mk9e6/) and others in calibrating models of survey data that use informative sub-national contextual information with observed election results (or high-quality estimates of results) at the geographic and group level. 

I model 2020 turnout and vote choice (from the voter-validated [Cooperative Election Study (CES)](https://cces.gov.harvard.edu)) as a function of a respondent's sex, age, race, education, and income, allowing the demographic coefficients to vary at the state and regional level. Both the state- and region-level coefficients are structured using the individual or aggregated (i) Democratic share of the vote in 2020, (ii) white evangelical share of the population and (iii) log number of people living 1 square mile away from the average resident.

I estimate state-level opinion on 14 key policies via a set of similar models of pooled data from the [Nationscape](https://www.voterstudygroup.org/nationscape) project at UCLA and the Democracy Fund Voter Study Group. The models are the same as above other than the use of the joint distribution of demographics and 2020 vote choice, as post-stratified and calibrated in the earlier stage.


## Notes

- You will need [git-lfs](https://git-lfs.github.com) to download some of the models in the `models` directory. The ones stored are those generated in `00_mrp_add_2020_vote_to_psframe.R`. But because of data-storare limits on Github you will have to run the `02_mrp_policy_support.R` scripts yourself. It took around 11 hours to run the 14 models called in that script on my computer (2020 Macbook Air with an 8-core M1 processor.)
- The output of these scripts is stored in `.csv` format and shown graphically in the `output` folder.


# License

This project is published under the [MIT license](https://opensource.org/licenses/MIT). Please link back to this repository if you cite it, and cite it as: G. Elliott Morris, "Replication data for: TK", _The New York Times_, TK Date, 2022.


