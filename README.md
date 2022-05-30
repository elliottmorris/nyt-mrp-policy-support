# nyt-mrp-policy-support
Code that uses survey data from the CCES and Nationscape to estimate state-level opinion on multiple policies


---

### Notes

- You will need [git-lfs](https://git-lfs.github.com) to download some of the models in the `models` directory. The ones stored are those generated in `00_mrp_add_2020_vote_to_psframe.R`. But because of data-storare limits on Github you will have to run the `02_mrp_policy_support.R` scripts yourself. It took around 11 hours to run the 14 models called in that script on my computer (2020 Macbook Air with an 8-core M1 processor.)
- The output of these scripts is stored in `.csv` format and shown graphically in the `output` folder.