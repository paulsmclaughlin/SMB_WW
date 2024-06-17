# SMB_WW

These R scripts run the data prep and model fitting performed for the paper:

Wagner, T., McLaughlin, P., Faunce, K. E., Austin, S., & Smalling, K. (2024). The Effects of Wastewater Reuse on Smallmouth Bass (Micropterus dolomieu) Relative Abundance in the Shenandoah River Watershed, USA. Environmental Toxicology and Chemistry, 43(5), 1138-1148.

Scripts are given for modeling smallmouth bass abundances given the percent of accumelated waste water effluent flows (ACCWW%) at a given site, assuming the ACCWW% came from a specific quarter of the year (smb_accww_QUARTERLY.R) or anually (smb_accww_ANNUAL.R). The scripts prep data for ACCWW% coming from a timeframe and specific age category of fish (both of which can be specified by users at the start of the script) and Bayesian negative-binomial model using the rstanarm R package.
