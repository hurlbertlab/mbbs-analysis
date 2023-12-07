# mbbs-analysis

THINGS TO FIX:
-check issues tab 

things to talk about at meeting with Bradley:
- modeling
- GEE 
- Bayesian
- Half or quarter routes vs full route modeling, differences in analysis
- Percent change per year is the raw output?
- Using NDVI of forest pixels to model forest complexity
- Urbanization as % change along routes, vs ?? also having a measure of how urbanized the route is overall either before or after? What's the reccomended way to model this, or do we not need to model that some routes start much more urbanized than others and might not change at all. 
- New way of modeling observer, based on mbbs meeting and talking with folks, primary observer will be assigned to whomever of the first three observers has the highest rank
- When filtering for the minimum number of sightings on the minimum number of routes, if the requirements are met do ALL routes with sightings stay in the analysis? Or only those routes with ie: greater than 10 years of observations

Upcoming mbbs updates:
- Exacting stop_num in a way that's less fragile, and captures the missing extracts from ebird
- Get stop information out for the ebird checklists from before the switch that do list stop information in the notes for each bird species
- Update the process for identifying the main observer
- adding a function to the package to merge the mbbs_county datasets
- adding information to the datasets to mention the change to stop level information after 2019, therefore also adding a function to the package to merge to route-level information? 
- fix ebird duplication if that's still an issue
- updating packages in the lockfile