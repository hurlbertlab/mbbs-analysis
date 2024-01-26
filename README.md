# mbbs-analysis

THINGS TO FIX:
-check issues tab 

things to talk about at meeting with Bradley:
- modeling
- GEE 
- Bayesian
- Half or quarter routes vs full route modeling, differences in analysis
- Percent change per year is the raw output?
   - - It is transformed, exponentiated - 1.05, -1, .05 increase per year
- Using NDVI of forest pixels to model forest complexity
- Urbanization as % change along routes, vs ?? also having a measure of how urbanized the route is overall either before or after? What's the reccomended way to model this, or do we not need to model that some routes start much more urbanized than others and might not change at all. 
    -- two different questions, impervious cover vs abundance, and how does change in pervious cover relate to change in abundance. Do you include the baseline or not? Bradley will recommend a book. 
https://www.amazon.com/Analysis-Longitudinal-Oxford-Statistical-Science/dp/0199676755
Chapter 5 or 6, GLM/GEE 
    auto-regressive component, build in an idea of temporal auto-correlation, year is not independent of year before (ie: ar1 call in gee model). Look up some examples of how to add into model
	- one way to factor out the baseline is difference modeling, assume independence of differences from y2-y1, y4-y3

- New way of modeling observer, based on mbbs meeting and talking with folks, primary observer will be assigned to whomever of the first three observers has the highest rank
      - unique ID for combos of observers

- When filtering for the minimum number of sightings on the minimum number of routes, if the requirements are met do ALL routes with sightings stay in the analysis? Or only those routes with ie: greater than 10 years of observations
     - all stay in for species info, remove for when we’re doing a route-specific analysis
- Add in zeros for the species that are not detected when a route is run. 
	- This is IN BRADLEY’S CODE nesting(), then has an integrity check 
SpeciesElm-codegen.R, look at the data manipulation section and see about moving things upstream

Upcoming mbbs updates:
- Exacting stop_num in a way that's less fragile, and captures the missing extracts from ebird
    - need at least a two step approach 
- Get stop information out for the ebird checklists from before the switch that do list stop information in the notes for each bird species
- Update the process for identifying the main observer
- adding a function to the package to merge the mbbs_county datasets
  - - merge the datasets automatically
- adding information to the datasets to mention the change to stop level information after 2019, therefore also adding a function to the package to merge to route-level information? This is issue #2 on the github
- fix ebird duplication if that's still an issue
- updating packages in the lockfile? - Nah, leave it. 
- merge the datasets 
- move the ‘hawk sp.’ ‘duck sp.’ up a lot earlier