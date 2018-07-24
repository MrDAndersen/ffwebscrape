# ffwebscrape

This package allows users to scrape projected stats from several sites that have
publicly available projections. Once data is scraped the use can then use functions
within the package to calculate projected points and produce rankings. The package
relies heavily on the vocabulary from the `tidyverse` and users will better be
able to use the package if they familiarize themselves with the `tidyverse` way 
of creating code.

The following sources are available for scraping:

* For seasonal data: CBS, ESPN, FantasyData, FantasyPros, FantasySharks, FFToday, 
FleaFlicker, NumberFire, Yahoo, FantasyFootballNerd, NFL, RTSports, Walterfootball
* For weekly data: CBS, ESPN, FantasyData, FantasyPros, FantasySharks, FFToday, 
FleaFlicker, NumberFire, Yahoo, FantasyFootballNerd, NFL

While the scrape functions allows the user to specify season and week, scraping
historical periods will not be successful.

## Projection sources
Projection sources are defined as `R6` classes and the `projection_sources` object
is a list containing the projection sources defined in the pacakge. Review the
`source_classes.R` file to see how these classes are defined and the `source_configs.R`
file in the `data-raw` directory has all the individual sources defined and running
that script will re-create the `projections_sources` object for the package

## Scraping data
THe main function for scraping data is `scrape_data`. This function will pull data
from the sources specified, for the positions specified in the season and week specificed.
To pull data for QBs, RBs, WRs, TEs and DSTs from CBS, ESPN and Yahoo for the 2018
season the user would run:
```
my_scrape <- scrape_data(src = c("CBS", "ESPN", "Yahoo"), 
                         pos = c("QB", "RB", "WR", "TE", "DST"),
                         season = 2018 week = 0)
```

`my_scrape` will be a list of tibbles, one for each positon scraped. 

