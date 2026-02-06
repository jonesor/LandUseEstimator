# LandUseEstimator

Shiny app that estimates land use proportions around input locations using the CORINE raster.

## Quick Start
1. Open the project in RStudio (or set your working directory to this repo).
2. Install dependencies if needed:
```r
install.packages(c("shiny","ggplot2","magrittr","dplyr","tidyr","ggthemes","raster","sf"))
```
3. Run the app:
```r
shiny::runApp()
```

## Input CSV Format
Your CSV must contain the following columns:
- `addressID` (unique identifier per row)
- `longitude` (numeric, WGS84)
- `latitude` (numeric, WGS84)

Example file: `sample_data/sample_points.csv`

## Notes
- Buffer is in meters.
- The app filters out “Ocean” and “Water bodies” land-use classes for summaries and plots.
