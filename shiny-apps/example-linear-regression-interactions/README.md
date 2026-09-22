# Interactions in Linear Regression

This local Shiny app extends the workshop example in `example-linear-regression-interactions.qmd`.

## Purpose

An interaction allows the association between study hours and predicted scores to have a different slope for each study method. The app lets you compare that model with one that requires parallel slopes, change the centering value, and inspect predictions at a selected number of study hours.

## Requirements

- R
- The `shiny` package

Install `shiny` in your usual R environment if it is not already available. The app does not install packages itself.

## Run Locally

1. Extract the complete ZIP archive.
2. Open R in the extracted `example-linear-regression-interactions` folder.
3. Run:

```r
shiny::runApp()
```

The app runs entirely on your computer. It does not require credentials, upload data, or contact an external service.

## Controls and Outputs

- **Model specification** compares different fitted slopes with parallel fitted slopes.
- **Center study hours at** changes the point at which the intercept and method coefficient are interpreted. With the interaction model, it does not change fitted predictions.
- **Compare methods at this many hours** updates the side-by-side predicted scores.
- **Show the synthetic student data** adds the observations behind the fitted lines.

The app displays fitted lines, group-specific hourly slopes, selected-hour predictions, and coefficient estimates.

## Data and Limitations

The app recreates the synthetic dataset from the source page with seed 2026. Its default interaction model reproduces the page's coefficients and predictions.

The fitted relationships are teaching examples, not evidence that either study method causes score changes. Predictions describe fitted mean scores for the synthetic data, not guaranteed individual outcomes. The app omits inferential details such as confidence intervals so it can stay focused on interpreting the interaction and centering.
