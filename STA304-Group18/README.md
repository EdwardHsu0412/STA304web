# Social media and student GPA

An R Shiny app for the Fall 2026 STA304H5 survey by Group 18 at the University of Toronto Mississauga.

## Open the app locally

Extract the project folder if you downloaded the ZIP. Open `app.R` in RStudio and select **Run App**. The app uses Shiny and ggplot2. In an R environment with renv, run `renv::restore()` from this folder to restore the versions recorded in `renv.lock`.

In Windows RGui, run `shiny::runApp(choose.dir(), launch.browser = TRUE)` from the R Console and select the extracted `STA304-Group18` folder.

## Explore the survey

Choose a chart and student group in the sidebar. **Plot appearance** contains the palette and the settings for the current chart. Choose **Custom colours** to pick a colour for each group. **Analysis** shows the filtered results for total social media use, the regression, and its diagnostics. **About** describes the sample and its limitations.

## Data and analysis

- `data/responses.csv` contains the 64 responses used in the group's exploratory analysis. The exclusions match the EDA appendix and include all six workbook rows with yellow ID cells plus two additional rows. The exported data omit student IDs.
- Daily social media time adds short-form video, text-based platforms, and calls and messaging. The regression models those three activities individually alongside study time, time off social media before bed, and student status when both groups are present.
- The website calculates results from the supplied workbook. Its calls and messaging values differ from the EDA summary. For the full sample, the workbook gives a Pearson correlation of 0.004 (p = 0.974) and an overall regression p-value of 0.874. The EDA reported −0.008, 0.947, and 0.920, respectively. Both sets of results show little evidence of a linear association between total social media use and cGPA.

## Posit Connect Cloud

When the group is ready to publish, commit the app files as a unit in the course GitHub repository. On the free Connect Cloud plan, the repository must be public. In [Posit Connect Cloud](https://connect.posit.cloud/), select **Publish** and choose **Shiny**, then select the repository and branch with `app.R` as the primary file. The accompanying `renv.lock` defines the dependencies.

Open the deployed link in a private browser window and check all three charts, the status filter, and the regression. Add the public app URL to this README before submitting. [Posit’s GitHub deployment guide](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html#deploy-from-github)
