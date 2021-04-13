
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- PROJECT LOGO -->

<br />

<p align="center">

<a href="www.google.it">
<img src="docs/reference/figures/BAEDA-logo-dashboard.png" alt="Logo" width="508" height="120">
</a>

<h3 align="center">

<i> Student Version </i>

</h3>

<a align="center">
href=“<https://github.com/baeda-polito/BAEDA_DASHBOARD_STUDENTS>”\><strong>Explore
the docs »</strong></a> <br />

<p align="center">

<a href="https://roberto-chiosa.shinyapps.io/BAEDA_DASHBOARD_STUDENTS/">View
Demo</a> ·
<a href="https://github.com/baeda-polito/BAEDA_DASHBOARD_STUDENTS/issues">Report
Bug</a> ·
<a href="https://github.com/baeda-polito/BAEDA_DASHBOARD_STUDENTS/issues">Request
Feature</a>

</p>

</p>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<!-- TABLE OF CONTENTS -->

## Table of Contents

  - [About the Project](#about-the-project)
  - [Getting Started](#getting-started)
      - [Online](#online)
      - [Local](#local)
  - [Contributing](#contributing)
  - [Acknowledgements](#acknowledgements)

<!-- ABOUT THE PROJECT -->

## About The Project: the package eDASH

This project consists in the design and development of interactive
dashboards for the visualization of energy data. In particular, we
propose to interact with the data in a deeper way by modifying the
hyperparameters of the code directly from the UI, giving the user great
flexibility in the exploration. In addition, we created an “Advanced”
section through which the users can perform advanced data analysis
workflows, ranging from daily load profile clustering to Adaptive
Symbolic Approximation (ASAX).

<!-- GETTING STARTED -->

## Getting Started

### Online

From this
[link](https://roberto-chiosa.shinyapps.io/BAEDA_DASHBOARD_STUDENTS/) it
is possible to access and use the dashboard online without having R or
Rstudio installed on your computer. Thanks to
[shinyapps.io](https://www.shinyapps.io/) the whole application is
hosted online and accessible without any requirements other than an
internet connection.

> :warning: **Warning**: Since the online hosting is performed through
> the free account, a limit of 25 active hours is set from the server.
> If this limit is exceded the app will not be available untill the next
> month cycle. Therefore, we suggest to exploit the dashboard
> functionalities by cloning the repository on your local machines as
> explained in the following.

### Local

First of all, be sure to have the latest version of
[RStudio](https://rstudio.com/products/rstudio/) installed. To run the
dashboard locally you need to clone the GitHub repository on your
computer. Follow these steps:

1.  In RStudio, start a new Project by following this path *“File \> New
    Project \> Version Control \> Git”*;
2.  In *“Repository URL”*, paste this URL
    `https://github.com/baeda-polito/BAEDA_DASHBOARD_STUDENTS`;
3.  Accept the default *“Project directory name”* which coincides with
    the GitHub repo name `BAEDA_DASHBOARD_STUDENTS`;
4.  Chose a local path to save the repository on your computer;
5.  Click *“Create Project”*.

<!-- CONTRIBUTING -->

## Contributing

Any contributions you make are **greatly appreciated**. If you want to
propose a feature or report a bug please open a new
[issue](https://github.com/baeda-polito/BAEDA_DASHBOARD_STUDENTS/issues).
If you want to contribute to the code, follow these steps:

1.  Fork the Project
2.  Create your Feature Branch, called for example `MYBRANCH` (`git
    checkout -b feature/MYBRANCH`);
3.  Commit your Changes (`git commit -m 'Add some AmazingFeature'`);
4.  Push to the Branch (`git push origin feature/MYBRANCH`);
5.  Open a Pull Request.

<!-- ACKNOWLEDGEMENTS -->

## Acknowledgements

  - [RStudio](https://rstudio.com/)
  - [Shiny](https://shiny.rstudio.com/)
  - [shinyapps.io](https://www.shinyapps.io/)
  - [Shinydashboard](https://rstudio.github.io/shinydashboard/)

## Installation

You can install the released version of eDASH from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("eDASH")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/baeda-polito/dashboard-student")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(eDASH)
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

<!-- MARKDOWN LINKS & IMAGES -->

<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
