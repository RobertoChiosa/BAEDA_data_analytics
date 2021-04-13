
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- PROJECT LOGO -->

<center>

![your image
caption](https://github.com/baeda-polito/dashboard-student/docs/reference/figures/BAEDA-logo-dashboard.png)

<p>

<a href="https://github.com/baeda-polito/dashboard-student"><strong>Explore
the docs » </strong></a> <br>
<a href="https://github.com/baeda-polito/dashboard-student/">View
Demo</a> ·
<a href="https://github.com/baeda-polito/dashboard-student/issues">Report
Bug</a> ·
<a href="https://github.com/baeda-polito/dashboard-student/issues">Request
Feature</a>

</p>

</center>

<!-- badges: start -->

<center>

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

</center>

<!-- badges: end -->

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

<!-- TABLE OF CONTENTS -->

## Table of Contents

  - [Getting Started](#getting-started)
      - [Online (shinyapps.io)](#online-\(shinyapps.io\))
      - [Online (AWS)](#online-\(aws\))
      - [Local (Version Control)](#local-\(version-control\))
      - [Local (Package version)](#local-\(package-version\))
  - [Contributing](#contributing)
  - [Author](#author)
  - [Acknowledgements](#acknowledgements)

<!-- GETTING STARTED -->

## Getting Started

### Online (shinyapps.io)

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

### Online (AWS)

From this
[link](https://roberto-chiosa.shinyapps.io/BAEDA_DASHBOARD_STUDENTS/) it
is possible to access and use the dashboard online without having R or
Rstudio installed on your computer. Thanks to [Amazon Web
Services](https://aws.amazon.com/it/) the whole application is hosted on
a virtual machine (EC2) accessible without any requirements other than
an internet connection.

### Local (Version Control)

This kind of installation is suitable for developers who have the
permission to download and modify the code. The process described allows
you to get the whole GitHub repository on your computer.

Be sure to have the latest version of
[RStudio](https://rstudio.com/products/rstudio/) installed, then follow
these steps:

1.  In RStudio, start a new Project by following this path *“File \> New
    Project \> Version Control \> Git”*;
2.  In *“Repository URL”*, paste this URL
    `https://github.com/baeda-polito/dashboard-student`;
3.  Accept the default *“Project directory name”* which coincides with
    the GitHub repo name `dashboard-student`;
4.  Chose a local path to save the repository on your computer;
5.  Click *“Create Project”*.

### Local (Package version)

This kind of installation is suitable for those who have the permission
to download the code and want to run the dashboard locally as if it was
a R package.

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/baeda-polito/dashboard-student")
```

Then you can run the application by simply typing:

``` r
library("eDASH")
eDASH::run_app()
```

<!-- CONTRIBUTING -->

## Contributing

Any contributions you make are **greatly appreciated**. If you want to
propose a feature or report a bug please open a new
[issue](https://github.com/baeda-polito/dashboard-student/issues). If
you want to contribute to the code, follow these steps:

1.  Fork the Project
2.  Create your Feature Branch, called for example `MYBRANCH` (`git
    checkout -b feature/MYBRANCH`);
3.  Commit your Changes (`git commit -m 'Add some AmazingFeature'`);
4.  Push to the Branch (`git push origin feature/MYBRANCH`);
5.  Open a Pull Request.

<!-- AUTHORS AND CONTRIBUTORS -->

## Author

  - [Roberto Chiosa]()

<!-- ACKNOWLEDGEMENTS -->

## Acknowledgements

  - [RStudio](https://rstudio.com/)
  - [Shiny](https://shiny.rstudio.com/)
  - [shinyapps.io](https://www.shinyapps.io/)
  - [Shinydashboard](https://rstudio.github.io/shinydashboard/)

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

<!-- MARKDOWN LINKS & IMAGES -->

<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
