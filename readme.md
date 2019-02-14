SPRE
================

Introduction to SPRE
--------------------

The methodology is first proposed by Dr. Deborah Weissman-Miller, which provides a new statistical method for occupational therapy data that has both internal and external validity.

A more detailed description of the methodology can be found in the following references:

\[1\] Weissman-Miller, D., Shotwell, M.P. & Miller, R.J. (2012). New single-subject and small-n design in occupational therapy: Application to weight loss in obesity. American Journal of Occupational Therapy, 66, 455-462.

\[2\] Weissman-Miller, D. (2013). Novel point estimation from a Semiparametric Ratio Estimator (SPRE): Long-term health outcomes from short-term linear data, with application to weight loss in obesity. International Journal of Biostatistics, 9(2): 175-184.

\[3\] Weissman-Miller, D., & Graham, K. C. (2015). Novel scale development for fear of falling and falls: Analysed using a Semiparametric Ratio Estimator (SPRE). International Journal of Statistics and Probability, 4(3), 161.

\[4\] Weissman-Miller, D. (2016). On predicting survival in prostate cancer: using an extended maximum spacing method at the change point of the semiparametric ratio estimator (SPRE). International Journal of Statistics and Probability, 5(2), 19.

\[5\] Weissman-Miller, D., Miller, R. J., & Shotwell, M. P. (2017). Translational Research for Occupational Therapy: Using SPRE in Hippotherapy for Children with Developmental Disabilities. Occupational therapy international, 2017.

About this R package
--------------------

This R package is revised based on the original R scripts found in the github repositories of the original author:

[Debbiewm5/SPRE.R](https://github.com/Debbiewm5/SPRE.R)

[Debbiewm5/SPRE.CODE](https://github.com/Debbiewm5/SPRE.CODE)

Download and install the package from github
--------------------------------------------

The whole process can be done in R. First check the availability of "devtools":

    if(!require("devtools"))install.packages("devtools")
    library(devtools)

After installing devtools, the SPRE package can be installed simply by running:

    install_github("ylgithubkaggle/SPRE.0.0.1")

Finally, everytime we need to use the package, we can load the package by running:

    library(SPRE)

View tutorial and help files
----------------------------

Please refer to the vignette for a tutorial on how to perform SPRE analysis in R. To view the vignette, we can do:

    vignette("SPRE")

There are two examples illustrating the use of SPRE package. The vignette can also be found here: [SPRE vignette](https://github.com/SPRE.0.0.1/vignettes_github/SPRE.md)

Besides the vignette, a help file can be loaded by running:

    help(package="SPRE")

Thank you for downloading and using the package! Please feel free to comment here if you have any question concerning the package.
