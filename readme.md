SPRE
================

Download and install the package from github
--------------------------------------------

The whole process can be done in R. First check the availability of "devtools":

    if(!require("devtools"))install.packages("devtools")
    library(devtools)

After installing devtools, the SPRE package can be installed simply by running:

    install_github("ylgithubkaggle/SPRE0.0.1")

Finally, everytime we need to use the package, we can load the package by running:

    library(SPRE)

View tutorial and help files
----------------------------

Please refer to the vignette for a tutorial on how to perform SPRE analysis in R. To view the vignette, we can do:

    vignette("SPRE")

There are two examples illustrating the use of SPRE package.

Besides the vignette, a help file can be loaded by running:

    help(package="SPRE")

Thank you for downloading and using the package! Please feel free to comment here if you have any question concerning the package.
