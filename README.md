# eChem Package

This package includes functions to simulate four different types of electrochemistry experiments: two potential step experiments (chronoamperometry and chronocoulometry), and two potential scan experiments (linear-sweep voltammetry and cyclic voltammetry). Each simluation allows for an initial oxidation reaction or an initial reduction reaction, and allows for a single preceding or a single following chemical step, where Z is a non-electroactive species.

## How Are The Simulations Accomplished?

The linear sweep voltammetry, cyclic voltammetry, and chronoamperomety simulations in this package use the explicit finite difference computational method outlined in Gosser, D. K. *Cyclic Voltammetry Simulation and Analysis of Reaction Mechanisms*, VCH, New York, 1993, and in Brown, J. H. "Development and Use of a Cyclic Voltammetry Simulator to Introduce Undergraduate Students to Electrochemical Simulations" *J. Chem. Educ.*, **2015**, *92*, 1490--1496; chronocoulometry simulations are completed by integrating the result of the corresponding chronoamperometry experiment. Although Gosser's and Brown's treatements are developed to simulate cyclic voltammetry experiments, their approach is easy to generalize to other diffusion-controlled electrochemistry experiments.

## How Are Functions Named?

The functions in this package take the general form `actionExperiment`, where `action` explains what the function does and `Experiment` indicates the specific electrochemistry experiment. The available actions are `simulate`, `sample`, `plot`, `annotate`, and `animate`; the experiments are identified as `CA`, for chronoamperometry, `CC` for chronocoulometry, `CV` for cyclic voltammetry, and `LSV` for linear sweep voltammetry.

## Where Can I Obtain More Information?

The vignette "Computational Details" explains how the simulations are completed and provides information on the accuracy of the simulations. The vignette "Using the eChem Package" explains how to use the package's functions to simulate and to visualize electrochemical experiments. Finally, the vignette "Additional Examples" provides detailed examples of how to use the package's functions.

