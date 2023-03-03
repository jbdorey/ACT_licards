# ACT_licards
## This repository is meant purely as a test of data from the Atlas of Living Australia.

## This repository contains the following:
  1. An R workflow document called "ACT_lizards_workflow.R" where the body of the script should be run
  2. An renv folder and lockfile with package information
  3. A folder that contains the input occurrence data called "lizardData" — https://doi.org/10.26197/ala.c6489d27-00a2-4428-bd72-ef523a6a9b82
  4. A folder with three functions for use with the workflow document
    (a) ColTypeR.R: Sets column types when reading in DarwinCore data
    (b) interactiveMapR.R: Builds and saves interactive .html maps to user specifications and taxa. This also includes a date-slider to explore changes in occurrences through time
    (c) ShinyMapR.R: Builds an interactive map for the user-specified taxa. This is very similar to (b) above, but a little prettier. However, these maps are not as easy to save and share
  5. A folder, "interactiveMaps", with the output of interactiveMapR to the family level and examining basisOfOccurrence
