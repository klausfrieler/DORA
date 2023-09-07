# DORA - Development of Rhythmic Ability
This RStudio project contains raw and derived data as well as analysis scripts for the **DORA** project by **Johanna Will**  (Max Planck Institute for Empirical Aesthetics, Frankfurt/M) and collaborators. 

The DORA projects investigates rhythm production abilities of children in the age range 5 to 8. It consist of two related studies, one dealing with production of isochronous rhythms and and one with reproduction of more complex rhythmical patterns. Currently, this README only covers the isochroous part, the rhythm reproduction part is soon to follow.

### How to use the repository 
 * Cloning the GitHub repository into a suitable directory.
 * Open the projec file ``DORA.RProj`` with RStudio. 
 * Source the  file ``read_data.R``
 * Run ``setup_workspace(reread = "none")``
 * The raw data will appear in a data frame ``iso_data``, the derived features in the data frame ``iso_features``.
 * From there on, you are basically on your own. Please refer to paper for further information.
 * The raw data is in the directory ``/data/iso`` as compiled RDS files and as a largish set of files with  extracted onsets (CSV format). The file names encodes the most relevant information: experimenter, age group (5, 6, 7, 8, or e = adults), participant id (running number), trial id (running number), source (``ex``-perimenter or ``pa``-rticipant), tempo-condition (``fa``-st or ``sl``-ow), and experimental setting (``ac``-oustic or ``so``-cial). Same information can be found in ``iso_features`` data frame.
 

### Requirements
RStudio, R, packages *tidyverse*, *circular* *dtw*.

### Authors and maintainer
Klaus Frieler & Johanna Will  
Max Planck Institute for Empirical Aesthetics  
Grüneburgweg 14  
60322 Frankfurt/Main  
klaus.frieler@ae.mpg.de
