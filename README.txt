This folder contains the R code and data files associated with the publication: 
Lois S. Taylor, Adrian Gonzalez, Michael E. Essington, Scott C. Lenaghan, C. Neal Stewart, Amy Z. Mundorff, Dawnie W. Steadman, and Jennifer M. DeBruyn 
"Soil elemental changes during human decomposition" 

Elemental_analysis_ICP_D3.R contains the code to run/create Kruskal-Wallace tests, boxplots, T-tests, PCA ordination, charge line graphs, correlation matrix, heatmap, and X-ray diffraction graphs

The following data files are required:
ICP_D3.csv
ICP_D3_charge.csv
ICP_D3_corr_noCon.csv
ICP_heatmap_wide.csv
XRD.csv

ICP_D3.csv is the original raw data file. This file is used for all statistical tests, and to construct boxplots, and the PCA diagram. 
Sample metadata and variables:
SampleID: this is the sample name. There are a total of 6 samples taken at each daily time point, three decomposition-impacted samples and three controls.
Date: this is the study date in 5-digit originating from Excel's conversion from dd-mm-yyyy to a numeric format.
Study-day: study day begins with day 0 on the first sampling date
ADH: accumulated degree hours calculated from the sum of hourly temperature data, with a baseline of 0 degrees C.
ADD: accumulated degree hours calculated from the sum of daily mean temperatures, with a baseline of 0 degrees C.
ARF_sector: this is the internal field locator for donors at the research site.
Sorting1: this field is a data-sorting field that splits controls from impacted samples, and inserts letters preceding values for study date for ease of graphical manipulation.
Sorting: this field is a data-sorting field that differentiates data by study date and inserts letters preceding values for study date for ease of graphical manipulation.
Treatment: D3_con denotes control samples, and D3_grave denotes decomposition-impacted samples.
pH: pH of each sample
EC: electrical conductivity of each sample (µS/cm).
Na, K, Mg, Ca, Al, Cu, Fe, Mn, Zn, Co, S180, P, B, Se: elemental concentrations of samples (µg/gdw soil).

ICP_D3_charge.csv is the data file containing charge concentrations. This file is used to construct the charge line graphs. 
Sample metadata and variables:
SampleID: this is the sample name. There are a total of 6 samples taken at each daily time point, three decomposition-impacted samples and three controls.
Date: this is the study date in 5-digit originating from Excel's conversion from dd-mm-yyyy to a numeric format.
Study-day: study day begins with day 0 on the first sampling date
ADH: accumulated degree hours calculated from the sum of hourly temperature data, with a baseline of 0 degrees C.
ADD: accumulated degree hours calculated from the sum of daily mean temperatures, with a baseline of 0 degrees C.
ARF_sector: this is the internal field locator for donors at the research site.
Sorting1: this field is a data-sorting field that splits controls from impacted samples, and inserts letters preceding values for study date for ease of graphical manipulation.
Sorting: this field is a data-sorting field that differentiates data by study date and inserts letters preceding values for study date for ease of graphical manipulation.
Treatment: D3_con denotes control samples, and D3_grave denotes decomposition-impacted samples.
pH: pH of each sample
EC: electrical conductivity of each sample (µS/cm).
Na, K, Mg, Ca, Al, Cu, Fe, Mn, Zn, Co, S180, P, B, Se: elemental concentrations of samples (µg/gdw soil).
Na (umolcharge/gdw soil), K (umolcharge/gdw soil), Mg (umolcharge/gdw soil), Ca (umolcharge/gdw soil): field denote charge concentrations.
 
ICP_D3_corr_noCon.csv is a subset of the ICP_D3 database in which controls and metadata have been removed. This file is used to construct the correlation matrix.
Sample metadata and variables:
pH: pH of each sample
EC: electrical conductivity of each sample (µS/cm).
Na, K, Mg, Ca, Al, Cu, Fe, Mn, Zn, Co, S180, P, B, Se: elemental concentrations of samples (µg/gdw soil).

ICP_heatmap_wide.csv is a means table of decomposition-impacted samples (no controls), sorted by study day. This file is used to construct the heatmap.
Sample metadata and variables:
Columns are study date.
Row values consist of means of impacted soils, calculated from technical replicates at each study date.

XRD.csv is a data table containing x-ray diffraction data. This file is used to construct the x-ray diffraction graphs.
Sample metadata and variables:
2-Theta: two times the angle of x-ray incidence
SP_K_room: x-ray diffraction intensity for room-temperature K-saturated clay fraction (site A)
SP_Mg: x-ray diffraction intensity for Mg-saturated clay fraction (site A)
SP_K_300: x-ray diffraction intensity for K-saturated clay fraction heated to 300 degrees C (site A)
SP_K_550: x-ray diffraction intensity for K-saturated clay fraction heated to 550 degrees C (site A)
SP_Mg_gly: x-ray diffraction intensity for Mg-glycol saturated clay fraction (site A)
WIN_K_room: x-ray diffraction intensity for room-temperature K-saturated clay fraction (site B)
WIN_Mg: x-ray diffraction intensity for Mg-saturated clay fraction (site B)
WIN_K_300: x-ray diffraction intensity for K-saturated clay fraction heated to 300 degrees C (site B)
WIN_K_550: x-ray diffraction intensity for K-saturated clay fraction heated to 550 degrees C (site B)
WIN_Mg_gly: x-ray diffraction intensity for Mg-glycol saturated clay fraction (site B)


