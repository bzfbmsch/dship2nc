# dship2nc
Fortran routine to convert dship formatted underway data to netcdf format

This routine is very preliminary and tested only with BSH-dhip data.

You need to link with the netcdf library.

Note, you need the following input files for a data set, usually delivered by the dship system.
- the datafile.txt file describing the data set content. Each variable needs a description like follows:
Channel 1
   Device      = "Fluorometer"
   Sensor      = "Concentration"
   Format      = "Real"
   Length      = 14
   Precision   = 5
   Phys. Unit  = "µg/l"
   Calculation = "None"
   Delivered values = Spot 
- the datafile.dat file containing the data values in table format:
  - in the beginning an additional time coordinate in the form DD.MM.YYYY\tHH:mm:SS 
  - followed by the tab-delimited variable values
  
- a file named files.cmd with the following structure:
Underway measurements M. S. Merian
processed with dship2nc by Martin Schmidt, 2015-jun-03
raw/Merian_2009
raw/Merian_2010
raw/Merian_2011
raw/Merian_2012
raw/Merian_2013

