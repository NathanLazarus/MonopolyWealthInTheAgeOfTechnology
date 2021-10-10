This code generates the values used in Chapter 6, based off of the Compustat data at
https://wrds-www.wharton.upenn.edu/

Open it in RStudio by clicking the file Compustat.Rproj. The file master.R contains
all of the scripts and calls them all in order.

First, to be able to pull the data, you will need to create a file with your WRDS
account info called CompustatPassword.csv, as follows:

username,password
YOUR_USERNAME, YOUR_PASSWORD

The file Z1Adjustment.R, used to adjust the assets to their present values, requires
Python with the NumPy and CasADi packages. It just makes a call to the system python.
Install them by running "pip install numpy", "pip install casadi" from the command line.

The decomposition that replicates and extends De Loecker et al.'s results uses Stata,
and should be compatible with any recent version, though it's only been tested back
to Stata 14.