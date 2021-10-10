This folder contains the code that runs the simulations in Chapter 2. By default, it
computes fourth order perturbation methods coefficients using the Mathematica notebook
"Model2VariableUtilityFunctions.nb", which are stored in the folder "MathematicaCoefs".

If you don't have Mathematica, or would rather compute the coefficients more quickly,
I've also included Schmitt-Grohe and Uribe's Matlab code that can compute coefficients
up to second order. Simply change "use_SchmittGrohe_Uribe_Matlab_code_to_find_coefficients"
from "false" to "true" in model2_run.m.

To get the impulse response functions, use CallModel2IRFs.m, and to get the simulations,
use CallModel2WithMultipleSpecifications.m.