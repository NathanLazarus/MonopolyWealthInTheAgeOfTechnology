The code included here computes the probabilities from the Pólya urn displayed in Table 7.3, as well as
the simulations that underlie Figures 7.3-6 and Tables 7.4-6. The simulations were run at the UW-Madison
Center For High Throughput Computing (CHTC), and can be run on any computing cluster that uses HTCondor
for job scheduling (or you can create your own such cluster on any of the major cloud platforms). For
examples of submit description files, that may help clarify how the job submission takes place, see
https://research.cs.wisc.edu/htcondor/cookbook.html. You could also re-run all of the comparisons, or
perhaps a subset of them, on your local machine by calling “python NoOptCondorSurvival.py 1",
“python NoOptCondorSurvival.py 2", etc. for the numbers 1-500.

The code relies on NumPy and CasADi, which can be installed from the PyPI repository with the commands
“pip install numpy” and “pip install casadi”. For more information on including python packages with
HTCondor jobs, see https://chtc.cs.wisc.edu/uw-research-computing/python-jobs, A Dockerfile is included
in the folder “Chapter 7 RD Model/CondorUtilityComparison” if you, like me, want to choose the simplest
option of simply putting the packages into a Docker container.