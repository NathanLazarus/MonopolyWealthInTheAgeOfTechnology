# set base image (host OS)
FROM python:3.9-slim-buster

# set the working directory in the container
WORKDIR /code

COPY NoOptCondorUtilities.py .
COPY NoOptCondorSurvival.py .
COPY NoOptCondorSurvivalInvPowersOf2.py .
COPY NoOptCondorSurvivalInvGrid.py .
COPY NoOptCondorUtilitiesAndSurvival.py .
COPY helperfunctions_noopt.py .
# COPY requirements.txt .
# RUN pip install --no-cache-dir -r requirements.txt
RUN pip install --no-cache-dir numpy
RUN pip install --no-cache-dir casadi