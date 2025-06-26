> [!IMPORTANT]
> Information on using GeoPressureTemplate are now provided in the [GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-intro.html)

# Lanius collurio GeopressureR

## Description
Position estimates from ambient pressure data for red-backed shrikes breeding in Denmark.

Data is provided for the accelerometer and pressure sensor of the loggers. 

Labelled data indicating migratory flights is also provided.

With our type of data GeopressureR uses entire hours of flight.

We considered one hour of flight in GeopressureR if the bird accelerometer recorded at least 7 out of 12 values with acc > 4.

Minimum 1 hour 40 min of activity to be considered a migratory flight (100 activity; thus 2 hours of flight in geopressureR), 
otherwise treated as different altitudes.
Specially important for the spring migration, when many short flights took place.


This repository was generated based on [GeoPressureTemplate (v1.2)](https://github.com/Rafnuss/GeoPressureTemplate).
