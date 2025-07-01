> [!IMPORTANT]
> Information on using GeoPressureTemplate are now provided in the [GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-intro.html)

# Lanius collurio GeopressureR

## Description
Migration position estimates from ambient pressure data recorded with multisensor data loggers built in Lund for Red-backed Shrikes (Lanius collurio) breeding in Denmark.

Raw data is provided for the loggers' accelerometer and pressure sensors. 

Labelled data indicating migratory flights and stopovers is also provided.

With our type of data, GeopressureR uses entire hours of flight.

We considered one hour of flight in GeopressureR if the bird's accelerometer recorded at least 7 out of 12 values with accelerometer values > 4 (scale: 0 to 5) within one hour.

A minimum of 1 hour and 40 minutes of activity with values > 4 was needed to be considered a migratory flight (2 hours of flight in GeopressureR), otherwise the short flight is treated as different altitudes.
This is especially important during the spring migration, when many short flights take place.


This repository was generated based on [GeoPressureTemplate (v1.2)](https://github.com/Rafnuss/GeoPressureTemplate).
