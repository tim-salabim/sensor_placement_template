# sensor_placement_template

As the name implies, this repo serves as a template for an exercise to locate 
areas for field sensor placement to monitor soil & plant water availability.
This exercisee is part of the Msc course "Advances Climate Data Analysis" at the 
Deparment of Geography, Friedrich Alexanader Universität, Erlangen-Nürnberg.

The aim is to utilise Sentinel 2 - L2A based vegetation and soil indices to derive
optimal locations for placing soil moisture sensors within an agricultural field.

The workflow is roughly as follows:

1. draw field boundary using `mapedit` - [01_generate_field_boundary](https://github.com/tim-salabim/sensor_placement_template/blob/main/01_generate_field_boundary.R)
2. download sentinel data for the field - [02_download_sentinel_data](https://github.com/tim-salabim/sensor_placement_template/blob/main/02_download_sentinel_data.R)
3. calculate and analyse the indices - [03_indices](https://github.com/tim-salabim/sensor_placement_template/blob/main/03_indices.R)

In this template, we showcase how the workflow may look like using NDVI as the 
classic index example and kmeans as the classifier. However, in order to find an 
optimal spot for the sensor(s), we may want to include a few other indices. 
For a comprehensive list of potential spectral index candidates, please see 
[Awesome Spectral Indices](https://github.com/awesome-spectral-indices/awesome-spectral-indices).