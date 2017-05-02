[![Stories in Ready](https://badge.waffle.io/timtrice/Hurricanes.png?label=ready&title=Ready)](https://waffle.io/timtrice/Hurricanes)
# Hurricanes

Hurricanes is a project to record real-time and archived observations for past and active storms. The idea is to collect as much information as possible: advisories, reconnaissance, watches and warnings, forecasts, sea and wind data and surface observations. 

Data is limited to Atlantic and east Pacific only and for years since 1998. Plans to implement central Pacific will begin once this moves to beta.

## Different Types of Products

The top four products listed below are issued every six hours. Public Advisory's are issued every three hours when watches/warnings are in effect. They may be issued every two hours if a storm impact is imminent. 

The remaining products (except the now deprecated strike probabilities) can be issued at any time. 

### Forecast/Advisory
Forecast/Advisory products contain the meat of data this project requires. It not only contains current basic info (lat, lon, wind, pressure) but also other data such as forward movement and speed, wind field, forecasts, eye size, etc. Plus it is in a somewhat-easy-to-read format. 

## Public Advisory
Public Advisory's are more people-friendly with some details of the FORECAST/ADVISORY but is more general information on current threats. Public Advisory's are issued more frequently when watches and warnings are in effect.

## Discussions
Discussions are text products from the NHC detailing the current conditions of the storm as well as forecasts. Technical information such as satellite signature can be found here

### Wind Speed Probabilities
The chance of a certain area seeing a certain speed of wind. 

### Strike Probabilities
This was older-school wind speed probabilities that listed the chances of a location actually seeing a landfall. Deprecated.

### Updates
Updates are generally issued as brief statements at any time when a storm is making landfall OR if the storm is undergoing a change that requires immediate attention.

### Position Estimates
Generally issued as storms are making landfall. These are issued more frequently than Public Advisory's when a storm is threatening but are generally not found in the archives.

## FAQ

1. Why is Adv and Date listed in every dataframe?

There are *potentially* cases where Adv may be the same but ObDate is different. For example, if an advisory was recently issued (within the hour) and a surface observation pushes the NHC to upgrade a storm or reissue the advisory. There is an assumption the advisory number could remain the same but the ObDate would be different. It is more a precaution than anything else.
