#' Test that we can parse data from FORECAST/ADVISORY products

#' Keys
#' Keys are the unique identifiers for each storm
#' Requires the time information to extract year where some keys are 6 digits
x <- c('\nNWS NATIONAL HURRICANE CENTER MIAMI FL       AL042016\n0900 UTC TUE JUN 21 2016 \n', 
       '\nNWS NATIONAL HURRICANE CENTER MIAMI FL AL0198\n0900 UTC TUE JUN 21 1998 \n')

context('Key')
test_that('Get Keys', {
  expect_identical(get_key(x[1]), 'AL042016')
  expect_identical(get_key(x[2]), 'AL011998')
})

# Name
# Get storm name from header
x <- c('\nTROPICAL DEPRESSOIN HERMINE FORECAST/ADVISORY NUMBER 1\n', # Typo
       '\nTROPICAL STORM ARLENE FORECAST/ADVISORY NUMBER   7...CORRECTED\n', 
       '\nHURRICANE GASTON FORECAST/ADVISORY NUMBER  26\n', 
       '\nREMNANTS OF DANIELLE FORECAST/ADVISORY NUMBER   7\n', 
       '\nSUBTROPICAL STORM EARLI FORECAST/ADVISORY NUMBER   39\n', 
       '\nSUB-TROPICAL STORM CELIA FORECAST/ADVISORY NUMBER   2\n', 
       '\nSUB TROPICAL STORM FRANCES FORECAST/ADVISORY NUMBER   21\n', 
       '\nEXTRATROPICAL STORM ORLENE FORECAST/ADVISORY NUMBER   13\n', 
       '\nEXTRA-TROPICAL STORM ULIKA FORECAST/ADVISORY NUMBER 9\n', 
       '\nEXTRA TROPICAL STORM DOROTHY FORECAST/ADVISORY NUMBER 91\n')

context("Name")

test_that('Get Names', {
  expect_identical(get_storm_name(x[1]), 'HERMINE')
  expect_identical(get_storm_name(x[2]), 'ARLENE')
  expect_identical(get_storm_name(x[3]), 'GASTON')
  expect_identical(get_storm_name(x[4]), 'DANIELLE')
  expect_identical(get_storm_name(x[5]), 'EARLI')
  expect_identical(get_storm_name(x[6]), 'CELIA')
  expect_identical(get_storm_name(x[7]), 'FRANCES')
  expect_identical(get_storm_name(x[8]), 'ORLENE')
  expect_identical(get_storm_name(x[9]), 'ULIKA')
  expect_identical(get_storm_name(x[10]), 'DOROTHY')
})

context('Getting advisory numbers')

test_that('Get Advisory Number', {
  expect_identical(fstadv_adv_num(x[1]), 1)
  expect_identical(fstadv_adv_num(x[2]), 7)
  expect_identical(fstadv_adv_num(x[3]), 26)
  expect_identical(fstadv_adv_num(x[4]), 7)
  expect_identical(fstadv_adv_num(x[5]), 39)
  expect_identical(fstadv_adv_num(x[6]), 2)
  expect_identical(fstadv_adv_num(x[7]), 21)
  expect_identical(fstadv_adv_num(x[8]), 13)
  expect_identical(fstadv_adv_num(x[9]), 9)
  expect_identical(fstadv_adv_num(x[10]), 91)
})

# Status
context('Status')
test_that('Status', {
  expect_identical(fstadv_status(x[1]), 'TROPICAL DEPRESSOIN')
  expect_identical(fstadv_status(x[2]), 'TROPICAL STORM')
  expect_identical(fstadv_status(x[3]), 'HURRICANE')
  expect_identical(fstadv_status(x[4]), 'REMNANTS OF')
  expect_identical(fstadv_status(x[5]), 'SUBTROPICAL STORM')
  expect_identical(fstadv_status(x[6]), 'SUB-TROPICAL STORM')
  expect_identical(fstadv_status(x[7]), 'SUB TROPICAL STORM')
  expect_identical(fstadv_status(x[8]), 'EXTRATROPICAL STORM')
  expect_identical(fstadv_status(x[9]), 'EXTRA-TROPICAL STORM')
  expect_identical(fstadv_status(x[10]), 'EXTRA TROPICAL STORM')
})

# Observation Dates
context('Observation Dates')
x <- c('\n0900 UTC TUE JUN 21 2016\n')

test_that('ObDate', {
  expect_identical(get_time_header(x[1]), 
                   lubridate::ymd_hms('2016-06-21 09:00:00'))
})

# Latitude and Longitude
x <- c('HURRICANE CENTER LOCATED NEAR 13.5N  71.6W AT 30/2100Z', 
       'TROPICAL STORM CENTER LOCATED NEAR 13.5S  71.6E AT 30/2100Z', 
       'SUB TROPICAL STORM CENTER LOCATED NEAR 13.5N 171.6W AT 30/2100Z', 
       'HOW MANY WORDS CAN I DO CENTER LOCATED NEAR 13.5S 171.6E AT 30/2100Z')

context('Latitude and Longitude')
test_that('Latitude and Longitude', {
  # Latitude
  expect_identical(fstadv_lat(x[1]), 13.5)
  expect_identical(fstadv_lat(x[2]), -13.5)
  expect_identical(fstadv_lat(x[3]), 13.5)
  expect_identical(fstadv_lat(x[4]), -13.5)
  # Longitude
  expect_identical(fstadv_lon(x[1]), -71.6)
  expect_identical(fstadv_lon(x[2]), 71.6)
  expect_identical(fstadv_lon(x[3]), -171.6)
  expect_identical(fstadv_lon(x[4]), 171.6)
})

# Position Accuracy
x <- c('POSITION ACCURATE WITHIN  20 NM')
context('Position Accuracy')
test_that('Position Accuracy', {
  expect_identical(fstadv_pos_accuracy(x[1]), 20)
})

# Forward Movement 
x <- c('PRESENT MOVEMENT TOWARD THE WEST-SOUTHWEST OR 255 DEGREES AT   8 KT')
context('Forward Movement')
test_that('Forward Movement', {
  expect_identical(fstadv_fwd_dir(x[1]), 255)
  expect_identical(fstadv_fwd_speed(x[1]), 8)
})

# Barometric Pressure 
x <- c('ESTIMATED MINIMUM CENTRAL PRESSURE  949 MB', 
       'MINIMUM CENTRAL PRESSURE  949 MB')
context('Barometric Pressure')
test_that('Barometric Pressure', {
  expect_identical(fstadv_pressure(x[1]), 949)
  expect_identical(fstadv_pressure(x[2]), 949)
})

# Eye
x <- c('EYE DIAMETER  15 NM', 
       '')
context('Eye')
test_that('Eye', {
  expect_identical(fstadv_eye(x[1]), 15)
  expect_equal(fstadv_eye(x[2]), NA_integer_)
})

# Winds and Gusts
x <- c('MAX SUSTAINED WINDS 120 KT WITH GUSTS TO 145 KT.', 
       'MAX SUSTAINED WINDS  20 KT WITH GUSTS TO 145 KT', 
       'MAX SUSTAINED WINDS 120 KT WITH GUSTS TO 145 KT', 
       'MAX SUSTAINED WINDS 120 KT WITH GUSTS TO  45 KT')
context('Winds and Gusts')
test_that('Winds and Gusts', {
  expect_identical(fstadv_winds(x[1]), 120)
  expect_identical(fstadv_winds(x[2]), 20)
  expect_identical(fstadv_gusts(x[3]), 145)
  expect_identical(fstadv_gusts(x[4]), 45)
})