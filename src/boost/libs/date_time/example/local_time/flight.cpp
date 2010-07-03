
#include "boost/date_time/local_time/local_time.hpp"
#include <iostream>

/* This example shows a program that calculates the arrival time of a plane
 * that flys from Phoenix to New York.  During the flight New York shifts
 * into daylight savings time (Phoenix doesn't because Arizona doesn't use
 * dst).  
 *
 * 
 */

int main()
{
  using namespace boost::gregorian; 
  using namespace boost::local_time;
  using namespace boost::posix_time;


  //setup some timezones for creating and adjusting local times
  //This user editable file can be found in libs/date_time/data.
  tz_database tz_db;
  tz_db.load_from_file("date_time_zonespec.csv");
  time_zone_ptr nyc_tz = tz_db.time_zone_from_region("America/New_York");
  //Use a 
  time_zone_ptr phx_tz(new posix_time_zone("MST-07:00:00"));

  //local departure time in phoenix is 11 pm on april 2 2005 
  // (ny changes to dst on apr 3 at 2 am)
  local_date_time phx_departure(date(2005, Apr, 2), hours(23), 
                                phx_tz, 
                                local_date_time::NOT_DATE_TIME_ON_ERROR);

  time_duration flight_length = hours(4) + minutes(30);
  local_date_time phx_arrival = phx_departure + flight_length;
  local_date_time nyc_arrival = phx_arrival.local_time_in(nyc_tz);

  std::cout << "departure phx time: " << phx_departure << std::endl;
  std::cout << "arrival phx time:   " << phx_arrival << std::endl;
  std::cout << "arrival nyc time:   " << nyc_arrival << std::endl;

}


/*  Copyright 2005: CrystalClear Software, Inc
 *  http://www.crystalclearsoftware.com
 *
 *  Subject to the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */
