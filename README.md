# Erlang GeoIP Library

An Erlang GeoIP library for the MaxMind data sets.

## Installation

Add it as a dependancy in your `rebar.config`:

    {deps,
      [
        {geoip, ".*", {git, "git://github.com/maximvl/erlang-geoip.git", "master"}},
      ]
    }.

## Basic Usage

    > {ok, Geoip} = geoip:new("GeoIP.dat").
    {ok, #Port<0.2755>}

    > geoip:get_country_name_by_ip(Geoip, {14,14,14,14}). 
    "Japan"

    > geoip:get_country_code_by_ip(Geoip, {14,14,14,14}). 
    "JP"

    > geoip:get_country_code3_by_ip(Geoip, {14,14,14,14}). 
    "JPN"

    > geoip:get_country_name_by_ip(Geoip, "24.24.24.24").
    "United States"

    > geoip:get_country_code_by_ip(Geoip, "24.24.24.24").
    "US"

    > geoip:get_country_code3_by_ip(Geoip, "24.24.24.24").
    "USA"

    > geoip:get_country_name_by_ip(Geoip, <<"80.24.24.80">>).
    "Spain"

    > geoip:delete(Geoip).
    ok

The argument passed to `new` is the location of the MaxMind binary data file. You can download a free to use country level dataset from [the MaxMind website](http://www.maxmind.com/app/geolitecountry).

## Server

`geoip_server` implements a `gen_server` behaviour for use with supervisors.

    > geoip_server:start_link("GeoIP.dat").
    {ok, <0.35.0>}

    > geoip_server:get_country_name_by_ip("24.24.24.24").
    "United States"

## Performance

In can do over 1,000,000 lookups per second on consumer grade hardware. 1.6GHz Intel Core i5 Macbook Air, 4GB RAM, OS X Lion:

    > geoip:benchmark().
    100000 lookups for "? GEOIP_STANDARD": (1209 ms) 
    100000 lookups for "? GEOIP_CHECK_CACHE": (1232 ms) 
    1000000 lookups for "? GEOIP_MEMORY_CACHE": (901 ms) 
    1000000 lookups for "? GEOIP_MEMORY_CACHE bor ? GEOIP_CHECK_CACHE": (925 ms)

## Advanced Usage

Invalid IPs return an empty list.

    > geoip:get_country_name_by_ip(Geoip, "not_an_ip").
    []

    > geoip:get_country_name_by_ip(Geoip, "360.12.32.1").
    []

Call `use_binary` to return results as binaries:

    > geoip:use_binary(Geoip).
    []

    > geoip:get_country_by_ip(Geoip, "24.24.24.24").
    <<"United States">>

And `use_string` to return them as strings again:

    > geoip:use_string(Geoip).
    []

    > geoip:get_country_by_ip(Geoip, "24.24.24.24").
    "United States"

The second flag is used to pass flags to the MaxMind C library. The following flags are supported:

* 0 - `GEOIP_STANDARD` - Instantiate the C library, and reload the data set, on each call. Slowest, but lowest memory usage.
* 1 - `GEOIP_MEMORY_CACHE`  - Instantiate the C library once, and use it for each call. Fastest, but highest memory usage. (Default)
* 2 - `GEOIP_CHECK_CACHE`  - Check whether the dataset has been changed, and reinstantiate if needed on each call. This isn't usually needed unless you are debugging data sets.


    > {ok, Geoip} = geoip:new("GeoIP.dat", 0).
    {ok, #Port<0.2755>}

Last argument is used for set default type of return values


    > {ok, Geoip} = geoip:new("GeoIP.dat", 1, binary).
    {ok, #Port<0.2755>}

    > geoip:get_country_code3_by_ip(Geoip, {14,14,14,14}). 
    <<"JPN">>

And same for server

    > geoip_server:start_link("GeoIP.dat", 1, binary).
    {ok,<0.5048.0>}

    > geoip_server:get_country_code3_by_ip({14,14,14,14}).
    <<"JPN">>


## Tests

To make the tests run, you need to put the `GeoIP.dat` file in the root directory.

    > geoip:test().
      Test passed.
    ok

## License

This uses code from MaxMind.com released under the GNU LGPL Version 2.1 license. See COPYING for more details.
