.open foo.duckdb
LOAD postgres;
LOAD icu;

-- todo: https://duckdb.org/docs/sql/expressions/collations#icu-collations
-- CREATE MACRO is_bcp_47_code(input) AS (
-- 	SELECT EXISTS (
-- 		SELECT collname
-- 		FROM pragma_collations()
-- 		WHERE collname = input
-- 		LIMIT 1
-- 	)
-- );

-- https://duckdb.org/docs/sql/data_types/timestamp#time-zones
-- CREATE MACRO is_timezone(tz) AS (
-- 	SELECT EXISTS (
-- 		SELECT name
-- 		FROM pg_timezone_names()
-- 		WHERE name = tz
-- 		LIMIT 1
-- 	)
-- );

-- https://developers.google.com/transit/gtfs/reference#agencytxt
-- CREATE TABLE agency (
-- 	agency_id TEXT PRIMARY KEY,
-- 	agency_name TEXT NOT NULL,
-- 	agency_url TEXT NOT NULL,
-- 	agency_timezone TEXT NOT NULL,
-- 	todo: CONSTRAINT valid_timezone CHECK (is_timezone(agency_timezone)),
-- 	agency_lang TEXT, -- todo: validate?
-- 	agency_phone TEXT,
-- 	agency_fare_url TEXT,
-- 	agency_email TEXT
-- );
-- INSERT INTO agency SELECT * FROM pg.agency;

-- https://developers.google.com/transit/gtfs/reference#calendartxt
-- CREATE TYPE availability AS ENUM (
-- 	'not_available' -- 0 – Service is not available for Mondays in the date range.
-- 	, 'available' -- 1 – Service is available for all Mondays in the date range.
-- );
-- CREATE TABLE calendar (
-- 	service_id TEXT PRIMARY KEY,
-- 	monday availability NOT NULL,
-- 	tuesday availability NOT NULL,
-- 	wednesday availability NOT NULL,
-- 	thursday availability NOT NULL,
-- 	friday availability NOT NULL,
-- 	saturday availability NOT NULL,
-- 	sunday availability NOT NULL,
-- 	start_date DATE NOT NULL,
-- 	end_date DATE NOT NULL,
-- );
-- INSERT INTO calendar SELECT * FROM pg.calendar;

-- CREATE TYPE exception_type_v AS ENUM (
-- 	'added' -- 1 – Service has been added for the specified date.
-- 	, 'removed' -- 2 – Service has been removed for the specified date.
-- );
-- CREATE TABLE calendar_dates (
-- 	service_id TEXT NOT NULL,
-- 	"date" DATE NOT NULL,
-- 	PRIMARY KEY (service_id, "date"),
-- 	exception_type exception_type_v NOT NULL
-- );
-- INSERT INTO calendar_dates SELECT * FROM pg.calendar_dates;

-- CREATE TYPE route_type_val AS ENUM (
-- 	-- basic types
-- 	'0' -- Tram, Streetcar, Light rail. Any light rail or street level system within a metropolitan area.
-- 	, '1' -- Subway, Metro. Any underground rail system within a metropolitan area.
-- 	, '2' -- Rail. Used for intercity or long-distance travel.
-- 	, '3' -- Bus. Used for short- and long-distance bus routes.
-- 	, '4' -- Ferry. Used for short- and long-distance boat service.
-- 	, '5' -- Cable tram. Used for street-level rail cars where the cable runs beneath the vehicle, e.g., cable car in San Francisco.
-- 	, '6' -- Aerial lift, suspended cable car (e.g., gondola lift, aerial tramway). Cable transport where cabins, cars, gondolas or open chairs are suspended by means of one or more cables.
-- 	, '7' -- Funicular. Any rail system designed for steep inclines.
-- 	, '11' -- Trolleybus. Electric buses that draw power from overhead wires using poles.
-- 	, '12' -- Monorail. Railway in which the track consists of a single rail or a beam.
-- 	-- extended types
-- 	, '100' -- Railway Service
-- 	, '101' -- High Speed Rail Service – TGV (FR), ICE (DE), Eurostar (GB)
-- 	, '102' -- Long Distance Trains – InterCity/EuroCity
-- 	, '103' -- Inter Regional Rail Service – InterRegio (DE), Cross County Rail (GB)
-- 	, '104' -- Car Transport Rail Service
-- 	, '105' -- Sleeper Rail Service – GNER Sleeper (GB)
-- 	, '106' -- Regional Rail Service – TER (FR), Regionalzug (DE)
-- 	, '107' -- Tourist Railway Service – Romney, Hythe & Dymchurch (GB)
-- 	, '108' -- Rail Shuttle (Within Complex) – Gatwick Shuttle (GB), Sky Line (DE)
-- 	, '109' -- Suburban Railway – S-Bahn (DE), RER (FR), S-tog (Kopenhagen)
-- 	, '110' -- Replacement Rail Service
-- 	, '111' -- Special Rail Service
-- 	, '112' -- Lorry Transport Rail Service
-- 	, '113' -- All Rail Services
-- 	, '114' -- Cross-Country Rail Service
-- 	, '115' -- Vehicle Transport Rail Service
-- 	, '116' -- Rack and Pinion Railway – Rochers de Naye (CH), Dolderbahn (CH)
-- 	, '117' -- Additional Rail Service
-- 	, '200' -- Coach Service
-- 	, '201' -- International Coach Service – EuroLine, Touring
-- 	, '202' -- National Coach Service – National Express (GB)
-- 	, '203' -- Shuttle Coach Service – Roissy Bus (FR), Reading-Heathrow (GB)
-- 	, '204' -- Regional Coach Service
-- 	, '205' -- Special Coach Service
-- 	, '206' -- Sightseeing Coach Service
-- 	, '207' -- Tourist Coach Service
-- 	, '208' -- Commuter Coach Service
-- 	, '209' -- All Coach Services
-- 	, '400' -- Urban Railway Service
-- 	, '401' -- Metro Service – Métro de Paris
-- 	, '402' -- Underground Service – London Underground, U-Bahn
-- 	, '403' -- Urban Railway Service
-- 	, '404' -- All Urban Railway Services
-- 	, '405' -- Monorail
-- 	, '700' -- Bus Service
-- 	, '701' -- Regional Bus Service – Eastbourne-Maidstone (GB)
-- 	, '702' -- Express Bus Service – X19 Wokingham-Heathrow (GB)
-- 	, '703' -- Stopping Bus Service – 38 London: Clapton Pond-Victoria (GB)
-- 	, '704' -- Local Bus Service
-- 	, '705' -- Night Bus Service – N prefixed buses in London (GB)
-- 	, '706' -- Post Bus Service – Maidstone P4 (GB)
-- 	, '707' -- Special Needs Bus
-- 	, '708' -- Mobility Bus Service
-- 	, '709' -- Mobility Bus for Registered Disabled
-- 	, '710' -- Sightseeing Bus
-- 	, '711' -- Shuttle Bus – 747 Heathrow-Gatwick Airport Service (GB)
-- 	, '712' -- School Bus
-- 	, '713' -- School and Public Service Bus
-- 	, '714' -- Rail Replacement Bus Service
-- 	, '715' -- Demand and Response Bus Service
-- 	, '716' -- All Bus Services
-- 	, '800' -- Trolleybus Service
-- 	, '900' -- Tram Service
-- 	, '901' -- City Tram Service
-- 	, '902' -- Local Tram Service – Munich (DE), Brussels (BE), Croydon (GB)
-- 	, '903' -- Regional Tram Service
-- 	, '904' -- Sightseeing Tram Service – Blackpool Seafront (GB)
-- 	, '905' -- Shuttle Tram Service
-- 	, '906' -- All Tram Services
-- 	, '1000' -- Water Transport Service
-- 	, '1100' -- Air Service
-- 	, '1200' -- Ferry Service
-- 	, '1300' -- Aerial Lift Service – Telefèric de Montjuïc (ES), Saleve (CH), Roosevel, foot Island TramwaUS)
-- 	, '1400' -- Funicular Service – Rigiblick (Zürich, CH)
-- 	, '1500' -- Taxi Service
-- 	, '1501' -- Communal Taxi Service – Marshrutka (RU), dolmuş (TR)
-- 	, '1502' -- Water Taxi Service
-- 	, '1503' -- Rail Taxi Service
-- 	, '1504' -- Bike Taxi Service
-- 	, '1505' -- Licensed Taxi Service
-- 	, '1506' -- Private Hire Service Vehicle
-- 	, '1507' -- All Taxi Services
-- 	, '1700' -- Miscellaneous Service
-- 	, '1702' -- Horse-drawn Carriage
-- );
-- CREATE TABLE routes (
-- 	route_id TEXT PRIMARY KEY,
-- 	agency_id TEXT,
-- 	FOREIGN KEY (agency_id) REFERENCES agency,
-- 	-- todo: Either route_short_name or route_long_name must be specified, or potentially both if appropriate.
-- 	route_short_name TEXT,
-- 	route_long_name TEXT,
-- 	route_desc TEXT,
-- 	route_type route_type_val NOT NULL,
-- 	route_url TEXT,
-- 	route_color TEXT,
-- 	route_text_color TEXT,
-- 	route_sort_order INT
-- );
-- INSERT INTO routes SELECT * FROM pg.routes;

-- CREATE TABLE shapes (
-- 	shape_id TEXT,
-- 	shape_pt_sequence INT,
-- 	-- todo
-- 	-- shape_pt_loc geography(POINT), -- shape_pt_lat/shape_pt_lon
-- 	-- shape_pt_lat DOUBLE PRECISION,
-- 	-- shape_pt_lon DOUBLE PRECISION,
-- 	shape_dist_traveled REAL
-- );
-- INSERT INTO shapes SELECT * EXCLUDE (id, shape_pt_loc) FROM pg.shapes;

-- CREATE TYPE wheelchair_accessibility AS ENUM (
-- 	'unknown' -- 0 or empty - No accessibility information for the trip.
-- 	, 'accessible' -- 1 – Vehicle being used on this particular trip can accommodate at least one rider in a wheelchair.
-- 	, 'not_accessible' -- 2 – No riders in wheelchairs can be accommodated on this trip.
-- );
-- CREATE TYPE bikes_allowance AS ENUM (
-- 	'unknown' -- 0 or empty - No bike information for the trip.
-- 	, 'allowed' -- 1 – Vehicle being used on this particular trip can accommodate at least one bicycle.
-- 	, 'not_allowed' -- 2 – No bicycles are allowed on this trip.
-- );
-- CREATE MACRO shape_exists(some_shape_id) AS (
-- 	SELECT EXISTS (
-- 		SELECT shape_id
-- 		FROM shapes
-- 		WHERE shape_id = some_shape_id
-- 		LIMIT 1
-- 	)
-- );
-- CREATE TABLE trips (
-- 	trip_id TEXT PRIMARY KEY,
-- 	route_id TEXT NOT NULL,
-- 	FOREIGN KEY (route_id) REFERENCES routes,
-- 	service_id TEXT NOT NULL, -- references service_days.service_id
-- 	trip_headsign TEXT,
-- 	trip_short_name TEXT,
-- 	direction_id INT,
-- 	block_id TEXT,
-- 	shape_id TEXT, -- todo: add NOT NULL?
-- 	-- todo: fails with one of the following error messages:
-- 	-- - "Parser Error: subqueries prohibited in CHECK constraints"
-- 	-- - "Binder Error: cannot use subquery in check constraint"
-- 	-- CHECK (shape_exists(shape_id)),
-- 	wheelchair_accessible wheelchair_accessibility,
-- 	bikes_allowed bikes_allowance
-- );
-- INSERT INTO trips SELECT * FROM pg.trips;

-- CREATE TYPE exact_times_v AS ENUM (
-- 	'frequency_based' -- 0 or empty - Frequency-based trips.
-- 	, 'schedule_based' -- 1 – Schedule-based trips with the exact same headway throughout the day. In this case the end_time value must be greater than the last desired trip start_time but less than the last desired trip start_time + headway_secs.
-- );
-- CREATE TABLE frequencies (
-- 	trip_id TEXT NOT NULL,
-- 	FOREIGN KEY (trip_id) REFERENCES trips,
-- 	start_time INTERVAL NOT NULL,
-- 	end_time INTERVAL NOT NULL,
-- 	headway_secs INT NOT NULL,
-- 	exact_times exact_times_v,
-- 	-- todo: https://github.com/duckdb/duckdb/issues/63#issuecomment-1502425536
-- 	-- UNIQUE (
-- 	-- 	trip_id,
-- 	-- 	start_time,
-- 	-- 	end_time,
-- 	-- 	headway_secs,
-- 	-- 	exact_times
-- 	-- )
-- );
-- INSERT INTO frequencies SELECT * FROM pg.frequencies;

-- CREATE TABLE levels (
-- 	level_id TEXT PRIMARY KEY,
-- 	level_index DOUBLE PRECISION NOT NULL,
-- 	level_name TEXT
-- );
-- COPY levels (
-- 	level_id,
-- 	level_index,
-- 	level_name
-- ) FROM STDIN csv;
-- INSERT INTO levels SELECT * FROM pg.levels;

-- CREATE TYPE location_type_val AS ENUM (
-- 	'stop' -- 0 (or blank): Stop (or Platform). A location where passengers board or disembark from a transit vehicle. Is called a platform when defined within a parent_station.
-- 	, 'station' -- 1 – Station. A physical structure or area that contains one or more platform.
-- 	, 'entrance_exit' -- 2 – Entrance/Exit. A location where passengers can enter or exit a station from the street. If an entrance/exit belongs to multiple stations, it can be linked by pathways to both, but the data provider must pick one of them as parent.
-- 	, 'node' -- 3 – Generic Node. A location within a station, not matching any other location_type, which can be used to link together pathways define in pathways.txt.
-- 	, 'boarding_area' -- 4 – Boarding Area. A specific location on a platform, where passengers can board and/or alight vehicles.
-- );
-- -- For parentless stops:
-- -- 0 or empty - No accessibility information for the stop.
-- -- 1 - Some vehicles at this stop can be boarded by a rider in a wheelchair.
-- -- 2 - Wheelchair boarding is not possible at this stop.

-- -- For child stops:
-- -- 0 or empty - Stop will inherit its wheelchair_boarding behavior from the parent station, if specified in the parent.
-- -- 1 - There exists some accessible path from outside the station to the specific stop/platform.
-- -- 2 - There exists no accessible path from outside the station to the specific stop/platform.

-- -- For station entrances/exits:
-- -- 0 or empty - Station entrance will inherit its wheelchair_boarding behavior from the parent station, if specified for the parent.
-- -- 1 - Station entrance is wheelchair accessible.
-- -- 2 - No accessible path from station entrance to stops/platforms.
-- CREATE TYPE wheelchair_boarding_val AS ENUM (
-- 	'no_info_or_inherit'
-- 	, 'accessible'
-- 	, 'not_accessible'
-- );
-- CREATE TABLE stops (
-- 	stop_id TEXT PRIMARY KEY,
-- 	stop_code TEXT,
-- 	-- todo: Required for locations which are stops (location_type=0), stations (location_type=1) or entrances/exits (location_type=2). Optional for locations which are generic nodes (location_type=3) or boarding areas (location_type=4).
-- 	stop_name TEXT,
-- 	stop_desc TEXT,
-- 	-- todo
-- 	-- stop_loc geography(POINT), -- stop_lat/stop_lon
-- 	-- stop_lat DOUBLE PRECISION,
-- 	-- stop_lon DOUBLE PRECISION,
-- 	zone_id TEXT,
-- 	stop_url TEXT,
-- 	location_type location_type_val,
-- 	parent_station TEXT,
-- 	stop_timezone TEXT,
-- 	-- todo: fails with one of the following error messages:
-- 	-- - "Parser Error: subqueries prohibited in CHECK constraints"
-- 	-- - "Binder Error: cannot use subquery in check constraint"
-- 	-- CHECK (is_timezone(stop_timezone)),
-- 	wheelchair_boarding wheelchair_boarding_val,
-- 	level_id TEXT,
-- 	FOREIGN KEY (level_id) REFERENCES levels,
-- 	platform_code TEXT
-- );
-- INSERT INTO stops SELECT * EXCLUDE (stop_loc) FROM pg.stops;

-- CREATE TYPE pathway_mode_v AS ENUM (
-- 	'walkway' -- 1
-- 	, 'stairs' -- 2
-- 	, 'moving_sidewalk_travelator' -- 3 – moving sidewalk/travelator
-- 	, 'escalator' -- 4
-- 	, 'elevator' -- 5
-- 	, 'fare_gate' -- 6 – (or payment gate): A pathway that crosses into an area of the station where a proof of payment is required (usually via a physical payment gate).
-- 	-- Fare gates may either separate paid areas of the station from unpaid ones, or separate different payment areas within the same station from each other. This information can be used to avoid routing passengers through stations using shortcuts that would require passengers to make unnecessary payments, like directing a passenger to walk through a subway platform to reach a busway.
-- 	, 'exit_gate' -- 7 – Indicates a pathway exiting an area where proof-of-payment is required into an area where proof-of-payment is no longer required.
-- );
-- CREATE TABLE pathways (
-- 	pathway_id TEXT PRIMARY KEY,
-- 	from_stop_id TEXT NOT NULL,
-- 	FOREIGN KEY (from_stop_id) REFERENCES stops (stop_id),
-- 	to_stop_id TEXT NOT NULL,
-- 	FOREIGN KEY (to_stop_id) REFERENCES stops (stop_id),
-- 	pathway_mode pathway_mode_v,
-- 	is_bidirectional BOOLEAN NOT NULL,
-- 	length DOUBLE PRECISION, -- todo: add non-negative constraint
-- 	traversal_time INTEGER, -- todo: add positive constraint
-- 	stair_count INTEGER, -- todo: add non-0 constraint
-- 	max_slope DOUBLE PRECISION,
-- 	min_width DOUBLE PRECISION, -- todo: add positive constraint
-- 	signposted_as TEXT,
-- 	reversed_signposted_as TEXT
-- );
-- INSERT INTO pathways SELECT * FROM pg.pathways;

-- CREATE TYPE pickup_drop_off_type AS ENUM (
-- 	'regular' -- 0 or empty - Regularly scheduled pickup/dropoff.
-- 	, 'not_available' -- 1 – No pickup/dropoff available.
-- 	, 'call' -- 2 – Must phone agency to arrange pickup/dropoff.
-- 	, 'driver' -- 3 – Must coordinate with driver to arrange pickup/dropoff.
-- );
-- CREATE TYPE timepoint_v AS ENUM (
-- 	'approximate' -- 0 – Times are considered approximate.
-- 	, 'exact' -- 1 or empty - Times are considered exact.
-- );
-- CREATE TABLE stop_times (
-- 	trip_id TEXT NOT NULL,
-- 	FOREIGN KEY (trip_id) REFERENCES trips,
-- 	-- https://gist.github.com/derhuerst/574edc94981a21ef0ce90713f1cff7f6
-- 	arrival_time interval,
-- 	departure_time interval,
-- 	stop_id TEXT NOT NULL,
-- 	FOREIGN KEY (stop_id) REFERENCES stops,
-- 	stop_sequence INT NOT NULL,
-- 	stop_sequence_consec INT,
-- 	stop_headsign TEXT,
-- 	pickup_type pickup_drop_off_type,
-- 	drop_off_type pickup_drop_off_type,
-- 	shape_dist_traveled REAL,
-- 	timepoint timepoint_v
-- );
-- INSERT INTO stop_times SELECT * FROM pg.stop_times;
-- UPDATE stop_times
-- SET stop_sequence_consec = t.seq
-- FROM (
-- 	SELECT
-- 		row_number() OVER (PARTITION BY trip_id ORDER BY stop_sequence ASC) - 1 AS seq,
-- 		trip_id, stop_sequence
-- 	FROM stop_times
-- ) AS t
-- WHERE stop_times.trip_id = t.trip_id
-- AND stop_times.stop_sequence = t.stop_sequence;

-- todo [breaking]: rename to service_dates?
-- CREATE VIEW service_days AS
-- SELECT
-- 	base_days.service_id,
-- 	base_days.date
-- -- "base" service days
-- FROM (
-- 	SELECT
-- 		service_id,
-- 		"date"
-- 	FROM (
-- 		SELECT
-- 			service_id,
-- 			"date",
-- 			extract(dow FROM "date") dow,
-- 			sunday,
-- 			monday,
-- 			tuesday,
-- 			wednesday,
-- 			thursday,
-- 			friday,
-- 			saturday
-- 		FROM (
-- 			SELECT
-- 				*,
-- 				UNNEST(generate_series (
-- 					start_date::TIMESTAMP,
-- 					end_date::TIMESTAMP,
-- 					'1 day'::INTERVAL
-- 				)) "date"
-- 			FROM calendar
-- 		) all_days_raw
-- 	) all_days
-- 	WHERE (sunday = 'available' AND dow = 0)
-- 	OR (monday = 'available' AND dow = 1)
-- 	OR (tuesday = 'available' AND dow = 2)
-- 	OR (wednesday = 'available' AND dow = 3)
-- 	OR (thursday = 'available' AND dow = 4)
-- 	OR (friday = 'available' AND dow = 5)
-- 	OR (saturday = 'available' AND dow = 6)
-- ) base_days
-- -- "removed" exceptions
-- LEFT JOIN (
-- 	SELECT *
-- 	FROM calendar_dates
-- 	WHERE exception_type = 'removed'
-- ) removed
-- ON base_days.service_id = removed.service_id
-- AND base_days.date = removed.date
-- WHERE removed.date IS NULL
-- -- "added" exceptions
-- UNION SELECT service_id, "date"
-- FROM calendar_dates
-- WHERE exception_type = 'added'
-- ORDER BY service_id, "date";









CREATE OR REPLACE VIEW arrivals_departures AS
WITH stop_times_based AS (
	SELECT
		trips.route_id,
		route_short_name,
		route_long_name,
		route_type,
		s.trip_id,
		trips.direction_id,
		trips.trip_headsign,
		service_days.service_id,
		trips.shape_id,
		"date",
		stop_sequence,
		stop_headsign,
		pickup_type,
		drop_off_type,
		shape_dist_traveled,
		timepoint,
		coalesce(stations.stop_timezone, stops.stop_timezone, agency.agency_timezone) as tz,
		arrival_time,
		(
			make_timestamptz(
				date_part('year', "date")::int,
				date_part('month', "date")::int,
				date_part('day', "date")::int,
				12, 0, 0,
				coalesce(stations.stop_timezone, stops.stop_timezone, agency.agency_timezone)
			)
			- interval '12 hours'
			+ arrival_time
		) t_arrival,
		departure_time,
		(
			make_timestamptz(
				date_part('year', "date")::int,
				date_part('month', "date")::int,
				date_part('day', "date")::int,
				12, 0, 0,
				coalesce(stations.stop_timezone, stops.stop_timezone, agency.agency_timezone)
			)
			- interval '12 hours'
			+ departure_time
		) t_departure,
		s.stop_id, stops.stop_name,
		stations.stop_id station_id, stations.stop_name station_name
	FROM (
		stop_times s
		JOIN stops ON s.stop_id = stops.stop_id
		LEFT JOIN stops stations ON stops.parent_station = stations.stop_id
		JOIN trips ON s.trip_id = trips.trip_id
		JOIN routes ON trips.route_id = routes.route_id
		LEFT JOIN agency ON routes.agency_id = agency.agency_id
		JOIN service_days ON trips.service_id = service_days.service_id
	)
	-- todo: this slows down slightly
	-- ORDER BY route_id, s.trip_id, "date", stop_sequence
)
-- stop_times-based arrivals/departures
SELECT
	(
		base64(trip_id::blob)
		|| ':' || base64((
			extract(ISOYEAR FROM "date")
			|| '-' || lpad(extract(MONTH FROM "date")::text, 2, '0')
			|| '-' || lpad(extract(DAY FROM "date")::text, 2, '0')
		)::blob)
		|| ':' || base64((stop_sequence::text)::blob)
		-- frequencies_row
		|| ':' || base64('-1'::blob)
		-- frequencies_it
		|| ':' || base64('-1'::blob)
	) as arrival_departure_id,

	stop_times_based.*,
	-- todo: expose local arrival/departure "wall clock time"?

	-1 AS frequencies_row,
	-1 AS frequencies_it
FROM stop_times_based
UNION ALL
-- frequencies-based arrivals/departures
SELECT
	(
		base64(trip_id::blob)
		|| ':' || base64((
			extract(ISOYEAR FROM "date")
			|| '-' || lpad(extract(MONTH FROM "date")::text, 2, '0')
			|| '-' || lpad(extract(DAY FROM "date")::text, 2, '0')
		)::blob)
		|| ':' || base64((stop_sequence::text)::blob)
		|| ':' || base64((frequencies_row::text)::blob)
		|| ':' || base64((frequencies_it::text)::blob)
	) as arrival_departure_id,

	-- stop_times_based.* except t_arrival & t_departure, duh
	-- todo: find a way to use all columns without explicitly enumerating them here
	route_id, route_short_name, route_long_name, route_type,
	trip_id, direction_id, trip_headsign,
	service_id,
	shape_id,
	"date",
	stop_sequence, stop_headsign, pickup_type, drop_off_type, shape_dist_traveled, timepoint,
	tz,
	arrival_time, -- todo [breaking]: this is misleading, remove it
	generate_series(
		t_arrival - stop_times_offset + start_time,
		t_arrival - stop_times_offset + end_time,
		interval '1 second' * headway_secs
	) as t_arrival,
	departure_time, -- todo [breaking]: this is misleading, remove it
	generate_series(
		t_departure - stop_times_offset + start_time,
		t_departure - stop_times_offset + end_time,
		interval '1 second' * headway_secs
	) as t_departure,
	stop_id, stop_name,
	station_id, station_name,
	frequencies_row, frequencies_it
FROM (
	SELECT
		stop_times_based.*,
		frequencies.start_time,
		frequencies.end_time,
		frequencies.headway_secs,
		-- todo: is frequencies.txt relative to 1st arrival_time or departure_time?
		coalesce(
			first_value(departure_time) OVER (PARTITION BY stop_times_based.trip_id, "date" ORDER BY stop_sequence),
			first_value(arrival_time) OVER (PARTITION BY stop_times_based.trip_id, "date" ORDER BY stop_sequence)
		) as stop_times_offset,
		frequencies_row,
		(row_number() OVER (PARTITION BY stop_times_based.trip_id, "date", frequencies_row ORDER BY stop_sequence))::integer as frequencies_it
	FROM stop_times_based
	JOIN (
		SELECT
			*,
			(row_number() OVER (PARTITION BY trip_id, exact_times))::integer as frequencies_row
		FROM frequencies
		WHERE frequencies.exact_times = 'schedule_based' -- todo: is this correct?
	) frequencies ON frequencies.trip_id = stop_times_based.trip_id
) frequencies_based;


-- INSERT INTO transfers SELECT * FROM pg.transfers;

-- SELECT count(*)
-- FROM arrivals_departures
-- WHERE stop_id = ''de:11000:900100001::4'' -- S+U Friedrichstr. (Berlin)
