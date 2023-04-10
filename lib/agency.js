'use strict'

// https://developers.google.com/transit/gtfs/reference#agencytxt
const importData = async (db, agency, opt) => {
	await dbRun(`\
CREATE TABLE "${opt.schema}".agency (
	agency_id TEXT PRIMARY KEY,
	agency_name TEXT NOT NULL,
	agency_url TEXT NOT NULL,
	agency_timezone TEXT NOT NULL
		CONSTRAINT valid_timezone CHECK ("${opt.schema}".is_timezone(agency_timezone)),
	agency_lang TEXT, -- todo: validate?
	agency_phone TEXT,
	agency_fare_url TEXT,
	agency_email TEXT
)
	`)

	// todo
}

module.exports = importData
