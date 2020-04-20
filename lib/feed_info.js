'use strict'

// https://developers.google.com/transit/gtfs/reference#feed_infotxt
const beforeAll = `\
CREATE TABLE feed_info (
	feed_publisher_name TEXT NOT NULL,
	feed_publisher_url TEXT NOT NULL,
	feed_lang TEXT NOT NULL,
	default_lang TEXT,
	feed_start_date DATE,
	feed_end_date DATE,
	feed_version TEXT,
	feed_contact_email TEXT,
	feed_contact_url TEXT
);
`

const formatFeedInfoRow = (sql, i) => {
	return sql `\
(
	${i.feed_publisher_name || null},
	${i.feed_publisher_url || null},
	${i.feed_lang || null},
	${i.default_lang || null},
	${i.feed_start_date},
	${i.feed_end_date},
	${i.feed_version || null},
	${i.feed_contact_email || null},
	${i.feed_contact_url || null}
)`
}

const head = `\
INSERT INTO agency (
	feed_publisher_name,
	feed_publisher_url,
	feed_lang,
	default_lang,
	feed_start_date,
	feed_end_date,
	feed_version,
	feed_contact_email,
	feed_contact_url
) VALUES`

module.exports = {
	beforeAll,
	head,
	formatRow: formatFeedInfoRow,
}