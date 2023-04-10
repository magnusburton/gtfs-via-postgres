'use strict'

const debug = require('debug')('gtfs-via-postgres')
const sequencify = require('sequencify')
const {Database} = require('duckdb')
const {promisify} = require('util')
const readCsv = require('gtfs-utils/read-csv')
const formatters = require('./lib')
const getDependencies = require('./lib/deps')
const pkg = require('./package.json')

const convertGtfsToSql = async (pathToDb, files, opt = {}) => {
	debug('pathToDb', pathToDb)

	opt = {
		silent: false,
		requireDependencies: false,
		ignoreUnsupportedFiles: false,
		routeTypesScheme: 'google-extended',
		tripsWithoutShapeId: false,
		routesWithoutAgencyId: false,
		stopsWithoutLevelId: !files.some(f => f.name === 'levels'),
		stopsLocationIndex: false,
		schema: 'public',
		postgraphile: false,
		...opt,
	}
	debug('opt', opt)
	const {
		silent,
		tripsWithoutShapeId,
		requireDependencies,
		ignoreUnsupportedFiles,
	} = opt

	if (ignoreUnsupportedFiles) {
		files = files.filter(f => !!formatters[f.name])
	}
	debug('files', files)

	const fileNames = files.map(f => f.name)
	const deps = getDependencies(opt, fileNames)
	debug('deps', deps)

	const tasks = { // file name -> [dep name]
	}

	for (const file of files) {
		if (!formatters[file.name]) {
			throw new Error('invalid/unsupported file: ' + file.name)
		}

		const dependencies = deps[file.name] || []
		for (const dep of dependencies) {
			if (requireDependencies && !tasks[dep] && !fileNames.includes(dep)) {
				const err = new Error(`${file.name} depends on ${dep}`)
				err.code = 'MISSING_GTFS_DEPENDENCY'
				throw err
			}
		}

		tasks[file.name] = {
			file: file.file,
			dep: Array.from(dependencies),
		}
	}
	debug('tasks', tasks)

	const order = []
	sequencify(tasks, Object.keys(tasks), order)
	debug('order', order)

	const db = new Database(pathToDb)
	const dbRun = promisify(db.run)

	await dbRun('BEGIN TRANSACTION')

	for (const name of order) {
		if (!silent) console.error(name)
		const task = tasks[name]

		const importData = formatters[name]

		if (task.file) {
			const input = await readCsv(task.file)
			await importData(db, input, opt)
		} else {
			await importData(db, opt)
		}
	}

	await dbRun('COMMIT')
}

module.exports = convertGtfsToSql
