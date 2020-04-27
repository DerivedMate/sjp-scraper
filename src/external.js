// @ts-check
require('isomorphic-fetch')
const XRegExp = require('xregexp');
const fs = require('fs')

const f = a =>
  fetch('https://wsjp.pl/ajax/pobierz_hasla.php', {
    credentials: 'include',
    headers: {
      'User-Agent':
        'Mozilla/5.0 (X11; Linux x86_64; rv:76.0) Gecko/20100101 Firefox/76.0',
      Accept: '*/*',
      'Accept-Language': 'es,en-US;q=0.7,en;q=0.3',
      'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
      'X-Requested-With': 'XMLHttpRequest',
      Pragma: 'no-cache',
      'Cache-Control': 'no-cache',
    },
    referrer: 'https://wsjp.pl/index.php?pwh=0',
    body: `letter=${a}&pwh=0`,
    method: 'POST',
    mode: 'cors',
  })
    .then(r => r.text())
    .catch(err => {
      console.error(`Error fetching t'api: ${err}`)
      return ''
    })

const fetch_id = id =>
  fetch(`https://wsjp.pl/do_druku.php?id_hasla=${id}`)
    .then(r => r.text())
    .then(html => [html, id])
    .catch(err => {
      console.error(`Error fetching t'api: ${err}`)
      return ''
    })

const clean_declension = row =>
  row
    .replace(/(^\n|\n$)/g, '')
    .replace(/\n+/g, '\n')
    .split('\n')
    .map(w =>
      w
        .replace(/[\w\s]+\:/g, '')
        .replace(/,[a-ząęśćóżź]+.[\w\sąęśćóżź]+/giu, '')
    )

const clean_conjugation = row => row.replace(/^\d\.\w+\./, '')
const open_languages = path => fs.readFileSync(path, { encoding: 'utf8' })
const nullish_op = (a, b) => a ?? b
const pointer_of_od = p => {
  let matches = /^(od\:|zob\.)\s*(.+)/i.exec(p)
  let str
  if (matches && matches.length == 2) {
    str = matches[1]
  } else if (matches && matches.length > 2) {
    str = matches[2]
  } else {
    str = p
  }

  return str.trim().toLocaleLowerCase()
}
const write_sync = (dir, content) => {
  fs.writeFileSync(dir, content, { encoding: 'utf8' })
}

const read_sync = file => fs.readFileSync(file, { encoding: 'utf8' })

const open_ids = dir => JSON.parse(fs.readFileSync(dir, { encoding: 'utf8' }))

module.exports.f = f
module.exports.fetch_id = fetch_id
module.exports.clean_declension = clean_declension
module.exports.clean_conjugation = clean_conjugation
module.exports.open_languages = open_languages
module.exports.pointer_of_od = pointer_of_od
module.exports.write_sync = write_sync
module.exports.read_sync = read_sync
module.exports.open_ids = open_ids
