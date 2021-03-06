// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Helpers = require("./Helpers.bs.js");
var ExternalJs = require("./external.js");

function fetch_api(prim) {
  return ExternalJs.f(prim);
}

function fetch_id(prim) {
  return ExternalJs.fetch_id(prim);
}

function html_of_id(id) {
  return Helpers.read_sync("./pages/" + (String(id) + ".html"));
}

exports.fetch_api = fetch_api;
exports.fetch_id = fetch_id;
exports.html_of_id = html_of_id;
/* Helpers Not a pure module */
