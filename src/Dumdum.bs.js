// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Rex = require("./Rex.bs.js");
var Const = require("./Const.bs.js");
var Decco = require("decco/src/Decco.js");
var Helpers = require("./Helpers.bs.js");
var Cheerio = require("cheerio");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var ExternalJs = require("./external.js");
var Cheerio$BsCheerio = require("bs-cheerio/src/Cheerio.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function clean_declension(prim) {
  return ExternalJs.clean_declension(prim);
}

function clean_conjugation(prim) {
  return ExternalJs.clean_conjugation(prim);
}

function id_of_api(resp) {
  var dom = Cheerio.load(resp);
  return Cheerio$BsCheerio.select(dom, "a").map((function (param, e) {
                  var tag = Cheerio(e).attr("href");
                  if (tag == null) {
                    return "0";
                  } else {
                    return Rex.id_of_url(tag);
                  }
                })).toArray();
}

function part_of_dom(dom) {
  return Helpers.get_null(Cheerio(Cheerio$BsCheerio.select(dom, "[class*=odmiana] em").get(0)).text());
}

function classif_of_dom(dom) {
  return Belt_Array.reduce(Belt_Array.map(Cheerio$BsCheerio.select(dom, ".center.word_content").map((function (param, el) {
                          return Helpers.get_null(Cheerio(el).text()).trim().toLocaleLowerCase();
                        })).toArray(), (function (t) {
                    return Belt_Array.keepMap(t.replace(/[\n\t]+/g, ";").split(/\;\s+/g), (function (r) {
                                  return r;
                                }));
                  })), [], (function (acc, a) {
                if (Belt_Array.some(acc, (function (b) {
                          return Belt_Array.eq(a, b, (function (a, b) {
                                        return a === b;
                                      }));
                        }))) {
                  return acc;
                } else {
                  return Belt_Array.concat(acc, [a]);
                }
              }));
}

function name_of_dom(dom) {
  return Helpers.get_null(Cheerio(Cheerio$BsCheerio.select(dom, "h1").get(0)).text()).trim();
}

function conjugation_of_dom(dom) {
  return Caml_array.caml_array_get(Belt_Array.map(Cheerio(Cheerio(Cheerio$BsCheerio.select(dom, ".odmiana_konspekt>tbody tr").get(2)).contents().get(1)).map((function (param, e) {
                            return Helpers.get_null_or(Cheerio(e).text(), "");
                          })).toArray(), (function (r) {
                      return Belt_Array.reduceWithIndex(Belt_Array.slice(Belt_Array.map(r.split("\n"), (function (w) {
                                            return ExternalJs.clean_conjugation(w);
                                          })), 3, 6), /* tuple */[
                                  [],
                                  ""
                                ], (function (param, a, i) {
                                    var acc = param[0];
                                    if (i % 2 === 0) {
                                      return /* tuple */[
                                              acc,
                                              a
                                            ];
                                    } else {
                                      return /* tuple */[
                                              Belt_Array.concat(acc, [[
                                                      param[1],
                                                      a
                                                    ]]),
                                              ""
                                            ];
                                    }
                                  }));
                    })), 0)[0];
}

function declension_of_dom(dom) {
  var arr = Cheerio$BsCheerio.select(dom, ".odmiana_tb > tbody").first().contents().map((function (param, e) {
            return Helpers.get_null(Cheerio(e).text());
          })).toArray();
  var arr$1;
  try {
    arr$1 = Belt_Array.sliceToEnd(arr, 4);
  }
  catch (exn){
    return Belt_Array.keep(Belt_Array.map(arr, (function (r) {
                      return ExternalJs.clean_declension(r);
                    })), (function (r) {
                  return Belt_Array.some(r, (function (w) {
                                return w.length > 0;
                              }));
                }));
  }
  return Belt_Array.keep(Belt_Array.map(arr$1, (function (r) {
                    return ExternalJs.clean_declension(r);
                  })), (function (r) {
                return Belt_Array.some(r, (function (w) {
                              return w.length > 0;
                            }));
              }));
}

function origin_map_encode(v) {
  return Decco.arrayToJson((function (param) {
                return Decco.arrayToJson(Decco.stringToJson, param);
              }), v);
}

function origin_map_decode(v) {
  return Decco.arrayFromJson((function (param) {
                return Decco.arrayFromJson(Decco.stringFromJson, param);
              }), v);
}

function origin_of_word(map, word) {
  return Caml_array.caml_array_get(Belt_Option.getExn(Belt_Array.getBy(map, (function (param) {
                        if (param.length !== 2) {
                          throw [
                                Caml_builtin_exceptions.match_failure,
                                /* tuple */[
                                  "Dumdum.re",
                                  120,
                                  24
                                ]
                              ];
                        }
                        var a = param[0];
                        return Caml_obj.caml_equal(a, word);
                      }))), 1);
}

function origins_of_dom(dom, map) {
  var explicit_origins = Belt_Array.map(Cheerio$BsCheerio.select(dom, ".pochodzenie > tbody > tr .pochodzenie_jezyk").map((function (param, e) {
                return Cheerio(e).text();
              })).toArray(), (function (l) {
          return origin_of_word(map, l.trim().toLocaleLowerCase());
        }));
  var match = explicit_origins.length;
  if (match !== 0) {
    return explicit_origins;
  } else {
    var origin = Helpers.get_null(Cheerio(Cheerio$BsCheerio.select(dom, ".pochodzenie > tbody > tr .pochodzenie_uwagi em").get(0)).text());
    return [
            Const.pointer_sig,
            origin
          ];
  }
}

function adjective_of_dom(dom) {
  return Helpers.partitions(Belt_Array.slice(Cheerio$BsCheerio.select(dom, ".odmiana tr").map((function (param, r) {
                          return Belt_Array.sliceToEnd(Cheerio(r).contents().map((function (param, e) {
                                              return Cheerio(e).text();
                                            })).toArray(), 1);
                        })).toArray(), 8, 35), 5);
}

exports.clean_declension = clean_declension;
exports.clean_conjugation = clean_conjugation;
exports.id_of_api = id_of_api;
exports.part_of_dom = part_of_dom;
exports.classif_of_dom = classif_of_dom;
exports.name_of_dom = name_of_dom;
exports.conjugation_of_dom = conjugation_of_dom;
exports.declension_of_dom = declension_of_dom;
exports.origin_map_encode = origin_map_encode;
exports.origin_map_decode = origin_map_decode;
exports.origin_of_word = origin_of_word;
exports.origins_of_dom = origins_of_dom;
exports.adjective_of_dom = adjective_of_dom;
/* Helpers Not a pure module */