// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Const = require("./Const.bs.js");
var Decco = require("decco/src/Decco.js");
var Dumdum = require("./Dumdum.bs.js");
var Fetcher = require("./Fetcher.bs.js");
var Helpers = require("./Helpers.bs.js");
var Js_dict = require("bs-platform/lib/js/js_dict.js");
var Js_json = require("bs-platform/lib/js/js_json.js");
var Cheerio = require("cheerio");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Belt_Result = require("bs-platform/lib/js/belt_Result.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function data_encode(v) {
  return Js_dict.fromArray([
              /* tuple */[
                "id",
                Decco.stringToJson(v.id)
              ],
              /* tuple */[
                "name",
                Decco.stringToJson(v.name)
              ],
              /* tuple */[
                "part",
                Decco.stringToJson(v.part)
              ],
              /* tuple */[
                "origin",
                Decco.arrayToJson(Decco.stringToJson, v.origin)
              ]
            ]);
}

function data_decode(v) {
  var match = Js_json.classify(v);
  if (typeof match === "number" || match.tag !== /* JSONObject */2) {
    return Decco.error(undefined, "Not an object", v);
  } else {
    var dict = match[0];
    var match$1 = Decco.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(dict, "id"), null));
    if (match$1.tag) {
      var e = match$1[0];
      return /* Error */Block.__(1, [{
                  path: ".id" + e.path,
                  message: e.message,
                  value: e.value
                }]);
    } else {
      var match$2 = Decco.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(dict, "name"), null));
      if (match$2.tag) {
        var e$1 = match$2[0];
        return /* Error */Block.__(1, [{
                    path: ".name" + e$1.path,
                    message: e$1.message,
                    value: e$1.value
                  }]);
      } else {
        var match$3 = Decco.stringFromJson(Belt_Option.getWithDefault(Js_dict.get(dict, "part"), null));
        if (match$3.tag) {
          var e$2 = match$3[0];
          return /* Error */Block.__(1, [{
                      path: ".part" + e$2.path,
                      message: e$2.message,
                      value: e$2.value
                    }]);
        } else {
          var match$4 = Decco.arrayFromJson(Decco.stringFromJson, Belt_Option.getWithDefault(Js_dict.get(dict, "origin"), null));
          if (match$4.tag) {
            var e$3 = match$4[0];
            return /* Error */Block.__(1, [{
                        path: ".origin" + e$3.path,
                        message: e$3.message,
                        value: e$3.value
                      }]);
          } else {
            return /* Ok */Block.__(0, [{
                        id: match$1[0],
                        name: match$2[0],
                        part: match$3[0],
                        origin: match$4[0]
                      }]);
          }
        }
      }
    }
  }
}

function words_encode(v) {
  return Decco.arrayToJson(data_encode, v);
}

function words_decode(v) {
  return Decco.arrayFromJson(data_decode, v);
}

var origin_map = Belt_Result.getExn(Dumdum.origin_map_decode(JSON.parse(Helpers.open_languages("languages.json"))));

var count = {
  contents: 0
};

function data_of_site(html, id) {
  var dom = Cheerio.load(html);
  var part = Dumdum.part_of_dom(dom);
  var name = Dumdum.name_of_dom(dom);
  var origin = Dumdum.origins_of_dom(dom, origin_map);
  if (origin.length !== 0 && Caml_array.caml_array_get(origin, 0) === Const.inline_origin_sig) {
    count.contents = count.contents + 1 | 0;
  }
  return {
          id: id,
          name: name,
          part: part,
          origin: origin
        };
}

function main(ids) {
  var count_inl = {
    contents: 0
  };
  var count_ptr = {
    contents: 0
  };
  var loop = function (len0, _i, _id, _rest) {
    while(true) {
      var rest = _rest;
      var id = _id;
      var i = _i;
      if (id !== undefined) {
        var id$1 = id;
        var exit = 0;
        var html;
        try {
          html = Fetcher.html_of_id(id$1);
          exit = 1;
        }
        catch (raw_e){
          var e = Caml_js_exceptions.internalToOCamlException(raw_e);
          console.error("" + (String(id$1) + (" => " + (String(e) + ""))));
          var match = Helpers.hd_tl_arr(rest);
          loop(len0, i, match[0], match[1]);
        }
        if (exit === 1) {
          var wd = data_of_site(html, id$1);
          var w = JSON.stringify(data_encode(wd));
          if (i === len0 && rest.length === 0) {
            console.log("" + (String(w) + ""));
          } else {
            console.log("" + (String(w) + ","));
          }
          if (wd.origin.length !== 0) {
            if (Caml_array.caml_array_get(wd.origin, 0) === Const.pointer_sig) {
              count_ptr.contents = count_ptr.contents + 1 | 0;
            } else if (Caml_array.caml_array_get(wd.origin, 0) === Const.inline_origin_sig) {
              count_inl.contents = count_inl.contents + 1 | 0;
            }
            
          }
          
        }
        var c = count.contents;
        var inl = count_inl.contents;
        var ptr = count_ptr.contents;
        var pr = (i / len0 * 100).toFixed(2);
        console.error("Progress: " + (String(i) + ("/" + (String(len0) + (" [" + (String(pr) + ("]; NoOri: " + (String(c) + ("; Ptr: " + (String(ptr) + ("; Inl: " + (String(inl) + ""))))))))))));
        var match$1 = Helpers.hd_tl_arr(rest);
        _rest = match$1[1];
        _id = match$1[0];
        _i = i + 1 | 0;
        continue ;
      } else {
        return /* () */0;
      }
    };
  };
  var match = Helpers.hd_tl_arr(ids);
  return loop(ids.length, 1, match[0], match[1]);
}

exports.data_encode = data_encode;
exports.data_decode = data_decode;
exports.words_encode = words_encode;
exports.words_decode = words_decode;
exports.origin_map = origin_map;
exports.count = count;
exports.data_of_site = data_of_site;
exports.main = main;
/* origin_map Not a pure module */
