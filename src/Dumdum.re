open BsCheerio;
type word = string;

[@bs.module "./external.js"]
external clean_declension: string => array(string) = "clean_declension";
[@bs.module "./external.js"]
external clean_conjugation: string => string = "clean_conjugation";

let id_of_api = resp => {
  let dom = Cheerio.load(resp);
  let a_selector = "a";
  dom
  ->Cheerio.select(a_selector)
  ->Element.map((_, e) => {
      let tag = e->Element.load->Element.attr1("href");
      switch (tag->Js.Nullable.toOption) {
      | Some(h) => h->Rex.id_of_url
      | None => "0"
      };
    })
  ->Element.toArray;
};

let part_of_dom = dom =>
  dom
  ->Cheerio.select("[class*=odmiana] em")
  ->Element.get1(0)
  ->Element.load
  ->Element.text0
  ->Helpers.get_null;

let classif_of_dom = dom =>
  dom
  ->Cheerio.select(".center.word_content")
  ->Element.map((_, el) =>
      el
      ->Element.load
      ->Element.text0
      ->Helpers.get_null
      ->Js.String2.trim
      ->Js.String2.toLocaleLowerCase
    )
  ->Element.toArray
  ->Belt.Array.map(t =>
      t
      ->Js.String2.replaceByRe([%bs.re "/[\\n\\t]+/g"], ";")
      ->Js.String2.splitByRe([%bs.re "/\;\s+/g"])
      ->Belt.Array.keepMap(r => r)
    )
  ->Belt.Array.reduce([||], (acc, a) =>
      if (acc->Belt.Array.some(b => Belt.Array.eq(a, b, (a, b) => a == b))) {
        acc;
      } else {
        Belt.Array.concat(acc, [|a|]);
      }
    );

let name_of_dom = dom =>
  dom
  ->Cheerio.select("h1")
  ->Element.get1(0)
  ->Element.load
  ->Element.text0
  ->Helpers.get_null
  ->Js.String2.trim;

let conjugation_of_dom = dom => {
  let (arr, _) = dom
                  ->Cheerio.select(".odmiana_konspekt>tbody tr")
                  ->Element.get1(2)
                  ->Element.load
                  ->Element.contents
                  ->Element.get1(1)
                  ->Element.load
                  ->Element.map((_, e) =>
                      e->Element.load->Element.text0->Helpers.get_null_or("")
                    )
                  ->Element.toArray
                  ->Belt.Array.map(r => {
                      r
                      ->Js.String2.split("\n")
                      ->Belt.Array.map(w => w->clean_conjugation)
                      ->Belt.Array.slice(~offset=3, ~len=6)
                      // Group into (singular, plugar) array pairs
                      ->Belt.Array.reduceWithIndex(
                          ([||], ""), ((acc, t), a, i) =>
                          if (i mod 2 == 0) {
                            (acc, a);
                          } else {
                            (Belt.Array.concat(acc, [|[|t, a|]|]), "");
                          }
                        )
                    })[0];
  arr;
};

let declension_of_dom = dom => {
  let arr =
    dom
    ->Cheerio.select(".odmiana_tb > tbody")
    ->Element.first
    ->Element.contents
    ->Element.map((_, e) => e->Element.load->Element.text0->Helpers.get_null)
    ->Element.toArray;
  switch (arr->Belt.Array.sliceToEnd(4)) {
  | arr =>
    arr
    ->Belt.Array.map(r => {r->clean_declension})
    ->Belt.Array.keep(r => r->Belt.Array.some(w => w->Js.String2.length > 0))
  | exception _ =>
    arr
    ->Belt.Array.map(r => {r->clean_declension})
    ->Belt.Array.keep(r => r->Belt.Array.some(w => w->Js.String2.length > 0))
  };
};

[@decco]
type origin_map = array(array(string));
let origin_of_word = (map, word) => {
  map->Belt.Array.getBy(([|a, _|]) => a == word)->Belt.Option.getExn[1];
};
let origins_of_dom = (dom, map) => {
  let explicit_origins =
    dom
    ->Cheerio.select(".pochodzenie > tbody > tr .pochodzenie_jezyk")
    ->Element.map((_, e) => e->Element.load->Element.text0)
    ->Element.toArray
    ->Belt.Array.map(l =>
        Js.String2.(l->trim->toLocaleLowerCase) |> origin_of_word(map)
      );

  switch (explicit_origins->Belt.Array.length) {
  | 0 =>
    let origin =
      dom
      ->Cheerio.select(".pochodzenie > tbody > tr .pochodzenie_uwagi em")
      ->Element.get1(0)
      ->Element.load
      ->Element.text0
      ->Helpers.get_null;

    [|Const.pointer_sig, origin|];
  | _ => explicit_origins
  };
};

let adjective_of_dom = dom =>
  dom
  ->Cheerio.select(".odmiana tr")
  ->Element.map((_, r) =>
      r
      ->Element.load
      ->Element.contents
      ->Element.map((_, e) => e->Element.load->Element.text0)
      ->Element.toArray
      ->Belt.Array.sliceToEnd(1)
    )
  ->Element.toArray
  ->Belt.Array.slice(~offset=8, ~len=35)
  ->Helpers.partitions(5);