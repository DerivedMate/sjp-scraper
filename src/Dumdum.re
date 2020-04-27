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
let origin_of_word = (map, word) =>
  switch (map->Belt.Array.getBy(([|a, _|]) => a == word)) {
  | Some([|_, a|]) => a
  | _ => ""
  };

let rex_pais = (map, raw) => {
  switch (
    map->Belt.Array.getBy(([|k, v|]) => {
      let clean_k = Js.String2.replace(k, ".", "\.");
      let r = Js.Re.fromStringWithFlags(clean_k, ~flags="iu");
      Js.Re.test_(r, raw);
    })
  ) {
  | Some([|_, p|]) => Some(p)
  | _ => None
  };
};
/*
 let origins_of_dom = (dom, map) => {
   let explicit_origins =
     dom
     ->Cheerio.select(".pochodzenie > tbody > tr .pochodzenie_jezyk")
     ->Element.map((_, e) => e->Element.load->Element.text0)
     ->Element.toArray
     ->Belt.Array.map(l =>
         Js.String2.(l->trim->toLocaleLowerCase) |> origin_of_word(map)
       )
     ->Belt.Array.keep(a => a->Js.String2.length > 0);

   switch (explicit_origins->Belt.Array.length) {
   | 0 =>
     let ems =
       dom->Cheerio.select(".pochodzenie > tbody > tr .pochodzenie_uwagi em");
     let em_count = ems->Element.length;
     let origin =
       if (em_count > 0) {
         let fst =
           ems
           ->Element.get1(0)
           ->Element.load
           ->Element.text0
           ->Helpers.get_null_or("");

         if (fst == "" || Js.Re.test_(Rex.re_pref, fst)) {
           ems
           ->Element.get1(em_count - 1)
           ->Element.load
           ->Element.text0
           ->Helpers.get_null_or("");
         } else if (Js.Re.test_(Rex.re_pais, fst)) {
           "";
         } else {
           fst;
         };
       } else {
         dom
         ->Cheerio.select(".pochodzenie_uwagi p")
         ->Element.get1(0)
         ->Element.load
         ->Element.text0
         ->Helpers.get_null_or("")
         ->Helpers.pointer_of_od;
       };
     if (origin == "") {
       [|Const.inline_origin_sig|];
     } else {
       [|Const.pointer_sig, origin|];
     };

   | _ => explicit_origins
   };
 };*/

let origins_of_dom = (dom, map) => {
  let explicit_origins =
    dom
    ->Cheerio.select(".pochodzenie > tbody > tr .pochodzenie_jezyk")
    ->Element.map((_, e) => e->Element.load->Element.text0)
    ->Element.toArray
    ->Belt.Array.map(l =>
        Js.String2.(l->trim->toLocaleLowerCase) |> origin_of_word(map)
      )
    ->Belt.Array.keep(a => a->Js.String2.length > 0);

  switch (explicit_origins->Belt.Array.length) {
  | 0 =>
    let poch = dom->Cheerio.select(".pochodzenie_uwagi");
    let raw = poch->Element.text0->Helpers.get_null_or("");

    let arr =
      switch (
        Js.Re.exec_(Rex.re_zob_fst, raw),
        Js.Re.exec_(Rex.re_zob_lst, raw),
        Js.Re.exec_(Rex.re_od_fst, raw),
        Js.Re.exec_(Rex.re_od_lst, raw),
      ) {
      | (Some(r), _, _, _)
      | (_, Some(r), _, _)
      | (_, _, Some(r), _)
      | (_, _, _, Some(r)) => r->Js.Re.captures
      | _ => [||]
      };

    Js.Console.log(raw);
    Js.Console.log(arr);

    let origin =
      switch (Helpers.arr_lst(arr)) {
      | Some(s) => s->Helpers.get_null->Js.String2.trim
      | None => ""
      };

    switch (rex_pais(map, raw)) {
    | Some(p) => [|p|]
    | None =>
      if (origin == "") {
        if (raw != "") {
          Js.Console.error(raw);
        };
        [|Const.inline_origin_sig|];
      } else {
        [|Const.pointer_sig, origin|];
      }
    };

  | _ => explicit_origins
  };
};

let adjective_of_dom = dom => {
  let arr =
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
    ->Element.toArray;
  (
    switch (arr->Belt.Array.slice(~offset=8, ~len=35)) {
    | exception _ => arr
    | x => x
    }
  )
  ->Helpers.partitions(5);
};