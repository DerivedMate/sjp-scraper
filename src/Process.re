open PromiseMonad;
open BsCheerio;

[@decco]
type data = {
  id: string,
  name: string, // h1
  part: string, // em > .odmiana_czesc_mowy
  origin: array(string),
  // classif: array(array(string)), // .center.word_content [arr]
  // flex: array(array(string)),
};

[@decco]
type words = array(data);

let origin_map =
  Helpers.open_languages("languages.json")
  ->Js.Json.parseExn
  ->Dumdum.origin_map_decode
  ->Belt.Result.getExn;

let count = ref(0);

let data_of_site = (html, id) => {
  let dom = Cheerio.load(html);
  let part = Dumdum.part_of_dom(dom);
  // let classif = Dumdum.classif_of_dom(dom);
  let name = Dumdum.name_of_dom(dom);
  /*let flex =
    switch (part) {
    | "czasownik" => Dumdum.conjugation_of_dom(dom)
    | "rzeczownik" => Dumdum.declension_of_dom(dom)
    | "przymiotnik" => Dumdum.adjective_of_dom(dom)
    | _ => [||]
    };*/
  let origin = dom->Dumdum.origins_of_dom(origin_map);
  if (Array.length(origin) > 0 && origin[0] == Const.inline_origin_sig) {
    count := count^ + 1;
  };

  {id, name, part, /*classif, flex,*/ origin};
};
/*
 let rec process = (ids, rest) => {
   ids
   ->Belt.Array.map(id =>
       id->Fetcher.fetch_id >>- (((h, id)) => data_of_site(h, id))
     )
   ->Js.Promise.all
   >>- (
     words =>
       words->Belt.Array.forEachWithIndex((i, w) => {
         let w = w->data_encode->Js.Json.stringify;
         if (i == Array.length(ids) - 1 && Array.length(rest) == 0) {
           Js.Console.log({j|$w|j});
         } else {
           Js.Console.log({j|$w,|j});
         };
       })
   )
   >>= (
     _ => {
       count := count^ + Belt.Array.length(ids);
       let c = count^;
       Js.Console.error({j|$c|j});
       let (hd, tl) = Helpers.hd_tl_arr(rest);
       switch (hd) {
       | Some(hd) => process(hd, tl)
       | _ =>
         Js.Console.log("]");
         Js.Promise.resolve(1);
       };
     }
   )
   >>| (
     e => {
       let id = Helpers.deep(ids);
       Js.Console.error(e);
       Js.Console.error({j|id lost: $id|j});
       let (hd, tl) = Helpers.hd_tl_arr(rest);
       switch (hd) {
       | Some(hd) =>
         process(
           hd,
           // Remove the ones that have already failed
           if (Belt.Array.length(ids) == 1) {
             tl;
           } else {
             Belt.Array.concat(tl, ids->Belt.Array.map(id => [|id|]));
           },
         )
       | _ =>
         Js.Console.log("]");
         Js.Promise.resolve(1);
       };
     }
   );
 };

 let explode = ids => {
   Js.Console.log("[");
   let (hd, tl) = Helpers.hd_tl_arr(ids);
   switch (hd) {
   | Some(hd) => process(hd, tl)
   | _ => Js.Promise.resolve(1)
   };
 };
 */

let main = ids => {
  let count_inl = ref(0);
  let count_ptr = ref(0);
  let rec loop = (len0, i, id, rest) => {
    switch (id) {
    | None => ()
    | Some(id) =>
      switch (Fetcher.html_of_id(id)) {
      | exception e =>
        Js.Console.error({j|$id => $e|j});
        let (hd, tl) = Helpers.hd_tl_arr(rest);
        loop(len0, i, hd, tl);
      | html =>
        let wd = html->data_of_site(id);
        let w = wd->data_encode->Js.Json.stringify;
        if (i == len0 && Array.length(rest) == 0) {
          Js.Console.log({j|$w|j});
        } else {
          Js.Console.log({j|$w,|j});
        };

        if (Array.length(wd.origin) > 0) {
          if(wd.origin[0] == Const.pointer_sig) {
            count_ptr := count_ptr^ + 1;
          } else if(wd.origin[0] == Const.inline_origin_sig) {
            count_inl := count_inl^ + 1;
          }
        }
      };
      let c = count^;
      let inl = count_inl^;
      let ptr = count_ptr^;
      let pr =
        Js.Int.(toFloat(i) /. toFloat(len0) *. 100.)
        ->Js.Float.toFixedWithPrecision(~digits=2);
      Js.Console.error(
        {j|Progress: $i/$len0 [$pr]; NoOri: $c; Ptr: $ptr; Inl: $inl|j},
      );
      let (hd, tl) = Helpers.hd_tl_arr(rest);
      loop(len0, i + 1, hd, tl);
    };
  };

  let (hd, tl) = Helpers.hd_tl_arr(ids);
  loop(Belt.Array.length(ids), 1, hd, tl);
};