open PromiseMonad;
open BsCheerio;

[@decco]
type data = {
  id: string,
  name: string, // h1
  part: string, // em > .odmiana_czesc_mowy
  origin: array(string),
  classif: array(array(string)), // .center.word_content [arr]
  flex: array(array(string)),
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
  let classif = Dumdum.classif_of_dom(dom);
  let name = Dumdum.name_of_dom(dom);
  let flex =
    switch (part) {
    | "czasownik" => Dumdum.conjugation_of_dom(dom)
    | "rzeczownik" => Dumdum.declension_of_dom(dom)
    | "przymiotnik" => Dumdum.adjective_of_dom(dom)
    | _ => [||]
    };
  let origin = dom->Dumdum.origins_of_dom(origin_map);

  {id, name, part, classif, flex, origin};
};

let rec process = (ids, rest) => {
  ids
  ->Belt.Array.map(id =>
      id->Fetcher.fetch_id >>- (((h, id)) => data_of_site(h, id))
    )
  ->Js.Promise.all
  >>- (
    words =>
      words->Belt.Array.forEach(w => {
        let w = w->data_encode->Js.Json.stringify;
        Js.Console.log({j|$w,|j});
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