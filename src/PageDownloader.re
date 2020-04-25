open PromiseMonad;
open Belt.Array;

[@decco]
type ids = array(string);

let download_ids = () => {
  Js.Promise.all(
    {js|aąbcćdefghijklłmnoóprsśtuwyzźż|js}
    ->Js.String2.split("")
    ->keep(a => a->Js.String2.length > 0)
    ->map(l => l->Js.String2.toLocaleUpperCase->Fetcher.fetch_api),
  )
  >>- (
    htmls => {
      let html = Js.String2.concatMany("", htmls);
      Dumdum.id_of_api(html)->Js.Json.stringArray->Js.Json.stringify
      |> Helpers.write_sync("./ids.json");
    }
  );
};

let main = () => {
  let p_size = 50;
  let timer_name = "PAGE_TIMER";

  let rec page_loop = (len0, count, id, rest) => {
    id->Belt.Array.map(Fetcher.fetch_id)->Js.Promise.all
    >>- (
      rs =>
        rs->Belt.Array.forEach(((h, id)) => {
          Helpers.write_sync({j|./pages/$id.html|j}, h);
          ();
        })
    )
    >>= (
      _ => {
        Js.Console.error({j|$count/$len0|j});
        let (hd, tl) = Helpers.hd_tl_arr(rest);
        switch (hd) {
        | Some(hd) => page_loop(len0, count + 1, hd, tl)
        | _ =>
          Js.Console.timeEnd(timer_name);
          Js.Promise.resolve(42);
        };
      }
    )
    >>| (
      err => {
        Js.Console.error(err);
        let rest = rest->Belt.Array.concat([|id|]);
        let (hd, tl) = Helpers.hd_tl_arr(rest);
        switch (hd) {
        | Some(hd) => page_loop(len0, count, hd, tl)
        | _ => Js.Promise.resolve(42)
        };
      }
    );
  };

  Js.Console.timeStart(timer_name);
  let ids = Helpers.open_ids("ids2.json")->Helpers.partitions(p_size);
  let len0 = Array.length(ids);
  let (hd, tl) = Helpers.hd_tl_arr(ids);
  switch (hd) {
  | Some(hd) => page_loop(len0, 1, hd, tl)
  | _ => Js.Promise.resolve(42)
  };
};