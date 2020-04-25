[%raw "require('isomorphic-fetch')"];
let partition_size = 12;
open PromiseMonad;

let main = () => {
  Js.Console.timeStart("timer");
  Js.Console.log("[");
  Helpers.open_ids("ids.json")
  /*
   [|
     "8126",
     "88158",
     "52507",
     "22624",
     "79729",
     "10693",
     "11506",
     "86380",
     "65678",
     "91778",
     "53766",
     "30688",
     "63110",
     "86593",
     "71707",
     "28984",
     "22623",
     "64898",
     "83024",
     "22613"
   |]*/
  ->Process.main;
  Js.Console.log("]");
  Js.Console.timeEnd("timer");
};
let fetch_id_diff = {
  let cids = Helpers.open_ids("ids.json");
  Belt.Array.(
    {js|aąbcćdefghijklłmnoóprsśtuwyzźż|js}
    ->Js.String2.split("")
    ->keep(a => a->Js.String2.length > 0)
    ->map(l => l->Js.String2.toLocaleUpperCase->Fetcher.fetch_api)
    ->Js.Promise.all
    >>- (
      htmls => {
        let html = Js.String2.concatMany("", htmls);
        let ids = Dumdum.id_of_api(html);
        ids
        ->Belt.Array.keep(id => !some(cids, a => a == id))
        ->Js.Json.stringArray
        ->Js.Json.stringify
        |> Helpers.write_sync("./ids2.json");
      }
    )
  )
  ->ignore;
  ();
};

main()