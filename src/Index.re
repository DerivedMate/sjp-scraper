[%raw "require('isomorphic-fetch')"];
open PromiseMonad;
open Belt.Array;
let partition_size = 20;

let main = () => {
  Js.Console.timeStart("timer");
  Js.Promise.all(
    "aąbcćde" // fghijklłmnoóprsśtuwyzźż
    ->Js.String2.split("")
    ->keep(a => a->Js.String2.length > 0)
    ->map(l => l->Js.String2.toLocaleUpperCase->Fetcher.fetch_api),
  )
  >>- (
    htmls => {
      let html = Js.String2.concatMany("", htmls);
      Dumdum.id_of_api(html)->Helpers.partitions(partition_size);
    }
  )
  >>= (x => x->Belt.Array.slice(~offset=0, ~len=50)->Process.explode)
  >>- (_ => Js.Console.timeEnd("timer"))
  >>/ Js.Console.error;
};

// main();

[|[|"53713"|]|]->Process.explode;