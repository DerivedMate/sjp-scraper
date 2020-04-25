[@bs.module "util"] external inspect: ('a, Js.t({..})) => 'b = "inspect";
let deep = x => inspect(x, {"depth": 50, "colors": true});
let do_log = true;
let deep_log = x => do_log ? Js.Console.log(deep(x)) : ();
let get_null = n => n->Js.Nullable.toOption->Belt.Option.getExn;
let get_null_or = (n, o) =>
  n->Js.Nullable.toOption->Belt.Option.getWithDefault(o);

let hd_tl_arr = arr => {
  open Belt.Array;
  let hd = get(arr, 0)
  and tl =
    switch (arr |> length) {
    | 0
    | 1 => [||]
    | _ => sliceToEnd(arr, 1)
    };
  (hd, tl);
};

let partitions = (arr, partition_size) =>
  Belt.Array.(
    arr
    ->reduceWithIndex([||], (acc, id, i) => {
        switch (i mod partition_size) {
        | 0 => concat([|[|id|]|], acc)
        | _ =>
          let tail = sliceToEnd(acc, 1)
          and head = getExn(acc, 0);
          concat([|concat([|id|], head)|], tail);
        }
      })
    ->map(a => a->reverse)
    ->reverse
  );

let arr_lst = arr => {
  let l = Belt.Array.length(arr);
  if (l == 0) {
    None;
  } else {
    Some(arr[l - 1]);
  };
};

[@bs.module "./external.js"]
external open_languages: string => string = "open_languages";

[@bs.module "./external.js"]
external pointer_of_od: string => string = "pointer_of_od";

[@bs.module "./external.js"]
external write_sync: (string, string) => unit = "write_sync";
[@bs.module "./external.js"]
external read_sync: string => string = "read_sync";

[@bs.module "./external.js"]
external open_ids: string => array(string) = "open_ids";

module Operators = {
  let (??) = (a, b) => a->Belt.Option.flatMap(b);
  let (^-^) = (a, b) => a->Belt.Result.flatMap(b);
};