let re_id_of_url = [%bs.re "/\d+/"];
let id_of_url = url => {
  switch (Js.Re.exec_(re_id_of_url, url)) {
  | Some(r) => Js.Re.captures(r)
               ->Belt.Array.keepMap(r => r->Js.Nullable.toOption)[0]
  | None =>
    raise(Js.Exn.raiseError({j|Encountered an unknown type of url $url|j}))
  };
};