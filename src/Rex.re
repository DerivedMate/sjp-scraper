let re_id_of_url = [%bs.re "/\d+/"];
let re_pref = [%bs.re "/\-$/"];
let re_pais = [%bs.re "/\(państwo\)$/"];

let re_zob_fst = [%bs.re "/zob\.\s*([a-ząęłóśćżźáń]+)/iu"];
let re_zob_lst = [%bs.re "/\s?([a-ząęłóśćżźáń]+)\s?\(zob\.\)/iu"];
let re_od_fst = [%bs.re "/od:?\s*([a-z\(\)ąęłóśćżźáń]+)/iu"];
let re_od_lst = [%bs.re "/od:?\s*([\w\(\)-ąęłóśćżźáń]+\s?,?)+/iu"];

let id_of_url = url => {
  switch (Js.Re.exec_(re_id_of_url, url)) {
  | Some(r) => Js.Re.captures(r)
               ->Belt.Array.keepMap(r => r->Js.Nullable.toOption)[0]
  | None =>
    raise(Js.Exn.raiseError({j|Encountered an unknown type of url $url|j}))
  };
};