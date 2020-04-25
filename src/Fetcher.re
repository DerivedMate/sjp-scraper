[@bs.module "./external.js"]
external fetch_api: string => Js.Promise.t(string) = "f";

[@bs.module "./external.js"]
external fetch_id: string => Js.Promise.t((string, string)) = "fetch_id";

let html_of_id = id => Helpers.read_sync({j|./pages/$id.html|j});