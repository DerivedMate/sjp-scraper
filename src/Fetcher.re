[@bs.module "./external.js"]
external fetch_api: string => Js.Promise.t(string) = "f";

[@bs.module "./external.js"]
external fetch_id: string => Js.Promise.t((string, string)) = "fetch_id";