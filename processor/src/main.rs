use serde::{Deserialize, Serialize};
use serde_json::Result;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;


#[derive(Serialize, Deserialize, Debug)]
struct Entry {
  id: String,
  name: String,
  part: String,
  classif: Vec<Vec<String>>,
  flex: Vec<Vec<String>>,
}

fn op<P: AsRef<Path>>(path: P) -> Result<HashMap<String, i64>> {
  let mut res = HashMap::new();

  let file = File::open(path).unwrap();
  let reader = BufReader::new(file);

  let records: Vec<Entry> = serde_json::from_reader(reader)?;

  let total = records.len();

  for (i, e) in records.iter().enumerate() {
    let old_count = match res.get(&e.part) {
      Some(c) => *c,
      None => 0,
    };
    res.insert(e.part.to_owned(), old_count + 1);

    println!("Processed {}/{}", i + 1, total);
  }

  Ok(res)
}

fn main() {
  let res = op("input.json").unwrap();
  let mut output = csv::Writer::from_path("out.csv").unwrap();

  for (k, v) in res.iter() {
    output.write_record(&[k, &v.to_string()]).unwrap();
  } 
  output.flush().unwrap();
}
