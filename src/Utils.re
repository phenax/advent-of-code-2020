[@bs.module "fs"]
external readFileSync_: (string, string) => string = "readFileSync";

let (@@) = (f, g, x) => f(g(x));

let readFile = readFileSync_(_, "utf-8");

let parseIntList =
  List.map(int_of_string) @@
  List.filter(x => x != "") @@
  Array.to_list @@
  Js.String.split("\n");

let toNone = () => None;
let toSome = x => Some(x);

let id = x => x;

let cata = (fnN, fnS, o) =>
  switch (o) {
  | None => fnN()
  | Some(v) => fnS(v)
  };

let fmap = fn => cata(toNone, toSome @@ fn);
