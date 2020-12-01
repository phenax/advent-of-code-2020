[@bs.module "fs"]
external readFileSync_: (string, string) => string = "readFileSync";

let (@@) = (f, g, x) => f(g(x));

let readFile = readFileSync_(_, "utf-8");

let parseIntList =
  Js.Array.map(int_of_string) @@
  Js.Array.filter(x => x != "") @@
  Js.String.split("\n");
