let (@@) = (f, g, x) => f(g(x));

let readFile = Node.Fs.readFileAsUtf8Sync;

let toNone = () => None;
let toSome = x => Some(x);

let id = x => x;
let always = (x, _) => x;
let eq = (a, b) => a == b;
let between = (min, max, x) => x >= min && x <= max;
let at = (index, arr) =>
  Js.Array.length(arr) > index ? Some(arr[index]) : None;

let cata = (fnN, fnS, o) =>
  switch (o) {
  | None => fnN()
  | Some(v) => fnS(v)
  };

let fmap = fn => cata(toNone, toSome @@ fn);
let foldOr = fn => cata(fn, id);
let chain = fn => cata(toNone, fn);

// Parsing
let parseStringList =
  List.filter(x => x != "") @@ Array.to_list @@ Js.String.split("\n");

let parseIntList = List.map(int_of_string) @@ parseStringList;

let parseRegexCapture = regex =>
  fmap(
    Array.to_list @@
    Js.Array.sliceFrom(1) @@
    Js.Array.map(Js.Nullable.toOption) @@
    Js.Re.captures,
  ) @@
  Js.Re.exec_(regex);
