let (@@) = (f, g, x) => f(g(x));

let readFile = Node.Fs.readFileAsUtf8Sync;

let toNone = () => None;
let toSome = x => Some(x);

let id = x => x;
let always = (x, _) => x;
let eq = (a, b) => a == b;
let not = (pred, x) => !pred(x);
let between = (min, max, x) => x >= min && x <= max;
let at = (index, arr) =>
  Js.Array.length(arr) > index ? Some(arr[index]) : None;

let isOneOf = (ls, v) => ls->Belt.List.has(v, eq);

let rec uniq =
  fun
  | [] => []
  | [h, ...t] when isOneOf(t, h) => uniq(t)
  | [h, ...t] => List.concat([[h], uniq(t)]);

let split = str => Array.to_list @@ Js.String.split(str);

let asymmetricDifference = (l1, l2) =>
  l1 |> List.filter(item => !isOneOf(l2, item));

let intersection = (l1, l2) => {
  let (l1, l2) = List.length(l1) < List.length(l2) ? (l2, l1) : (l1, l2);
  l1 |> List.filter(isOneOf(l2));
};

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

// Has key in list of pairs
let has = k => Belt.List.has(_, (k, ""), ((key, _), (k, _)) => key == k);

let testRegex = reg => Js.Re.test_(Js.Re.fromString(reg));

// Print list
let printList = (printItem, ls) => {
  print_string("[ ");
  ls
  |> List.iter(id => {
       printItem(id);
       print_string(", ");
     });
  print_string("]");
};

let tap = (fn, x) => {
  fn(x);
  x;
};

let tapList = fn => tap(List.iter(fn));

let foldFromHead = fn =>
  fun
  | [] => []
  | [head, ...tail] => tail |> List.fold_left(fn, head);
