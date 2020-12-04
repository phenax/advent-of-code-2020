open Utils;
let (@@) = Utils.(@@);

let input_file_path = "./src/day4/input";

let isOneOf = (ls, v) => ls->Belt.List.has(v, eq);

let has = k =>
  cata(() => false, _ => true) @@ List.find_opt(((key, _)) => key == k);

let isValidPassport1 = passport => {
  let len = List.length(passport);
  switch (passport) {
  | _ when len == 7 && !has("cid", passport) => true
  | _ when len >= 8 => true
  | _ => false
  };
};

let parseHeight =
  chain(result => {
    switch (result) {
    | [Some(h), Some(u)] => Some((int_of_string(h), u))
    | _ => None
    }
  }) @@
  parseRegexCapture(Js.Re.fromString("^(\d+)([a-z]+)$"));

let testRegex = reg => Js.Re.test_(Js.Re.fromString(reg));

let log = (v, x) => {
  print_endline(v ++ ":::" ++ (x ? "1" : "----------"));
  x;
};

let isFieldValid = p =>
  switch (p) {
  | ("byr", value) => value |> int_of_string |> between(1920, 2002)
  | ("iyr", value) => value |> int_of_string |> between(2010, 2020)
  | ("eyr", value) => value |> int_of_string |> between(2020, 2030)
  | ("hgt", value) =>
    let (h, u) = value |> parseHeight |> foldOr(always((0, "")));
    switch (u) {
    | "cm" => h |> between(150, 193)
    | "in" => h |> between(59, 76)
    | _ => false
    };
  | ("hcl", value) => value |> testRegex("^#[a-f0-9]{6}$")
  | ("ecl", value) =>
    value |> isOneOf(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  | ("pid", value) => value |> testRegex("^[0-9]{9}$")
  | ("cid", _) => true
  | _ => false
  };

let isValidPassport2 = passport =>
  isValidPassport1(passport)
  && passport
  |> List.map(isFieldValid)
  |> List.find_opt(eq(false))
  |> cata(() => true, _ => false);

let countValidPassports = isValid =>
  List.fold_left((acc, p) => acc + (isValid(p) ? 1 : 0), 0);

let toKeyVal = ls =>
  switch (ls) {
  | [] => ("", "")
  | [k] => (k, "")
  | [k, v, ..._] => (k, v)
  };

let getInputList =
  List.map(
    List.map(
      cata(
        always(("", "")),
        toKeyVal @@ Array.to_list @@ Js.String.split(":"),
      ),
    ) @@
    List.filter(cata(() => false, s => s != "")) @@
    Array.to_list @@
    Js.String.splitByRe(Js.Re.fromString("\\s+")),
  ) @@
  Array.to_list @@
  Js.String.split("\n\n") @@
  readFile;

let printValidProduct = () => {
  let inputList = getInputList(input_file_path);

  print_string("1: ");
  print_int(countValidPassports(isValidPassport1, inputList));
  print_newline();
  print_string("2: ");
  print_int(countValidPassports(isValidPassport2, inputList));
  print_newline();
};

print_newline();
printValidProduct();
