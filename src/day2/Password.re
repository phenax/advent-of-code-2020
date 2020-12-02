open Utils;
let (@@) = Utils.(@@);

let input_file_path = "./src/day2/input";

// Check if valid
let isValidPasswordPart1 = ((min, max, ch)) =>
  between(min, max) @@
  Js.Array.length @@
  Js.Array.filter(eq(ch)) @@
  Js.String.split("");

let isValidPasswordPart2 = ((fst, snd, ch), pass) => {
  false;
};

// Parse input
let parsePassword =
  chain(result => {
    let captures = result |> List.map(cata(() => "-", id));

    switch (captures) {
    | [min, max, ch, password] =>
      Some((int_of_string(min), int_of_string(max), ch, password))
    | _ => None
    };
  }) @@
  parseRegexCapture(Js.Re.fromString("^(\\d+)-(\\d+)\\s+(\\w):\\s+(.*)$"));

let getInputList = parseStringList @@ readFile;

let printValidProduct = () => {
  let inputList = getInputList(input_file_path);

  let parsedResults =
    inputList
    |> List.map(parsePassword)
    |> List.filter(cata(always(false), always(true)))
    |> List.map(cata(() => (0, 0, "", ""), id));

  let getValidPasswordCount = fn =>
    List.length @@
    List.filter(((mn, mx, ch, pass)) => fn((mn, mx, ch), pass));

  print_string("1: ");
  print_int(getValidPasswordCount(isValidPasswordPart1, parsedResults));
  print_newline();
  print_string("2: ");
  print_int(getValidPasswordCount(isValidPasswordPart2, parsedResults));
  print_newline();
};

print_newline();
printValidProduct();
