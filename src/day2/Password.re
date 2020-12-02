open Utils;
let (@@) = Utils.(@@);

let input_file_path = "./src/day2/input";

// Check if valid
let isValidPassword = ((min, max, ch)) =>
  between(min, max) @@
  Js.Array.length @@
  Js.Array.filter(eq(ch)) @@
  Js.String.split("");

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

  let matches =
    inputList
    |> List.map(parsePassword)
    |> List.filter(cata(always(false), always(true)))
    |> List.map(cata(() => (0, 0, "", ""), id));

  let result =
    matches
    |> List.filter(((min, max, ch, pass)) =>
         isValidPassword((min, max, ch), pass)
       );

  print_int(List.length(result));
};

print_newline();
printValidProduct();
