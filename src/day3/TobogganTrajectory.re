open Utils;
let (@@) = Utils.(@@);

let input_file_path = "./src/day3/input";

let rec getTreeCount = ((x, y), current, grid) => {
  switch (grid) {
  | [] => 0
  | grid =>
    let (init, rest) = Belt.List.splitAt(grid, y) |> foldOr(() => ([], []));
    let cellWidth =
      init |> Belt.List.get(_, 0) |> cata(() => 0, List.length);
    let current = current mod cellWidth;
    let cell =
      init
      |> Belt.List.get(_, 0)
      |> chain(Belt.List.get(_, current))
      |> foldOr(() => ".");
    getTreeCount((x, y), current + x, rest) + (cell == "#" ? 1 : 0);
  };
};

let getInputList =
  List.map(Array.to_list @@ Js.String.split("")) @@
  parseStringList @@
  readFile;

let printValidProduct = () => {
  let inputList = getInputList(input_file_path);

  print_string("1: ");
  print_int(getTreeCount((3, 1), 0, inputList));
  /*print_newline();*/
  /*print_string("2: ");*/
  /*print_int(getValidPasswordCount(isValidPasswordPart2, parsedResults));*/
  print_newline();
};

print_newline();
printValidProduct();
