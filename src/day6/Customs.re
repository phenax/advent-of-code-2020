open Utils;
let (@@) = Utils.(@@);

let input_file_path = "./src/day6/input";

let getAnswerCount = List.length @@ uniq @@ List.flatten;
let getTotalAnswers = List.fold_left((+), 0) @@ List.map(getAnswerCount);

let getGroupCommons = List.length @@ uniq @@ foldFromHead(intersection);
let sumGroupCommons = List.fold_left((+), 0) @@ List.map(getGroupCommons);

let getInputList =
  List.map(List.map(split("")) @@ split("\n")) @@
  split("\n\n") @@
  Js.String.trim @@
  readFile;

let printValidProduct = () => {
  let inputList = getInputList(input_file_path);

  print_newline();
  print_string("1: ");
  print_int(getTotalAnswers(inputList));
  print_newline();
  print_string("2: ");
  print_int(sumGroupCommons(inputList));
  print_newline();
  print_newline();
};

print_newline();
printValidProduct();
