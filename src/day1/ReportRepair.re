open Utils;
let (@@) = Utils.(@@);

let input_file_path = "./src/day1/input";

let findMatchingPair = (target, x) => List.find_opt(n => n + x == target);

let rec findValidPairs = (target, ls) =>
  switch (ls) {
  | [] => None
  | [head, ...tail] =>
    switch (findMatchingPair(target, head, tail)) {
    | None => findValidPairs(target, tail)
    | Some(x) => Some((head, x))
    }
  };

let rec findValid3Tuple = (target, ls) =>
  switch (ls) {
  | [] => None
  | [head, ...tail] =>
    switch (findValidPairs(target - head, tail)) {
    | None => findValid3Tuple(target, tail)
    | Some((x, y)) => Some((head, x, y))
    }
  };

let getInputList = parseIntList @@ readFile;

let printValidProduct = () => {
  let inputList = getInputList(input_file_path);

  let (fst, snd) =
    inputList |> findValidPairs(2020) |> cata(() => (0, 0), id);
  print_string("1: ");
  print_int(fst * snd);
  print_newline();

  let (fst, snd, thr) =
    inputList |> findValid3Tuple(2020) |> cata(() => (0, 0, 0), id);
  print_string("2: ");
  print_int(fst * snd * thr);
  print_newline();
};

print_newline();
printValidProduct();
