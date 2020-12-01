open Utils;

let input = "./src/day1/input" |> readFile |> parseIntList |> Array.to_list;

let findMatchingPair = target => List.find_opt(n => n + target == 2020);

let rec findValidPairs = ls =>
  switch (ls) {
  | [] => None
  | [head, ...tail] =>
    switch (findMatchingPair(head, tail)) {
    | None => findValidPairs(tail)
    | Some(x) => Some((head, x))
    }
  };

let printValidProduct = () => {
  let (fst, snd) = input |> findValidPairs |> cata(() => (0, 0), id);

  print_newline();
  print_string("Valid product: ");
  print_int(fst * snd);
  print_newline();
};

printValidProduct();
