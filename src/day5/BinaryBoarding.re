open Utils;
let (@@) = Utils.(@@);

let input_file_path = "./src/day5/input";

let snd = ((_, x)) => x;

let getSeat = ((rows, cols)) =>
  snd @@
  List.fold_left(
    (((sx, sy), (ex, ey)), ch) => {
      let xh = (ex + sx) / 2;
      let yh = (ey + sy) / 2;
      switch (ch) {
      | "B" => ((xh, sy), (ex, ey))
      | "F" => ((sx, sy), (xh, ey))
      | "R" => ((sx, yh), (ex, ey))
      | "L" => ((sx, sy), (ex, yh))
      | _ => ((sx, sy), (ex, ey))
      };
    },
    ((0, 0), (rows, cols)),
  );

let getIdFromSeat = ((row, col)) => row * 8 + col;

let getId = g => getIdFromSeat @@ getSeat(g);

let getMaxId = g => List.fold_left(max, 0) @@ List.map(getId(g));

let getSeatIds = g => List.map(getId(g));

let getInputList =
  List.sort((a, b) => getId((127, 7), a) - getId((127, 7), b)) @@
  List.map(Array.to_list @@ Js.String.split("")) @@
  parseStringList @@
  readFile;

let printValidProduct = () => {
  let inputList = getInputList(input_file_path);
  let g = (127, 7);

  print_string("1: ");
  print_int(getMaxId((127, 7), inputList));
  print_newline();
  print_string("2: ");
  print_newline();
  let l1 = Belt.List.makeBy(getIdFromSeat(g), id);
  let l2 = getSeatIds(g, inputList);
  asymmetricDifference(l1, l2) |> printList(print_int);
  print_newline();
};

print_newline();
printValidProduct();
