open Utils;
let (@@) = Utils.(@@);

let input_file_path = "./src/day5/input";

let getSeat = ((rows, cols), ls) => {
  let (_, (ex, ey)) =
    ls
    |> List.fold_left(
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

  (ex, ey);
};

let getIdFromSeat = ((row, col)) => row * 8 + col;
let getId = g => getIdFromSeat @@ getSeat(g);

let getMaxId = g => List.fold_left(max, 0) @@ List.map(getId(g));

let getInputList =
  List.map(Array.to_list @@ Js.String.split("")) @@
  parseStringList @@
  readFile;

let printValidProduct = () => {
  let inputList = getInputList(input_file_path);

  print_string("1: ");
  print_int(getMaxId((127, 7), inputList));
  print_newline();
  print_string("2: ");
  /*print_int(*/
  /*getProductOfTreeCount(*/
  /*[(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)],*/
  /*inputList,*/
  /*),*/
  /*);*/
  print_newline();
};

print_newline();
printValidProduct();
