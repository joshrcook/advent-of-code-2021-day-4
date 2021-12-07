let filepath = Node.Path.resolve("src", "input.txt")
let getFileContents = filepath => Node.Fs.readFileAsUtf8Sync(filepath)

let getBingoBoardFromInput = str =>
  Js.String2.splitByRe(str, %re("/\\n/"))
  ->Js.Array2.filter(Js.Option.isSome)
  ->Js.Array2.map(Js.Option.getExn)

let getBingoNumbers = filepath =>
  getFileContents(filepath)
  ->Js.String2.splitByReAtMost(%re("/\\n/"), ~limit=1)
  ->Js.Array2.map(Js.Option.getWithDefault("", _))
  ->Js.Array2.shift
  ->Js.Option.getWithDefault("", _)
  ->Js.String2.split(",")
  ->Js.Array2.map(Belt.Int.fromString)
  ->Js.Array2.filter(Belt.Option.isSome)
  ->Js.Array2.map(Belt.Option.getExn)
  ->Belt.List.fromArray

let getBingoBoards = filepath =>
  getFileContents(filepath)
  ->Js.String2.splitByRe(%re("/\\n\\n/"))
  ->Js.Array2.map(Js.Option.getExn)
  ->Belt.List.fromArray
  ->Belt.List.drop(1)
  ->Js.Option.getWithDefault(list{}, _)
  ->Belt.List.map(x =>
    Js.String2.splitByRe(x, %re("/\\n/"))
    ->Js.Array2.map(Belt.Option.getWithDefault(_, ""))
    ->Js.Array2.map(xs =>
      Js.String2.split(xs, " ")
      ->Js.Array2.map(Belt.Int.fromString)
      ->Js.Array2.filter(Belt.Option.isSome)
      ->Js.Array2.map(Belt.Option.getExn)
      ->Belt.List.fromArray
    )
    ->Belt.List.fromArray
  )

let bingoNumbers = getBingoNumbers(filepath)
let bingoBoards = getBingoBoards(filepath)

let rec transpose = ls => {
  switch ls {
  | list{}
  | list{list{}, ..._} => list{}
  | ls => list{List.map(List.hd, ls), ...transpose(List.map(List.tl, ls))}
  }
}

let rec processInput = (numbers, boards) => {
  let numbers = drawNumbers(numbers, boards, list{})
  switch numbers {
  | None => "No winners"
  | Some(lastNum, numList) =>
    Belt.Int.toString(lastNum * numList->Belt.List.reduce(0, (acc, num) => acc + num))
  }
}
and drawNumbers = (numbers, boards, drawnNumbers) => {
  switch checkBoards(drawnNumbers, boards) {
  | Some(row) => Some(Belt.List.head(drawnNumbers)->Belt.Option.getExn, row)
  | None =>
    switch numbers {
    | list{first, ...rest} => drawNumbers(rest, boards, list{first, ...drawnNumbers})
    | _ => None
    }
  }
}
and checkBoards = (numbers, boards) => {
  switch boards {
  | list{first, ...rest} => {
      let fullBoard = Belt.List.concat(first, transpose(first))
      switch checkBoard(numbers, fullBoard) {
      | None => checkBoards(numbers, rest)
      | Some(_) => {
          let kept =
            Belt.List.flatten(first)->Belt.List.keep(num =>
              numbers->Belt.List.has(num, (a, b) => a == b) == false
            )
          Some(kept)
        }
      }
    }

  | list{} => None
  }
}
and checkBoard = (numbers, board) => {
  switch board {
  | list{first, ...rest} =>
    switch checkRow(numbers, first) {
    | false => checkBoard(numbers, rest)
    | true => Some(first)
    }
  | list{} => None
  }
}
and checkRow = (numbers, row) => {
  switch row {
  | list{first, ...rest} =>
    numbers->Belt.List.has(first, (a, b) => a == b) && checkRow(numbers, rest)
  | list{} => true
  }
}

Js.log(processInput(bingoNumbers, bingoBoards))
