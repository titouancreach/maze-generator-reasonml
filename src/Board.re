type board = array(array(Cell.t));
type t = {
  board,
  curr: Point.t,
  stack: Stack.t(Point.t),
  preview: bool,
  player: Point.t,
};

let make = (~curr=Point.make(0, 0), ~preview=true, cols: int, rows: int): t => {
  {
    board: ArrayLabels.make_matrix(~dimx=rows, ~dimy=cols, Cell.defaultCell),
    curr,
    stack: Stack.create(),
    preview,
    player: Point.make(0, 0),
  };
};

let getStackSize = (board: t): int => Stack.length(board.stack);

let getPercentageGenerated = (board: t, cols: int, rows: int): int => {
  let totalVisited =
    ArrayLabels.fold_left(
      ~f=
        (sum, item) =>
          sum
          + ArrayLabels.fold_left(
              ~f=(sumx, cell: Cell.t) => sumx + (cell.visited ? 1 : 0),
              ~init=0,
              item,
            ),
      ~init=0,
      board.board,
    );

  float_of_int(totalVisited)
  /. float_of_int(cols * rows)
  *. 100.
  |> int_of_float;
};

let getCell = (board: t, x: int, y: int): Cell.t => {
  ArrayLabels.get(ArrayLabels.get(board.board, y), x);
};

let getCellFromPoint = (board: t, (x, y): Point.t): Cell.t => {
  getCell(board, x, y);
};

let getNeighbours = (cols: int, rows: int, curr: Point.t): list(Point.t) => {
  let (x, y) = curr;
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  |> List.filter(((x, y)) => x >= 0 && x < cols && y >= 0 && y < rows);
};

let neighboursThatHasntBeVisited =
    (board: t, cols: int, rows: int, curr: Point.t): list(Point.t) => {
  let neightbours = getNeighbours(cols, rows, curr);
  neightbours
  |> List.filter(((x, y)) => {
       let cell = getCell(board, x, y);
       !cell.visited;
     });
};

let setCell = (board: t, x: int, y: int, newCell: Cell.t): t => {
  ArrayLabels.set(ArrayLabels.get(board.board, y), x, newCell);
  board;
};

let setHasVisited = (board, (x, y): Point.t): t => {
  let v = getCell(board, x, y);
  setCell(board, x, y, {...v, visited: true});
};

let setEntranceAndExit = (board: t, cols: int, rows: int): t => {
  let entrance = getCell(board, 0, 0);
  let exit = getCell(board, cols - 1, rows - 1);

  board
  ->setCell(0, 0, {...entrance, first: true})
  ->setCell(cols - 1, rows - 1, {...exit, last: true});
};

let findDirection =
    ((currX, currY): Point.t, (choosenX, choosenY): Point.t)
    : (Direction.t, Direction.t) => {
  let xDiff = currX - choosenX;
  let yDiff = currY - choosenY;
  switch (xDiff) {
  | (-1) => (Direction.East, Direction.West)
  | 1 => (Direction.West, Direction.East)
  | 0 =>
    switch (yDiff) {
    | 1 => (Direction.North, Direction.South)
    | (-1) => (Direction.South, Direction.North)
    | y => failwith("the diff in y is: " ++ string_of_int(y))
    }
  | x => failwith("the diff in x is: " ++ string_of_int(x))
  };
};

let removeWall = (board: t, (x, y): Point.t, dir: Direction.t) => {
  let cell = getCell(board, x, y);
  let withoutWall = Cell.removeWall(cell, dir);
  setCell(board, x, y, withoutWall);
};

let rec generator = (board: t, cols: int, rows: int): (t, bool) => {
  setHasVisited(board, board.curr) |> ignore; /* Mark the current cell as visited */
  let neightbours =
    neighboursThatHasntBeVisited(board, cols, rows, board.curr);
  if (List.length(neightbours) == 0) {
    switch (Stack.is_empty(board.stack)) {
    | false =>
      let lastPosition = Stack.pop(board.stack);
      generator({...board, curr: lastPosition}, cols, rows);
    | _ => (board, true)
    };
  } else {
    let random = Random.int(List.length(neightbours));
    let choosenNeighbour = List.nth(neightbours, random);
    Stack.push(board.curr, board.stack);
    /* remove wall between the current cell and the choosenNeightbour */
    let (dirCurrent, dirChoosen) =
      findDirection(board.curr, choosenNeighbour);
    let board =
      board
      ->removeWall(board.curr, dirCurrent)
      ->removeWall(choosenNeighbour, dirChoosen);
    board.preview ?
      ({...board, curr: choosenNeighbour}, false) :
      generator({...board, curr: choosenNeighbour}, cols, rows);
  };
};

let playerCanMove = (board: t, direction: Direction.t): bool => {
  /* borders should be a bitfield instead of boolean, should be easier with a fucking binary mask */
  switch (direction) {
  | Direction.West => !getCellFromPoint(board, board.player).west
  | Direction.East => !getCellFromPoint(board, board.player).east
  | Direction.North => !getCellFromPoint(board, board.player).north
  | Direction.South => !getCellFromPoint(board, board.player).south
  };
};

let getPlayNextCell = (board: t, direction: Direction.t): Point.t => {
  let (x, y) = board.player;
  switch (direction) {
  | Direction.West => (x + 1, y)
  | Direction.East => (x - 1, y)
  | Direction.North => (x, y - 1)
  | Direction.South => (x, y + 1)
  };
};

let removePlayer = (board: t, (x, y): Point.t): t => {
  let cell = getCell(board, x, y);
  setCell(board, x, y, {...cell, player: false});
};

let setPlayer = (board: t, (x, y): Point.t): t => {
  let cell = getCell(board, x, y);
  {...setCell(board, x, y, {...cell, player: true}), player: (x, y)};
};

let playerMove = (board: t, direction: Direction.t): t =>
  if (playerCanMove(board, direction)) {
    let nextPosition = getPlayNextCell(board, direction);
    board->removePlayer(board.player)->setPlayer(nextPosition);
  } else {
    board;
  };