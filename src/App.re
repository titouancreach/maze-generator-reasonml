let makeEmptyBoard = (cols, rows) => {
  ArrayLabels.make_matrix(~dimx=rows, ~dimy=cols, Cell.defaultCell);
};

type timerId = ref(option(Js.Global.intervalId));

let id: timerId = ref(None);

type point = (int, int);

type state = {
  rows: int,
  cols: int,
  cellSize: int,
  board: array(array(Cell.t)),
  isGenerating: bool,
  curr: point,
  stack: list(point),
  speed: int,
};
let getCell = (board, x, y) => {
  ArrayLabels.get(ArrayLabels.get(board, y), x);
};
let getNeighbours = (cols, rows, curr) => {
  let (x, y) = curr;
  [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  |> List.filter(((x, y)) => x >= 0 && x < cols && y >= 0 && y < rows);
};
let neighboursThatHasntBeVisited =
    (board: array(array(Cell.t)), cols: int, rows: int, curr: point) => {
  let neightbours = getNeighbours(cols, rows, curr);
  neightbours
  |> List.filter(((x, y)) => {
       let cell = getCell(board, x, y);
       !cell.visited;
     });
};

let setCell = (board, x, y, newCell: Cell.t) => {
  ArrayLabels.set(ArrayLabels.get(board, y), x, newCell);
};

let setHasVisited = (board, (x, y): point) => {
  let v = getCell(board, x, y);
  setCell(board, x, y, {...v, visited: true});
};

let setIsOutput = (board, (x, y): point) => {
  let v = getCell(board, x, y);
  setCell(board, x, y, {...v, last: true});
  board;
};

let findDirection = ((currX, currY), (choosenX, choosenY)) => {
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

let removeWall = (board, (x, y), dir) => {
  let cell = getCell(board, x, y);
  let withoutWall = Cell.removeWall(cell, dir);
  setCell(board, x, y, withoutWall);
  board;
};

let rec generator = (board, cols, rows, curr: point, stack, lastDiscovered) => {
  setHasVisited(board, curr); /* Mark the current cell as visited */
  let neightbours = neighboursThatHasntBeVisited(board, cols, rows, curr);
  if (List.length(neightbours) == 0) {
    switch (stack) {
    | [lastCell, ...newStack] =>
      generator(board, cols, rows, lastCell, newStack, lastDiscovered)
    | _ =>
      let newBoard = setIsOutput(board, lastDiscovered);
      (newBoard, curr, stack, true, lastDiscovered);
    };
  } else {
    let random = Random.int(List.length(neightbours));
    let choosenNeighbour = List.nth(neightbours, random);
    let newStack = [curr, ...stack];
    /* remove wall between the current cell and the choosenNeightbour */
    let (dirCurrent, dirChoosen) = findDirection(curr, choosenNeighbour);
    let newBoard =
      board
      ->removeWall(curr, dirCurrent)
      ->removeWall(choosenNeighbour, dirChoosen);
    (newBoard, choosenNeighbour, newStack, false, choosenNeighbour);
  };
};

type action =
  | NumberRowChange(string)
  | NumberColChange(string)
  | SpeedChange(string)
  | CellSizeChange(string)
  | GenerationTick
  | Generate;

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    rows: 20,
    cols: 20,
    board: makeEmptyBoard(20, 20),
    cellSize: 25,
    isGenerating: false,
    curr: (0, 0),
    stack: [],
    speed: 25,
  },

  reducer: (action, state) =>
    switch (action) {
    | NumberColChange(x) =>
      ReasonReact.Update({
        ...state,
        cols: int_of_string(x),
        board: makeEmptyBoard(int_of_string(x), state.rows),
      })
    | NumberRowChange(y) =>
      ReasonReact.Update({
        ...state,
        rows: int_of_string(y),
        board: makeEmptyBoard(state.cols, int_of_string(y)),
      })
    | CellSizeChange(n) =>
      ReasonReact.Update({...state, cellSize: int_of_string(n)})
    | GenerationTick =>
      let (board, curr, stack, finished, _) =
        generator(
          state.board,
          state.cols,
          state.rows,
          state.curr,
          state.stack,
          state.curr,
        );
      ReasonReact.UpdateWithSideEffects(
        {...state, board, curr, stack, isGenerating: !finished},
        _self =>
          if (finished) {
            switch (id^) {
            | Some(id) => Js.Global.clearInterval(id)
            | None => ()
            };
          },
      );
    | Generate =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          isGenerating: true,
          board: makeEmptyBoard(state.cols, state.rows),
        },
        self =>
          id :=
            Some(
              Js.Global.setInterval(
                () => self.send(GenerationTick),
                state.speed,
              ),
            ),
      )
    | SpeedChange(n) =>
      ReasonReact.Update({...state, speed: int_of_string(n)})
    },

  render: self =>
    <div>
      {ReasonReact.string("Maze generator and resolver")}
      <div>
        <div>
          <label htmlFor="number_speed">
            {ReasonReact.string("Speed (interval between 2 redraw)")}
            <input
              id="number_speed"
              min=0
              type_="number"
              value={string_of_int(self.state.speed)}
              onChange={event =>
                self.send(SpeedChange(ReactEvent.Form.target(event)##value))
              }
            />
          </label>
        </div>
        <div>
          <label htmlFor="number_column">
            {ReasonReact.string("Number of columns")}
            <input
              id="number_column"
              min=0
              type_="number"
              value={string_of_int(self.state.cols)}
              onChange={event =>
                self.send(
                  NumberColChange(ReactEvent.Form.target(event)##value),
                )
              }
            />
          </label>
        </div>
        <div>
          <label htmlFor="number_row">
            {ReasonReact.string("Number of rows")}
            <input
              id="number_row"
              min=0
              type_="number"
              value={string_of_int(self.state.rows)}
              onChange={event =>
                self.send(
                  NumberRowChange(ReactEvent.Form.target(event)##value),
                )
              }
            />
          </label>
        </div>
        <div>
          <label htmlFor="rowSize">
            {ReasonReact.string("Size of a cell")}
            <input
              id="rowSize"
              type_="number"
              min=4
              value={string_of_int(self.state.cellSize)}
              onChange={event =>
                self.send(
                  CellSizeChange(ReactEvent.Form.target(event)##value),
                )
              }
            />
          </label>
        </div>
        <div>
          <button
            onClick={_event => self.send(Generate)}
            disabled={self.state.isGenerating}>
            {ReasonReact.string("Generate")}
          </button>
        </div>
      </div>
      <Maze board={self.state.board} cellSize={self.state.cellSize} />
    </div>,
};