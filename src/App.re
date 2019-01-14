type timerId = ref(option(Js.Global.intervalId));

let id: timerId = ref(None);

type state = {
  rows: int,
  cols: int,
  cellSize: int,
  isGenerating: bool,
  speed: int,
  board: Board.t,
  maxStackSize: int,
  currentStackSize: int,
  fps: int,
  totalTick: int,
  firstRendering: option(float),
  preview: bool,
  canPlay: bool,
};

type action =
  | NumberRowChange(string)
  | NumberColChange(string)
  | SpeedChange(string)
  | CellSizeChange(string)
  | KeyPressed(string)
  | TogglePreview
  | GenerationTick
  | Generate;

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => {
    rows: 20,
    cols: 20,
    board: Board.make(20, 20),
    cellSize: 25,
    isGenerating: false,
    speed: 25,
    maxStackSize: 0,
    currentStackSize: 0,
    fps: 0,
    totalTick: 0,
    firstRendering: None,
    preview: true,
    canPlay: false,
  },

  reducer: (action, state) =>
    switch (action) {
    | NumberColChange(x) =>
      ReasonReact.Update({
        ...state,
        cols: int_of_string(x),
        board: Board.make(int_of_string(x), state.rows),
        canPlay: false,
      })
    | NumberRowChange(y) =>
      ReasonReact.Update({
        ...state,
        rows: int_of_string(y),
        board: Board.make(state.cols, int_of_string(y)),
        canPlay: false,
      })
    | CellSizeChange(n) =>
      ReasonReact.Update({...state, cellSize: int_of_string(n)})
    | TogglePreview => ReasonReact.Update({...state, preview: !state.preview})
    | GenerationTick =>
      let totalTick = state.totalTick + 1;
      let tickPerSecond =
        switch (state.firstRendering) {
        | Some(date) =>
          float_of_int(totalTick)
          /. (Js.Date.now() -. date)
          *. 1000.
          |> int_of_float
        | None => failwith("Generation tick has been called before Generate")
        };
      let (board, finished) =
        Board.generator(state.board, state.cols, state.rows);
      let stackSize = Board.getStackSize(board);
      let maxStackSize = max(stackSize, state.maxStackSize);
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          board:
            finished ?
              board
              ->Board.setEntranceAndExit(state.cols, state.rows)
              ->Board.setPlayer((0, 0)) :
              board,
          isGenerating: !finished,
          canPlay: finished,
          currentStackSize: stackSize,
          maxStackSize,
          totalTick,
          fps: tickPerSecond,
        },
        _self =>
          if (finished) {
            switch (id^) {
            | Some(id) => Js.Global.clearInterval(id)
            | None => ()
            };
          },
      );
    | Generate =>
      let startX = Random.int(state.cols);
      let startY = Random.int(state.rows);
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          isGenerating: true,
          canPlay: false,
          board:
            Board.make(
              state.cols,
              state.rows,
              ~curr=(startX, startY),
              ~preview=state.preview,
            ),
          currentStackSize: 0,
          maxStackSize: 0,
          fps: 0,
          totalTick: 0,
          firstRendering: Some(Js.Date.now()),
        },
        self =>
          id :=
            Some(
              Js.Global.setInterval(
                () => self.send(GenerationTick),
                state.speed,
              ),
            ),
      );
    | SpeedChange(n) =>
      ReasonReact.Update({...state, speed: int_of_string(n)})
    | KeyPressed(x) =>
      if (!state.canPlay) {
        ReasonReact.Update(state);
      } else {
        let direction =
          switch (String.lowercase(x)) {
          | "z"
          | "w" => Some(Direction.North)
          | "d" => Some(Direction.East)
          | "s" => Some(Direction.South)
          | "q"
          | "a" => Some(Direction.West)
          | _ => None
          };
        switch (direction) {
        | None => ReasonReact.Update(state)
        | Some(dir) =>
          ReasonReact.Update({
            ...state,
            board: Board.playerMove(state.board, dir),
          })
        };
      }
    },

  render: self =>
    <div>
      <h1> {ReasonReact.string("Maze generator")} </h1>
      <div>
        <div>
          <label htmlFor="preview">
            {ReasonReact.string("Enable preview")}
            <input
              id="preview"
              type_="checkbox"
              checked={self.state.preview}
              onChange={_event => self.send(TogglePreview)}
            />
          </label>
        </div>
        <div>
          <label htmlFor="number_column">
            {ReasonReact.string("Number of columns")}
            <input
              id="number_column"
              min=1
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
              min=1
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
              min=5
              value={string_of_int(self.state.cellSize)}
              onChange={event =>
                self.send(
                  CellSizeChange(ReactEvent.Form.target(event)##value),
                )
              }
            />
          </label>
        </div>
        {self.state.canPlay ?
           <div>
             <span>
               {ReasonReact.string(
                  "[play mode] use '(z q s d) or (z a s d)' to move the player around",
                )}
             </span>
           </div> :
           ReasonReact.null}
        {self.state.preview ?
           <>
             <div>
               <label htmlFor="number_speed">
                 {ReasonReact.string("Speed (interval between 2 redraw)")}
                 <input
                   id="number_speed"
                   min=0
                   type_="number"
                   value={string_of_int(self.state.speed)}
                   onChange={event =>
                     self.send(
                       SpeedChange(ReactEvent.Form.target(event)##value),
                     )
                   }
                 />
               </label>
             </div>
             <div>
               <span>
                 {ReasonReact.string(
                    "Completed: "
                    ++ string_of_int(
                         Board.getPercentageGenerated(
                           self.state.board,
                           self.state.cols,
                           self.state.rows,
                         ),
                       )
                    ++ "%",
                  )}
               </span>
               <span> {ReasonReact.string("|")} </span>
               <span>
                 {ReasonReact.string(
                    "Stack size (backtracking): "
                    ++ string_of_int(self.state.currentStackSize)
                    ++ " (max: "
                    ++ string_of_int(self.state.maxStackSize)
                    ++ ")",
                  )}
               </span>
               <span> {ReasonReact.string("|")} </span>
               <span>
                 {ReasonReact.string(
                    "fps: " ++ string_of_int(self.state.fps),
                  )}
               </span>
             </div>
           </> :
           ReasonReact.null}
        <div>
          <button
            onClick={_event => self.send(Generate)}
            disabled={self.state.isGenerating}>
            {ReasonReact.string("Generate")}
          </button>
        </div>
      </div>
      <Maze
        board={self.state.board.board}
        cellSize={self.state.cellSize}
        onKeyPress={_event =>
          self.send(KeyPressed(ReactEvent.Keyboard.key(_event)))
        }
      />
    </div>,
};