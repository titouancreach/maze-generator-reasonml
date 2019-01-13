type t = {
  visited: bool,
  west: bool,
  east: bool,
  north: bool,
  south: bool,
  last: bool,
  first: bool,
};

let defaultCell = {
  visited: false,
  west: true,
  east: true,
  north: true,
  south: true,
  last: false,
  first: false,
};

let colorVisited = "#068587";
let colorLast = "#ED553B";
let colorFirst = "#F2B134";
let colorDefault = "#4FB99F";
let colorWall = "#000";

let removeWall = (cell: t, dir: Direction.t) => {
  switch (dir) {
  | Direction.East => {...cell, east: false}
  | Direction.West => {...cell, west: false}
  | Direction.North => {...cell, north: false}
  | Direction.South => {...cell, south: false}
  };
};

let getStyle = (cell: t, cellSize: int) => {
  let solidBorder = {j|1px solid $colorWall|j};
  let noBorder = {j|1px solid $colorVisited|j};

  let size = string_of_int(cellSize) ++ "px";
  let color =
    switch (cell) {
    | {last: true} => colorLast
    | {first: true} => colorFirst
    | {visited: true} => colorVisited
    | _ => colorDefault
    };
  let rightBorder = cell.east ? solidBorder : noBorder;
  let bottomBorder = cell.south ? solidBorder : noBorder;
  let leftBorder = cell.west ? solidBorder : noBorder;
  let topBorder = cell.north ? solidBorder : noBorder;

  ReactDOMRe.Style.make(
    ~height=size,
    ~width=size,
    ~borderLeft=leftBorder,
    ~borderRight=rightBorder,
    ~borderTop=topBorder,
    ~borderBottom=bottomBorder,
    ~background=color,
    (),
  );
};