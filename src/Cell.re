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

let removeWall = (cell: t, dir: Direction.t) => {
  switch (dir) {
  | Direction.East => {...cell, east: false}
  | Direction.West => {...cell, west: false}
  | Direction.North => {...cell, north: false}
  | Direction.South => {...cell, south: false}
  };
};

let getStyle = (cell: t, cellSize: int) => {
  let solidBorder = "1px solid #000";
  let noBorder = "1px solid red";

  let size = string_of_int(cellSize) ++ "px";
  let color =
    switch (cell) {
    | {last: true} => "green"
    | {first: true} => "yellow"
    | {visited: true} => "red"
    | _ => "blue"
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