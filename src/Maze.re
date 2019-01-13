let component = ReasonReact.statelessComponent("Maze");

let rowContainerStyle = ReactDOMRe.Style.make(~display="flex", ());

let make = (~cellSize, ~board, _children) => {
  ...component,

  render: _ => {
    <div>
      {ReasonReact.array(
         ArrayLabels.mapi(
           ~f=
             (j, rows) =>
               <div key={string_of_int(j)} style=rowContainerStyle>
                 {ReasonReact.array(
                    ArrayLabels.mapi(
                      ~f=
                        (i, cell) =>
                          <div
                            key={string_of_int(j) ++ string_of_int(i)}
                            style={Cell.getStyle(cell, cellSize)}
                          />,
                      rows,
                    ),
                  )}
               </div>,
           board,
         ),
       )}
    </div>;
  },
};