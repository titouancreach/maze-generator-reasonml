type t = (int, int);

let make = (x, y) => (x, y);
let x = ((x, _): t) => x;
let y = ((_, y): t) => y;