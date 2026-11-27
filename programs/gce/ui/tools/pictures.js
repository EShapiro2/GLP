/* Drawing programs: how a UI style draws a value as a picture, and how a person gives a value by tapping one.
 *
 * A UI style names a program:  Board IS DRAWN BY chessboard.   Move IS PICKED ON chessboard FROM Board.
 *                              Sum IS PICKED ON tens.
 * A program knows nothing of the app and decides nothing of the contract; it answers two questions:
 *
 *   draw(V)       the picture of the value V (or of nothing, when V is undefined): {cols, cells}, each cell
 *                 {sym, shade, tap}: a symbol, a shade (light, dark, on, off) and the name a tap on it gives
 *   pick(Taps, V) for the taps so far on the picture of V: {value} when they give a value, {more: true} when
 *                 one more tap is needed, {none: true} when they give nothing (the taps then start again)
 *
 * and declares the kind of value it picks, text or number, and how a value it draws is named in a sentence.
 * The renderer draws only cells, and a box to type the value always stays beside a picture to pick on, so no
 * UI style that uses a program loses an option the contract allows.  Here the programs are JavaScript, as the
 * page simulates everything else; in the app they are GLP programs, shared and installed as programs are.
 */
(function (G) {
  const PIECE = {K: '♔', Q: '♕', R: '♖', B: '♗', N: '♘', P: '♙', k: '♚', q: '♛', r: '♜', b: '♝', n: '♞', p: '♟'};
  const square = i => String.fromCharCode(97 + i % 8) + (8 - (i >> 3));
  const PICTURES = {
    /* a position, in FEN, as an 8×8 board; a move, in UCI, as two taps: the square from, then the square to */
    chessboard: {
      kind: 'text', label: 'a position',
      accepts: v => typeof v === 'string' && !!G.CHESS && !!G.CHESS.board(v),
      draw: v => {
        const b = (G.CHESS && v !== undefined && G.CHESS.board(v)) || Array(64).fill(null);
        return {cols: 8, frame: 'board', cells: b.map((x, i) => ({sym: x ? PIECE[x] : '', shade: ((i >> 3) + i % 8) % 2 ? 'dark' : 'light', tap: square(i)}))};
      },
      pick: taps => taps.length < 2 ? {more: true} : taps[0] === taps[1] ? {none: true} : {value: taps[0] + taps[1]}
    },
    /* an amount as ten coins, 10 to 100; a tap on the n-th gives 10n, and any other amount is typed */
    tens: {
      kind: 'number', label: 'an amount',
      accepts: v => !isNaN(+v),
      draw: v => ({cols: 10, cells: Array.from({length: 10}, (_, i) => ({sym: String(10 * (i + 1)), shade: v !== undefined && +v >= 10 * (i + 1) ? 'on' : 'off', tap: String(10 * (i + 1))}))}),
      pick: taps => ({value: taps[taps.length - 1]})
    }
  };
  G.PICTURES = PICTURES;
  if (typeof module !== 'undefined') module.exports = PICTURES;
})(typeof window !== 'undefined' ? window : globalThis);
