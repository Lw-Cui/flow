// @flow

declare var cond: boolean;

module.exports = {
  f1: () => (cond ? "0" : "1"),
  f2: () => (cond ? "A0" : "A1"),
  f3: () => (cond ? "Aa" : "Bb"),
  f4: () => (cond ? "A_" : "B_"),

  f5: () => (cond ? "1/1/2000" : "1/1/2001"),
}
