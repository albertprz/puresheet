use "strict"

export const indexImpl = (i) => (v) => v[i];
export const modifyImpl = (i) => (f) => (v) => {
  const r = f(v[i]);
  const a = v.slice();
  a[i] = r;
  return a;
}
