// -----------------------------------------------------------------------------

// Arrays are in fact records.

const xs: number[] = [1, 2, 3]
console.log(xs[0])

const ys = xs
console.log(ys[0])

const zs: Record<number, number> = xs
console.log(zs[0])

// -----------------------------------------------------------------------------
