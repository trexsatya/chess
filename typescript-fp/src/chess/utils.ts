
const range = (from: number, to: number, step: number=1) =>
    [...Array(Math.floor((to - from) / step) + 1)].map((_, i) => from + i * step);

const charRangeInclusive = (start: string, stop: string)  => {
    let startN: number = start.charCodeAt(0);
    let stopN: number = stop.charCodeAt(0);
    return range(startN, stopN , 1).map(code => String.fromCharCode(code));
};

// const replicate = <A>(n: number, a: A) => range(0, n-1).map(_=> a)

export const  updatedList = <A>(list: A[], items: [number, A][]) => {
    const result: A[] = []
    const map: Record<number, A> = {}

    items.forEach((tuple) => {
        map[tuple[0]] = tuple[1]
    })

    range(0, list.length).forEach((i, x) =>
        result[i] = map[i] ? map[i] : list[i]
    )
    return result
}

export {range, charRangeInclusive}