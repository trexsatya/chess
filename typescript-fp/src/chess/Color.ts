

export enum Color {
    Black,
    White
}

export function other(col: Color): Color {
    return col == Color.White ? Color.Black : Color.White
}