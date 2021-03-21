import {charRangeInclusive, range} from "./utils";
import {none, Option, some} from "fp-ts/Option";

type Position = [number, number]

export function toIndex(pos: Position) {
    return 8 * pos[0] + pos[1]
}

function valid(pos: Position) {
    const inRange = (x: number) => x in range(0, 7)
    let [f, r] = pos
    return inRange(f) && inRange(r)
}

export function fromString(posStr: string): Option<Position>{
    const match = posStr.match(/^([abcdefgh])(\d)$/)
    if(!match) {
        return none
    }
    const [f, r]: [string, number] = [match[1], +match[2]]
    const isValid = (charRangeInclusive('a', 'h').includes(f)) && (range(1,8).includes(r))
    //@ts-ignore
    return isValid ? some([r-1, f.codePointAt(0)-97])
                   : none
}

export {Position, valid}