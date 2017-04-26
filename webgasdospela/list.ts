export class Nil{}

export class Cons<T>{
    constructor(public readonly head: T, public readonly tail: List<T>) {}
}

export type List<T> = Nil | Cons<T>

export function forEach<T>(f: (T) => void) {
    let rec =
        (list: List<T>) => {
            if (list instanceof Cons) {
                f(list.head);
                rec(list.tail);
            }
        }
    return rec;
}

export function length<T>(list: List<T>) {
    let l = 0;
    forEach(() => l++);
    return l;
}

export function lookup<K, T>(k: K, list: List<[K, T]>) {
    while(list instanceof Cons){
        if(k === list.head[0]){
            return list.head[1];
        }
        list = list.tail;
    }
}
