from "std/io" { print }


fn bar<T extends string>(arg: T) {
    print(arg);
}

fn main() { 
    bar("hello");
}
