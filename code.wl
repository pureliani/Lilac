type Foo = {
    bar: string | i32,
};

fn main() {
    let f: Foo = { bar: 1 };

    let bar_is_number = f.bar::is(i32);
    
    let value = if bar_is_number {
        f.bar
    } else {
        123
    };
}
