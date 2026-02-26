
type Arg = { value: string | i32 };

fn foo(arg: Arg) {
    arg.value = 15;
}

fn main() {
    let initial = { y: "Hello world!" };
    foo(initial);
    let x: { value: string } = initial;
}
