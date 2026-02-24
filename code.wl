
fn foo(arg: { value: string | i32 }) {
    arg.value = 15;
}

fn main() {
    let initial = { value: "Hello world!" };
    foo(initial);
    let x: { value: string } = initial;
}