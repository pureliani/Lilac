
type Arg = { value: string | i32 };

fn main() {
    let initial: Arg = { value: "Hello world!" };
    let x: { value: string } = initial;
}
