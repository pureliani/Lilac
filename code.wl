type OptionalUser = {
    kind: #Some,
    id: i32,
} | {
    kind: #None
};

fn main(): void {
    let a: OptionalUser = { kind: #Some, id: 15 };

    if a::is({ kind: #None }) {
        a.id = 11;
    };
}
