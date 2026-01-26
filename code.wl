type OptionalUser = {
    kind: #Some,
    id: i64,
} | {
    kind: #None
};

fn main(): void {
    let a: OptionalUser = { kind: #Some, id: 15 };

    if 1 > 2 {
        a = { kind: #None };
    } else {
        a = { kind: #None };
    }

    let b: { kind: #None } = a;
}
