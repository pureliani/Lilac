type OptionalUser = {
    kind: #Some,
    id: i64,
} | {
    kind: #None
};

fn main(): void {
    let a: OptionalUser = { kind: #Some, id: 15 };

}
