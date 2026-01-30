type OptionalUser = {
    kind: #Some,
    id: i32,
} | {
    kind: #None
};

fn main(): void {
    let a: OptionalUser = { kind: #Some, id: 15 };

    let x = a::is({ kind: #Some, id: i32 });
   

   if x { };
}
