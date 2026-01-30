type OptionalUser = {
    kind: #Some,
    id: i32,
} | {
    kind: #None
};

fn main() {
    let user: OptionalUser = { kind: #Some, id: 15 };

    let condition = user::is({ kind: #Some, id: i32 });
    let condition_alias = condition;

    if (condition_alias) {
        user.id = 10i64;
    };

    15
}
