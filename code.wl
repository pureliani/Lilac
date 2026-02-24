type Data = { val: i32 };
type Node = { left: Data, right: Data };

fn process(a: Node) {}

fn main() {
    let shared_data: Data = { val: 99 };
    let bad_node: Node = { left: shared_data, right: shared_data };

    process(bad_node);
}