use binary_search_tree::Bst;

fn main() {
    let mut tree = Bst::new();
    tree.add(4);
    tree.add(6);
    tree.add(2);
    println!("{:?}", tree);

    assert_eq!(tree.find(3), false);
    assert_eq!(tree.find(2), true);
    assert_eq!(tree.find(4), true);
    assert_eq!(tree.find(6), true);

    tree.walk(&|x| println!("value: {}", x));
}
