use crate::OrderedMap;

#[test]
#[should_panic(expected = "Failed to insert to map as the key already existed")]
fn insert_unique() {
    let mut map = OrderedMap::new();
    map.insert_unique("alpha", 1);
    map.insert_unique("alpha", 2);
}
