use crate::UnorderedMap;
use crate::hash_one_fixed;

#[test]
fn eq_order_independent() {
    #[derive(Debug, PartialEq, Eq, Hash)]
    struct Foo(&'static str, u32);

    let a_map = UnorderedMap::from([
        ("a", Foo("value 0", 0)),
        ("b", Foo("value 1", 1)),
        ("c", Foo("value 2", 2)),
        ("alpha", Foo("value 3", 3)),
        ("beta", Foo("value 4", 4)),
        ("gamma", Foo("value 5", 5)),
    ]);

    let b_map = UnorderedMap::from([
        ("b", Foo("value 1", 1)),
        ("a", Foo("value 0", 0)),
        ("c", Foo("value 2", 2)),
        ("beta", Foo("value 4", 4)),
        ("alpha", Foo("value 3", 3)),
        ("gamma", Foo("value 5", 5)),
    ]);

    let c_map = UnorderedMap::from([
        ("gamma", Foo("value 5", 5)),
        ("beta", Foo("value 4", 4)),
        ("alpha", Foo("value 3", 3)),
        ("c", Foo("value 2", 2)),
        ("b", Foo("value 1", 1)),
        ("a", Foo("value 0", 0)),
    ]);

    assert_eq!(&a_map, &b_map);
    assert_eq!(&b_map, &c_map);
    assert_eq!(&a_map, &c_map);
}

#[test]
fn hash_order_independent() {
    #[derive(Debug, PartialEq, Eq, Hash)]
    struct Foo(&'static str, u32);

    let a_map = UnorderedMap::from([
        ("a", Foo("value 0", 0)),
        ("b", Foo("value 1", 1)),
        ("c", Foo("value 2", 2)),
        ("alpha", Foo("value 3", 3)),
        ("beta", Foo("value 4", 4)),
        ("gamma", Foo("value 5", 5)),
    ]);

    let b_map = UnorderedMap::from([
        ("b", Foo("value 1", 1)),
        ("a", Foo("value 0", 0)),
        ("c", Foo("value 2", 2)),
        ("beta", Foo("value 4", 4)),
        ("alpha", Foo("value 3", 3)),
        ("gamma", Foo("value 5", 5)),
    ]);

    let c_map = UnorderedMap::from([
        ("gamma", Foo("value 5", 5)),
        ("beta", Foo("value 4", 4)),
        ("alpha", Foo("value 3", 3)),
        ("c", Foo("value 2", 2)),
        ("b", Foo("value 1", 1)),
        ("a", Foo("value 0", 0)),
    ]);

    let a_hash = hash_one_fixed(&a_map);
    let b_hash = hash_one_fixed(&b_map);
    let c_hash = hash_one_fixed(&c_map);

    assert_eq!(a_hash, c_hash);
    assert_eq!(b_hash, c_hash);
    assert_eq!(a_hash, c_hash);
}

#[test]
fn eq_hash_equivalence() {
    #[derive(Debug, PartialEq, Eq, Hash)]
    struct Foo(&'static str, u32);

    let a_map = UnorderedMap::from([
        ("a", Foo("value 0", 0)),
        ("b", Foo("value 1", 1)),
        ("c", Foo("value 2", 2)),
        ("alpha", Foo("value 3", 3)),
        ("beta", Foo("value 4", 4)),
        ("gamma", Foo("value 5", 5)),
    ]);

    let b_map = UnorderedMap::from([
        ("b", Foo("value 1", 1)),
        ("a", Foo("value 0", 0)),
        ("c", Foo("value 2", 2)),
        ("beta", Foo("value 4", 4)),
        ("alpha", Foo("value 3", 3)),
        ("gamma", Foo("value 5", 5)),
    ]);

    let c_map = UnorderedMap::from([
        ("gamma", Foo("value 5", 5)),
        ("beta", Foo("value 4", 4)),
        ("alpha", Foo("value 3", 3)),
        ("c", Foo("value 2", 2)),
        ("b", Foo("value 1", 1)),
        ("a", Foo("value 0", 0)),
    ]);

    assert_eq!(&a_map, &b_map);
    assert_eq!(&b_map, &c_map);
    assert_eq!(&a_map, &c_map);

    let a_hash = hash_one_fixed(&a_map);
    let b_hash = hash_one_fixed(&b_map);
    let c_hash = hash_one_fixed(&c_map);

    assert_eq!(a_hash, c_hash);
    assert_eq!(b_hash, c_hash);
    assert_eq!(a_hash, c_hash);
}

#[test]
#[should_panic(expected = "Failed to insert to map as the key already existed")]
fn insert_unique() {
    let mut map = UnorderedMap::new();
    map.insert_unique("alpha", 1);
    map.insert_unique("alpha", 2);
}
