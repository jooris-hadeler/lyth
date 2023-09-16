struct str {
    ptr: *const char,
    length: usize,
}

struct User {
    name: str,
    age: i32,
    gender: Gender,
}

enum Gender {
    Male,
    Female,
    NoAnswer,
}

fn main() -> i32 {
    debug(User {
        name: "John",
        age: 20,
        gender: Gender::Male,
    });

    return -1;
}
