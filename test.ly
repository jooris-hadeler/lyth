fn main() u32 {
    let someone = Person::{
        name: "Bob",
        age: 27,
        job: &Job::{
            name: "teacher",
            salary: 50000
        }
    };
}

fn greet(person: *Person) {
    cout.println("Hello, {}!", person.name);
}

struct Person {
    name: *u8,
    age: u16,
    job: *Job
}

struct Job {
    name: *u8,
    salary: u64
}