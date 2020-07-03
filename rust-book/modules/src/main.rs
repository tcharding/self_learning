mod sound {
    pub mod instrument {
        pub fn clarinet() {
            println!("clarinet");
        }
    }
}

fn main() {
    // Absolute path
    crate::sound::instrument::clarinet();

    // Relative path
    sound::instrument::clarinet();
}
