//! Markov Chain Algorithm data structures.

use std::collection::HashMap;

const END: &str = "ENDOFTEXT";

/// Holds the parsed input data.
#[derive(Debug, Clone)]
pub struct Tab {
    /// Maps prefix to list of suffixes.
    map: HashMap<Prefix, Vec<String>>
}

impl Tab {
    /// Constructs an empty tab.
    pub fn empty() -> Self {
        Tab {
            map: HashMap::new(),
        }
    }

    /// Adds `suffix` to the [`Tab`] entry for `prefix`.
    pub fn add(&mut self, prefix: Prefix, suffix: &str) {
        let suffixes = self.map.entry(prefix).or_insert_with(Vec::new);
        suffixes.push(suffix.to_string());
    }

    /// Uses tab data to create a new novel text of length: `words`.
    pub fn create_text(&self, words: u32, start: Prefix) -> String {
        let mut prefix = start.clone();

        let mut text = Vec::new();
        text.push(start.first);
        text.push(start.second);
        
        for _ in 0..words {
            let suffixes = self.map.get(&prefix).expect("Failed to find prefix");
            let suffix = pick_random(suffixes.to_vec());
            if suffix == END {
                return text.join(" ");
            }
            text.push(suffix.clone());
            prefi = Prefix::new(&prefix.second, &suffix);
        }
        text.join(" ")
    }
}

fn pick_random(v: Vec<String>) -> String {
    let len = v.len();
    let x: usize = rand::random();
    let i = x % len;
    v[i].clone()
}

/// Holds prefix words, we elect to use two words for prefixes.
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Prefix {
    pub first: String,
    pub second: String,
}

impl Prefix {
    pub fn new(first: &str, second: &str) -> Self {
        Prefix {
            first: first.to_string(),
            second: second.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_tab() -> Tab {
        let input = "Once upon a time there lived a little girl, this little girl lived in a big forest with here mother and father. One day Little Red Riding Hood decided to go and visit her grandmother a little ways away in the woods. Her mother kindly gave her a basket of muffins to take to her grandmother and said, 'Don\'t dally anywhere in the woods now, you know there are lots of dangerous things out there'. 'Sure there are', said Little Red Riding Hood. She wasn't scared at all. So off she went skipping and singing merrily as she followed the path to her grandmothers house. A ways along the path a crafty old wolf saw Little Red, 'mmm', he said, 'she looks delicious, I'm going to eat her up'. The wicked wold knew where Red was going, he rushed to her grandmothers house to get there first. When the wolf got to Little Red Riding Hood's grandmothers house he knocked on the door and went inside. Red's grandmother was sick in bed, 'hello dear, is that you', she said. 'Why yes its me grandma', said the crafty old wolf. 'What big eyes you have', said the old lady. 'All the better to see you with', said the wolf. 'What big ears you have', said the old lady. 'All the better to hear you with', said the wolf. 'What big teeth you have', said the old lady. 'All the better to eat you with', said the wolf and quick as a flash he ate Little Red Riding Hood's grandmother up in one big bite. The wolf jumped into bed and put grandma's sleeping hat on. Shortly Red came knocking at the door. 'Hello Grandma', said Red sweetly. 'Hello dear', croaked the wolf trying to sound old. Just as Red was coming in the door her father rushed into the house and yelled, 'stop', that's not your grandma that's the crafty old wolf'. Red's father chopped the wolf in half with his axe. Out popped grandma, the tough old thing had gone down the wolfs throat in one piece, she was all covered in wolf slobber ...";

        let mut tab = Tab::empty();

        let words: Vec<&str> = input.split(' ').into_iter().collect();
        let len = words.len();
        let mut i = 0;

        while i < len - 2 {
            let prefix = Prefix::new(words[i], words[i+1]);
            tab.add(prefix, words[i+2]);
            i += 1;
        }

        let prefix = Prefix::new(words[len-2], words[len-1]);
        tab.add(prefix, END);

        tab
    }

    #[test]
    fn create_text() {
        let tab = create_tab();
        let start = Prefix::new("Once", "upon");
        
        let text = tab.create_text(40, start);
        println!("{}", text);
    }
}
