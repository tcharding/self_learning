use std::env;
use std::path::Path;

fn main() {
    let image_path = args().skip(1).next().unwrap();
    let path = Path::new(&image_path);
    let img = image::open(path).unwrap();
    let rotated = img.rotate180();

    let stem = image_path.file_stem();
    let extension = image_path.extension();
    let parent = image_path.parent();

    let new_file = format!("{}_rotated.{}", stem, extension);
    let new_path = parent.to_path_buf().push(new_file);

    rotated.save(new_path).unwrap();
}
