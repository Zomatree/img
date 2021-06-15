use img::compile;

fn main() {
    compile("OPEN \"img.png\" img\nFLIPV img\nSAVE img \"output.png\"")
        .unwrap()
        .run()
        .unwrap();
}
