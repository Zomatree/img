use img::Code;

#[test]
fn test() {
	Code::compile("
OPEN \"img.png\" myimg1
PRINT myimg1
RESIZE myimg1 50 50 \"nearest\" output
SAVE output \"output.png\"").unwrap().run();
}
