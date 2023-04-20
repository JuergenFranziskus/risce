use risce::assembler::{code_gen::CodeGen, linker::Linker, parser::Parser, token::lex};

fn main() {
    let args: Vec<_> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let path = &args[1];

    let src = std::fs::read_to_string(path).unwrap();
    let tokens = lex(&src);

    let ast = Parser::new(&tokens).parse();
    let obj = CodeGen::new().gen_code(&ast);
    let objects = [obj];
    let bytes = Linker::new(&objects).link();
    print_hex_file(&bytes);
}

#[allow(dead_code)]
fn print_hex_file(bytes: &[u8]) {
    println!("v2.0 raw");
    for chunk in bytes.chunks(4) {
        let mut bytes = [0; 4];
        for (i, &byte) in chunk.iter().enumerate() {
            bytes[i] = byte;
        }
        let value = u32::from_le_bytes(bytes);
        print_hex(value);
        println!();
    }
}

fn print_hex(num: u32) {
    let num = &format!("{num:#04x}")[2..];
    print!("{num}");
}
