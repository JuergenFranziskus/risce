use risce::assembler::{token::lex, parser::Parser, code_gen::CodeGen};

fn main() {
    let args: Vec<_> = std::env::args().collect();
    assert_eq!(args.len(), 2);
    let path = &args[1];

    let src = std::fs::read_to_string(path).unwrap();
    let tokens = lex(&src);
    let ast = Parser::new(&tokens).parse();
    let bytes = CodeGen::new().gen_code(&ast);

    println!("v2.0 raw");
    for chunk in bytes.chunks(4) {
        let mut bytes = [0; 4];
        for (i, &byte) in chunk.into_iter().enumerate() {
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
