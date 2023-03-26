

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub length: usize,
}
impl Span {
    pub fn new(start: usize, length: usize) -> Self {
        Self {
            start,
            length
        }
    }

    pub fn merge(a: Span, b: Span) -> Span {
        let start = a.start.min(b.start);
        let end = a.end().max(b.end());
        let length = end - start;
        Self::new(start, length)
    }

    pub fn end(self) -> usize {
        self.start + self.length
    }
}

