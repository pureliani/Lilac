use crate::{
    ast::type_annotation::{TypeAnnotation, TypeAnnotationKind},
    globals::TAG_INTERNER,
    parse::{Parser, ParsingError},
    tokenize::PunctuationKind,
};

impl Parser {
    pub fn parse_tag_type_annotation(&mut self) -> Result<TypeAnnotation, ParsingError> {
        let start_offset = self.offset;
        self.consume_punctuation(PunctuationKind::Hash)?;
        let identifier = self.consume_identifier()?;
        let span = self.get_span(start_offset, self.offset - 1)?;

        let tag_id = TAG_INTERNER.intern(&identifier.name);

        Ok(TypeAnnotation {
            kind: TypeAnnotationKind::Tag(tag_id),
            span,
        })
    }
}
