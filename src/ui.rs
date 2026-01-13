pub fn glyph_width(ui: &egui::Ui, c: char) -> f32 {
    let font_id = ui
        .style()
        .override_text_style
        .as_ref()
        .unwrap()
        .resolve(ui.style());

    ui.fonts(|f| f.glyph_width(&font_id, c))
}

pub fn spacing(ui: &egui::Ui) -> f32 {
    let glyph_width = glyph_width(ui, '.');
    let spacing = 6.0 * glyph_width;

    spacing
}

pub const NODE_UNIT_ROW_HEIGHT: f32 = 18.0;
