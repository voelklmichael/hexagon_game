use std::collections::HashMap;

use crate::*;
pub struct RenderTask {
    pub hexagons: Vec<HexagonPosition>,
    pub connectors: Vec<UsedConnector>,
    pub hexagon_to_highlight: Option<HexagonPosition>,
}

pub struct UsedConnector {
    pub connector: Connector,
    pub used_by: Vec<PlayerId>,
    pub previews_used_by: Vec<PlayerId>,
    pub is_connected_to_dead_end: bool,
    pub is_connected_to_player_start: Option<PlayerId>,
    pub is_connected_to_player_target: Option<PlayerId>,
}

// a color like #ff0000
pub struct Color(&'static str);
impl Color {
    pub const GRAY: Color = Color("#808080");
    pub const GOLDEN: Color = Color("#FFD700");
}

pub struct PlayerData {
    pub colors: HashMap<PlayerId, Color>,
    pub dead_end_color: Color,
    pub unused_color: Color,
}

impl RenderTask {
    pub fn render(&self, player_data: &PlayerData) -> Result<svg::Document, String> {
        use svg::Document;

        let mut document = Document::new();

        let Self {
            hexagons,
            connectors,
            hexagon_to_highlight,
        } = self;

        // Step1: add the hexagon boundaries to the svg
        // fill also in the background
        // note: for the highlighted hexagon, use a different boundary color and a different background color

        // Step2: add the connectors

        Ok(document)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_render_only_4_hexagons() {
        let rendertask = RenderTask {
            hexagons: [
                HexagonPosition { x: 0, y: 0 },
                HexagonPosition { x: 1, y: 0 },
                HexagonPosition { x: 0, y: 1 },
                HexagonPosition { x: 1, y: 1 },
            ]
            .into(),
            connectors: Default::default(),
            hexagon_to_highlight: Some(HexagonPosition { x: 0, y: 1 }),
        };
        let player_data = PlayerData {
            colors: Default::default(),
            dead_end_color: Color::GRAY,
            unused_color: Color::GOLDEN,
        };
        let svg = rendertask.render(&player_data).unwrap();
        let path = format!("{}/../target/test.svg", env!("CARGO_MANIFEST_DIR"));
        dbg!(&path);
        std::fs::write(path, svg.to_string()).unwrap();
    }
}
