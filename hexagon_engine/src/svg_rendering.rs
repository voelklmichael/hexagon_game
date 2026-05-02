use std::path::Path;

impl super::HexagonBoard {
    pub fn render(&self, output_file: &Path) {
        let Self {
            hexagons,
            connectors,
        } = self;

        // render the hexagons in a svg file

        std::fs::write(output_file, "bla").unwrap();
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn test_board_construction() {
        for radius in 2..5 {
            for add_outer_connectors in [true, false] {
                let options = crate::BoardConstructionOptionsSimple {
                    radius,
                    add_outer_connectors,
                };
                let board = options.construct().unwrap();
                let ouputfile = format!(
                    "{}/target/simple_{radius}_{add_outer_connectors}.svg",
                    env!("CARGO_MANIFEST_DIR")
                );
                let path = Path::new(&ouputfile);
                board.render(path);
            }
        }
    }
}
