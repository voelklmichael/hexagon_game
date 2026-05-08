use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{Player, PlayerId};

#[derive(Clone, Serialize, Deserialize, Default)]
pub struct Statistics {
    pub total_path_segments: HashMap<PlayerId, u32>,
    pub total_path_weight: HashMap<PlayerId, u32>,
    pub max_velocity: HashMap<PlayerId, u32>,
}

impl Statistics {
    pub fn compute(players: &[Player]) -> Self {
        let mut total_path_segments = HashMap::new();
        let mut total_path_weight = HashMap::new();
        let mut max_velocity = HashMap::new();

        for p in players {
            // history[0] is the starting position, not a move
            let moves = p.history.iter().skip(1);
            let mut segments = 0u32;
            let mut weight = 0u32;
            let mut max_vel = 0u32;
            for turn in moves {
                let turn_weight: u32 = turn.connectors.iter().map(|hc| hc.weight).sum();
                segments += turn.connectors.len() as u32;
                weight += turn_weight;
                max_vel = max_vel.max(turn_weight);
            }
            total_path_segments.insert(p.id, segments);
            total_path_weight.insert(p.id, weight);
            max_velocity.insert(p.id, max_vel);
        }

        Self {
            total_path_segments,
            total_path_weight,
            max_velocity,
        }
    }
}
