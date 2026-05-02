pub mod derive_alias {

    derive_aliases::define! {
        SerdeClone = ::core::clone::Clone, ::serde::Serialize, ::serde::Deserialize;
    }
}

mod hexagon_types;
mod random_number_generator;
pub use hexagon_types::*;
pub use ranom_number_generator::RandomNumberGenerator as Rng;

mod board_construction;
