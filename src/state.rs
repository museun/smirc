use crate::{Actions, Buffer, Channel, UserMap};

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct State {
    pub icon_size: f32,
    pub pixels_per_point: f32,
    pub channels: Vec<Channel>,
    #[serde(skip)]
    pub buffers: Vec<Buffer>,
    pub selected: usize,
    pub actions: Actions,
    pub user_map: UserMap,
}

impl Default for State {
    fn default() -> Self {
        let (channels, buffers, selected, actions, user_map) = <_>::default();
        Self {
            icon_size: 32.0,
            pixels_per_point: 2.0,
            channels,
            buffers,
            selected,
            actions,
            user_map,
        }
    }
}
