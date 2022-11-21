use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Action {
    ToggleJoinWindow,
    ShowCommandPalette,
    ShowKeyBindings,
    ShowSettings,
    SwitchChannel0,
    SwitchChannel1,
    SwitchChannel2,
    SwitchChannel3,
    SwitchChannel4,
    SwitchChannel5,
    SwitchChannel6,
    SwitchChannel7,
    SwitchChannel8,
    SwitchChannel9,
    NextChannel,
    PreviousChannel,
}

impl Action {
    pub const fn stringify(&self) -> &'static str {
        match self {
            Self::ToggleJoinWindow => "Toggle join window",
            Self::ShowCommandPalette => "Show command palette",
            Self::ShowSettings => "Show settings",
            Self::ShowKeyBindings => "Show key bindings",
            Self::SwitchChannel0 => "Switch channel 0",
            Self::SwitchChannel1 => "Switch channel 1",
            Self::SwitchChannel2 => "Switch channel 2",
            Self::SwitchChannel3 => "Switch channel 3",
            Self::SwitchChannel4 => "Switch channel 4",
            Self::SwitchChannel5 => "Switch channel 5",
            Self::SwitchChannel6 => "Switch channel 6",
            Self::SwitchChannel7 => "Switch channel 7",
            Self::SwitchChannel8 => "Switch channel 8",
            Self::SwitchChannel9 => "Switch channel 9",
            Self::NextChannel => "Next channel",
            Self::PreviousChannel => "Previous channel",
        }
    }
}
