use std::fmt::{self, Display, Formatter};
use std::ops::{Add, Deref, Index, IndexMut, Mul};
use std::str::FromStr;

use log::trace;
use serde::de::{Error as SerdeError, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use alacritty_config_derive::SerdeReplace;
use alacritty_terminal::term::color::COUNT;
use alacritty_terminal::vte::ansi::{NamedColor, Rgb as VteRgb};

use crate::config::color::Colors;

/// Factor for automatic computation of dim colors.
pub const DIM_FACTOR: f32 = 0.66;

#[derive(Copy, Clone)]
pub struct List([Rgb; COUNT]);

impl From<&'_ Colors> for List {
    fn from(colors: &Colors) -> List {
        // Type inference fails without this annotation.
        let mut list = List([Rgb::default(); COUNT]);

        list.fill_named(colors);
        if colors.generate_indexed {
            list.fill_generated_256(colors);
        } else {
            list.fill_cube(colors);
            list.fill_gray_ramp(colors);
        }

        list
    }
}

impl List {
    pub fn fill_named(&mut self, colors: &Colors) {
        // Normals.
        self[NamedColor::Black] = colors.normal.black;
        self[NamedColor::Red] = colors.normal.red;
        self[NamedColor::Green] = colors.normal.green;
        self[NamedColor::Yellow] = colors.normal.yellow;
        self[NamedColor::Blue] = colors.normal.blue;
        self[NamedColor::Magenta] = colors.normal.magenta;
        self[NamedColor::Cyan] = colors.normal.cyan;
        self[NamedColor::White] = colors.normal.white;

        // Brights.
        self[NamedColor::BrightBlack] = colors.bright.black;
        self[NamedColor::BrightRed] = colors.bright.red;
        self[NamedColor::BrightGreen] = colors.bright.green;
        self[NamedColor::BrightYellow] = colors.bright.yellow;
        self[NamedColor::BrightBlue] = colors.bright.blue;
        self[NamedColor::BrightMagenta] = colors.bright.magenta;
        self[NamedColor::BrightCyan] = colors.bright.cyan;
        self[NamedColor::BrightWhite] = colors.bright.white;
        self[NamedColor::BrightForeground] =
            colors.primary.bright_foreground.unwrap_or(colors.primary.foreground);

        // Foreground and background.
        self[NamedColor::Foreground] = colors.primary.foreground;
        self[NamedColor::Background] = colors.primary.background;

        // Dims.
        self[NamedColor::DimForeground] =
            colors.primary.dim_foreground.unwrap_or(colors.primary.foreground * DIM_FACTOR);
        match colors.dim {
            Some(ref dim) => {
                trace!("Using config-provided dim colors");
                self[NamedColor::DimBlack] = dim.black;
                self[NamedColor::DimRed] = dim.red;
                self[NamedColor::DimGreen] = dim.green;
                self[NamedColor::DimYellow] = dim.yellow;
                self[NamedColor::DimBlue] = dim.blue;
                self[NamedColor::DimMagenta] = dim.magenta;
                self[NamedColor::DimCyan] = dim.cyan;
                self[NamedColor::DimWhite] = dim.white;
            },
            None => {
                trace!("Deriving dim colors from normal colors");
                self[NamedColor::DimBlack] = colors.normal.black * DIM_FACTOR;
                self[NamedColor::DimRed] = colors.normal.red * DIM_FACTOR;
                self[NamedColor::DimGreen] = colors.normal.green * DIM_FACTOR;
                self[NamedColor::DimYellow] = colors.normal.yellow * DIM_FACTOR;
                self[NamedColor::DimBlue] = colors.normal.blue * DIM_FACTOR;
                self[NamedColor::DimMagenta] = colors.normal.magenta * DIM_FACTOR;
                self[NamedColor::DimCyan] = colors.normal.cyan * DIM_FACTOR;
                self[NamedColor::DimWhite] = colors.normal.white * DIM_FACTOR;
            },
        }
    }

    pub fn fill_cube(&mut self, colors: &Colors) {
        let mut index: usize = 16;
        // Build colors.
        for r in 0..6 {
            for g in 0..6 {
                for b in 0..6 {
                    // Override colors 16..232 with the config (if present).
                    if let Some(indexed_color) =
                        colors.indexed_colors.iter().find(|ic| ic.index() == index as u8)
                    {
                        self[index] = indexed_color.color;
                    } else {
                        self[index] = Rgb::new(
                            if r == 0 { 0 } else { r * 40 + 55 },
                            if g == 0 { 0 } else { g * 40 + 55 },
                            if b == 0 { 0 } else { b * 40 + 55 },
                        );
                    }
                    index += 1;
                }
            }
        }

        debug_assert!(index == 232);
    }

    pub fn fill_gray_ramp(&mut self, colors: &Colors) {
        let mut index: usize = 232;

        for i in 0..24 {
            // Index of the color is number of named colors + number of cube colors + i.
            let color_index = 16 + 216 + i;

            // Override colors 232..256 with the config (if present).
            if let Some(indexed_color) =
                colors.indexed_colors.iter().find(|ic| ic.index() == color_index)
            {
                self[index] = indexed_color.color;
                index += 1;
                continue;
            }

            let value = i * 10 + 8;
            self[index] = Rgb::new(value, value, value);
            index += 1;
        }

        debug_assert!(index == 256);
    }

    fn fill_generated_256(&mut self, colors: &Colors) {
        let mut overrides: [Option<Rgb>; 256] = [None; 256];
        for ic in &colors.indexed_colors {
            overrides[ic.index() as usize] = Some(ic.color);
        }

        let base8_lab: [Lab; 8] = [
            Lab::from_rgb(colors.primary.background),
            Lab::from_rgb(colors.normal.red),
            Lab::from_rgb(colors.normal.green),
            Lab::from_rgb(colors.normal.yellow),
            Lab::from_rgb(colors.normal.blue),
            Lab::from_rgb(colors.normal.magenta),
            Lab::from_rgb(colors.normal.cyan),
            Lab::from_rgb(colors.primary.foreground),
        ];

        // Color cube (indices 16-231).
        let mut index: usize = 16;
        for ri in 0..6 {
            let tr = ri as f32 / 5.0;
            let c0 = Lab::lerp(tr, base8_lab[0], base8_lab[1]);
            let c1 = Lab::lerp(tr, base8_lab[2], base8_lab[3]);
            let c2 = Lab::lerp(tr, base8_lab[4], base8_lab[5]);
            let c3 = Lab::lerp(tr, base8_lab[6], base8_lab[7]);
            for gi in 0..6 {
                let tg = gi as f32 / 5.0;
                let c4 = Lab::lerp(tg, c0, c1);
                let c5 = Lab::lerp(tg, c2, c3);
                for bi in 0..6 {
                    if let Some(color) = overrides[index] {
                        self[index] = color;
                    } else {
                        self[index] = Lab::lerp(bi as f32 / 5.0, c4, c5).to_rgb();
                    }
                    index += 1;
                }
            }
        }
        debug_assert!(index == 232);

        // Grayscale ramp (indices 232-255).
        for i in 0..24 {
            let t = (i + 1) as f32 / 25.0;
            if let Some(color) = overrides[index] {
                self[index] = color;
            } else {
                self[index] = Lab::lerp(t, base8_lab[0], base8_lab[7]).to_rgb();
            }
            index += 1;
        }
        debug_assert!(index == 256);
    }
}

impl Index<usize> for List {
    type Output = Rgb;

    #[inline]
    fn index(&self, idx: usize) -> &Self::Output {
        &self.0[idx]
    }
}

impl IndexMut<usize> for List {
    #[inline]
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        &mut self.0[idx]
    }
}

impl Index<NamedColor> for List {
    type Output = Rgb;

    #[inline]
    fn index(&self, idx: NamedColor) -> &Self::Output {
        &self.0[idx as usize]
    }
}

impl IndexMut<NamedColor> for List {
    #[inline]
    fn index_mut(&mut self, idx: NamedColor) -> &mut Self::Output {
        &mut self.0[idx as usize]
    }
}

#[derive(SerdeReplace, Debug, Eq, PartialEq, Copy, Clone, Default)]
pub struct Rgb(pub VteRgb);

impl Rgb {
    #[inline]
    pub const fn new(r: u8, g: u8, b: u8) -> Self {
        Self(VteRgb { r, g, b })
    }

    #[inline]
    pub fn as_tuple(self) -> (u8, u8, u8) {
        (self.0.r, self.0.g, self.0.b)
    }
}

impl From<VteRgb> for Rgb {
    fn from(value: VteRgb) -> Self {
        Self(value)
    }
}

impl Deref for Rgb {
    type Target = VteRgb;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Mul<f32> for Rgb {
    type Output = Rgb;

    fn mul(self, rhs: f32) -> Self::Output {
        Rgb(self.0 * rhs)
    }
}

impl Add<Rgb> for Rgb {
    type Output = Rgb;

    fn add(self, rhs: Rgb) -> Self::Output {
        Rgb(self.0 + rhs.0)
    }
}

/// Deserialize Rgb color from a hex string.
impl<'de> Deserialize<'de> for Rgb {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct RgbVisitor;

        // Used for deserializing reftests.
        #[derive(Deserialize)]
        struct RgbDerivedDeser {
            r: u8,
            g: u8,
            b: u8,
        }

        impl Visitor<'_> for RgbVisitor {
            type Value = Rgb;

            fn expecting(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.write_str("hex color like #ff00ff")
            }

            fn visit_str<E>(self, value: &str) -> Result<Rgb, E>
            where
                E: serde::de::Error,
            {
                Rgb::from_str(value).map_err(|_| {
                    E::custom(format!(
                        "failed to parse rgb color {value}; expected hex color like #ff00ff"
                    ))
                })
            }
        }

        // Return an error if the syntax is incorrect.
        let value = toml::Value::deserialize(deserializer)?;

        // Attempt to deserialize from struct form.
        if let Ok(RgbDerivedDeser { r, g, b }) = RgbDerivedDeser::deserialize(value.clone()) {
            return Ok(Rgb::new(r, g, b));
        }

        // Deserialize from hex notation (either 0xff00ff or #ff00ff).
        value.deserialize_str(RgbVisitor).map_err(D::Error::custom)
    }
}

/// Serialize Rgb color to a hex string.
impl Serialize for Rgb {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Display for Rgb {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{:02x}{:02x}{:02x}", self.r, self.g, self.b)
    }
}

impl FromStr for Rgb {
    type Err = ();

    fn from_str(s: &str) -> Result<Rgb, ()> {
        let chars = if s.starts_with("0x") && s.len() == 8 {
            &s[2..]
        } else if s.starts_with('#') && s.len() == 7 {
            &s[1..]
        } else {
            return Err(());
        };

        match u32::from_str_radix(chars, 16) {
            Ok(mut color) => {
                let b = (color & 0xff) as u8;
                color >>= 8;
                let g = (color & 0xff) as u8;
                color >>= 8;
                let r = color as u8;
                Ok(Rgb::new(r, g, b))
            },
            Err(_) => Err(()),
        }
    }
}

/// RGB color optionally referencing the cell's foreground or background.
#[derive(SerdeReplace, Serialize, Copy, Clone, Debug, PartialEq, Eq)]
pub enum CellRgb {
    CellForeground,
    CellBackground,
    #[serde(untagged)]
    Rgb(Rgb),
}

impl CellRgb {
    pub fn color(self, foreground: Rgb, background: Rgb) -> Rgb {
        match self {
            Self::CellForeground => foreground,
            Self::CellBackground => background,
            Self::Rgb(rgb) => rgb,
        }
    }
}

impl Default for CellRgb {
    fn default() -> Self {
        Self::Rgb(Rgb::default())
    }
}

impl<'de> Deserialize<'de> for CellRgb {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        const EXPECTING: &str = "CellForeground, CellBackground, or hex color like #ff00ff";

        struct CellRgbVisitor;
        impl Visitor<'_> for CellRgbVisitor {
            type Value = CellRgb;

            fn expecting(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.write_str(EXPECTING)
            }

            fn visit_str<E>(self, value: &str) -> Result<CellRgb, E>
            where
                E: serde::de::Error,
            {
                // Attempt to deserialize as enum constants.
                match value {
                    "CellForeground" => return Ok(CellRgb::CellForeground),
                    "CellBackground" => return Ok(CellRgb::CellBackground),
                    _ => (),
                }

                Rgb::from_str(value).map(CellRgb::Rgb).map_err(|_| {
                    E::custom(format!("failed to parse color {value}; expected {EXPECTING}"))
                })
            }
        }

        deserializer.deserialize_str(CellRgbVisitor).map_err(D::Error::custom)
    }
}

/// CIELAB color space for perceptually uniform interpolation.
#[derive(Copy, Clone, Debug, PartialEq)]
struct Lab {
    l: f32,
    a: f32,
    b: f32,
}

impl Lab {
    fn from_rgb(rgb: Rgb) -> Self {
        let mut r = rgb.r as f32 / 255.0;
        let mut g = rgb.g as f32 / 255.0;
        let mut b = rgb.b as f32 / 255.0;

        r = if r > 0.04045 { ((r + 0.055) / 1.055).powf(2.4) } else { r / 12.92 };
        g = if g > 0.04045 { ((g + 0.055) / 1.055).powf(2.4) } else { g / 12.92 };
        b = if b > 0.04045 { ((b + 0.055) / 1.055).powf(2.4) } else { b / 12.92 };

        let mut x = (r * 0.4124564 + g * 0.3575761 + b * 0.1804375) / 0.95047;
        let mut y = r * 0.2126729 + g * 0.7151522 + b * 0.0721750;
        let mut z = (r * 0.0193339 + g * 0.1191920 + b * 0.9503041) / 1.08883;

        x = if x > 0.008856 { x.cbrt() } else { 7.787 * x + 16.0 / 116.0 };
        y = if y > 0.008856 { y.cbrt() } else { 7.787 * y + 16.0 / 116.0 };
        z = if z > 0.008856 { z.cbrt() } else { 7.787 * z + 16.0 / 116.0 };

        Lab { l: 116.0 * y - 16.0, a: 500.0 * (x - y), b: 200.0 * (y - z) }
    }

    fn to_rgb(self) -> Rgb {
        let y = (self.l + 16.0) / 116.0;
        let x = self.a / 500.0 + y;
        let z = y - self.b / 200.0;

        let x3 = x * x * x;
        let y3 = y * y * y;
        let z3 = z * z * z;
        let xf = (if x3 > 0.008856 { x3 } else { (x - 16.0 / 116.0) / 7.787 }) * 0.95047;
        let yf = if y3 > 0.008856 { y3 } else { (y - 16.0 / 116.0) / 7.787 };
        let zf = (if z3 > 0.008856 { z3 } else { (z - 16.0 / 116.0) / 7.787 }) * 1.08883;

        let mut r = xf * 3.2404542 - yf * 1.5371385 - zf * 0.4985314;
        let mut g = -xf * 0.9692660 + yf * 1.8760108 + zf * 0.0415560;
        let mut b = xf * 0.0556434 - yf * 0.2040259 + zf * 1.0572252;

        r = if r > 0.0031308 { 1.055 * r.powf(1.0 / 2.4) - 0.055 } else { 12.92 * r };
        g = if g > 0.0031308 { 1.055 * g.powf(1.0 / 2.4) - 0.055 } else { 12.92 * g };
        b = if b > 0.0031308 { 1.055 * b.powf(1.0 / 2.4) - 0.055 } else { 12.92 * b };

        Rgb::new(
            (r.clamp(0.0, 1.0) * 255.0 + 0.5) as u8,
            (g.clamp(0.0, 1.0) * 255.0 + 0.5) as u8,
            (b.clamp(0.0, 1.0) * 255.0 + 0.5) as u8,
        )
    }

    fn lerp(t: f32, a: Lab, b: Lab) -> Lab {
        Lab { l: a.l + t * (b.l - a.l), a: a.a + t * (b.a - a.a), b: a.b + t * (b.b - a.b) }
    }
}
