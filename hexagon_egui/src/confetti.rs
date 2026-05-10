use egui::{Color32, Painter, Pos2, Rect, Shape, Stroke, pos2};
use hexagon_engine::RandomNumberGenerator;

const SPAWN_DURATION: f32 = 4.0;
const SPAWN_INTERVAL: f32 = 0.08;
const PARTICLES_PER_BURST: usize = 8;
const GRAVITY: f32 = 130.0;

const COLORS: &[[u8; 3]] = &[
    [255, 60, 60],
    [60, 200, 80],
    [80, 130, 255],
    [255, 200, 0],
    [255, 100, 210],
    [80, 230, 230],
    [190, 90, 255],
];

struct ConfettiParticle {
    x: f32,
    y: f32,
    vx: f32,
    vy: f32,
    color: Color32,
    w: f32,
    h: f32,
    rotation: f32,
    rot_speed: f32,
}

pub struct ConfettiState {
    particles: Vec<ConfettiParticle>,
    spawn_timer: f32,
    elapsed: f32,
    rng: RandomNumberGenerator,
}

impl Default for ConfettiState {
    fn default() -> Self {
        Self {
            particles: Vec::new(),
            spawn_timer: 0.0,
            elapsed: SPAWN_DURATION + 1.0, // not active initially
            rng: RandomNumberGenerator::new(42),
        }
    }
}

impl ConfettiState {
    pub fn activate(&mut self) {
        if !self.is_active() {
            self.elapsed = 0.0;
            self.spawn_timer = 0.0;
            self.particles.clear();
            self.rng = RandomNumberGenerator::new(42);
        }
    }

    pub fn is_active(&self) -> bool {
        self.elapsed < SPAWN_DURATION || !self.particles.is_empty()
    }

    fn next_f32(&mut self) -> f32 {
        self.rng.next_f64() as f32
    }

    pub fn update_and_draw(&mut self, dt: f32, rect: Rect, painter: &Painter) {
        if !self.is_active() {
            return;
        }

        self.elapsed += dt;

        // Spawn burst
        if self.elapsed < SPAWN_DURATION {
            self.spawn_timer -= dt;
            if self.spawn_timer <= 0.0 {
                self.spawn_timer = SPAWN_INTERVAL;
                for _ in 0..PARTICLES_PER_BURST {
                    let x = rect.left() + self.next_f32() * rect.width();
                    let color_idx =
                        (self.next_f32() * COLORS.len() as f32) as usize % COLORS.len();
                    let w = 5.0 + self.next_f32() * 7.0;
                    let h = 3.0 + self.next_f32() * 5.0;
                    let vx = (self.next_f32() - 0.5) * 70.0;
                    let vy = 30.0 + self.next_f32() * 80.0;
                    let rotation = self.next_f32() * std::f32::consts::TAU;
                    let rot_speed = (self.next_f32() - 0.5) * 9.0;
                    let rgb = COLORS[color_idx];
                    self.particles.push(ConfettiParticle {
                        x,
                        y: rect.top() - h,
                        vx,
                        vy,
                        color: Color32::from_rgb(rgb[0], rgb[1], rgb[2]),
                        w,
                        h,
                        rotation,
                        rot_speed,
                    });
                }
            }
        }

        // Update positions and draw
        let painter_clip = painter.with_clip_rect(rect);
        self.particles.retain_mut(|p| {
            p.vy += GRAVITY * dt;
            p.x += p.vx * dt;
            p.y += p.vy * dt;
            p.rotation += p.rot_speed * dt;

            if p.y > rect.bottom() + p.h {
                return false;
            }

            let cos = p.rotation.cos();
            let sin = p.rotation.sin();
            let hw = p.w / 2.0;
            let hh = p.h / 2.0;
            let corners: [Pos2; 4] = [
                pos2(p.x + (-hw) * cos - (-hh) * sin, p.y + (-hw) * sin + (-hh) * cos),
                pos2(p.x + hw * cos - (-hh) * sin, p.y + hw * sin + (-hh) * cos),
                pos2(p.x + hw * cos - hh * sin, p.y + hw * sin + hh * cos),
                pos2(p.x + (-hw) * cos - hh * sin, p.y + (-hw) * sin + hh * cos),
            ];
            painter_clip.add(Shape::convex_polygon(
                corners.to_vec(),
                p.color,
                Stroke::NONE,
            ));
            true
        });
    }
}
