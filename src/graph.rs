extern crate sdl2;

use sdl2::{
    render::WindowCanvas,
    pixels::Color,
    EventPump,
    event::Event,
    rect::Point,
};

pub enum Status {
    Running,
    Stopped,
}


const FILL_COLOR: Color = Color::RGB(30, 30, 30);

pub struct Graph {
    canvas: WindowCanvas,
    event_pump: EventPump,
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    n: usize
}

impl Graph {
    pub fn new(graph_name: &str) -> Result<Self, String> {
        let sdl = sdl2::init()?;
        let video_sub = sdl.video()?;

        let window = video_sub.window(graph_name, 400, 500)
            .position_centered()
            .opengl()
            .build()
            .map_err(|e| e.to_string())?;

        let canvas = window.into_canvas()
            .build()
            .map_err(|e| e.to_string())?;

        let event_pump = sdl.event_pump()?;

        Ok(Self { canvas, event_pump, min_x: -10.0, max_x: 10.0, min_y: -10.0, max_y: 10.0, n: 1000 })
    }
    fn convert_coords(&self, x: f64, y: f64) -> (i32, i32) {
        let (width, height) = self.canvas.window().size();
        let x: i32 = ((x - self.min_x) / (self.max_x - self.min_x) * (width as f64)) as i32;
        let y: i32 = ((y - self.min_y) / (self.max_y - self.min_y) * (height as f64)) as i32;
        (x, y)
    }
    fn draw_line(&mut self, x_1: f64, y_1: f64, x_2: f64, y_2: f64, color: Color) -> Result<(), String> {
        let (x_1, y_1) = self.convert_coords(x_1, y_1);
        let (x_2, y_2) = self.convert_coords(x_2, y_2);
        self.canvas.set_draw_color(color);
        self.canvas.draw_line(Point::new(x_1, y_1), Point::new(x_2, y_2))?;
        Ok(())
    }
    fn graph(&mut self, f: fn(f64) -> f64) {
        let dx = (self.max_x - self.min_x) / (self.n as f64);

        let mut x_1 = self.min_x;
        while x_1 < self.max_x {
            let x_2 = x_1 + dx;
            let y_1 = f(x_1);
            let y_2 = f(x_2);
            let _ = self.draw_line(x_1, y_1, x_2, y_2, Color::RED);
            x_1 = x_2;
        }
    }
    pub fn render(&mut self) -> Status {
        self.canvas.set_draw_color(FILL_COLOR);
        self.canvas.clear();

        self.graph(|x| -1.0 * x * x.sin());

        self.canvas.present();

        use Status::*;
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => return Stopped,
                _ => {}
            }
        }
        Running
    }
}
