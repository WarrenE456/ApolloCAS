extern crate sdl2;

use sdl2::{
    render::WindowCanvas,
    pixels::Color,
    EventPump,
    event::{WindowEvent, Event},
    rect::Point,
    VideoSubsystem,
};

use crate::parser::expr::Expr;
use crate::runtime::{val::Val, Interpreter};

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::mpsc::Receiver;

use std::sync::{Arc, RwLock};

pub enum GraphSignal {
    Create(String),
    Graph {
        graph_name: String,
        fn_name: String,
        e: Expr,
    },
}

pub struct Grapher {
    v_sub: VideoSubsystem,
    event_pump: EventPump,
    global: Arc<RwLock<Interpreter>>,
    name_to_id: HashMap<String, u32>,
    graphs: HashMap<u32, Graph>,
    graph_rx: Receiver<GraphSignal>
}

impl Grapher {
    pub fn new(global: Arc<RwLock<Interpreter>>, graph_rx: Receiver<GraphSignal>) -> Self {
        let sdl2 = sdl2::init().unwrap();
        let v_sub = sdl2.video().unwrap();
        let event_pump = sdl2.event_pump().unwrap();
        Self { global, graphs: HashMap::new(), graph_rx, v_sub, event_pump, name_to_id: HashMap::new() }
    }
    pub fn clear(&mut self) {
        self.graphs.clear();
    }
    pub fn create(&mut self, name: String) {
        let (id, new_graph) = match self.gen_new_window(&name).map(|window| Graph::new(window)) {
            Ok(Ok(g)) => g,
            Err(e) | Ok(Err(e)) => {
                eprintln!("{}", e);
                return;
            }
        };
        self.name_to_id.insert(name, id);
        self.graphs.insert(id, new_graph);
    }
    pub fn graph(&mut self, graph_name: &String, fn_name: String, e: Expr) {
        if let Some(graph) = self.name_to_id.get(graph_name).map(|id| self.graphs.get_mut(id)).flatten() {
            graph.fns.insert(fn_name, e);
        } else {
            eprintln!("Error: No such graph '{}'.", graph_name);
        }
    }
    pub fn update(&mut self) {
        use GraphSignal::*;
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Window { 
                    window_id: id,
                    win_event: WindowEvent::Resized(w, h),
                    ..
                } => {
                    self.graphs.get_mut(&id).map(|g| g.set_dim(w as u32, h as u32));
                }
                _ => {}
            }
        }
        for (_, graph) in self.graphs.iter_mut() {
            graph.render(&self.global);
        }
        if let Ok(signal) = self.graph_rx.try_recv() {
            match signal {
                Create(name) => self.create(name),
                Graph {graph_name, fn_name, e} => self.graph(&graph_name, fn_name, e),
            }
        }
    }
    fn gen_new_window(&self, name: &str) -> Result<sdl2::video::Window, String> {
        let window = self.v_sub.window(name, 400, 500)
            .position_centered()
            .resizable()
            .opengl()
            .build()
            .map_err(|e| e.to_string())?;

        Ok(window)
    }
}

const FILL_COLOR: Color = Color::RGB(30, 30, 30);
const N: f64 = 1000.;

pub struct Graph {
    canvas: RefCell<WindowCanvas>,
    width: u32,
    height: u32,
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    fns: HashMap<String, Expr>,
}

impl Graph {
    pub fn set_dim(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
    }
    pub fn new(window: sdl2::video::Window) -> Result<(u32, Self), String> {
        let id = window.id();

        let canvas = window.into_canvas()
            .build()
            .map_err(|e| e.to_string())
            .map(|c| RefCell::new(c))?;

        let (width, height) = canvas.borrow().window().size();

        Ok((id, Self {
            canvas, min_x: -10.0, max_x: 10.0, min_y: -10.0, max_y: 10.0, fns: HashMap::new(), width, height
        }))
    }
    fn convert_coords(&self, x: f64, y: f64) -> Option<(i32, i32)> {
        let x: i32 = ((x - self.min_x) / (self.max_x - self.min_x) * (self.width as f64)) as i32;
        if !y.is_infinite() && !y.is_nan() {
            let y: i32 = (self.height as i32).checked_sub(((y - self.min_y) / (self.max_y - self.min_y) * (self.height as f64)) as i32)?;
            Some((x, y))
        }
        else {
            None
        }
    }
    fn draw_line(&self, x_1: f64, y_1: f64, x_2: f64, y_2: f64, color: Color) -> Result<(), String> {
        if let Some((x_1, y_1)) = self.convert_coords(x_1, y_1) {
            if let Some((x_2, y_2)) = self.convert_coords(x_2, y_2) {
                let mut canvas = self.canvas.borrow_mut();
                canvas.set_draw_color(color);
                canvas.draw_line(Point::new(x_1, y_1), Point::new(x_2, y_2))?;
            }
        }
        Ok(())
    }
    fn graph(&self, e: &Expr, i: &Arc<RwLock<Interpreter>>) {
        let i = &i.read().unwrap();
        let scope = Interpreter::from(i);
        let dx = (self.max_x - self.min_x) / N;

        let mut x_1 = self.min_x;
        while x_1 < self.max_x {
            let x_2 = x_1 + dx;
            let y_1 = match scope.eval_expr_at(&e, "x", x_1) {
                Ok(Val::Num(v)) => v.to_float(),
                _ => continue,
            };
            let y_2 = match scope.eval_expr_at(&e, "x", x_2) {
                Ok(Val::Num(v)) => v.to_float(),
                _ => continue,
            };
            let _ = self.draw_line(x_1, y_1, x_2, y_2, Color::RED);
            x_1 = x_2;
        }
    }
    // TODO remove status maybe
    pub fn render(&mut self, i: &Arc<RwLock<Interpreter>>) {
        {
            let mut canvas = self.canvas.borrow_mut();
            canvas.set_draw_color(FILL_COLOR);
            canvas.clear();
        }

        for (_, e) in self.fns.iter() {
            self.graph(e, i);
        }

        self.canvas.borrow_mut().present();
    }
}
