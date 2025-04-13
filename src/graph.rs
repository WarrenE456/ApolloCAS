extern crate sdl2;

use sdl2::{
    render::WindowCanvas,
    pixels::Color,
    rect::Point,
};

use crate::statement::Expr;
use crate::interpreter::{Val, Interpreter};

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

pub struct Grapher<'a> {
    global: Arc<RwLock<Interpreter<'a>>>,
    graphs: HashMap<String, Graph>,
    graph_rx: Receiver<GraphSignal>
}

impl<'a> Grapher<'a> {
    pub fn new(global: Arc<RwLock<Interpreter<'a>>>, graph_rx: Receiver<GraphSignal>) -> Self {
        Self { global, graphs: HashMap::new(), graph_rx }
    }
    pub fn create(&mut self, name: String) {
        let new_graph = match Graph::new(&name) {
            Ok(g) => g,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        };
        self.graphs.insert(name, new_graph);
    }
    pub fn graph(&mut self, graph_name: &String, fn_name: String, e: Expr) {
        if let Some(graph) = self.graphs.get_mut(graph_name) {
            graph.fns.insert(fn_name, e);
        } else {
            eprintln!("Error: No such graph '{}'.", graph_name);
        }
    }
    pub fn update(&mut self) {
        use GraphSignal::*;
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
}

const FILL_COLOR: Color = Color::RGB(30, 30, 30);

pub struct Graph {
    canvas: RefCell<WindowCanvas>,
    min_x: f64,
    max_x: f64,
    min_y: f64,
    max_y: f64,
    n: usize,
    fns: HashMap<String, Expr>,
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
            .map_err(|e| e.to_string())
            .map(|c| RefCell::new(c))?;

        Ok(Self {
            canvas, min_x: -10.0, max_x: 10.0, min_y: -10.0, max_y: 10.0, n: 1000, fns: HashMap::new(),
        })
    }
    fn convert_coords(&self, x: f64, y: f64) -> (i32, i32) {
        let (width, height) = self.canvas.borrow().window().size();
        let x: i32 = ((x - self.min_x) / (self.max_x - self.min_x) * (width as f64)) as i32;
        let y: i32 = (height as i32) - ((y - self.min_y) / (self.max_y - self.min_y) * (height as f64)) as i32;
        (x, y)
    }
    fn draw_line(&self, x_1: f64, y_1: f64, x_2: f64, y_2: f64, color: Color) -> Result<(), String> {
        let (x_1, y_1) = self.convert_coords(x_1, y_1);
        let (x_2, y_2) = self.convert_coords(x_2, y_2);
        {
            let mut canvas = self.canvas.borrow_mut();
            canvas.set_draw_color(color);
            canvas.draw_line(Point::new(x_1, y_1), Point::new(x_2, y_2))?;
        }
        Ok(())
    }
    fn graph(&self, e: Expr, i: &Arc<RwLock<Interpreter>>) {
        let i = &i.read().unwrap();
        let scope = Interpreter::from(i);
        let dx = (self.max_x - self.min_x) / (self.n as f64);

        let mut x_1 = self.min_x;
        while x_1 < self.max_x {
            let x_2 = x_1 + dx;
            let y_1 = match scope.eval_expr_at(&e, "x", x_1) {
                Ok(Val::Number(v)) => v,
                _ => continue,
            };
            let y_2 = match scope.eval_expr_at(&e, "x", x_2) {
                Ok(Val::Number(v)) => v,
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
            // TODO remove cloning nonsense
            self.graph(e.clone(), i);
        }

        self.canvas.borrow_mut().present();
    }
}
