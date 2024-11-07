use core::f32;
use std::{collections::BTreeSet, sync::Arc};

use eframe::egui::{self, RichText};

use crate::{
    base::{Context, Response, RuntimeError},
    interact::{Blocker, Environment, Event, External},
    parse::{parse_program, Name, ParseError},
};

pub struct Playground {
    code: String,
    parsed: Option<Result<Context<Arc<Name>, External>, ParseError>>,
    environment: Option<Result<Environment<Arc<Name>>, RuntimeError<Arc<Name>, External>>>,
    hidden: BTreeSet<External>,
}

impl Playground {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Box<Self> {
        cc.egui_ctx.all_styles_mut(|style| {
            style.text_styles.extend([
                (egui::TextStyle::Monospace, egui::FontId::monospace(16.0)),
                (egui::TextStyle::Button, egui::FontId::monospace(18.0)),
                (egui::TextStyle::Body, egui::FontId::proportional(16.0)),
            ]);
            style.visuals.code_bg_color = egui::Color32::TRANSPARENT;
        });
        Box::new(Self {
            code: DEFAULT_CODE.to_string(),
            parsed: None,
            environment: None,
            hidden: BTreeSet::new(),
        })
    }
}

impl eframe::App for Playground {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            egui::SidePanel::right("running")
                .resizable(true)
                .show_separator_line(true)
                .default_width(16.0 * 32.0)
                .show_inside(ui, |ui| {
                    ui.horizontal_top(|ui| {
                        if ui.button(RichText::new("PARSE").strong()).clicked() {
                            self.parsed = Some(parse_program(self.code.as_str()));
                        }

                        if let Some(Ok(context)) = &self.parsed {
                            ui.menu_button(RichText::new("RUN").strong(), |ui| {
                                for name in context.statics.keys() {
                                    if ui.button(&name.string).clicked() {
                                        self.environment =
                                            Some(Environment::new(context.clone(), name));
                                        self.hidden.clear();
                                        ui.close_menu();
                                    }
                                }
                            });
                        }
                    });

                    ui.separator();

                    if let Some(Err(parse_error)) = &self.parsed {
                        ui.label(
                            egui::RichText::new(&parse_error.message)
                                .color(egui::Color32::from_hex("#DE3C4B").unwrap())
                                .code(),
                        );
                    }

                    if let Some(environment) = &mut self.environment {
                        show_environment(ui, environment, &mut self.hidden);
                    }

                    ui.allocate_space(ui.available_size());
                });

            egui::CentralPanel::default().show_inside(ui, |ui| {
                egui::ScrollArea::vertical().show(ui, |ui| {
                    egui::TextEdit::multiline(&mut self.code)
                        .code_editor()
                        .frame(false)
                        .desired_width(f32::INFINITY)
                        .desired_rows(32)
                        .show(ui);
                    ui.add_space(16.0 * 32.0);
                });
            });
        });
    }
}

fn show_environment(
    ui: &mut egui::Ui,
    environment: &mut Result<Environment<Arc<Name>>, RuntimeError<Arc<Name>, External>>,
    hidden: &mut BTreeSet<External>,
) {
    if let Ok(env) = environment {
        if let Err(err) = env.run_to_suspension() {
            let _ = std::mem::replace(environment, Err(err));
        }
    }
    match environment {
        Ok(environment) => {
            show_external(ui, environment, environment.primary.clone(), hidden);
        }
        Err(runtime_error) => {
            ui.label(
                egui::RichText::new(format!("{}", runtime_error))
                    .color(egui::Color32::from_hex("#DE3C4B").unwrap())
                    .code(),
            );
        }
    }
}

fn show_external(
    ui: &mut egui::Ui,
    environment: &mut Environment<Arc<Name>>,
    external: External,
    hidden: &mut BTreeSet<External>,
) {
    if hidden.contains(&external) {
        return;
    }

    let history = environment
        .histories
        .get(&external)
        .cloned()
        .unwrap_or_else(|| Vec::new());

    egui::Frame::default()
        .stroke(egui::Stroke::new(1.0, egui::Color32::GRAY))
        .inner_margin(egui::Margin::same(4.0))
        .outer_margin(egui::Margin::same(2.0))
        .show(ui, |ui| {
            ui.horizontal_top(|ui| {
                let mut to_the_side = Vec::new();

                ui.vertical(|ui| {
                    for event in history {
                        match event {
                            Event::Break => {
                                ui.horizontal(|ui| {
                                    ui.label(egui::RichText::new("break").italics().code());
                                    if ui.small_button("X").clicked() {
                                        hidden.insert(external.clone());
                                    }
                                });
                            }
                            Event::Continue => {
                                ui.horizontal(|ui| {
                                    ui.label(egui::RichText::new("continue").italics().code());
                                    if ui.small_button("X").clicked() {
                                        hidden.insert(external.clone());
                                    }
                                });
                            }
                            Event::Send(child) => {
                                show_external(ui, environment, child.clone(), hidden);
                            }
                            Event::Receive(child) => {
                                to_the_side.push(child.clone());
                            }
                            Event::Select(selected) => {
                                ui.horizontal(|ui| {
                                    ui.label(
                                        RichText::new("+")
                                            .strong()
                                            .code()
                                            .color(egui::Color32::from_hex("#118ab2").unwrap()),
                                    );
                                    ui.label(egui::RichText::new(&selected.string).strong().code());
                                });
                            }
                            Event::Case(selected) => {
                                ui.horizontal(|ui| {
                                    ui.label(
                                        RichText::new("&")
                                            .strong()
                                            .code()
                                            .color(egui::Color32::from_hex("#7ac74f").unwrap()),
                                    );
                                    ui.label(egui::RichText::new(&selected.string).strong().code());
                                });
                            }
                        }
                    }

                    if let Some(Blocker::Case(branches, otherwise)) =
                        environment.blockers.get(&external).cloned()
                    {
                        ui.menu_button(RichText::new("CASE").strong(), |ui| {
                            for branch in branches {
                                if ui.button(&branch.string).clicked() {
                                    environment.respond(
                                        external.clone(),
                                        Response::Case(Some(branch.clone())),
                                    );
                                    ui.close_menu();
                                }
                            }
                            if let Some(()) = otherwise {
                                if ui.button("---").clicked() {
                                    environment.respond(external.clone(), Response::Case(None));
                                    ui.close_menu();
                                }
                            }
                        });
                    }
                });

                for side in to_the_side {
                    show_external(ui, environment, side, hidden);
                }
            })
        });
}

static DEFAULT_CODE: &str = r#"
define yes_or_no = ask {
  ask[value];
  value.case {
    no => {
      ask(r{ r.no; r() });
      value[];
      ask()
    }
    yes => {
      ask(r{ r.yes; r() });
      value[];
      ask()
    }
  }
}

define play_with_stack = user {
  let loop = yes_no_stack_loop;
  loop(drained);
  user <> loop
}

define yes_no_stack_loop = user {
  user[stack];
  user.case {
    pop => {
      stack.pop;
      stack.case {
        empty => {
          user.empty;
          stack[];
          user()
        }
        item => {
          user.item;
          stack[value];
          user(value);
          let loop = yes_no_stack_loop;
          loop(stack);
          user <> loop
        }
      }
    }
    push => {
      user[value];
      let validated = out {
        let validate = yes_or_no;
        validate(value);
        validate[value];
        validate[];
        out <> value
      };
      stack.push;
      stack(validated);
      let loop = yes_no_stack_loop;
      loop(stack);
      user <> loop
    }
  }
}

define drained = items {
  items.case {
    pop => {
      items.empty;
      items()
    }
    push => {
      let above = stacked;
      above(drained);
      items <> above
    }
  }
}

define stacked = items {
  items[under];
  items[top];
  items.case {
    pop => {
      items.item;
      items(top);
      items <> under
    }
    push => {
      let above = stacked;
      let self = stacked;
      self(under);
      self(top);
      above(self);
      items <> above
    }
  }
}
"#;
