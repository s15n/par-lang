use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write,
    sync::Arc,
};

use eframe::egui;
use egui_code_editor::{CodeEditor, ColorTheme, Syntax};

use crate::{
    base::{Context, Request, Response, RuntimeError},
    interact::{Environment, Event, External},
    parse::{parse_program, Name, ParseError},
    print::{self, print_context},
};

pub struct Playground {
    code: String,
    parsed: Result<Parsed, Option<ParseError>>,
    environment: Result<Environment<Arc<Name>>, Option<RuntimeError<Arc<Name>, External>>>,
    hidden: BTreeSet<External>,
    show_parsed: bool,
}

struct Parsed {
    context: Context<Arc<Name>, External>,
    pretty: String,
}

impl Playground {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Box<Self> {
        cc.egui_ctx.all_styles_mut(|style| {
            style.text_styles.extend([
                (egui::TextStyle::Monospace, egui::FontId::monospace(16.0)),
                (egui::TextStyle::Button, egui::FontId::proportional(18.0)),
                (egui::TextStyle::Body, egui::FontId::proportional(16.0)),
            ]);
            style.visuals.code_bg_color = egui::Color32::TRANSPARENT;
            style.wrap_mode = Some(egui::TextWrapMode::Extend);
        });
        Box::new(Self {
            code: DEFAULT_CODE.to_string(),
            parsed: Err(None),
            environment: Err(None),
            hidden: BTreeSet::new(),
            show_parsed: false,
        })
    }
}

impl eframe::App for Playground {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            egui::SidePanel::left("interaction")
                .resizable(true)
                .show_separator_line(true)
                .default_width(16.0 * 32.0)
                .show_inside(ui, |ui| {
                    let code_to_show = match &mut self.parsed {
                        Ok(Parsed { pretty, .. }) if self.show_parsed => pretty,
                        _ => &mut self.code,
                    };

                    egui::ScrollArea::vertical().show(ui, |ui| {
                        ui.checkbox(
                            &mut self.show_parsed,
                            egui::RichText::new("Show desugared").strong(),
                        );
                        ui.separator();

                        let theme = if ui.visuals().dark_mode {
                            fix_dark_theme(ColorTheme::GRUVBOX_DARK)
                        } else {
                            fix_light_theme(ColorTheme::GRUVBOX_LIGHT)
                        };
                        CodeEditor::default()
                            .id_source("code")
                            .with_syntax(par_syntax())
                            .with_rows(32)
                            .with_fontsize(16.0)
                            .with_theme(theme)
                            .with_numlines(true)
                            .show(ui, code_to_show);
                    });
                });

            self.show_interaction(ui);

            egui::TopBottomPanel::bottom("introspection")
                .resizable(true)
                .show_separator_line(true)
                .default_height(16.0 * 8.0)
                .show_inside(ui, |ui| {
                    egui::ScrollArea::both().show(ui, |ui| {
                        self.show_introspection(ui);
                        ui.allocate_space(ui.available_size());
                    });
                });
        });
    }
}

impl Playground {
    fn show_interaction(&mut self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            ui.horizontal_top(|ui| {
                ui.add_space(6.0);

                if ui.button(egui::RichText::new("PARSE").strong()).clicked() {
                    self.parsed = parse_program(self.code.as_str()).map_err(Some).map(
                        |(context, definitions)| {
                            let mut pretty = String::new();
                            let mut buf = String::new();
                            for (name, expression) in &definitions {
                                buf.clear();
                                write!(&mut buf, "{}", expression).unwrap();
                                write!(&mut pretty, "define {} = ", name).unwrap();
                                print::pretty(&mut pretty, &buf, 0).unwrap();
                            }
                            Parsed { context, pretty }
                        },
                    )
                }

                if let Ok(Parsed { context, .. }) = &self.parsed {
                    egui::menu::menu_custom_button(
                        ui,
                        egui::Button::new(
                            egui::RichText::new("RUN")
                                .strong()
                                .color(egui::Color32::BLACK),
                        )
                        .fill(green().lerp_to_gamma(egui::Color32::WHITE, 0.3)),
                        |ui| {
                            for name in context.statics.keys() {
                                if ui.button(&name.string).clicked() {
                                    self.environment =
                                        Environment::new(context.clone(), name).map_err(Some);
                                    self.hidden.clear();
                                    run_to_suspension(&mut self.environment);
                                    ui.close_menu();
                                }
                            }
                        },
                    );
                }
            });

            ui.separator();

            egui::CentralPanel::default().show_inside(ui, |ui| {
                egui::ScrollArea::both().show(ui, |ui| {
                    if let Err(Some(parse_error)) = &self.parsed {
                        ui.label(
                            egui::RichText::new(&parse_error.message)
                                .color(red())
                                .code(),
                        );
                    }

                    match &mut self.environment {
                        Ok(environment) => {
                            if show_external(
                                ui,
                                environment,
                                &environment.get_requests(),
                                environment.primary.clone(),
                                &mut self.hidden,
                            ) {
                                run_to_suspension(&mut self.environment);
                            }
                        }
                        Err(Some(runtime_error)) => {
                            ui.label(
                                egui::RichText::new(format!("{}", runtime_error))
                                    .color(red())
                                    .code(),
                            );
                        }
                        Err(None) => {
                            ui.vertical_centered(|ui| {
                                ui.label("Interaction");
                            });
                        }
                    }
                });
            });
        });
    }

    fn show_introspection(&mut self, ui: &mut egui::Ui) {
        let Ok(environment) = &self.environment else {
            ui.vertical_centered(|ui| {
                ui.label("Introspection");
            });
            return;
        };

        ui.vertical(|ui| {
            let mut buf = String::new();
            for (i, running) in environment.runnings.iter().enumerate() {
                if i > 0 {
                    ui.separator();
                }

                buf.clear();
                let _ = write!(&mut buf, "{}", running.process);
                ui.label(egui::RichText::new(&buf).strong().code());

                buf.clear();
                let _ = print_context(&mut buf, &running.context, 0);
                ui.label(egui::RichText::new(&buf).code());
            }
        });
    }
}

fn show_external(
    ui: &mut egui::Ui,
    environment: &mut Environment<Arc<Name>>,
    requests: &BTreeMap<External, Request<Arc<Name>>>,
    external: External,
    hidden: &mut BTreeSet<External>,
) -> bool {
    if hidden.contains(&external) {
        return false;
    }

    let mut need_run = false;

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
                                need_run =
                                    show_external(ui, environment, requests, child.clone(), hidden)
                                        || need_run;
                            }
                            Event::Message(message) => {
                                ui.horizontal(|ui| {
                                    ui.label(
                                        egui::RichText::new("!").strong().code().color(blue()),
                                    );
                                    ui.label(egui::RichText::new(&message).strong());
                                });
                            }
                            Event::Receive(child) => {
                                to_the_side.push(child.clone());
                            }
                            Event::Select(selected) => {
                                ui.horizontal(|ui| {
                                    ui.label(
                                        egui::RichText::new("+").strong().code().color(blue()),
                                    );
                                    ui.label(egui::RichText::new(&selected.string).strong().code());
                                });
                            }
                            Event::Case(selected) => {
                                ui.horizontal(|ui| {
                                    ui.label(
                                        egui::RichText::new("&").strong().code().color(green()),
                                    );
                                    ui.label(egui::RichText::new(&selected.string).strong().code());
                                });
                            }
                        }
                    }

                    if let Some(Request::Case(branches)) = requests.get(&external) {
                        ui.menu_button(egui::RichText::new("CASE").strong(), |ui| {
                            for branch in branches {
                                if ui.button(&branch.string).clicked() {
                                    environment
                                        .respond(external.clone(), Response::Case(branch.clone()));
                                    need_run = true;
                                    ui.close_menu();
                                }
                            }
                        });
                    }
                });

                for side in to_the_side {
                    need_run = show_external(ui, environment, requests, side, hidden) || need_run;
                }
            })
        });

    need_run
}

fn run_to_suspension(
    environment: &mut Result<Environment<Arc<Name>>, Option<RuntimeError<Arc<Name>, External>>>,
) {
    if let Ok(env) = environment {
        if let Err(runtime_error) = env.run_to_suspension() {
            *environment = Err(Some(runtime_error));
        }
    }
}

fn par_syntax() -> Syntax {
    Syntax {
        language: "Par",
        case_sensitive: true,
        comment: "//",
        comment_multiline: [r#"/*"#, r#"*/"#],
        hyperlinks: BTreeSet::from([]),
        keywords: BTreeSet::from(["define", "chan", "let", "pass"]),
        types: BTreeSet::from([]),
        special: BTreeSet::from(["<>"]),
    }
}

fn fix_dark_theme(mut theme: ColorTheme) -> ColorTheme {
    theme.bg = "#1F1F1F";
    theme.functions = theme.literals;
    theme
}

fn fix_light_theme(mut theme: ColorTheme) -> ColorTheme {
    theme.bg = "#EFEFEF";
    theme.functions = theme.literals;
    theme
}

fn red() -> egui::Color32 {
    egui::Color32::from_hex("#DE3C4B").unwrap()
}

fn green() -> egui::Color32 {
    egui::Color32::from_hex("#7ac74f").unwrap()
}

fn blue() -> egui::Color32 {
    egui::Color32::from_hex("#118ab2").unwrap()
}

static DEFAULT_CODE: &str = r#"define play = ("Happy poppin'") loop(drained)

define loop = [stack] chan user {
  user {
    pop => {
      stack.pop {
        empty[] => { user.empty() }
        item[value] => {
          user.item(value) <> loop(stack)
        }
      }
    }
    push[value] => {
      stack.push(rgb(value));
      user <> loop(stack)
    }
  }
}

define drained = chan items {
  items {
    pop => { items.empty() }
    push => { items <> stacked(drained) }
  }
}

define stacked = [under][top] chan items {
  items {
    pop => { items.item(top) <> under }
    push => {
      items <> stacked(stacked(under)(top))
    }
  }
}

define rgb = [value] chan out {
  value {
    red[] => { out.red() }
    green[] => { out.green() }
    blue[] => { out.blue() }
  }
}
"#;
