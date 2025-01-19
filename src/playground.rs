use std::{
    collections::BTreeSet,
    sync::{Arc, Mutex},
};

use eframe::egui;
use egui_code_editor::{CodeEditor, ColorTheme, Syntax};
use indexmap::IndexMap;

use crate::{
    interact::{Event, Handle, Request},
    language::{CompileError, Internal},
    parse::{parse_program, Loc, Name, ParseError},
    process::Expression,
    runtime::{self, Context},
    spawn::TokioSpawn,
};

pub struct Playground {
    code: String,
    compiled: Option<Result<Compiled, Error>>,
    interact: Option<Arc<Mutex<Handle<Loc, Internal<Name>>>>>,
    editor_font_size: f32,
}

#[derive(Debug)]
enum Error {
    Parse(ParseError),
    Compile(CompileError<Loc>),
    Runtime(runtime::Error<Loc, Internal<Name>>),
}

struct Compiled {
    globals: Arc<IndexMap<Internal<Name>, Arc<Expression<Loc, Internal<Name>>>>>,
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
            compiled: None,
            interact: None,
            editor_font_size: 16.0,
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
                    egui::ScrollArea::vertical().show(ui, |ui| {
                        ui.horizontal(|ui| {
                            if ui.button(egui::RichText::new("-").monospace()).clicked() {
                                self.editor_font_size = (self.editor_font_size - 1.0).max(8.0);
                            }
                            ui.label(
                                egui::RichText::new(self.editor_font_size.to_string()).strong(),
                            );
                            if ui.button(egui::RichText::new("+").monospace()).clicked() {
                                self.editor_font_size = (self.editor_font_size + 1.0).min(320.0);
                            }
                        });

                        ui.separator();

                        let theme = if ui.visuals().dark_mode {
                            fix_dark_theme(ColorTheme::SONOKAI)
                        } else {
                            fix_light_theme(ColorTheme::GRUVBOX_LIGHT)
                        };
                        CodeEditor::default()
                            .id_source("code")
                            .with_syntax(par_syntax())
                            .with_rows(32)
                            .with_fontsize(self.editor_font_size)
                            .with_theme(theme)
                            .with_numlines(true)
                            .show(ui, &mut self.code);
                    });
                });

            self.show_interaction(ui);
        });
    }
}

impl Playground {
    fn show_interaction(&mut self, ui: &mut egui::Ui) {
        ui.vertical(|ui| {
            ui.horizontal_top(|ui| {
                ui.add_space(5.0);

                if ui.button(egui::RichText::new("Compile").strong()).clicked() {
                    self.compiled = Some(
                        parse_program(self.code.as_str())
                            .map_err(|error| Error::Parse(error))
                            .and_then(|definitions| {
                                let compile_result = definitions
                                    .into_iter()
                                    .map(|(name, def)| {
                                        def.compile().map(|compiled| {
                                            (
                                                Internal::Original(name.clone()),
                                                Arc::new(compiled.fix_captures(&IndexMap::new()).0),
                                            )
                                        })
                                    })
                                    .collect::<Result<_, CompileError<Loc>>>();
                                match compile_result {
                                    Ok(compiled) => Ok(Compiled {
                                        globals: Arc::new(compiled),
                                    }),
                                    Err(error) => Err(Error::Compile(error)),
                                }
                            }),
                    );
                }

                if let Some(Ok(Compiled { globals })) = &self.compiled {
                    egui::menu::menu_custom_button(
                        ui,
                        egui::Button::new(
                            egui::RichText::new("Run")
                                .strong()
                                .color(egui::Color32::BLACK),
                        )
                        .fill(green().lerp_to_gamma(egui::Color32::WHITE, 0.3)),
                        |ui| {
                            for (internal_name, expression) in globals.as_ref() {
                                if let Internal::Original(name) = internal_name {
                                    if ui.button(&name.string).clicked() {
                                        if let Some(int) = self.interact.take() {
                                            int.lock().expect("lock failed").cancel();
                                        }
                                        self.interact = Some(Handle::start_expression(
                                            Arc::new({
                                                let ctx = ui.ctx().clone();
                                                move || ctx.request_repaint()
                                            }),
                                            Context::new(Arc::new(TokioSpawn), Arc::clone(globals)),
                                            expression,
                                        ));
                                        ui.close_menu();
                                    }
                                }
                            }
                        },
                    );
                }
            });

            ui.separator();

            egui::CentralPanel::default().show_inside(ui, |ui| {
                egui::ScrollArea::both().show(ui, |ui| {
                    if let Some(Err(error)) = &self.compiled {
                        ui.label(
                            egui::RichText::new(format!("{:?}", error))
                                .color(red())
                                .code(),
                        );
                    }

                    if let Some(int) = &self.interact {
                        self.show_interact(ui, Arc::clone(int));
                    }
                });
            });
        });
    }

    fn show_interact(
        &mut self,
        ui: &mut egui::Ui,
        handle: Arc<Mutex<Handle<Loc, Internal<Name>>>>,
    ) {
        let locked_handle = handle.lock().expect("lock failed");

        egui::Frame::default()
            .stroke(egui::Stroke::new(1.0, egui::Color32::GRAY))
            .inner_margin(egui::Margin::same(4.0))
            .outer_margin(egui::Margin::same(2.0))
            .show(ui, |ui| {
                ui.horizontal_top(|ui| {
                    let mut to_the_side = Vec::new();

                    ui.vertical(|ui| {
                        for event in locked_handle.events() {
                            match event {
                                Event::Send(_, argument) => {
                                    self.show_interact(ui, Arc::clone(argument))
                                }

                                Event::Receive(_, parameter) => {
                                    to_the_side.push(Arc::clone(parameter))
                                }

                                Event::Choose(_, chosen) => {
                                    ui.horizontal(|ui| {
                                        ui.label(
                                            egui::RichText::new("+").strong().code().color(blue()),
                                        );
                                        ui.label(
                                            egui::RichText::new(format!("{}", chosen))
                                                .strong()
                                                .code(),
                                        );
                                    });
                                }

                                Event::Either(_, chosen) => {
                                    ui.horizontal(|ui| {
                                        ui.label(
                                            egui::RichText::new(">").strong().code().color(green()),
                                        );
                                        ui.label(
                                            egui::RichText::new(format!("{}", chosen))
                                                .strong()
                                                .code(),
                                        );
                                    });
                                }

                                Event::Break(_) => {
                                    ui.horizontal(|ui| {
                                        ui.label(egui::RichText::new("break").italics().code());
                                    });
                                }

                                Event::Continue(_) => {
                                    ui.horizontal(|ui| {
                                        ui.label(egui::RichText::new("continue").italics().code());
                                    });
                                }
                            }
                        }

                        if let Some(result) = locked_handle.interaction() {
                            ui.horizontal(|ui| match result {
                                Ok(Request::Dynamic(_)) => {
                                    ui.label(egui::RichText::new("?").strong().code());
                                }
                                Ok(Request::Either(loc, choices)) => {
                                    drop(locked_handle);
                                    ui.menu_button(
                                        egui::RichText::new("=>").strong().code(),
                                        |ui| {
                                            for choice in choices.iter() {
                                                if ui.button(format!("{}", choice)).clicked() {
                                                    Handle::choose(
                                                        Arc::clone(&handle),
                                                        loc.clone(),
                                                        choice.clone(),
                                                    );
                                                    ui.close_menu();
                                                }
                                            }
                                        },
                                    );
                                }
                                Err(error) => {
                                    ui.label(
                                        egui::RichText::new(format!("{:?}", error)).color(red()),
                                    );
                                }
                            });
                        }
                    });

                    for side in to_the_side {
                        self.show_interact(ui, side);
                    }
                });
            });
    }
}

fn par_syntax() -> Syntax {
    Syntax {
        language: "Par",
        case_sensitive: true,
        comment: "//",
        comment_multiline: [r#"/*"#, r#"*/"#],
        hyperlinks: BTreeSet::from([]),
        keywords: BTreeSet::from([
            "define", "chan", "let", "pass", "iterate", "loop", "in", "do",
        ]),
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
    theme.bg = "#F9F9F9";
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

static DEFAULT_CODE: &str = include_str!("sample.par");
