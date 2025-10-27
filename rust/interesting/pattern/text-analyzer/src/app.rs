use anyhow::Result;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout, Margin, Rect},
    style::{Color, Modifier, Style},
    text::Line,
    widgets::{
        Block, Borders, Clear, List, ListItem, Paragraph, Tabs, Wrap,
    },
    Frame, Terminal,
};
use std::{
    io,
    path::PathBuf,
    time::{Duration, Instant},
};

use crate::analyzer::TextAnalysis;
use crate::file_handler::FileHandler;

#[derive(Debug, Clone, PartialEq)]
pub enum AppTab {
    Overview,
    Statistics,
    WordFrequency,
    LineAnalysis,
    ParagraphAnalysis,
    Readability,
}

impl AppTab {
    pub fn title(&self) -> &'static str {
        match self {
            AppTab::Overview => "Overview",
            AppTab::Statistics => "Statistics",
            AppTab::WordFrequency => "Word Frequency",
            AppTab::LineAnalysis => "Line Analysis",
            AppTab::ParagraphAnalysis => "Paragraph Analysis",
            AppTab::Readability => "Readability",
        }
    }

    pub fn all() -> Vec<AppTab> {
        vec![
            AppTab::Overview,
            AppTab::Statistics,
            AppTab::WordFrequency,
            AppTab::LineAnalysis,
            AppTab::ParagraphAnalysis,
            AppTab::Readability,
        ]
    }
}

pub struct App {
    pub should_quit: bool,
    pub current_tab: AppTab,
    pub analysis: Option<TextAnalysis>,
    pub file_handler: FileHandler,
    pub show_help: bool,
    pub show_file_dialog: bool,
    pub file_input: String,
    pub scroll_offset: usize,
}

impl App {
    pub fn new() -> Self {
        App {
            should_quit: false,
            current_tab: AppTab::Overview,
            analysis: None,
            file_handler: FileHandler::new(),
            show_help: false,
            show_file_dialog: false,
            file_input: String::new(),
            scroll_offset: 0,
        }
    }

    pub fn run<B: Backend>(mut self, terminal: &mut Terminal<B>) -> Result<()> {
        let mut last_tick = Instant::now();
        let tick_rate = Duration::from_millis(250);

        loop {
            terminal.draw(|f| self.ui(f))?;

            let timeout = tick_rate
                .checked_sub(last_tick.elapsed())
                .unwrap_or_else(|| Duration::from_secs(0));

            if crossterm::event::poll(timeout)? {
                if let Event::Key(key) = event::read()? {
                    if key.kind == KeyEventKind::Press {
                        self.handle_input(key.code)?;
                    }
                }
            }

            if last_tick.elapsed() >= tick_rate {
                last_tick = Instant::now();
            }

            if self.should_quit {
                break;
            }
        }

        Ok(())
    }

    fn handle_input(&mut self, key: KeyCode) -> Result<()> {
        if self.show_file_dialog {
            match key {
                KeyCode::Enter => {
                    if !self.file_input.is_empty() {
                        let path = PathBuf::from(&self.file_input);
                        match self.file_handler.load_file(path) {
                            Ok((filename, content)) => {
                                self.analysis = Some(TextAnalysis::new(filename, content));
                                self.show_file_dialog = false;
                                self.file_input.clear();
                            }
                            Err(_e) => {
                                // Handle error - for now just close dialog
                                self.show_file_dialog = false;
                                self.file_input.clear();
                            }
                        }
                    }
                }
                KeyCode::Esc => {
                    self.show_file_dialog = false;
                    self.file_input.clear();
                }
                KeyCode::Backspace => {
                    self.file_input.pop();
                }
                KeyCode::Char(c) => {
                    self.file_input.push(c);
                }
                _ => {}
            }
            return Ok(());
        }

        if self.show_help {
            match key {
                KeyCode::Esc | KeyCode::Char('h') => {
                    self.show_help = false;
                }
                _ => {}
            }
            return Ok(());
        }

        match key {
            KeyCode::Char('q') => {
                self.should_quit = true;
            }
            KeyCode::Char('h') => {
                self.show_help = true;
            }
            KeyCode::Char('o') => {
                self.show_file_dialog = true;
            }
            KeyCode::Tab => {
                self.next_tab();
            }
            KeyCode::BackTab => {
                self.previous_tab();
            }
            KeyCode::Up => {
                if self.scroll_offset > 0 {
                    self.scroll_offset -= 1;
                }
            }
            KeyCode::Down => {
                self.scroll_offset += 1;
            }
            KeyCode::Char(c) if c.is_ascii_digit() => {
                let num = c.to_digit(10).unwrap() as usize;
                let tabs = AppTab::all();
                if num > 0 && num <= tabs.len() {
                    self.current_tab = tabs[num - 1].clone();
                    self.scroll_offset = 0;
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn next_tab(&mut self) {
        let tabs = AppTab::all();
        let current_index = tabs.iter().position(|t| *t == self.current_tab).unwrap_or(0);
        let next_index = (current_index + 1) % tabs.len();
        self.current_tab = tabs[next_index].clone();
        self.scroll_offset = 0;
    }

    fn previous_tab(&mut self) {
        let tabs = AppTab::all();
        let current_index = tabs.iter().position(|t| *t == self.current_tab).unwrap_or(0);
        let prev_index = if current_index == 0 {
            tabs.len() - 1
        } else {
            current_index - 1
        };
        self.current_tab = tabs[prev_index].clone();
        self.scroll_offset = 0;
    }

    fn ui(&mut self, f: &mut Frame) {
        let size = f.size();

        // Create main layout
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3), // Title
                Constraint::Length(3), // Tabs
                Constraint::Min(0),    // Content
                Constraint::Length(3), // Status bar
            ])
            .split(size);

        // Title
        let title = Paragraph::new("Text Analyzer - Notepad++ AnalyzePlugin Clone")
            .style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD))
            .block(Block::default().borders(Borders::ALL));
        f.render_widget(title, chunks[0]);

        // Tabs
        let tabs = AppTab::all();
        let tab_titles: Vec<Line> = tabs.iter().map(|t| Line::from(t.title())).collect();
        let current_tab_index = tabs.iter().position(|t| *t == self.current_tab).unwrap_or(0);
        
        let tabs_widget = Tabs::new(tab_titles)
            .block(Block::default().borders(Borders::ALL).title("Navigation"))
            .style(Style::default().fg(Color::White))
            .highlight_style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD))
            .select(current_tab_index);
        f.render_widget(tabs_widget, chunks[1]);

        // Content area
        match &self.analysis {
            Some(analysis) => {
                self.render_analysis_content(f, chunks[2], analysis);
            }
            None => {
                let no_file = Paragraph::new("No file loaded. Press 'o' to open a file.")
                    .style(Style::default().fg(Color::Gray))
                    .wrap(Wrap { trim: true })
                    .block(Block::default().borders(Borders::ALL).title("Content"));
                f.render_widget(no_file, chunks[2]);
            }
        }

        // Status bar
        let status_text = format!(
            "Press 'h' for help | 'o' to open file | 'q' to quit | Tab/Shift+Tab to navigate | ↑↓ to scroll"
        );
        let status = Paragraph::new(status_text)
            .style(Style::default().fg(Color::White))
            .block(Block::default().borders(Borders::ALL));
        f.render_widget(status, chunks[3]);

        // Show overlays
        if self.show_help {
            self.render_help_popup(f, size);
        }

        if self.show_file_dialog {
            self.render_file_dialog(f, size);
        }
    }

    fn render_analysis_content(&self, f: &mut Frame, area: Rect, analysis: &TextAnalysis) {
        match self.current_tab {
            AppTab::Overview => self.render_overview(f, area, analysis),
            AppTab::Statistics => self.render_statistics(f, area, analysis),
            AppTab::WordFrequency => self.render_word_frequency(f, area, analysis),
            AppTab::LineAnalysis => self.render_line_analysis(f, area, analysis),
            AppTab::ParagraphAnalysis => self.render_paragraph_analysis(f, area, analysis),
            AppTab::Readability => self.render_readability(f, area, analysis),
        }
    }

    fn render_overview(&self, f: &mut Frame, area: Rect, analysis: &TextAnalysis) {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(area);

        // File info and quick stats
        let file_info = vec![
            format!("File: {}", analysis.file_name),
            String::new(),
            format!("Characters: {}", analysis.stats.total_characters),
            format!("Characters (no spaces): {}", analysis.stats.total_characters_no_spaces),
            format!("Words: {}", analysis.stats.total_words),
            format!("Lines: {}", analysis.stats.total_lines),
            format!("Paragraphs: {}", analysis.stats.total_paragraphs),
            format!("Sentences: {}", analysis.stats.total_sentences),
            String::new(),
            format!("Unique words: {}", analysis.stats.unique_words),
            format!("Blank lines: {}", analysis.stats.blank_lines),
        ];

        let info_text = file_info.join("\n");
        let info_paragraph = Paragraph::new(info_text)
            .block(Block::default().borders(Borders::ALL).title("File Information"))
            .wrap(Wrap { trim: true });
        f.render_widget(info_paragraph, chunks[0]);

        // Content preview
        let content_preview = if analysis.content.len() > 500 {
            format!("{}...", &analysis.content[..500])
        } else {
            analysis.content.clone()
        };

        let content_paragraph = Paragraph::new(content_preview)
            .block(Block::default().borders(Borders::ALL).title("Content Preview"))
            .wrap(Wrap { trim: true });
        f.render_widget(content_paragraph, chunks[1]);
    }

    fn render_statistics(&self, f: &mut Frame, area: Rect, analysis: &TextAnalysis) {
        let stats = &analysis.stats;
        let stats_text = vec![
            format!("Total Characters: {}", stats.total_characters),
            format!("Characters (no spaces): {}", stats.total_characters_no_spaces),
            format!("Total Words: {}", stats.total_words),
            format!("Total Lines: {}", stats.total_lines),
            format!("Total Paragraphs: {}", stats.total_paragraphs),
            format!("Total Sentences: {}", stats.total_sentences),
            String::new(),
            format!("Average words per line: {:.2}", stats.average_words_per_line),
            format!("Average characters per line: {:.2}", stats.average_characters_per_line),
            format!("Average words per sentence: {:.2}", stats.average_words_per_sentence),
            format!("Average characters per word: {:.2}", stats.average_characters_per_word),
            String::new(),
            format!("Longest line: {} characters", stats.longest_line_length),
            format!("Shortest line: {} characters", stats.shortest_line_length),
            format!("Blank lines: {}", stats.blank_lines),
            format!("Unique words: {}", stats.unique_words),
        ];

        let statistics_text = stats_text.join("\n");
        let statistics_paragraph = Paragraph::new(statistics_text)
            .block(Block::default().borders(Borders::ALL).title("Detailed Statistics"))
            .wrap(Wrap { trim: true });
        f.render_widget(statistics_paragraph, area);
    }

    fn render_word_frequency(&self, f: &mut Frame, area: Rect, analysis: &TextAnalysis) {
        let most_frequent = analysis.get_most_frequent_words(20);
        let items: Vec<ListItem> = most_frequent
            .iter()
            .enumerate()
            .map(|(i, (word, count))| {
                ListItem::new(format!("{:2}. {} ({})", i + 1, word, count))
            })
            .collect();

        let list = List::new(items)
            .block(Block::default().borders(Borders::ALL).title("Most Frequent Words"))
            .style(Style::default().fg(Color::White));
        f.render_widget(list, area);
    }

    fn render_line_analysis(&self, f: &mut Frame, area: Rect, analysis: &TextAnalysis) {
        let visible_lines = area.height.saturating_sub(2) as usize;
        let start_line = self.scroll_offset;
        let end_line = (start_line + visible_lines).min(analysis.line_stats.len());


        let items: Vec<ListItem> = analysis.line_stats[start_line..end_line]
            .iter()
            .map(|line_stat| {
                let status = if line_stat.is_blank { " (blank)" } else { "" };
                ListItem::new(format!(
                    "Line {}: {} chars, {} words{}",
                    line_stat.line_number,
                    line_stat.character_count,
                    line_stat.word_count,
                    status
                ))
            })
            .collect();

        let list = List::new(items)
            .block(Block::default().borders(Borders::ALL).title("Line Analysis"))
            .style(Style::default().fg(Color::White));
        f.render_widget(list, area);
    }

    fn render_paragraph_analysis(&self, f: &mut Frame, area: Rect, analysis: &TextAnalysis) {
        let visible_paragraphs = area.height.saturating_sub(2) as usize;
        let start_para = self.scroll_offset;
        let end_para = (start_para + visible_paragraphs).min(analysis.paragraph_stats.len());

        let items: Vec<ListItem> = analysis.paragraph_stats[start_para..end_para]
            .iter()
            .map(|para_stat| {
                ListItem::new(format!(
                    "Para {}: {} lines, {} chars, {} words, {} sentences",
                    para_stat.paragraph_number,
                    para_stat.line_count,
                    para_stat.character_count,
                    para_stat.word_count,
                    para_stat.sentence_count
                ))
            })
            .collect();

        let list = List::new(items)
            .block(Block::default().borders(Borders::ALL).title("Paragraph Analysis"))
            .style(Style::default().fg(Color::White));
        f.render_widget(list, area);
    }

    fn render_readability(&self, f: &mut Frame, area: Rect, analysis: &TextAnalysis) {
        let readability = analysis.get_readability_metrics();
        
        let readability_text = vec![
            format!("Flesch Reading Ease: {:.1}", readability.flesch_reading_ease),
            format!("Flesch-Kincaid Grade Level: {:.1}", readability.flesch_kincaid_grade),
            format!("Readability: {}", readability.readability_description),
            String::new(),
            "Flesch Reading Ease Scale:".to_string(),
            "90-100: Very Easy".to_string(),
            "80-89: Easy".to_string(),
            "70-79: Fairly Easy".to_string(),
            "60-69: Standard".to_string(),
            "50-59: Fairly Difficult".to_string(),
            "30-49: Difficult".to_string(),
            "0-29: Very Difficult".to_string(),
        ];

        let readability_paragraph = Paragraph::new(readability_text.join("\n"))
            .block(Block::default().borders(Borders::ALL).title("Readability Analysis"))
            .wrap(Wrap { trim: true });
        f.render_widget(readability_paragraph, area);
    }

    fn render_help_popup(&self, f: &mut Frame, area: Rect) {
        let popup_area = Self::centered_rect(60, 20, area);
        
        let help_text = vec![
            "Text Analyzer - Help",
            "",
            "Keyboard Shortcuts:",
            "  h - Show/hide this help",
            "  o - Open file",
            "  q - Quit application",
            "  Tab - Next tab",
            "  Shift+Tab - Previous tab",
            "  ↑/↓ - Scroll content",
            "  1-6 - Jump to specific tab",
            "",
            "Tabs:",
            "  1. Overview - File info and preview",
            "  2. Statistics - Detailed text statistics",
            "  3. Word Frequency - Most common words",
            "  4. Line Analysis - Line-by-line breakdown",
            "  5. Paragraph Analysis - Paragraph statistics",
            "  6. Readability - Reading level metrics",
            "",
            "Press 'h' or Esc to close this help",
        ];

        f.render_widget(Clear, popup_area);
        let help_paragraph = Paragraph::new(help_text.join("\n"))
            .block(Block::default().borders(Borders::ALL).title("Help"))
            .style(Style::default().fg(Color::White).bg(Color::DarkGray))
            .wrap(Wrap { trim: true });
        f.render_widget(help_paragraph, popup_area);
    }

    fn render_file_dialog(&self, f: &mut Frame, area: Rect) {
        let popup_area = Self::centered_rect(60, 7, area);
        
        f.render_widget(Clear, popup_area);
        
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Length(3), Constraint::Length(3)])
            .split(popup_area.inner(&Margin::new(1, 1)));

        let instruction = Paragraph::new("Enter file path:")
            .style(Style::default().fg(Color::White));
        f.render_widget(instruction, chunks[0]);

        let input = Paragraph::new(self.file_input.clone())
            .style(Style::default().fg(Color::Yellow))
            .block(Block::default().borders(Borders::ALL));
        f.render_widget(input, chunks[1]);

        let block = Block::default()
            .borders(Borders::ALL)
            .title("Open File")
            .style(Style::default().bg(Color::DarkGray));
        f.render_widget(block, popup_area);
    }

    fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
        let popup_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Percentage((100 - percent_y) / 2),
                Constraint::Percentage(percent_y),
                Constraint::Percentage((100 - percent_y) / 2),
            ])
            .split(r);

        Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage((100 - percent_x) / 2),
                Constraint::Percentage(percent_x),
                Constraint::Percentage((100 - percent_x) / 2),
            ])
            .split(popup_layout[1])[1]
    }
}

pub fn run() -> Result<()> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app and run it
    let app = App::new();
    let res = app.run(&mut terminal);

    // Restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{err:?}");
    }

    Ok(())
}
