use anyhow::{Context, Result};
use std::fs;
use std::path::PathBuf;

pub struct FileHandler {
    current_file: Option<PathBuf>,
}

impl FileHandler {
    pub fn new() -> Self {
        FileHandler {
            current_file: None,
        }
    }

    pub fn load_file(&mut self, path: PathBuf) -> Result<(String, String)> {
        // Check if file exists
        if !path.exists() {
            return Err(anyhow::anyhow!("File does not exist: {}", path.display()));
        }

        // Check if it's a file (not a directory)
        if !path.is_file() {
            return Err(anyhow::anyhow!("Path is not a file: {}", path.display()));
        }

        // Read file content
        let content = fs::read_to_string(&path)
            .with_context(|| format!("Failed to read file: {}", path.display()))?;

        // Get filename
        let filename = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("Unknown")
            .to_string();

        self.current_file = Some(path);

        Ok((filename, content))
    }

    pub fn get_current_file(&self) -> Option<&PathBuf> {
        self.current_file.as_ref()
    }

    pub fn save_analysis_report(&self, analysis: &crate::analyzer::TextAnalysis, output_path: PathBuf) -> Result<()> {
        let report = self.generate_report(analysis);
        fs::write(&output_path, report)
            .with_context(|| format!("Failed to save report to: {}", output_path.display()))?;
        Ok(())
    }

    fn generate_report(&self, analysis: &crate::analyzer::TextAnalysis) -> String {
        let stats = &analysis.stats;
        let readability = analysis.get_readability_metrics();
        let top_words = analysis.get_most_frequent_words(10);

        format!(
            r#"Text Analysis Report
====================

File: {}

Basic Statistics:
- Total Characters: {}
- Characters (no spaces): {}
- Total Words: {}
- Total Lines: {}
- Total Paragraphs: {}
- Total Sentences: {}
- Blank Lines: {}
- Unique Words: {}

Averages:
- Words per line: {:.2}
- Characters per line: {:.2}
- Words per sentence: {:.2}
- Characters per word: {:.2}

Line Length:
- Longest line: {} characters
- Shortest line: {} characters

Readability Metrics:
- Flesch Reading Ease: {:.1}
- Flesch-Kincaid Grade Level: {:.1}
- Readability Level: {}

Most Frequent Words:
{}

Analysis completed with Text Analyzer
"#,
            analysis.file_name,
            stats.total_characters,
            stats.total_characters_no_spaces,
            stats.total_words,
            stats.total_lines,
            stats.total_paragraphs,
            stats.total_sentences,
            stats.blank_lines,
            stats.unique_words,
            stats.average_words_per_line,
            stats.average_characters_per_line,
            stats.average_words_per_sentence,
            stats.average_characters_per_word,
            stats.longest_line_length,
            stats.shortest_line_length,
            readability.flesch_reading_ease,
            readability.flesch_kincaid_grade,
            readability.readability_description,
            top_words
                .iter()
                .enumerate()
                .map(|(i, (word, count))| format!("{}. {} ({})", i + 1, word, count))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }

    /// Get file extension and suggest appropriate analysis
    pub fn get_file_info(path: &PathBuf) -> Option<FileInfo> {
        if let Some(extension) = path.extension().and_then(|ext| ext.to_str()) {
            let file_type = match extension.to_lowercase().as_str() {
                "txt" => FileType::PlainText,
                "md" | "markdown" => FileType::Markdown,
                "rs" => FileType::Rust,
                "py" => FileType::Python,
                "js" | "ts" => FileType::JavaScript,
                "html" | "htm" => FileType::Html,
                "xml" => FileType::Xml,
                "json" => FileType::Json,
                "log" => FileType::Log,
                _ => FileType::Other,
            };

            Some(FileInfo {
                file_type: file_type.clone(),
                extension: extension.to_string(),
                suitable_for_analysis: matches!(
                    file_type,
                    FileType::PlainText | FileType::Markdown | FileType::Log
                ),
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct FileInfo {
    pub file_type: FileType,
    pub extension: String,
    pub suitable_for_analysis: bool,
}

#[derive(Debug, Clone)]
pub enum FileType {
    PlainText,
    Markdown,
    Rust,
    Python,
    JavaScript,
    Html,
    Xml,
    Json,
    Log,
    Other,
}

impl FileType {
    pub fn description(&self) -> &'static str {
        match self {
            FileType::PlainText => "Plain Text",
            FileType::Markdown => "Markdown",
            FileType::Rust => "Rust Source Code",
            FileType::Python => "Python Source Code",
            FileType::JavaScript => "JavaScript/TypeScript",
            FileType::Html => "HTML",
            FileType::Xml => "XML",
            FileType::Json => "JSON",
            FileType::Log => "Log File",
            FileType::Other => "Other",
        }
    }
}