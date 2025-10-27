use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TextAnalysis {
    pub file_name: String,
    pub content: String,
    pub stats: TextStats,
    pub word_frequency: HashMap<String, usize>,
    pub line_stats: Vec<LineStats>,
    pub paragraph_stats: Vec<ParagraphStats>,
}

#[derive(Debug, Clone)]
pub struct TextStats {
    pub total_characters: usize,
    pub total_characters_no_spaces: usize,
    pub total_words: usize,
    pub total_lines: usize,
    pub total_paragraphs: usize,
    pub total_sentences: usize,
    pub average_words_per_line: f64,
    pub average_characters_per_line: f64,
    pub average_words_per_sentence: f64,
    pub average_characters_per_word: f64,
    pub longest_line_length: usize,
    pub shortest_line_length: usize,
    pub blank_lines: usize,
    pub unique_words: usize,
}

#[derive(Debug, Clone)]
pub struct LineStats {
    pub line_number: usize,
    pub character_count: usize,
    pub word_count: usize,
    pub is_blank: bool,
}

#[derive(Debug, Clone)]
pub struct ParagraphStats {
    pub paragraph_number: usize,
    pub line_count: usize,
    pub character_count: usize,
    pub word_count: usize,
    pub sentence_count: usize,
}

impl TextAnalysis {
    pub fn new(file_name: String, content: String) -> Self {
        let mut analysis = TextAnalysis {
            file_name,
            content: content.clone(),
            stats: TextStats::default(),
            word_frequency: HashMap::new(),
            line_stats: Vec::new(),
            paragraph_stats: Vec::new(),
        };
        
        analysis.analyze();
        analysis
    }

    fn analyze(&mut self) {
        self.analyze_lines();
        self.analyze_paragraphs();
        self.analyze_words();
        self.calculate_stats();
    }

    fn analyze_lines(&mut self) {
        for (line_number, line) in self.content.lines().enumerate() {
            let character_count = line.chars().count();
            let word_count = line.split_whitespace().count();
            let is_blank = line.trim().is_empty();

            self.line_stats.push(LineStats {
                line_number: line_number + 1,
                character_count,
                word_count,
                is_blank,
            });
        }
    }

    fn analyze_paragraphs(&mut self) {
        let paragraphs: Vec<&str> = self.content
            .split("\n\n")
            .filter(|p| !p.trim().is_empty())
            .collect();

        for (paragraph_number, paragraph) in paragraphs.iter().enumerate() {
            let lines: Vec<&str> = paragraph.lines().collect();
            let line_count = lines.len();
            let character_count = paragraph.chars().count();
            let word_count = paragraph.split_whitespace().count();
            
            // Simple sentence counting (periods, exclamation marks, question marks)
            let sentence_regex = Regex::new(r"[.!?]+").unwrap();
            let sentence_count = sentence_regex.find_iter(paragraph).count();

            self.paragraph_stats.push(ParagraphStats {
                paragraph_number: paragraph_number + 1,
                line_count,
                character_count,
                word_count,
                sentence_count,
            });
        }
    }

    fn analyze_words(&mut self) {
        let words: Vec<&str> = self.content
            .split_whitespace()
            .map(|word| word.trim_matches(|c: char| !c.is_alphabetic()))
            .filter(|word| !word.is_empty())
            .collect();

        for word in words {
            let lowercase_word = word.to_lowercase();
            *self.word_frequency.entry(lowercase_word).or_insert(0) += 1;
        }
    }

    fn calculate_stats(&mut self) {
        let total_characters = self.content.chars().count();
        let total_characters_no_spaces = self.content.chars().filter(|c| !c.is_whitespace()).count();
        let total_words = self.word_frequency.values().sum();
        let total_lines = self.line_stats.len();
        let total_paragraphs = self.paragraph_stats.len();
        let blank_lines = self.line_stats.iter().filter(|l| l.is_blank).count();
        let unique_words = self.word_frequency.len();

        // Calculate sentence count
        let sentence_regex = Regex::new(r"[.!?]+").unwrap();
        let total_sentences = sentence_regex.find_iter(&self.content).count().max(1);

        // Line length statistics
        let line_lengths: Vec<usize> = self.line_stats
            .iter()
            .filter(|l| !l.is_blank)
            .map(|l| l.character_count)
            .collect();

        let longest_line_length = line_lengths.iter().max().copied().unwrap_or(0);
        let shortest_line_length = line_lengths.iter().min().copied().unwrap_or(0);

        // Calculate averages
        let average_words_per_line = if total_lines > 0 {
            total_words as f64 / total_lines as f64
        } else {
            0.0
        };

        let average_characters_per_line = if total_lines > 0 {
            total_characters as f64 / total_lines as f64
        } else {
            0.0
        };

        let average_words_per_sentence = if total_sentences > 0 {
            total_words as f64 / total_sentences as f64
        } else {
            0.0
        };

        let average_characters_per_word = if total_words > 0 {
            total_characters_no_spaces as f64 / total_words as f64
        } else {
            0.0
        };

        self.stats = TextStats {
            total_characters,
            total_characters_no_spaces,
            total_words,
            total_lines,
            total_paragraphs,
            total_sentences,
            average_words_per_line,
            average_characters_per_line,
            average_words_per_sentence,
            average_characters_per_word,
            longest_line_length,
            shortest_line_length,
            blank_lines,
            unique_words,
        };
    }

    pub fn get_most_frequent_words(&self, limit: usize) -> Vec<(String, usize)> {
        let mut word_vec: Vec<(String, usize)> = self.word_frequency
            .iter()
            .map(|(word, count)| (word.clone(), *count))
            .collect();
        
        word_vec.sort_by(|a, b| b.1.cmp(&a.1));
        word_vec.into_iter().take(limit).collect()
    }

    pub fn get_readability_metrics(&self) -> ReadabilityMetrics {
        ReadabilityMetrics::calculate(&self.stats)
    }
}

impl Default for TextStats {
    fn default() -> Self {
        TextStats {
            total_characters: 0,
            total_characters_no_spaces: 0,
            total_words: 0,
            total_lines: 0,
            total_paragraphs: 0,
            total_sentences: 0,
            average_words_per_line: 0.0,
            average_characters_per_line: 0.0,
            average_words_per_sentence: 0.0,
            average_characters_per_word: 0.0,
            longest_line_length: 0,
            shortest_line_length: 0,
            blank_lines: 0,
            unique_words: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReadabilityMetrics {
    pub flesch_reading_ease: f64,
    pub flesch_kincaid_grade: f64,
    pub readability_description: String,
}

impl ReadabilityMetrics {
    pub fn calculate(stats: &TextStats) -> Self {
        if stats.total_sentences == 0 || stats.total_words == 0 {
            return ReadabilityMetrics {
                flesch_reading_ease: 0.0,
                flesch_kincaid_grade: 0.0,
                readability_description: "Unable to calculate".to_string(),
            };
        }

        let avg_sentence_length = stats.total_words as f64 / stats.total_sentences as f64;
        let avg_syllables_per_word = stats.average_characters_per_word * 0.5; // Rough approximation

        // Flesch Reading Ease
        let flesch_reading_ease = 206.835 
            - (1.015 * avg_sentence_length) 
            - (84.6 * avg_syllables_per_word);

        // Flesch-Kincaid Grade Level
        let flesch_kincaid_grade = (0.39 * avg_sentence_length) 
            + (11.8 * avg_syllables_per_word) 
            - 15.59;

        let readability_description = match flesch_reading_ease {
            x if x >= 90.0 => "Very Easy",
            x if x >= 80.0 => "Easy",
            x if x >= 70.0 => "Fairly Easy",
            x if x >= 60.0 => "Standard",
            x if x >= 50.0 => "Fairly Difficult",
            x if x >= 30.0 => "Difficult",
            _ => "Very Difficult",
        }.to_string();

        ReadabilityMetrics {
            flesch_reading_ease,
            flesch_kincaid_grade,
            readability_description,
        }
    }
}