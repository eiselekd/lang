mod analyzer;
mod app;
mod file_handler;

use anyhow::Result;
use clap::{Arg, Command};
use std::path::PathBuf;

fn main() -> Result<()> {
    let matches = Command::new("text-analyzer")
        .version("0.1.0")
        .author("Your Name")
        .about("A text analysis tool similar to Notepad++ AnalyzePlugin")
        .arg(
            Arg::new("file")
                .help("Text file to analyze")
                .value_name("FILE")
                .index(1),
        )
        .arg(
            Arg::new("report")
                .short('r')
                .long("report")
                .help("Generate a text report and save to file")
                .value_name("OUTPUT"),
        )
        .get_matches();

    // If a file is provided as command line argument
    if let Some(file_path) = matches.get_one::<String>("file") {
        let path = PathBuf::from(file_path);
        
        // If report flag is provided, generate report and exit
        if let Some(output_path) = matches.get_one::<String>("report") {
            return generate_report(path, PathBuf::from(output_path));
        }
        
        // Otherwise, run TUI with the file pre-loaded
        return run_with_file(path);
    }

    // Run TUI without pre-loaded file
    app::run()
}

fn generate_report(input_path: PathBuf, output_path: PathBuf) -> Result<()> {
    let mut file_handler = file_handler::FileHandler::new();
    let (filename, content) = file_handler.load_file(input_path)?;
    
    let analysis = analyzer::TextAnalysis::new(filename, content);
    file_handler.save_analysis_report(&analysis, output_path.clone())?;
    
    println!("Analysis report saved to: {}", output_path.display());
    Ok(())
}

fn run_with_file(file_path: PathBuf) -> Result<()> {
    // This would be implemented to pre-load a file in the TUI
    // For now, just run the normal TUI
    println!("Loading file: {}", file_path.display());
    app::run()
}
