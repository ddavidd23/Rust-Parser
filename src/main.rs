#![deny(unsafe_code)]

use std::collections::HashMap;
use std::env;
use std::error;
use std::fs;
use std::io::{Read, stdin};
use std::panic;


macro_rules! die {
    ($($arg:tt)*) => {
        eprintln!("proj3: {}", format_args!($($arg)*));
        panic!();
    };
}

enum PreprocState {
    Plain,
    CommentLine1,
    CommentLine2 // after newline
}

#[derive(Copy, Clone, PartialEq)]
enum State {
    Plain,
    CallMacro,
    DefMacroName,
    DefArg,
    CustomMacroArg,
    Undef,
    Include,
    ExpandAfterArg1,
    ExpandAfterArg2,
    IfCond,
    Then,
    Else,
    IfDefCond
}

fn preproc_text(input_text: String) -> String {
    let mut state = PreprocState::Plain;
    let mut prev_is_escaped = false; // Whether previous character is escaped
    let mut preprocessed_str = String::new();

    for c in input_text.chars() {
        match state {
            PreprocState::Plain => {
                if c == '%' && !prev_is_escaped {
                    state = PreprocState::CommentLine1;
                } else if c == '\\' {
                    prev_is_escaped = !prev_is_escaped; // Toggle escape state
                    preprocessed_str.push(c);
                } else {
                    preprocessed_str.push(c);
                    prev_is_escaped = false; // Reset escape state if not a backslash
                }
            },
            PreprocState::CommentLine1 => {
                if c == '\n' {
                    state = PreprocState::CommentLine2;
                    prev_is_escaped = false; // Reset escape state at newline
                }
            },
            PreprocState::CommentLine2 => {
                if c != ' ' && c != '\t' {
                    state = PreprocState::Plain;
                    preprocessed_str.push(c);
                } else if c == '\n' {
                    // Stay in CommentLine2 state but reset escape state
                    prev_is_escaped = false;
                }
            }
        }

        if c != '\\' {
            prev_is_escaped = false;
        }
    }

    return preprocessed_str;
    // let reversed_str = preprocessed_str.chars().rev().collect();
    // return reversed_str;
}

fn expand_macro(map: &mut HashMap<String, String>, macro_name: &String, arg: &String) -> Result<String, String> {
    let mut expanded = String::new();
    if let Some(macro_val) = map.get(macro_name) {
        let mut prev_is_escaping_backslash = false;
        for c in macro_val.chars() {
            if c == '#' && !prev_is_escaping_backslash {
                expanded.push_str(&arg);
            } else {
                expanded.push(c);
            }

            if c == '\\' && !prev_is_escaping_backslash {
                prev_is_escaping_backslash = true;
            } else {
                prev_is_escaping_backslash = false;
            }
        }
    } else {
        return Err("Macro not defined.".to_string());
    }

    Ok(expanded)
}

fn process_str(map: &mut HashMap<String, String>, input: &mut String) -> Result<String, String> {
    let mut prev_state = State::Plain;
    let mut state = State::Plain;

    let mut c: Option<char>;
    let mut u: char;
    
    let mut output = String::new();
    let mut macro_name = String::new(); // Used both for macro call and storing macro names in def and undef
    let mut arg = String::new();

    let mut brace_count = 0;
    let mut cond_count = 0;

    let mut prev_is_escaping_backslash = false;
    let mut update_prev_state = true;
    let mut cond_is_empty = false;

    loop {
        c = input.pop();
        if c.is_none() {
            break;
        }
        u = c.unwrap();
        match (state, u, prev_is_escaping_backslash) {

            /*
                Plaintext
            */
            (State::Plain, '\\', false)  => {
                prev_state = state;
                update_prev_state = false;
                state = State::CallMacro;
            },
            (State::Plain, _, _) => output.push(u),
            
            /*
                Encountered backslash, now calling macro
            */
            (State::CallMacro, _, true) => {
                if u == '\\' || u == '#' || u == '%' || u == '{' || u == '}' {
                    output.push(u);
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Plain;
                } else if !u.is_alphanumeric() {
                    output.push('\\');
                    output.push(u);
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Plain;
                } else if prev_state == State::Plain { macro_name.push(u) }
            },
            (State::CallMacro, '{', false) => {
                brace_count += 1;
                prev_state = state;
                update_prev_state = false;
                if macro_name == "def" {
                    state = State::DefMacroName;
                    macro_name.clear();
                } else if macro_name == "undef" {
                    state = State::Undef;
                    macro_name.clear();
                } else if macro_name == "include" {
                    state = State::Include;
                    macro_name.clear();
                } else if macro_name == "expandafter" {
                    state = State::ExpandAfterArg1;
                    macro_name.clear();
                } else if macro_name == "if" { 
                    state = State::IfCond;
                    macro_name.clear();
                } else if macro_name == "ifdef" {
                    state = State::IfDefCond;
                    macro_name.clear();
                } else { state = State::CustomMacroArg }
            },
            (State::CallMacro, _, false) => {
                // println!("{}", u);
                if u.is_alphanumeric() { macro_name.push(u) }
                else { return Err("Non-alphanumeric in macro name".to_string()) }
            },

            /*
                Called \def, now defining name of macro
                Only comes from State::CallMacro
                Only goes to State::DefArg
            */
            (State::DefMacroName, '}', _) => {
                brace_count -= 1;
                prev_state = state;
                update_prev_state = false;
                state = State::DefArg;
            },
            (State::DefMacroName, _, _) => {
                if !u.is_alphanumeric() { return Err("Non-alphanumeric while defining macro name.".to_string()) }
                else { macro_name.push(u) }
            },

            /*
                Defined name of macro in DefMacroName, now defining arguments
                Only comes from State::DefMacroName
                Only goes to State::Plain
            */
            (State::DefArg, '}', false) => {
                brace_count -= 1;
                if brace_count != 0 { arg.push(u) }
                else {
                    if map.contains_key(&macro_name) { return Err("Macro already defined.".to_string()) }
                    map.insert(macro_name.clone(), arg.clone());
                    macro_name.clear();
                    arg.clear();
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Plain;
                }
            },
            (State::DefArg, '{', false) => {
                brace_count += 1;
                if prev_state != State::DefMacroName { arg.push(u) }
            },
            (State::DefArg, _, _) => {
                if prev_state == State::DefMacroName { return Err("Incomplete macro.".to_string()) }
                arg.push(u)
            }

            /*
                Inputting arguments to defined macro
                Only comes from State::CallMacro
                Only goes to State::Plain
            */
            (State::CustomMacroArg, '}', false) => {
                brace_count -= 1;
                if brace_count == 0 {
                    let expanded: String = expand_macro(map, &macro_name, &arg)?.chars().rev().collect();
                    input.push_str(&expanded);
                    macro_name.clear();
                    arg.clear();
                    update_prev_state = false;
                    prev_state = state;
                    state = State::Plain;
                } else { arg.push(u) }
            }
            (State::CustomMacroArg, '{', false) => {
                brace_count += 1;
                arg.push(u);
            },
            (State::CustomMacroArg, _, _) => arg.push(u),

            /*
                Undef
                Only comes from State::Macro
                Only goes to State::Plain
            */
            (State::Undef, '}', false) => {
                brace_count -= 1;
                if brace_count == 0 {
                    if let None = map.remove(&macro_name) {
                        return Err("Macro not defined.".to_string());
                    }
                    macro_name.clear();
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Plain;
                } else {
                    return Err("Incomplete macro.".to_string());
                }
            },
            (State::Undef, _, _) => {
                if !u.is_alphanumeric() {
                    return Err("Non-alphanumeric in un-define.".to_string());
                } else {
                    macro_name.push(u);
                }
            }

            /*
                Include
                Only comes from State::Macro
                Only goes to State::Plain
            */
            (State::Include, '}', false) => {
                brace_count -= 1;
                // match required here because Error<String> is error return type
                if brace_count == 0 {
                    let file_content_result = fs::read_to_string(&arg);
                    match file_content_result {
                        Ok(file_content) => {
                            let preprocessed: String = preproc_text(file_content).chars().rev().collect(); // Already reversed
                            input.push_str(&preprocessed);
                            arg.clear();
                            prev_state = state;
                            update_prev_state = false;
                            state = State::Plain;
                        },
                        Err(_) => {
                            return Err("Include error.".to_string());
                        }
                    }
                } else { arg.push(u) }
            },
            (State::Include, '{', false) => brace_count += 1,
            (State::Include, _, _) => arg.push(u),

            /*
                First argument of expandafter
                Only comes from State::CallMacro
                Only goes to ExpandAfterArg2
            */
            (State::ExpandAfterArg1, '}', false) => {
                brace_count -= 1;
                if brace_count == 0 { 
                    prev_state = state;
                    update_prev_state = false;
                    state = State::ExpandAfterArg2;
                } else { macro_name.push(u) }
            },
            (State::ExpandAfterArg1, '{', false) => {
                brace_count += 1;
                macro_name.push(u);
            },
            (State::ExpandAfterArg1, _, _) => macro_name.push(u),

            /*
                Second argument of expandafter
            */
            (State::ExpandAfterArg2, '}', false) => {
                brace_count -= 1;
                if brace_count == 0 {
                    let mut reversed_arg2: String = arg.chars().rev().collect();
                    let processed_arg2: String = process_str(map, &mut reversed_arg2)?.chars().rev().collect();
                    input.push_str(&processed_arg2);
                    arg.clear();
                    
                    let reversed_arg1: String = macro_name.chars().rev().collect();
                    input.push_str(&reversed_arg1);
                    macro_name.clear();
                    
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Plain;
                } else { arg.push(u) }
            },
            (State::ExpandAfterArg2, '{', false) => {
                brace_count += 1;
                if prev_state != State::ExpandAfterArg1 { arg.push(u) }
            },
            (State::ExpandAfterArg2, _, _) => arg.push(u),

            /*
                Condition for if
            */
            (State::IfCond, '}', false) => {
                brace_count -= 1;
                if brace_count == 0 {
                    cond_is_empty = if cond_count == 0 { true } else { false };
                    cond_count = 0;
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Then;
                } else { cond_count += 1}
            },
            (State::IfCond, '{', false) => {
                brace_count += 1;
                cond_count += 1;
            },
            (State::IfCond, _, _) => cond_count += 1,

            /*
                Then for if and ifdef
            */
            (State::Then, '}', false) => {
                brace_count -= 1;
                if brace_count == 0 {
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Else;
                } else if !cond_is_empty { macro_name.push(u) }
            },
            (State::Then, '{', false) => {
                brace_count += 1;
                if !cond_is_empty && prev_state != State::IfCond && prev_state != State::IfDefCond { macro_name.push(u) }
            },
            (State::Then, _, _) => {
                if prev_state == State::IfCond || prev_state == State::IfDefCond { return Err("Incomplete macro.".to_string()) }
                if !cond_is_empty { macro_name.push(u) }
            },

            /*
                Else for if and ifdef
            */
            (State::Else, '}', false) => {
                brace_count -= 1;
                if brace_count == 0 {
                    let reversed: String = macro_name.chars().rev().collect();
                    input.push_str(&reversed);
                    macro_name.clear();
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Plain;
                } else if cond_is_empty { macro_name.push(u) }
            },
            (State::Else, '{', false) => {
                brace_count += 1;
                if cond_is_empty && prev_state != State::Then { macro_name.push(u) }
            },
            (State::Else, _, _) => {
                if prev_state == State::Then { return Err("Incomplete macro.".to_string()) }
                if cond_is_empty { macro_name.push(u) }
            },

            /*
                Ifdef
            */
            (State::IfDefCond, '}', false) => {
                brace_count -= 1;
                if brace_count == 0 {
                    cond_is_empty = !map.contains_key(&macro_name);
                    macro_name.clear();
                    prev_state = state;
                    update_prev_state = false;
                    state = State::Then;
                } else { cond_count += 1}
            },
            (State::IfDefCond, '{', false) => {
                brace_count += 1;
                macro_name.push(u);
            },
            (State::IfDefCond, _, _) => { macro_name.push(u) }
        }

        /*
            Update prev_is_escaping_backslash: true iff u is a backslash and previous character
            is not an escaping backslash (thus not making u an escape character)
        */
        if u == '\\' && !prev_is_escaping_backslash { prev_is_escaping_backslash = true}
        else { prev_is_escaping_backslash = false }
        
        if update_prev_state {
            prev_state = state
        }
        update_prev_state = true; // Reset to true if update_prev_state = false was set
    }
    if state != State::Plain || brace_count != 0 {
        if state == State::CallMacro && prev_is_escaping_backslash {
            output.push('\\');
        } else {
            return Err("Incomplete macro.".to_string());
        }
    }
    Ok(output)
}

fn read_file(args: Vec<String>) -> Result<(), Box<dyn error::Error>> {
    let mut preprocessed = String::new();
    if args.is_empty() {
        let mut content = String::new();
        stdin().read_to_string(&mut content)?;
        preprocessed = preproc_text(content);
    } else {
        for arg in args {
            let preprocessed_file = preproc_text(fs::read_to_string(arg)?);
            preprocessed.push_str(&preprocessed_file);
        }
    }
    preprocessed = preprocessed.chars().rev().collect();
    let mut map: HashMap<String, String> = HashMap::new();
    let output = process_str(&mut map, &mut preprocessed)?;
    print!("{}", output); // No newline
    // for (key, value) in map {
    //     println!("{}: {}", key, value);
    // }
    Ok(())
}

fn main() {
    panic::set_hook(Box::new(|_| { }));

    let args: Vec<String> = env::args().skip(1).collect();
    if let Err(e) = read_file(args) {
        die!("{}", e);
    }
}
